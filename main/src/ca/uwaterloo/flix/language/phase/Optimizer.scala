/*
 * Copyright 2017 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.{BinaryOperator, SimplifiedAst, Symbol, Type, UnaryOperator, SourceLocation}
import ca.uwaterloo.flix.language.ast.SimplifiedAst.{Expression, FormalParam, FreeVar}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

/**
  * The Optimization phase performs intra-procedural optimizations.
  *
  * Specifically,
  *
  * - Elimination of run-time tag checks for Unit.
  * - Elimination of run-time tag checks of single-valued enums.
  * - Elimination of dead branches (e.g. if (true) e1 else e2).
  * - Copy propagation (e.g. let z = w; let y = z; let x = y; x -> w)
  * - Redundant branching (e.g. if(c1, if(c2, e2, e3), e3) -> if (c1 && c2, e2, e3))
  */
object Optimizer extends Phase[SimplifiedAst.Root, SimplifiedAst.Root] {

  /**
    * Returns an optimized version of the given AST `root`.
    */
  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = {

    /**
      * Returns `true` if and only if `enum` is a single-valued enum.
      */
    def isSingleCaseEnum(enum: Symbol.EnumSym): Boolean = root.enums.get(enum) match {
      case None => throw new Exception("Enum reference does not exist!")
      case Some(defn) => defn.cases.size == 1
    }

    /**
      * Returns the type of the first case in `enum`.
      */
    def getFirstCaseType(enum: Symbol.EnumSym): Type = root.enums.get(enum) match {
      case None => throw new Exception("Enum reference does not exist!")
      case Some(defn) => defn.cases.values.iterator.next().tpe
    }

    /**
      * Adjusts the type of each element in `fs` to remove any references to single-valued enums.
      *
      * That is, applies `adjustFormalParam` to each element in `fs` and returns the results in a List.
      */
    def adjustFormalParams(fs: List[SimplifiedAst.FormalParam]): List[SimplifiedAst.FormalParam] = fs.map(adjustFormalParam)

    /**
      * Adjusts the type of `f0` to remove any references to single-valued enums.
      */
    def adjustFormalParam(f0: SimplifiedAst.FormalParam): SimplifiedAst.FormalParam = f0 match {
      case FormalParam(sym, tpe) => FormalParam(sym, adjustType(tpe))
    }

    /**
      * Adjusts the type of each element in `fs` to remove any references to single-valued enums.
      */
    def adjustFreeVars(fs: List[FreeVar]): List[FreeVar] = fs map {case FreeVar(sym, tpe) => FreeVar(sym, adjustType(tpe))}

    /**
      * Adjusts `t0` to remove any references to single-valued enums.
      */
    def adjustType(t0: Type): Type = t0 match {
      case Type.Enum(sym, kind) => if (isSingleCaseEnum(sym)) adjustType(getFirstCaseType(sym)) else t0
      case Type.Apply(t, ts) =>
        t match {
          case Type.Enum(sym, kind) => if (isSingleCaseEnum(sym)) adjustType(getFirstCaseType(sym)) else Type.Apply(t, ts.map(adjustType))
          case _ => Type.Apply(t, ts.map(adjustType))
        }
      case _ => t0
    }

    /**
      * Converts the given Boolean `b0` to an Expression.
      */
    def toExp(b0: Boolean): Expression = if (b0) Expression.True else Expression.False

    /**
      * Applies the UnaryOperator.LogicalNot operator to the given Expression `e0`.
      */
    def not(e0: Expression, loc: SourceLocation): Expression = Expression.Unary(UnaryOperator.LogicalNot, e0, Type.Bool, loc)

    /**
      * Applies the BinaryOperator.LogicalAnd operator to the given Expressions `e1` and `e2`.
      */
    def and(e1: Expression, e2: Expression, loc: SourceLocation): Expression = Expression.Binary(BinaryOperator.LogicalAnd, e1, e2, Type.Bool, loc)

    /**
      * Performs a single pass optimization on the given Expression.Binary `e0`.
      */
    def optimizeBinaryExpression(e0: Expression.Binary): Expression = e0 match {
      case Expression.Binary(op, exp1, exp2, tpe, loc) =>
        val e1 = optimize(exp1)
        val e2 = optimize(exp2)
        (op, e1, e2) match {
          // Optimizations for Bool.
          case (BinaryOperator.LogicalAnd, Expression.True, exp) => exp
          case (BinaryOperator.LogicalAnd, exp, Expression.True) => exp
          case (BinaryOperator.LogicalOr, Expression.False, exp) => exp
          case (BinaryOperator.LogicalOr, exp, Expression.False) => exp
          case (BinaryOperator.LogicalAnd, Expression.False, _) | (BinaryOperator.LogicalAnd, _, Expression.False) => Expression.False
          case (BinaryOperator.LogicalOr, _, Expression.True) | (BinaryOperator.LogicalOr, Expression.True, _) => Expression.True
          // Optimizations for Char.
          case (_, Expression.Char(lit1), Expression.Char(lit2)) =>
            op match {
              case BinaryOperator.Equal => toExp(lit1 == lit2)
              case BinaryOperator.NotEqual => toExp(lit1 != lit2)
              case _ => Expression.Binary(op, e1, e2, adjustType(tpe), loc)
            }
          // Optimizations for Float32.
          case (_, Expression.Float32(lit1), Expression.Float32(lit2)) =>
            op match {
              case BinaryOperator.Equal => toExp(lit1 == lit2)
              case BinaryOperator.NotEqual => toExp(lit1 != lit2)
              case BinaryOperator.Plus => Expression.Float32(lit1 + lit2)
              case BinaryOperator.Minus => Expression.Float32(lit1 - lit2)
              case BinaryOperator.Times => Expression.Float32(lit1 * lit2)
              case BinaryOperator.Divide => Expression.Float32(lit1 / lit2)
              case BinaryOperator.Exponentiate => Expression.Float32(scala.math.pow(lit1, lit2).toFloat)
              case BinaryOperator.Less => toExp(lit1 < lit2)
              case BinaryOperator.LessEqual => toExp(lit1 <= lit2)
              case BinaryOperator.Greater => toExp(lit1 > lit2)
              case BinaryOperator.GreaterEqual => toExp(lit1 >= lit2)
              case _ => Expression.Binary(op, e1, e2, adjustType(tpe), loc)
            }
          // Optimizations for Float64.
          case (_, Expression.Float64(lit1), Expression.Float64(lit2)) =>
            op match {
              case BinaryOperator.Equal => toExp(lit1 == lit2)
              case BinaryOperator.NotEqual => toExp(lit1 != lit2)
              case BinaryOperator.Plus => Expression.Float64(lit1 + lit2)
              case BinaryOperator.Minus => Expression.Float64(lit1 - lit2)
              case BinaryOperator.Times => Expression.Float64(lit1 * lit2)
              case BinaryOperator.Divide => Expression.Float64(lit1 / lit2)
              case BinaryOperator.Exponentiate => Expression.Float64(scala.math.pow(lit1, lit2))
              case BinaryOperator.Less => toExp(lit1 < lit2)
              case BinaryOperator.LessEqual => toExp(lit1 <= lit2)
              case BinaryOperator.Greater => toExp(lit1 > lit2)
              case BinaryOperator.GreaterEqual => toExp(lit1 >= lit2)
              case _ => Expression.Binary(op, e1, e2, adjustType(tpe), loc)
            }
          // Optimizations for Int8.
          case (_, Expression.Int8(lit1), Expression.Int8(lit2)) =>
            op match {
              case BinaryOperator.Equal => toExp(lit1 == lit2)
              case BinaryOperator.NotEqual => toExp(lit1 != lit2)
              case BinaryOperator.Plus => Expression.Int8((lit1 + lit2).toByte)
              case BinaryOperator.Minus => Expression.Int8((lit1 - lit2).toByte)
              case BinaryOperator.Times => Expression.Int8((lit1 * lit2).toByte)
              case BinaryOperator.Divide => if (lit2 == 0.toByte) Expression.Binary(op, e1, e2, adjustType(tpe), loc) else Expression.Int8((lit1 / lit2).toByte)
              case BinaryOperator.Exponentiate => Expression.Int8(scala.math.pow(lit1, lit2).toByte)
              case BinaryOperator.Less => toExp(lit1 < lit2)
              case BinaryOperator.LessEqual => toExp(lit1 <= lit2)
              case BinaryOperator.Greater => toExp(lit1 > lit2)
              case BinaryOperator.GreaterEqual => toExp(lit1 >= lit2)
              case _ => Expression.Binary(op, e1, e2, adjustType(tpe), loc)
            }
          // Optimizations for Int16.
          case (_, Expression.Int16(lit1), Expression.Int16(lit2)) =>
            op match {
              case BinaryOperator.Equal => toExp(lit1 == lit2)
              case BinaryOperator.NotEqual => toExp(lit1 != lit2)
              case BinaryOperator.Plus => Expression.Int16((lit1 + lit2).toShort)
              case BinaryOperator.Minus => Expression.Int16((lit1 - lit2).toShort)
              case BinaryOperator.Times => Expression.Int16((lit1 * lit2).toShort)
              case BinaryOperator.Divide => if (lit2 == 0.toShort) Expression.Binary(op, e1, e2, adjustType(tpe), loc) else Expression.Int16((lit1 / lit2).toShort)
              case BinaryOperator.Exponentiate => Expression.Int16(scala.math.pow(lit1, lit2).toShort)
              case BinaryOperator.Less => toExp(lit1 < lit2)
              case BinaryOperator.LessEqual => toExp(lit1 <= lit2)
              case BinaryOperator.Greater => toExp(lit1 > lit2)
              case BinaryOperator.GreaterEqual => toExp(lit1 >= lit2)
              case _ => Expression.Binary(op, e1, e2, adjustType(tpe), loc)
            }
          // Optimizations for Int32.
          case (_, Expression.Int32(lit1), Expression.Int32(lit2)) =>
            op match {
              case BinaryOperator.Equal => toExp(lit1 == lit2)
              case BinaryOperator.NotEqual => toExp(lit1 != lit2)
              case BinaryOperator.Plus => Expression.Int32(lit1 + lit2)
              case BinaryOperator.Minus => Expression.Int32(lit1 - lit2)
              case BinaryOperator.Times => Expression.Int32(lit1 * lit2)
              case BinaryOperator.Divide => if (lit2 == 0) Expression.Binary(op, e1, e2, adjustType(tpe), loc) else Expression.Int32(lit1 / lit2)
              case BinaryOperator.Exponentiate => Expression.Int32(scala.math.pow(lit1, lit2).toInt)
              case BinaryOperator.Less => toExp(lit1 < lit2)
              case BinaryOperator.LessEqual => toExp(lit1 <= lit2)
              case BinaryOperator.Greater => toExp(lit1 > lit2)
              case BinaryOperator.GreaterEqual => toExp(lit1 >= lit2)
              case _ => Expression.Binary(op, e1, e2, adjustType(tpe), loc)
            }
          // Optimizations for Int64.
          case (_, Expression.Int64(lit1), Expression.Int64(lit2)) =>
            op match {
              case BinaryOperator.Equal => toExp(lit1 == lit2)
              case BinaryOperator.NotEqual => toExp(lit1 != lit2)
              case BinaryOperator.Plus => Expression.Int64(lit1 + lit2)
              case BinaryOperator.Minus => Expression.Int64(lit1 - lit2)
              case BinaryOperator.Times => Expression.Int64(lit1 * lit2)
              case BinaryOperator.Divide => if (lit2 == 0.toLong) Expression.Binary(op, e1, e2, adjustType(tpe), loc) else Expression.Int64(lit1 / lit2)
              case BinaryOperator.Exponentiate => Expression.Int64(scala.math.pow(lit1, lit2).toLong)
              case BinaryOperator.Less => toExp(lit1 < lit2)
              case BinaryOperator.LessEqual => toExp(lit1 <= lit2)
              case BinaryOperator.Greater => toExp(lit1 > lit2)
              case BinaryOperator.GreaterEqual => toExp(lit1 >= lit2)
              case _ => Expression.Binary(op, e1, e2, adjustType(tpe), loc)
            }
          // Optimizations for BigInt.
          case (_, Expression.BigInt(lit1), Expression.BigInt(lit2)) =>
            op match {
              case BinaryOperator.Equal => toExp(lit1 == lit2)
              case BinaryOperator.NotEqual => toExp(lit1 != lit2)
              case BinaryOperator.Plus => Expression.BigInt(lit1.add(lit2))
              case BinaryOperator.Minus => Expression.BigInt(lit1.subtract(lit2))
              case BinaryOperator.Times => Expression.BigInt(lit1.multiply(lit2))
              case BinaryOperator.Divide => if (lit2 == java.math.BigInteger.ZERO) Expression.Binary(op, e1, e2, adjustType(tpe), loc) else Expression.BigInt(lit1.divide(lit2))
              //case BinaryOperator.Exponentiate => Expression.BigInt(...) Cannot exponentiate BinInts
              case BinaryOperator.Less => toExp(lit1.compareTo(lit2) == -1)
              case BinaryOperator.LessEqual => toExp(lit1.compareTo(lit2) <= 0)
              case BinaryOperator.Greater => toExp(lit1.compareTo(lit2) == 1)
              case BinaryOperator.GreaterEqual => toExp(lit1.compareTo(lit2) >= 0)
              case _ => Expression.Binary(op, e1, e2, adjustType(tpe), loc)
            }
          case _ => Expression.Binary(op, e1, e2, adjustType(tpe), loc)
        }
    }


    /**
      * Performs a single pass optimization on each element in `es`.
      *
      * That is, applies `optimize` to each element in `es` and returns the results in a List.
      */
    def optimizeExps(es: List[Expression]): List[Expression] = es.map(optimize)

    /**
      * Performs a single pass optimization on the given Expression `e0`.
      */
    def optimize(e0: Expression): Expression = e0 match {
      case Expression.Unit => Expression.Unit
      case Expression.True => Expression.True
      case Expression.False => Expression.False
      case Expression.Char(lit) => Expression.Char(lit)
      case Expression.Float32(lit) => Expression.Float32(lit)
      case Expression.Float64(lit) => Expression.Float64(lit)
      case Expression.Int8(lit) => Expression.Int8(lit)
      case Expression.Int16(lit) => Expression.Int16(lit)
      case Expression.Int32(lit) => Expression.Int32(lit)
      case Expression.Int64(lit) => Expression.Int64(lit)
      case Expression.BigInt(lit) => Expression.BigInt(lit)
      case Expression.Str(lit) => Expression.Str(lit)
      case Expression.LoadBool(exp, offset) => Expression.LoadBool(optimize(exp), offset)
      case Expression.LoadInt8(exp, offset) => Expression.LoadInt8(optimize(exp), offset)
      case Expression.LoadInt16(exp, offset) => Expression.LoadInt16(optimize(exp), offset)
      case Expression.LoadInt32(exp, offset) => Expression.LoadInt32(optimize(exp), offset)
      case Expression.StoreBool(exp, offset, v) => Expression.StoreBool(optimize(exp), offset, optimize(v))
      case Expression.StoreInt8(exp, offset, v) => Expression.StoreInt8(optimize(exp), offset, optimize(v))
      case Expression.StoreInt16(exp, offset, v) => Expression.StoreInt16(optimize(exp), offset, optimize(v))
      case Expression.StoreInt32(exp, offset, v) => Expression.StoreInt32(optimize(exp), offset, optimize(v))
      case Expression.Var(sym, tpe, loc) => Expression.Var(sym, adjustType(tpe), loc)
      case Expression.Ref(sym, tpe, loc) => Expression.Ref(sym, adjustType(tpe), loc)
      case Expression.Lambda(args, body, tpe, loc) => Expression.Lambda(adjustFormalParams(args), optimize(body), adjustType(tpe), loc)
      case Expression.Hook(hook, tpe, loc) => Expression.Hook(hook, adjustType(tpe), loc)
      case Expression.MkClosure(lambda, freeVars, tpe, loc) =>
        Expression.MkClosure(optimize(lambda).asInstanceOf[Expression.Lambda], adjustFreeVars(freeVars), adjustType(tpe), loc)
      case Expression.MkClosureRef(ref, freeVars, tpe, loc) =>
        Expression.MkClosureRef(optimize(ref).asInstanceOf[Expression.Ref], adjustFreeVars(freeVars), adjustType(tpe), loc)
      case Expression.ApplyRef(sym, args, tpe, loc) => Expression.ApplyRef(sym, optimizeExps(args), adjustType(tpe), loc)
      case Expression.ApplyTail(sym, formals, actuals, tpe, loc) =>
        Expression.ApplyTail(sym, adjustFormalParams(formals), optimizeExps(actuals), adjustType(tpe), loc)
      case Expression.ApplyHook(hook, args, tpe, loc) => Expression.ApplyHook(hook, optimizeExps(args), adjustType(tpe), loc)
      case Expression.Apply(exp, args, tpe, loc) => Expression.Apply(optimize(exp), optimizeExps(args), adjustType(tpe), loc)
      case Expression.Unary(op, exp, tpe, loc) => Expression.Unary(op, optimize(exp), adjustType(tpe), loc)
      case Expression.Binary(op, exp1, exp2, tpe, loc) => optimizeBinaryExpression(e0.asInstanceOf[Expression.Binary])
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        val e1 = optimize(exp1)
        e1 match {
          // Elimination of dead branches (e.g. if (true) e1 else e2).
          case Expression.True => optimize(exp2)
          case Expression.False => optimize(exp3)
          case _ =>
            // Elimination of redundant branching (e.g. if(c1, if(c2, e2, e3), e3) -> if (c1 && c2, e2, e3))
            val e2 = optimize(exp2)
            val e3 = optimize(exp3)
            (e2, e3) match {
              case (Expression.IfThenElse(se1, se2, se3, _, _), _) =>
                if (se3 == e3) Expression.IfThenElse(and(e1, se1, loc), se2, se3, adjustType(tpe), loc)
                else if (se2 == e3) Expression.IfThenElse(and(e1, not(se1, loc), loc), se3, se2, adjustType(tpe), loc)
                else Expression.IfThenElse(e1, e2, e3, adjustType(tpe), loc)
              case (_, Expression.IfThenElse(se1, se2, se3, _, _)) =>
                if (se3 == e2) Expression.IfThenElse(and(not(e1, loc), se1, loc), se2, se3, adjustType(tpe), loc)
                else if (se2 == e2) Expression.IfThenElse(and(not(e1, loc), not(se1, loc), loc), se3, se2, adjustType(tpe), loc)
                else Expression.IfThenElse(e1, e2, e3, adjustType(tpe), loc)
              case _ => Expression.IfThenElse(e1, e2, e3, adjustType(tpe), loc)
            }
        }
      case Expression.Let(sym, exp1, exp2, tpe, loc) => Expression.Let(sym, optimize(exp1), optimize(exp2), adjustType(tpe), loc)
      case Expression.LetRec(sym, exp1, exp2, tpe, loc) => Expression.LetRec(sym, optimize(exp1), optimize(exp2), adjustType(tpe), loc)
      case Expression.Is(sym, tag, exp, loc) =>
        val e = optimize(exp)
        // Elimination of run-time tag checks for Unit.
        if (e == Expression.Unit) Expression.False
        // Elimination of run-time tag checks of single-valued enums.
        else if (isSingleCaseEnum(sym)) Expression.True
        else Expression.Is(sym, tag, e, loc)
      case Expression.Tag(sym, tag, exp, tpe, loc) =>
        // Remove the tag on a single-valued enum.
        val e = optimize(exp)
        if (isSingleCaseEnum(sym)) e else Expression.Tag(sym, tag, e, adjustType(tpe), loc)
      case Expression.Untag(sym, tag, exp, tpe, loc) =>
        // Remove the untag on a single-valued enum.
        val e = optimize(exp)
        if (isSingleCaseEnum(sym)) e else Expression.Untag(sym, tag, e, adjustType(tpe), loc)
      case Expression.Index(base, offset, tpe, loc) => Expression.Index(optimize(base), offset, adjustType(tpe), loc)
      case Expression.Tuple(elms, tpe, loc) => Expression.Tuple(optimizeExps(elms), adjustType(tpe), loc)
      case Expression.Existential(fparam, exp, loc) => Expression.Existential(adjustFormalParam(fparam), optimize(exp), loc)
      case Expression.Universal(fparam, exp, loc) => Expression.Universal(adjustFormalParam(fparam), optimize(exp), loc)
      case Expression.NativeConstructor(constructor, args, tpe, loc) =>
        Expression.NativeConstructor(constructor, optimizeExps(args), adjustType(tpe), loc)
      case Expression.NativeField(field, tpe, loc) => Expression.NativeField(field, adjustType(tpe), loc)
      case Expression.NativeMethod(method, args, tpe, loc) =>
        Expression.NativeMethod(method, optimizeExps(args), adjustType(tpe), loc)
      case Expression.UserError(tpe, loc) => Expression.UserError(adjustType(tpe), loc)
      case Expression.MatchError(tpe, loc) => Expression.MatchError(adjustType(tpe), loc)
      case Expression.SwitchError(tpe, loc) => Expression.SwitchError(adjustType(tpe), loc)
    }

    // Start the timer.
    val t = System.nanoTime()

    // Optimize expressions in the definitions.
    val optDefinitions = root.definitions map {case (sym, defn) => (sym, defn.copy(exp = optimize(defn.exp), formals = adjustFormalParams(defn.formals), tpe = adjustType(defn.tpe)))}

    // Calculate the elapsed time.
    val e = System.nanoTime() - t

    // Reassemble the AST.
    root.copy(
      definitions = optDefinitions,
      time = root.time.copy(optimizer = e)
    ).toSuccess
  }
}
