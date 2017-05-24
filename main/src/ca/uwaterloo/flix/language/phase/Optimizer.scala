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
import ca.uwaterloo.flix.language.ast.{BinaryOperator, SimplifiedAst, Symbol, UnaryOperator}
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

/**
  * The Optimization phase performs intra-procedural optimizations.
  *
  * Specifically,
  *
  * - Elimination of run-time tag checks for Unit.
  * - Elimination of run-time tag checks of singleton-valued enums.
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
      * Returns `true` if and only if `enum` is a singleton-valued enum.
      */
    def isSingleCaseEnum(enum: Symbol.EnumSym): Boolean = root.enums.get(enum) match {
      case None => false
      case Some(defn) => defn.cases.size <= 1
    }

    def toExp(b0: Boolean): Expression = if (b0) Expression.True else Expression.False

    def optimizeBinaryExpression(e0: Expression.Binary): Expression = e0 match {
      case Expression.Binary(op, exp1, exp2, tpe, loc) =>
        val e1 = optimize(exp1)
        val e2 = optimize(exp2)
        (op, e1, e2) match {
          // Elimination of run-time tag checks of singleton-valued enums.
          case (BinaryOperator.Equal, Expression.Tag(sym, tag, exp, tp, lc), _) =>
            if (isSingleCaseEnum(sym) && exp == Expression.Unit) Expression.True
            else Expression.Binary(op, e1, e2, tpe, loc)
          case (BinaryOperator.Equal, _, Expression.Tag(sym, tag, exp, tp, lc)) =>
            if (isSingleCaseEnum(sym) && exp == Expression.Unit) Expression.True
            else Expression.Binary(op, e1, e2, tpe, loc)
          // Optimizations for Bool.
          case (BinaryOperator.Equal | BinaryOperator.LogicalAnd, Expression.True, exp) => exp
          case (BinaryOperator.Equal | BinaryOperator.LogicalAnd, exp, Expression.True) => exp
          case (BinaryOperator.Equal, Expression.False, exp) => Expression.Unary(UnaryOperator.LogicalNot, exp, tpe, loc)
          case (BinaryOperator.Equal, exp, Expression.False) => Expression.Unary(UnaryOperator.LogicalNot, exp, tpe, loc)
          case (BinaryOperator.NotEqual, Expression.True, exp) => Expression.Unary(UnaryOperator.LogicalNot, exp, tpe, loc)
          case (BinaryOperator.NotEqual, exp, Expression.True) => Expression.Unary(UnaryOperator.LogicalNot, exp, tpe, loc)
          case (BinaryOperator.NotEqual | BinaryOperator.LogicalOr, Expression.False, exp) => exp
          case (BinaryOperator.NotEqual | BinaryOperator.LogicalOr, exp, Expression.False) => exp
          case (BinaryOperator.LogicalAnd, Expression.False, _) | (BinaryOperator.LogicalAnd, _, Expression.False) => Expression.False
          case (BinaryOperator.LogicalOr, _, Expression.True) | (BinaryOperator.LogicalOr, Expression.True, _) => Expression.True
          // Optimizations for Char.
          case (_, Expression.Char(lit1), Expression.Char(lit2)) =>
            op match {
              case BinaryOperator.Equal => toExp(lit1 == lit2)
              case BinaryOperator.NotEqual => toExp(lit1 != lit2)
              case _ => Expression.Binary(op, e1, e2, tpe, loc)
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
              case _ => Expression.Binary(op, e1, e2, tpe, loc)
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
              case _ => Expression.Binary(op, e1, e2, tpe, loc)
            }
          // Optimizations for Int8.
          case (_, Expression.Int8(lit1), Expression.Int8(lit2)) =>
            op match {
              case BinaryOperator.Equal => toExp(lit1 == lit2)
              case BinaryOperator.NotEqual => toExp(lit1 != lit2)
              case BinaryOperator.Plus => Expression.Int8((lit1 + lit2).toByte)
              case BinaryOperator.Minus => Expression.Int8((lit1 - lit2).toByte)
              case BinaryOperator.Times => Expression.Int8((lit1 * lit2).toByte)
              case BinaryOperator.Divide => Expression.Int8((lit1 / lit2).toByte)
              case BinaryOperator.Exponentiate => Expression.Int8(scala.math.pow(lit1, lit2).toByte)
              case BinaryOperator.Less => toExp(lit1 < lit2)
              case BinaryOperator.LessEqual => toExp(lit1 <= lit2)
              case BinaryOperator.Greater => toExp(lit1 > lit2)
              case BinaryOperator.GreaterEqual => toExp(lit1 >= lit2)
              case _ => Expression.Binary(op, e1, e2, tpe, loc)
            }
          // Optimizations for Int16.
          case (_, Expression.Int16(lit1), Expression.Int16(lit2)) =>
            op match {
              case BinaryOperator.Equal => toExp(lit1 == lit2)
              case BinaryOperator.NotEqual => toExp(lit1 != lit2)
              case BinaryOperator.Plus => Expression.Int16((lit1 + lit2).toShort)
              case BinaryOperator.Minus => Expression.Int16((lit1 - lit2).toShort)
              case BinaryOperator.Times => Expression.Int16((lit1 * lit2).toShort)
              case BinaryOperator.Divide => Expression.Int16((lit1 / lit2).toShort)
              case BinaryOperator.Exponentiate => Expression.Int16(scala.math.pow(lit1, lit2).toShort)
              case BinaryOperator.Less => toExp(lit1 < lit2)
              case BinaryOperator.LessEqual => toExp(lit1 <= lit2)
              case BinaryOperator.Greater => toExp(lit1 > lit2)
              case BinaryOperator.GreaterEqual => toExp(lit1 >= lit2)
              case _ => Expression.Binary(op, e1, e2, tpe, loc)
            }
          // Optimizations for Int32.
          case (_, Expression.Int32(lit1), Expression.Int32(lit2)) =>
            op match {
              case BinaryOperator.Equal => toExp(lit1 == lit2)
              case BinaryOperator.NotEqual => toExp(lit1 != lit2)
              case BinaryOperator.Plus => Expression.Int32(lit1 + lit2)
              case BinaryOperator.Minus => Expression.Int32(lit1 - lit2)
              case BinaryOperator.Times => Expression.Int32(lit1 * lit2)
              case BinaryOperator.Divide => Expression.Int32(lit1 / lit2)
              case BinaryOperator.Exponentiate => Expression.Int32(scala.math.pow(lit1, lit2).toInt)
              case BinaryOperator.Less => toExp(lit1 < lit2)
              case BinaryOperator.LessEqual => toExp(lit1 <= lit2)
              case BinaryOperator.Greater => toExp(lit1 > lit2)
              case BinaryOperator.GreaterEqual => toExp(lit1 >= lit2)
              case _ => Expression.Binary(op, e1, e2, tpe, loc)
            }
          // Optimizations for Int64.
          case (_, Expression.Int64(lit1), Expression.Int64(lit2)) =>
            op match {
              case BinaryOperator.Equal => toExp(lit1 == lit2)
              case BinaryOperator.NotEqual => toExp(lit1 != lit2)
              case BinaryOperator.Plus => Expression.Int64(lit1 + lit2)
              case BinaryOperator.Minus => Expression.Int64(lit1 - lit2)
              case BinaryOperator.Times => Expression.Int64(lit1 * lit2)
              case BinaryOperator.Divide => Expression.Int64(lit1 / lit2)
              case BinaryOperator.Exponentiate => Expression.Int64(scala.math.pow(lit1, lit2).toLong)
              case BinaryOperator.Less => toExp(lit1 < lit2)
              case BinaryOperator.LessEqual => toExp(lit1 <= lit2)
              case BinaryOperator.Greater => toExp(lit1 > lit2)
              case BinaryOperator.GreaterEqual => toExp(lit1 >= lit2)
              case _ => Expression.Binary(op, e1, e2, tpe, loc)
            }
          // Optimizations for BigInt.
          case (_, Expression.BigInt(lit1), Expression.BigInt(lit2)) =>
            op match {
              case BinaryOperator.Equal => toExp(lit1 == lit2)
              case BinaryOperator.NotEqual => toExp(lit1 != lit2)
              case BinaryOperator.Plus => Expression.BigInt(lit1.add(lit2))
              case BinaryOperator.Minus => Expression.BigInt(lit1.subtract(lit2))
              case BinaryOperator.Times => Expression.BigInt(lit1.multiply(lit2))
              case BinaryOperator.Divide => Expression.BigInt(lit1.divide(lit2))
              //case BinaryOperator.Exponentiate => Expression.BigInt(...) Cannot exponentiate BinInts
              case BinaryOperator.Less => toExp(lit1.compareTo(lit2) == -1)
              case BinaryOperator.LessEqual => toExp(lit1.compareTo(lit2) <= 0)
              case BinaryOperator.Greater => toExp(lit1.compareTo(lit2) == 1)
              case BinaryOperator.GreaterEqual => toExp(lit1.compareTo(lit2) >= 0)
              case _ => Expression.Binary(op, e1, e2, tpe, loc)
            }
          case _ => Expression.Binary(op, e1, e2, tpe, loc)
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
      case Expression.Var(sym, tpe, loc) => Expression.Var(sym, tpe, loc)
      case Expression.Ref(sym, tpe, loc) => Expression.Ref(sym, tpe, loc)
      case Expression.Lambda(args, body, tpe, loc) => Expression.Lambda(args, optimize(body), tpe, loc)
      case Expression.Hook(hook, tpe, loc) => Expression.Hook(hook, tpe, loc)
      case Expression.MkClosure(lambda, freeVars, tpe, loc) =>
        Expression.MkClosure(optimize(lambda).asInstanceOf[Expression.Lambda], freeVars, tpe, loc)
      case Expression.MkClosureRef(ref, freeVars, tpe, loc) =>
        Expression.MkClosureRef(optimize(ref).asInstanceOf[Expression.Ref], freeVars, tpe, loc)
      case Expression.ApplyRef(sym, args, tpe, loc) => Expression.ApplyRef(sym, optimizeExps(args), tpe, loc)
      case Expression.ApplyTail(sym, formals, actuals, tpe, loc) =>
        Expression.ApplyTail(sym, formals, optimizeExps(actuals), tpe, loc)
      case Expression.ApplyHook(hook, args, tpe, loc) => Expression.ApplyHook(hook, optimizeExps(args), tpe, loc)
      case Expression.Apply(exp, args, tpe, loc) => Expression.Apply(optimize(exp), optimizeExps(args), tpe, loc)
      case Expression.Unary(op, exp, tpe, loc) => Expression.Unary(op, optimize(exp), tpe, loc)
      case Expression.Binary(op, exp1, exp2, tpe, loc) => optimizeBinaryExpression(e0.asInstanceOf[Expression.Binary])
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        // Elimination of dead branches (e.g. if (true) e1 else e2).
        val e1 = optimize(exp1)
        e1 match {
          case Expression.True => optimize(exp2)
          case Expression.False => optimize(exp3)
          case _ =>
            val e2 = optimize(exp2)
            val e3 = optimize(exp3)
            Expression.IfThenElse(e1, e2, e3, tpe, loc)
        }
      case Expression.Let(sym, exp1, exp2, tpe, loc) => Expression.Let(sym, optimize(exp1), optimize(exp2), tpe, loc)
      case Expression.LetRec(sym, exp1, exp2, tpe, loc) => Expression.LetRec(sym, optimize(exp1), optimize(exp2), tpe, loc)
      // TODO: Elimination of run-time tag checks of singleton-valued enums.
      case Expression.Is(sym, tag, exp, loc) => Expression.Is(sym, tag, optimize(exp), loc)
      // TODO: Remove the tag and untag on a single case enum.
      case Expression.Tag(sym, tag, exp, tpe, loc) => Expression.Tag(sym, tag, optimize(exp), tpe, loc)
      case Expression.Untag(sym, tag, exp, tpe, loc) => Expression.Untag(sym, tag, optimize(exp), tpe, loc)
      case Expression.Index(base, offset, tpe, loc) => Expression.Index(optimize(base), offset, tpe, loc)
      case Expression.Tuple(elms, tpe, loc) => Expression.Tuple(optimizeExps(elms), tpe, loc)
      case Expression.Existential(fparam, exp, loc) => Expression.Existential(fparam, optimize(exp), loc)
      case Expression.Universal(fparam, exp, loc) => Expression.Universal(fparam, optimize(exp), loc)
      case Expression.NativeConstructor(constructor, args, tpe, loc) =>
        Expression.NativeConstructor(constructor, optimizeExps(args), tpe, loc)
      case Expression.NativeField(field, tpe, loc) => Expression.NativeField(field, tpe, loc)
      case Expression.NativeMethod(method, args, tpe, loc) =>
        Expression.NativeMethod(method, optimizeExps(args), tpe, loc)
      case Expression.UserError(tpe, loc) => Expression.UserError(tpe, loc)
      case Expression.MatchError(tpe, loc) => Expression.MatchError(tpe, loc)
      case Expression.SwitchError(tpe, loc) => Expression.SwitchError(tpe, loc)
    }

    // Start the timer.
    val t = System.nanoTime()

    // Optimize expressions in the definitions.
    val optDefinitions = root.definitions.mapValues(defn => defn.copy(exp = optimize(defn.exp)))

    // Calculate the elapsed time.
    val e = System.nanoTime() - t

    // Reassemble the AST.
    root.copy(
      definitions = optDefinitions,
      time = root.time.copy(optimizer = e)
    ).toSuccess
  }
}
