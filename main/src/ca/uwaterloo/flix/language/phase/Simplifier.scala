/*
 * Copyright 2015-2016 Magnus Madsen, Ming-Ho Yee
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
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

import scala.collection.mutable

/**
  * A phase that simplifies the TypedAst by elimination of pattern matching and other rewritings.
  */
object Simplifier extends Phase[TypedAst.Root, SimplifiedAst.Root] {

  type TopLevel = mutable.Map[Symbol.DefnSym, SimplifiedAst.Def]

  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = flix.phase("Simplifier") {
    //
    // Put GenSym into the implicit scope.
    //
    implicit val _ = flix.genSym

    //
    // A mutable map to contain fresh top-level definitions.
    //
    val toplevel: TopLevel = mutable.Map.empty

    /**
      * Translates the given `constraint0` to the SimplifiedAst.
      */
    def visitConstraint(constraint0: TypedAst.Constraint): SimplifiedAst.Constraint = {
      val cparams = constraint0.cparams.map {
        case TypedAst.ConstraintParam.HeadParam(sym, tpe, loc) => SimplifiedAst.ConstraintParam.HeadParam(sym, tpe, loc)
        case TypedAst.ConstraintParam.RuleParam(sym, tpe, loc) => SimplifiedAst.ConstraintParam.RuleParam(sym, tpe, loc)
      }
      val head = visitHeadPred(constraint0.head, constraint0.cparams)
      val body = constraint0.body.map(p => visitBodyPred(p, constraint0.cparams))

      SimplifiedAst.Constraint(cparams, head, body)
    }

    /**
      * Translates the given definition `def0` to the SimplifiedAst.
      */
    def visitDef(def0: TypedAst.Def): SimplifiedAst.Def = {
      val fs = def0.fparams.map(visitFormalParam)
      val exp = visitExp(def0.exp)
      SimplifiedAst.Def(def0.ann, def0.mod, def0.sym, fs, exp, def0.tpe, def0.loc)
    }

    /**
      * Translates the given effect `eff0` to the SimplifiedAst.
      */
    def visitEff(eff0: TypedAst.Eff): SimplifiedAst.Eff = {
      val fs = eff0.fparams.map(visitFormalParam)
      SimplifiedAst.Eff(eff0.ann, eff0.mod, eff0.sym, fs, eff0.tpe, eff0.loc)
    }

    /**
      * Translates the given handler `handler0` to the SimplifiedAst.
      */
    def visitHandler(handler0: TypedAst.Handler): SimplifiedAst.Handler = {
      val fs = handler0.fparams.map(visitFormalParam)
      val exp = visitExp(handler0.exp)
      SimplifiedAst.Handler(handler0.ann, handler0.mod, handler0.sym, fs, exp, handler0.tpe, handler0.loc)
    }

    /**
      * Translates the given expression `exp` to the SimplifiedAst.
      */
    def visitExp(expr: TypedAst.Expression): SimplifiedAst.Expression = expr match {
      case TypedAst.Expression.Var(sym, tpe, eff, loc) => SimplifiedAst.Expression.Var(sym, tpe, loc)
      case TypedAst.Expression.Def(sym, tpe, eff, loc) => SimplifiedAst.Expression.Def(sym, tpe, loc)
      case TypedAst.Expression.Eff(sym, tpe, eff, loc) => SimplifiedAst.Expression.Eff(sym, tpe, loc)
      case TypedAst.Expression.Hole(sym, tpe, eff, loc) => SimplifiedAst.Expression.HoleError(sym, tpe, loc)
      case TypedAst.Expression.Unit(loc) => SimplifiedAst.Expression.Unit
      case TypedAst.Expression.True(loc) => SimplifiedAst.Expression.True
      case TypedAst.Expression.False(loc) => SimplifiedAst.Expression.False
      case TypedAst.Expression.Char(lit, loc) => SimplifiedAst.Expression.Char(lit)
      case TypedAst.Expression.Float32(lit, loc) => SimplifiedAst.Expression.Float32(lit)
      case TypedAst.Expression.Float64(lit, loc) => SimplifiedAst.Expression.Float64(lit)
      case TypedAst.Expression.Int8(lit, loc) => SimplifiedAst.Expression.Int8(lit)
      case TypedAst.Expression.Int16(lit, loc) => SimplifiedAst.Expression.Int16(lit)
      case TypedAst.Expression.Int32(lit, loc) => SimplifiedAst.Expression.Int32(lit)
      case TypedAst.Expression.Int64(lit, loc) => SimplifiedAst.Expression.Int64(lit)
      case TypedAst.Expression.BigInt(lit, loc) => SimplifiedAst.Expression.BigInt(lit)
      case TypedAst.Expression.Str(lit, loc) => SimplifiedAst.Expression.Str(lit)

      case TypedAst.Expression.Lambda(fparam, exp, tpe, eff, loc) =>
        val p = visitFormalParam(fparam)
        val e = visitExp(exp)
        SimplifiedAst.Expression.Lambda(List(p), e, tpe, loc)

      case TypedAst.Expression.Apply(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        SimplifiedAst.Expression.Apply(e1, List(e2), tpe, loc)

      case TypedAst.Expression.Unary(op, e, tpe, eff, loc) =>
        /*
         * Special Case 1: Unary Plus.
         */
        (op, e.tpe) match {
          case (UnaryOperator.Plus, _) =>
            // A unary plus has no semantic effect.
            return visitExp(e)
          case _ => // fallthrough
        }

        /*
         * Compute the semantic operator based on types.
         */
        val sop = op match {
          case UnaryOperator.LogicalNot => SemanticOperator.BoolOp.Not
          case UnaryOperator.BitwiseNegate => e.tpe match {
            case Type.Cst(TypeConstructor.Int8) => SemanticOperator.Int8Op.Not
            case Type.Cst(TypeConstructor.Int16) => SemanticOperator.Int16Op.Not
            case Type.Cst(TypeConstructor.Int32) => SemanticOperator.Int32Op.Not
            case Type.Cst(TypeConstructor.Int64) => SemanticOperator.Int64Op.Not
            case Type.Cst(TypeConstructor.BigInt) => SemanticOperator.BigIntOp.Not
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case UnaryOperator.Plus =>
            throw InternalCompilerException(s"Impossible.")
          case UnaryOperator.Minus => e.tpe match {
            case Type.Cst(TypeConstructor.Float32) => SemanticOperator.Float32Op.Neg
            case Type.Cst(TypeConstructor.Float64) => SemanticOperator.Float64Op.Neg
            case Type.Cst(TypeConstructor.Int8) => SemanticOperator.Int8Op.Neg
            case Type.Cst(TypeConstructor.Int16) => SemanticOperator.Int16Op.Neg
            case Type.Cst(TypeConstructor.Int32) => SemanticOperator.Int32Op.Neg
            case Type.Cst(TypeConstructor.Int64) => SemanticOperator.Int64Op.Neg
            case Type.Cst(TypeConstructor.BigInt) => SemanticOperator.BigIntOp.Neg
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
        }

        SimplifiedAst.Expression.Unary(sop, op, visitExp(e), tpe, loc)

      case TypedAst.Expression.Binary(op, e1, e2, tpe, eff, loc) =>

        /*
         * Special Case 1: Unit
         */
        (op, e1.tpe, e2.tpe) match {
          case (BinaryOperator.Equal, Type.Cst(TypeConstructor.Unit), Type.Cst(TypeConstructor.Unit)) =>
            // Unit is always equal to itself.
            return SimplifiedAst.Expression.True
          case (BinaryOperator.NotEqual, Type.Cst(TypeConstructor.Unit), Type.Cst(TypeConstructor.Unit)) =>
            // Unit is never not equal to itself.
            return SimplifiedAst.Expression.False
          case _ => // fallthrough
        }

        /*
         * Compute the semantic operator based on types.
         */
        val sop = op match {
          case BinaryOperator.Plus => e1.tpe match {
            case Type.Cst(TypeConstructor.Float32) => SemanticOperator.Float32Op.Add
            case Type.Cst(TypeConstructor.Float64) => SemanticOperator.Float64Op.Add
            case Type.Cst(TypeConstructor.Int8) => SemanticOperator.Int8Op.Add
            case Type.Cst(TypeConstructor.Int16) => SemanticOperator.Int16Op.Add
            case Type.Cst(TypeConstructor.Int32) => SemanticOperator.Int32Op.Add
            case Type.Cst(TypeConstructor.Int64) => SemanticOperator.Int64Op.Add
            case Type.Cst(TypeConstructor.BigInt) => SemanticOperator.BigIntOp.Add
            case Type.Cst(TypeConstructor.Str) => SemanticOperator.StringOp.Concat
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.Minus => e1.tpe match {
            case Type.Cst(TypeConstructor.Float32) => SemanticOperator.Float32Op.Sub
            case Type.Cst(TypeConstructor.Float64) => SemanticOperator.Float64Op.Sub
            case Type.Cst(TypeConstructor.Int8) => SemanticOperator.Int8Op.Sub
            case Type.Cst(TypeConstructor.Int16) => SemanticOperator.Int16Op.Sub
            case Type.Cst(TypeConstructor.Int32) => SemanticOperator.Int32Op.Sub
            case Type.Cst(TypeConstructor.Int64) => SemanticOperator.Int64Op.Sub
            case Type.Cst(TypeConstructor.BigInt) => SemanticOperator.BigIntOp.Sub
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.Times => e1.tpe match {
            case Type.Cst(TypeConstructor.Float32) => SemanticOperator.Float32Op.Mul
            case Type.Cst(TypeConstructor.Float64) => SemanticOperator.Float64Op.Mul
            case Type.Cst(TypeConstructor.Int8) => SemanticOperator.Int8Op.Mul
            case Type.Cst(TypeConstructor.Int16) => SemanticOperator.Int16Op.Mul
            case Type.Cst(TypeConstructor.Int32) => SemanticOperator.Int32Op.Mul
            case Type.Cst(TypeConstructor.Int64) => SemanticOperator.Int64Op.Mul
            case Type.Cst(TypeConstructor.BigInt) => SemanticOperator.BigIntOp.Mul
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.Divide => e1.tpe match {
            case Type.Cst(TypeConstructor.Float32) => SemanticOperator.Float32Op.Div
            case Type.Cst(TypeConstructor.Float64) => SemanticOperator.Float64Op.Div
            case Type.Cst(TypeConstructor.Int8) => SemanticOperator.Int8Op.Div
            case Type.Cst(TypeConstructor.Int16) => SemanticOperator.Int16Op.Div
            case Type.Cst(TypeConstructor.Int32) => SemanticOperator.Int32Op.Div
            case Type.Cst(TypeConstructor.Int64) => SemanticOperator.Int64Op.Div
            case Type.Cst(TypeConstructor.BigInt) => SemanticOperator.BigIntOp.Div
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.Modulo => e1.tpe match {
            case Type.Cst(TypeConstructor.Float32) => SemanticOperator.Float32Op.Rem
            case Type.Cst(TypeConstructor.Float64) => SemanticOperator.Float64Op.Rem
            case Type.Cst(TypeConstructor.Int8) => SemanticOperator.Int8Op.Rem
            case Type.Cst(TypeConstructor.Int16) => SemanticOperator.Int16Op.Rem
            case Type.Cst(TypeConstructor.Int32) => SemanticOperator.Int32Op.Rem
            case Type.Cst(TypeConstructor.Int64) => SemanticOperator.Int64Op.Rem
            case Type.Cst(TypeConstructor.BigInt) => SemanticOperator.BigIntOp.Rem
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.Exponentiate => e1.tpe match {
            case Type.Cst(TypeConstructor.Float32) => SemanticOperator.Float32Op.Exp
            case Type.Cst(TypeConstructor.Float64) => SemanticOperator.Float64Op.Exp
            case Type.Cst(TypeConstructor.Int8) => SemanticOperator.Int8Op.Exp
            case Type.Cst(TypeConstructor.Int16) => SemanticOperator.Int16Op.Exp
            case Type.Cst(TypeConstructor.Int32) => SemanticOperator.Int32Op.Exp
            case Type.Cst(TypeConstructor.Int64) => SemanticOperator.Int64Op.Exp
            case Type.Cst(TypeConstructor.BigInt) => SemanticOperator.BigIntOp.Exp
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.Less => e1.tpe match {
            case Type.Cst(TypeConstructor.Char) => SemanticOperator.CharOp.Lt
            case Type.Cst(TypeConstructor.Float32) => SemanticOperator.Float32Op.Lt
            case Type.Cst(TypeConstructor.Float64) => SemanticOperator.Float64Op.Lt
            case Type.Cst(TypeConstructor.Int8) => SemanticOperator.Int8Op.Lt
            case Type.Cst(TypeConstructor.Int16) => SemanticOperator.Int16Op.Lt
            case Type.Cst(TypeConstructor.Int32) => SemanticOperator.Int32Op.Lt
            case Type.Cst(TypeConstructor.Int64) => SemanticOperator.Int64Op.Lt
            case Type.Cst(TypeConstructor.BigInt) => SemanticOperator.BigIntOp.Lt
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.LessEqual => e1.tpe match {
            case Type.Cst(TypeConstructor.Char) => SemanticOperator.CharOp.Le
            case Type.Cst(TypeConstructor.Float32) => SemanticOperator.Float32Op.Le
            case Type.Cst(TypeConstructor.Float64) => SemanticOperator.Float64Op.Le
            case Type.Cst(TypeConstructor.Int8) => SemanticOperator.Int8Op.Le
            case Type.Cst(TypeConstructor.Int16) => SemanticOperator.Int16Op.Le
            case Type.Cst(TypeConstructor.Int32) => SemanticOperator.Int32Op.Le
            case Type.Cst(TypeConstructor.Int64) => SemanticOperator.Int64Op.Le
            case Type.Cst(TypeConstructor.BigInt) => SemanticOperator.BigIntOp.Le
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.Greater => e1.tpe match {
            case Type.Cst(TypeConstructor.Char) => SemanticOperator.CharOp.Gt
            case Type.Cst(TypeConstructor.Float32) => SemanticOperator.Float32Op.Gt
            case Type.Cst(TypeConstructor.Float64) => SemanticOperator.Float64Op.Gt
            case Type.Cst(TypeConstructor.Int8) => SemanticOperator.Int8Op.Gt
            case Type.Cst(TypeConstructor.Int16) => SemanticOperator.Int16Op.Gt
            case Type.Cst(TypeConstructor.Int32) => SemanticOperator.Int32Op.Gt
            case Type.Cst(TypeConstructor.Int64) => SemanticOperator.Int64Op.Gt
            case Type.Cst(TypeConstructor.BigInt) => SemanticOperator.BigIntOp.Gt
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.GreaterEqual => e1.tpe match {
            case Type.Cst(TypeConstructor.Char) => SemanticOperator.CharOp.Ge
            case Type.Cst(TypeConstructor.Float32) => SemanticOperator.Float32Op.Ge
            case Type.Cst(TypeConstructor.Float64) => SemanticOperator.Float64Op.Ge
            case Type.Cst(TypeConstructor.Int8) => SemanticOperator.Int8Op.Ge
            case Type.Cst(TypeConstructor.Int16) => SemanticOperator.Int16Op.Ge
            case Type.Cst(TypeConstructor.Int32) => SemanticOperator.Int32Op.Ge
            case Type.Cst(TypeConstructor.Int64) => SemanticOperator.Int64Op.Ge
            case Type.Cst(TypeConstructor.BigInt) => SemanticOperator.BigIntOp.Ge
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.Equal => e1.tpe match {
            case Type.Cst(TypeConstructor.Bool) => SemanticOperator.BoolOp.Eq
            case Type.Cst(TypeConstructor.Char) => SemanticOperator.CharOp.Eq
            case Type.Cst(TypeConstructor.Float32) => SemanticOperator.Float32Op.Eq
            case Type.Cst(TypeConstructor.Float64) => SemanticOperator.Float64Op.Eq
            case Type.Cst(TypeConstructor.Int8) => SemanticOperator.Int8Op.Eq
            case Type.Cst(TypeConstructor.Int16) => SemanticOperator.Int16Op.Eq
            case Type.Cst(TypeConstructor.Int32) => SemanticOperator.Int32Op.Eq
            case Type.Cst(TypeConstructor.Int64) => SemanticOperator.Int64Op.Eq
            case Type.Cst(TypeConstructor.BigInt) => SemanticOperator.BigIntOp.Eq
            case Type.Cst(TypeConstructor.Str) => SemanticOperator.StringOp.Eq
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.NotEqual => e1.tpe match {
            case Type.Cst(TypeConstructor.Bool) => SemanticOperator.BoolOp.Neq
            case Type.Cst(TypeConstructor.Char) => SemanticOperator.CharOp.Neq
            case Type.Cst(TypeConstructor.Float32) => SemanticOperator.Float32Op.Neq
            case Type.Cst(TypeConstructor.Float64) => SemanticOperator.Float64Op.Neq
            case Type.Cst(TypeConstructor.Int8) => SemanticOperator.Int8Op.Neq
            case Type.Cst(TypeConstructor.Int16) => SemanticOperator.Int16Op.Neq
            case Type.Cst(TypeConstructor.Int32) => SemanticOperator.Int32Op.Neq
            case Type.Cst(TypeConstructor.Int64) => SemanticOperator.Int64Op.Neq
            case Type.Cst(TypeConstructor.BigInt) => SemanticOperator.BigIntOp.Neq
            case Type.Cst(TypeConstructor.Str) => SemanticOperator.StringOp.Neq
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.LogicalAnd => e1.tpe match {
            case Type.Cst(TypeConstructor.Bool) => SemanticOperator.BoolOp.And
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.LogicalOr => e1.tpe match {
            case Type.Cst(TypeConstructor.Bool) => SemanticOperator.BoolOp.Or
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.BitwiseAnd => e1.tpe match {
            case Type.Cst(TypeConstructor.Int8) => SemanticOperator.Int8Op.And
            case Type.Cst(TypeConstructor.Int16) => SemanticOperator.Int16Op.And
            case Type.Cst(TypeConstructor.Int32) => SemanticOperator.Int32Op.And
            case Type.Cst(TypeConstructor.Int64) => SemanticOperator.Int64Op.And
            case Type.Cst(TypeConstructor.BigInt) => SemanticOperator.BigIntOp.And
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.BitwiseOr => e1.tpe match {
            case Type.Cst(TypeConstructor.Int8) => SemanticOperator.Int8Op.Or
            case Type.Cst(TypeConstructor.Int16) => SemanticOperator.Int16Op.Or
            case Type.Cst(TypeConstructor.Int32) => SemanticOperator.Int32Op.Or
            case Type.Cst(TypeConstructor.Int64) => SemanticOperator.Int64Op.Or
            case Type.Cst(TypeConstructor.BigInt) => SemanticOperator.BigIntOp.Or
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.BitwiseXor => e1.tpe match {
            case Type.Cst(TypeConstructor.Int8) => SemanticOperator.Int8Op.Xor
            case Type.Cst(TypeConstructor.Int16) => SemanticOperator.Int16Op.Xor
            case Type.Cst(TypeConstructor.Int32) => SemanticOperator.Int32Op.Xor
            case Type.Cst(TypeConstructor.Int64) => SemanticOperator.Int64Op.Xor
            case Type.Cst(TypeConstructor.BigInt) => SemanticOperator.BigIntOp.Xor
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.BitwiseLeftShift => e1.tpe match {
            case Type.Cst(TypeConstructor.Int8) => SemanticOperator.Int8Op.Shl
            case Type.Cst(TypeConstructor.Int16) => SemanticOperator.Int16Op.Shl
            case Type.Cst(TypeConstructor.Int32) => SemanticOperator.Int32Op.Shl
            case Type.Cst(TypeConstructor.Int64) => SemanticOperator.Int64Op.Shl
            case Type.Cst(TypeConstructor.BigInt) => SemanticOperator.BigIntOp.Shl
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.BitwiseRightShift => e1.tpe match {
            case Type.Cst(TypeConstructor.Int8) => SemanticOperator.Int8Op.Shr
            case Type.Cst(TypeConstructor.Int16) => SemanticOperator.Int16Op.Shr
            case Type.Cst(TypeConstructor.Int32) => SemanticOperator.Int32Op.Shr
            case Type.Cst(TypeConstructor.Int64) => SemanticOperator.Int64Op.Shr
            case Type.Cst(TypeConstructor.BigInt) => SemanticOperator.BigIntOp.Shr
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
        }

        SimplifiedAst.Expression.Binary(sop, op, visitExp(e1), visitExp(e2), tpe, loc)

      case TypedAst.Expression.IfThenElse(e1, e2, e3, tpe, eff, loc) =>
        SimplifiedAst.Expression.IfThenElse(visitExp(e1), visitExp(e2), visitExp(e3), tpe, loc)

      case TypedAst.Expression.Stm(e1, e2, tpe, eff, loc) =>
        SimplifiedAst.Expression.Let(Symbol.freshVarSym(), visitExp(e1), visitExp(e2), tpe, loc)

      case TypedAst.Expression.Switch(rules, tpe, eff, loc) =>
        val zero = SimplifiedAst.Expression.SwitchError(tpe, loc)
        rules.foldRight(zero: SimplifiedAst.Expression) {
          case ((e1, e2), acc) =>
            val cond = visitExp(e1)
            val body = visitExp(e2)
            SimplifiedAst.Expression.IfThenElse(cond, body, acc, tpe, loc)
        }

      case TypedAst.Expression.Let(sym, e1, e2, tpe, eff, loc) =>
        SimplifiedAst.Expression.Let(sym, visitExp(e1), visitExp(e2), tpe, loc)

      case TypedAst.Expression.LetRec(sym, e1, e2, tpe, eff, loc) =>
        SimplifiedAst.Expression.LetRec(sym, visitExp(e1), visitExp(e2), tpe, loc)

      case TypedAst.Expression.Match(exp0, rules, tpe, eff, loc) =>
        patternMatchWithLabels(exp0, rules, tpe, loc)

      case TypedAst.Expression.Tag(sym, tag, e, tpe, eff, loc) =>
        SimplifiedAst.Expression.Tag(sym, tag, visitExp(e), tpe, loc)

      case TypedAst.Expression.Tuple(elms, tpe, eff, loc) =>
        SimplifiedAst.Expression.Tuple(elms map visitExp, tpe, loc)

      case TypedAst.Expression.RecordEmpty(tpe, eff, loc) =>
        SimplifiedAst.Expression.RecordEmpty(tpe, loc)

      case TypedAst.Expression.RecordSelect(base, label, tpe, eff, loc) =>
        val b = visitExp(base)
        SimplifiedAst.Expression.RecordSelect(b, label, tpe, loc)

      case TypedAst.Expression.RecordExtend(label, value, rest, tpe, eff, loc) =>
        val v = visitExp(value)
        val r = visitExp(rest)
        SimplifiedAst.Expression.RecordExtend(label, v, r, tpe, loc)

      case TypedAst.Expression.RecordRestrict(label, rest, tpe, eff, loc) =>
        val r = visitExp(rest)
        SimplifiedAst.Expression.RecordRestrict(label, r, tpe, loc)

      case TypedAst.Expression.ArrayLit(elms, tpe, eff, loc) =>
        SimplifiedAst.Expression.ArrayLit(elms map visitExp, tpe, loc)

      case TypedAst.Expression.ArrayNew(elm, len, tpe, eff, loc) =>
        val e = visitExp(elm)
        val ln = visitExp(len)
        SimplifiedAst.Expression.ArrayNew(e, ln, tpe, loc)

      case TypedAst.Expression.ArrayLoad(base, index, tpe, eff, loc) =>
        val b = visitExp(base)
        val i = visitExp(index)
        SimplifiedAst.Expression.ArrayLoad(b, i, tpe, loc)

      case TypedAst.Expression.ArrayStore(base, index, elm, tpe, eff, loc) =>
        val b = visitExp(base)
        val i = visitExp(index)
        val e = visitExp(elm)
        SimplifiedAst.Expression.ArrayStore(b, i, e, tpe, loc)

      case TypedAst.Expression.ArrayLength(base, tpe, eff, loc) =>
        val b = visitExp(base)
        SimplifiedAst.Expression.ArrayLength(b, tpe, loc)

      case TypedAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) =>
        val b = visitExp(base)
        val i1 = visitExp(beginIndex)
        val i2 = visitExp(endIndex)
        SimplifiedAst.Expression.ArraySlice(b, i1, i2, tpe, loc)

      case TypedAst.Expression.VectorLit(elms, tpe, eff, loc) =>
        val es = elms map visitExp
        val t = Type.mkArray(tpe.typeArguments.head)
        SimplifiedAst.Expression.ArrayLit(es, t, loc)

      case TypedAst.Expression.VectorNew(elm, len, tpe, eff, loc) =>
        val e = visitExp(elm)
        val t = Type.mkArray(elm.tpe)
        val i = SimplifiedAst.Expression.Int32(len)
        SimplifiedAst.Expression.ArrayNew(e, i, t, loc)

      case TypedAst.Expression.VectorLoad(base, index, tpe, eff, loc) =>
        val b = visitExp(base)
        val i = SimplifiedAst.Expression.Int32(index)
        SimplifiedAst.Expression.ArrayLoad(b, i, tpe, loc)

      case TypedAst.Expression.VectorStore(base, index, elm, tpe, eff, loc) =>
        val b = visitExp(base)
        val e = visitExp(elm)
        val i = SimplifiedAst.Expression.Int32(index)
        SimplifiedAst.Expression.ArrayStore(b, i, e, tpe, loc)

      case TypedAst.Expression.VectorLength(base, tpe, eff, loc) =>
        val b = visitExp(base)
        SimplifiedAst.Expression.ArrayLength(b, tpe, loc)

      case TypedAst.Expression.VectorSlice(base, startIndex, endIndex, tpe, eff, loc) =>
        val b = visitExp(base)
        val i1 = SimplifiedAst.Expression.Int32(startIndex)
        val i2 = visitExp(endIndex)
        SimplifiedAst.Expression.ArraySlice(b, i1, i2, tpe, loc)

      case TypedAst.Expression.Ref(exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.Ref(e, tpe, loc)

      case TypedAst.Expression.Deref(exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.Deref(e, tpe, loc)

      case TypedAst.Expression.Assign(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        SimplifiedAst.Expression.Assign(e1, e2, tpe, loc)

      case TypedAst.Expression.HandleWith(exp, bindings, tpe, eff, loc) =>
        val e = visitExp(exp)
        val bs = bindings map {
          case TypedAst.HandlerBinding(sym, handler) => SimplifiedAst.HandlerBinding(sym, visitExp(handler))
        }
        SimplifiedAst.Expression.HandleWith(e, bs, tpe, loc)

      case TypedAst.Expression.Existential(fparam, exp, eff, loc) =>
        val p = SimplifiedAst.FormalParam(fparam.sym, fparam.mod, fparam.tpe, fparam.loc)
        val e = visitExp(exp)
        SimplifiedAst.Expression.Existential(p, e, loc)

      case TypedAst.Expression.Universal(fparam, exp, eff, loc) =>
        val p = SimplifiedAst.FormalParam(fparam.sym, fparam.mod, fparam.tpe, fparam.loc)
        val e = visitExp(exp)
        SimplifiedAst.Expression.Universal(p, e, loc)

      case TypedAst.Expression.Ascribe(exp, tpe, eff, loc) => visitExp(exp)

      case TypedAst.Expression.Cast(exp, tpe, eff, loc) => visitExp(exp)

      case TypedAst.Expression.TryCatch(exp, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules map {
          case TypedAst.CatchRule(sym, clazz, body) =>
            val b = visitExp(body)
            SimplifiedAst.CatchRule(sym, clazz, b)
        }
        SimplifiedAst.Expression.TryCatch(e, rs, tpe, loc)

      case TypedAst.Expression.NativeConstructor(constructor, args, tpe, eff, loc) =>
        val es = args.map(e => visitExp(e))
        SimplifiedAst.Expression.NativeConstructor(constructor, es, tpe, loc)

      case TypedAst.Expression.NativeField(field, tpe, eff, loc) =>
        SimplifiedAst.Expression.NativeField(field, tpe, loc)

      case TypedAst.Expression.NativeMethod(method, args, tpe, eff, loc) =>
        val es = args.map(e => visitExp(e))
        SimplifiedAst.Expression.NativeMethod(method, es, tpe, loc)

      case TypedAst.Expression.NewChannel(exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.NewChannel(e, tpe, loc)

      case TypedAst.Expression.GetChannel(exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.GetChannel(e, tpe, loc)

      case TypedAst.Expression.PutChannel(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        SimplifiedAst.Expression.PutChannel(e1, e2, tpe, loc)

      case TypedAst.Expression.SelectChannel(rules, default, tpe, eff, loc) =>
        val rs = rules map {
          case TypedAst.SelectChannelRule(sym, chan, exp) =>
            val c = visitExp(chan)
            val e = visitExp(exp)
            SimplifiedAst.SelectChannelRule(sym, c, e)
        }

        val d = default.map(visitExp(_))

        SimplifiedAst.Expression.SelectChannel(rs, d, tpe, loc)

      case TypedAst.Expression.Spawn(exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        // Make a function type, () -> e.tpe
        val newTpe = Type.mkArrow(Type.Cst(TypeConstructor.Unit), e.tpe)
        // Rewrite our Spawn expression to a Lambda
        val lambda = SimplifiedAst.Expression.Lambda(List(), e, newTpe, loc)
        SimplifiedAst.Expression.Spawn(lambda, newTpe, loc)

      case TypedAst.Expression.Sleep(exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.Sleep(e, tpe, loc)

      case TypedAst.Expression.FixpointConstraint(c0, tpe, eff, loc) =>
        val c = visitConstraint(c0)
        SimplifiedAst.Expression.FixpointConstraint(c, tpe, loc)

      case TypedAst.Expression.FixpointCompose(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        SimplifiedAst.Expression.FixpointCompose(e1, e2, tpe, loc)

      case TypedAst.Expression.FixpointSolve(exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.FixpointSolve(e, tpe, loc)

      case TypedAst.Expression.FixpointProject(pred, exp, tpe, eff, loc) =>
        val p = visitPredicateWithParam(pred)
        val e = visitExp(exp)
        SimplifiedAst.Expression.FixpointProject(p, e, tpe, loc)

      case TypedAst.Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        SimplifiedAst.Expression.FixpointEntails(e1, e2, tpe, loc)

      case TypedAst.Expression.Wild(tpe, eff, loc) => throw InternalCompilerException(s"Unexpected expression: $expr.")

    }

    /**
      * Translates the given `head` predicate to the SimplifiedAst.
      */
    def visitHeadPred(head: TypedAst.Predicate.Head, cparams: List[TypedAst.ConstraintParam]): SimplifiedAst.Predicate.Head = head match {
      case TypedAst.Predicate.Head.Atom(pred, terms, tpe, loc) =>
        val p = visitPredicateWithParam(pred)
        val ts = terms.map(t => exp2HeadTerm(t, cparams))
        SimplifiedAst.Predicate.Head.Atom(p, ts, tpe, loc)
    }

    /**
      * Translates the given `body` predicate to the SimplifiedAst.
      */
    def visitBodyPred(body: TypedAst.Predicate.Body, cparams: List[TypedAst.ConstraintParam]): SimplifiedAst.Predicate.Body = body match {
      case TypedAst.Predicate.Body.Atom(pred, polarity, terms, tpe, loc) =>
        val p = visitPredicateWithParam(pred)
        val ts = terms.map(p => pat2BodyTerm(p, cparams))
        SimplifiedAst.Predicate.Body.Atom(p, polarity, ts, tpe, loc)

      case TypedAst.Predicate.Body.Filter(sym, terms, loc) =>
        SimplifiedAst.Predicate.Body.Filter(sym, terms.map(t => exp2BodyTerm(t, cparams)), loc)

      case TypedAst.Predicate.Body.Functional(sym, term, loc) =>
        val cps = cparams.filter {
          case TypedAst.ConstraintParam.HeadParam(sym2, _, _) => sym != sym2
          case TypedAst.ConstraintParam.RuleParam(sym2, _, _) => sym != sym2
        }
        SimplifiedAst.Predicate.Body.Functional(sym, exp2HeadTerm(term, cps), loc)
    }

    /**
      * Translates the given predicate with parameter `p0` to the SimplifiedAst.
      */
    def visitPredicateWithParam(p0: TypedAst.PredicateWithParam): SimplifiedAst.PredicateWithParam = p0 match {
      case TypedAst.PredicateWithParam(sym, exp) =>
        val e = visitExp(exp)
        SimplifiedAst.PredicateWithParam(sym, e)
    }

    /**
      * Translates the given expression `e0` to the SimplifiedAst.
      */
    def exp2HeadTerm(e0: TypedAst.Expression, cparams: List[TypedAst.ConstraintParam]): SimplifiedAst.Term.Head = e0 match {
      case TypedAst.Expression.Var(sym, tpe, eff, loc) =>
        val isQuantified = cparams.exists(p => p.sym == sym)
        if (isQuantified)
          SimplifiedAst.Term.Head.QuantVar(sym, tpe, loc)
        else
          SimplifiedAst.Term.Head.CapturedVar(sym, tpe, loc)

      case TypedAst.Expression.Apply(TypedAst.Expression.Def(sym, _, _, _), exp, tpe, eff, loc) if exp.isInstanceOf[TypedAst.Expression.Var] =>
        val v = exp.asInstanceOf[TypedAst.Expression.Var]
        SimplifiedAst.Term.Head.App(sym, List(v.sym), tpe, loc)

      case _ =>
        // Determine if the expression is a literal.
        if (isExpLiteral(e0)) {
          // The expression is a literal.
          val lit = visitExp(e0)
          SimplifiedAst.Term.Head.Lit(lit, e0.tpe, e0.loc)
        } else {
          // The expression is not a literal.
          // Must create a new top-level definition for the expression.

          // Generate a fresh symbol for the new definition.
          val freshSym = Symbol.freshDefnSym("head")

          //
          // Special Case: No constraint parameters.
          //
          if (cparams.isEmpty) {
            // Construct the definition type.
            val arrowType = Type.mkArrow(Type.Cst(TypeConstructor.Unit), e0.tpe)

            // Assemble the fresh definition.
            val ann = Ast.Annotations.Empty
            val mod = Ast.Modifiers(List(Ast.Modifier.Synthetic))

            val varX = Symbol.freshVarSym("_unit")
            val param = SimplifiedAst.FormalParam(varX, mod, Type.Cst(TypeConstructor.Unit), SourceLocation.Unknown)

            val defn = SimplifiedAst.Def(ann, mod, freshSym, List(param), visitExp(e0), arrowType, e0.loc)

            // Add the fresh definition to the top-level.
            toplevel += freshSym -> defn

            // Return a head term that calls the freshly generated top-level definition.
            return SimplifiedAst.Term.Head.App(freshSym, Nil, e0.tpe, e0.loc)
          }

          // Generate fresh symbols for the formal parameters of the definition.
          val freshSymbols = cparams.map {
            case TypedAst.ConstraintParam.HeadParam(sym, tpe, _) => sym -> (Symbol.freshVarSym(sym), tpe)
            case TypedAst.ConstraintParam.RuleParam(sym, tpe, _) => sym -> (Symbol.freshVarSym(sym), tpe)
          }

          // Generate fresh formal parameters for the definition.
          val formals = freshSymbols.map {
            case (oldSym, (newSym, tpe)) => SimplifiedAst.FormalParam(newSym, Ast.Modifiers.Empty, tpe, oldSym.loc)
          }

          // The expression of the fresh definition is simply `e0` with fresh local variables.
          val exp = copyExp(visitExp(e0), freshSymbols.map(x => (x._1, x._2._1)).toMap)

          // Construct the definition type.
          val arrowType = Type.mkUncurriedArrow(freshSymbols.map(_._2._2), e0.tpe)

          // Assemble the fresh definition.
          val ann = Ast.Annotations.Empty
          val mod = Ast.Modifiers(List(Ast.Modifier.Synthetic))
          val defn = SimplifiedAst.Def(ann, mod, freshSym, formals, exp, arrowType, e0.loc)

          // Add the fresh definition to the top-level.
          toplevel += freshSym -> defn

          // Compute the argument symbols, i.e. the symbols of the rule.
          val argSymbols = freshSymbols.map(_._1)

          // Return a head term that calls the freshly generated top-level definition.
          SimplifiedAst.Term.Head.App(freshSym, argSymbols, e0.tpe, e0.loc)
        }
    }

    /**
      * Translates the given `lattice0` to the SimplifiedAst.
      */
    def visitLatticeComponents(lattice0: TypedAst.LatticeComponents): SimplifiedAst.LatticeComponents = lattice0 match {
      case TypedAst.LatticeComponents(tpe, bot0, top0, equ0, leq0, lub0, glb0, loc) =>

        /**
          * Introduces a unit function for the given expression `exp0`.
          */
        def mkUnitDef(name: String, exp0: TypedAst.Expression): Symbol.DefnSym = {
          val freshSym = Symbol.freshDefnSym(name)
          val ann = Ast.Annotations.Empty
          val mod = Ast.Modifiers(List(Ast.Modifier.Synthetic))
          val varX = Symbol.freshVarSym()
          val fparam = SimplifiedAst.FormalParam(varX, Ast.Modifiers.Empty, Type.Cst(TypeConstructor.Unit), SourceLocation.Unknown)
          val exp = visitExp(exp0)
          val freshDef = SimplifiedAst.Def(ann, mod, freshSym, List(fparam), exp, Type.mkArrow(Type.Cst(TypeConstructor.Unit), exp.tpe), loc)

          toplevel += (freshSym -> freshDef)
          freshSym
        }

        val bot = mkUnitDef("bot", bot0)
        val top = mkUnitDef("top", top0)

        // TODO: Unsafe assumption that these are symbols.
        val equ = visitExp(equ0).asInstanceOf[SimplifiedAst.Expression.Def].sym
        val leq = visitExp(leq0).asInstanceOf[SimplifiedAst.Expression.Def].sym
        val lub = visitExp(lub0).asInstanceOf[SimplifiedAst.Expression.Def].sym
        val glb = visitExp(glb0).asInstanceOf[SimplifiedAst.Expression.Def].sym
        SimplifiedAst.LatticeComponents(tpe, bot, top, equ, leq, lub, glb, loc)
    }

    /**
      * Translates the given `relation0` to the SimplifiedAst.
      */
    def visitRelation(relation0: TypedAst.Relation): SimplifiedAst.Relation = relation0 match {
      case TypedAst.Relation(doc, mod, sym, tparams, attributes, loc) =>
        SimplifiedAst.Relation(mod, sym, attributes.map(visitAttribute), loc)
    }

    /**
      * Translates the given `lattice0` to the SimplifiedAst.
      */
    def visitLattice(lattice0: TypedAst.Lattice): SimplifiedAst.Lattice = lattice0 match {
      case TypedAst.Lattice(doc, mod, sym, tparams, attributes, loc) =>
        SimplifiedAst.Lattice(mod, sym, attributes.map(visitAttribute), loc)
    }

    /**
      * Translates the given attribute `a` to the SimplifiedAst.
      */
    def visitAttribute(a: TypedAst.Attribute): SimplifiedAst.Attribute =
      SimplifiedAst.Attribute(a.name, a.tpe)

    /**
      * Translates the given formal param `p` to the SimplifiedAst.
      */
    def visitFormalParam(p: TypedAst.FormalParam): SimplifiedAst.FormalParam =
      SimplifiedAst.FormalParam(p.sym, p.mod, p.tpe, p.loc)

    /**
      * Translates the property `p` to the SimplifiedAst.
      */
    def visitProperty(p: TypedAst.Property): SimplifiedAst.Property =
      SimplifiedAst.Property(p.law, p.defn, visitExp(p.exp))

    /**
      * Translates the given pattern `p` to a body term.
      */
    def pat2BodyTerm(p: TypedAst.Pattern, cparams: List[TypedAst.ConstraintParam]): SimplifiedAst.Term.Body = p match {
      case TypedAst.Pattern.Wild(tpe, loc) => SimplifiedAst.Term.Body.Wild(tpe, loc)
      case TypedAst.Pattern.Var(sym, tpe, loc) =>
        val isQuantified = cparams.exists(p => p.sym == sym)
        if (isQuantified)
          SimplifiedAst.Term.Body.QuantVar(sym, tpe, loc)
        else
          SimplifiedAst.Term.Body.CapturedVar(sym, tpe, loc)

      case _ => if (isPatLiteral(p))
        SimplifiedAst.Term.Body.Lit(pat2exp(p), p.tpe, p.loc)
      else
        ???
    }

    /**
      * Translates the given expression `e` to a body term.
      */
    def exp2BodyTerm(e: TypedAst.Expression, cparams: List[TypedAst.ConstraintParam]): SimplifiedAst.Term.Body = e match {
      case TypedAst.Expression.Wild(tpe, eff, loc) => SimplifiedAst.Term.Body.Wild(tpe, loc)
      case TypedAst.Expression.Var(sym, tpe, eff, loc) =>
        val isQuantified = cparams.exists(p => p.sym == sym)
        if (isQuantified)
          SimplifiedAst.Term.Body.QuantVar(sym, tpe, loc)
        else
          SimplifiedAst.Term.Body.CapturedVar(sym, tpe, loc)

      case _ => SimplifiedAst.Term.Body.Lit(visitExp(e), e.tpe, e.loc) // TODO: Only certain expressions should be allow here.
    }

    /**
      * Returns the given pattern `pat0` as an expression.
      */
    def pat2exp(pat0: TypedAst.Pattern): SimplifiedAst.Expression = pat0 match {
      case TypedAst.Pattern.Unit(loc) => SimplifiedAst.Expression.Unit
      case TypedAst.Pattern.True(loc) => SimplifiedAst.Expression.True
      case TypedAst.Pattern.False(loc) => SimplifiedAst.Expression.False
      case TypedAst.Pattern.Char(lit, loc) => SimplifiedAst.Expression.Char(lit)
      case TypedAst.Pattern.Float32(lit, loc) => SimplifiedAst.Expression.Float32(lit)
      case TypedAst.Pattern.Float64(lit, loc) => SimplifiedAst.Expression.Float64(lit)
      case TypedAst.Pattern.Int8(lit, loc) => SimplifiedAst.Expression.Int8(lit)
      case TypedAst.Pattern.Int16(lit, loc) => SimplifiedAst.Expression.Int16(lit)
      case TypedAst.Pattern.Int32(lit, loc) => SimplifiedAst.Expression.Int32(lit)
      case TypedAst.Pattern.Int64(lit, loc) => SimplifiedAst.Expression.Int64(lit)
      case TypedAst.Pattern.BigInt(lit, loc) => SimplifiedAst.Expression.BigInt(lit)
      case TypedAst.Pattern.Str(lit, loc) => SimplifiedAst.Expression.Str(lit)
      case TypedAst.Pattern.Tag(sym, tag, p, tpe, loc) => SimplifiedAst.Expression.Tag(sym, tag, pat2exp(p), tpe, loc)
      case TypedAst.Pattern.Tuple(elms, tpe, loc) => SimplifiedAst.Expression.Tuple(elms map pat2exp, tpe, loc)
      case _ => throw InternalCompilerException(s"Unexpected non-literal pattern $pat0.")
    }

    /**
      * Returns `true` if the given pattern `pat0` is a literal.
      */
    def isPatLiteral(pat0: TypedAst.Pattern): Boolean = pat0 match {
      case TypedAst.Pattern.Unit(loc) => true
      case TypedAst.Pattern.True(loc) => true
      case TypedAst.Pattern.False(loc) => true
      case TypedAst.Pattern.Char(lit, loc) => true
      case TypedAst.Pattern.Float32(lit, loc) => true
      case TypedAst.Pattern.Float64(lit, loc) => true
      case TypedAst.Pattern.Int8(lit, loc) => true
      case TypedAst.Pattern.Int16(lit, loc) => true
      case TypedAst.Pattern.Int32(lit, loc) => true
      case TypedAst.Pattern.Int64(lit, loc) => true
      case TypedAst.Pattern.BigInt(lit, loc) => true
      case TypedAst.Pattern.Str(lit, loc) => true
      case _ => false
    }

    /**
      * Returns `true` if the given expression `exp0` is a literal.
      */
    def isExpLiteral(exp0: TypedAst.Expression): Boolean = exp0 match {
      case TypedAst.Expression.Unit(loc) => true
      case TypedAst.Expression.True(loc) => true
      case TypedAst.Expression.False(loc) => true
      case TypedAst.Expression.Char(lit, loc) => true
      case TypedAst.Expression.Float32(lit, loc) => true
      case TypedAst.Expression.Float64(lit, loc) => true
      case TypedAst.Expression.Int8(lit, loc) => true
      case TypedAst.Expression.Int16(lit, loc) => true
      case TypedAst.Expression.Int32(lit, loc) => true
      case TypedAst.Expression.Int64(lit, loc) => true
      case TypedAst.Expression.BigInt(lit, loc) => true
      case TypedAst.Expression.Str(lit, loc) => true
      case TypedAst.Expression.Tag(sym, tag, exp, tpe, eff, loc) => isExpLiteral(exp)
      case TypedAst.Expression.Tuple(elms, tpe, eff, loc) => elms forall isExpLiteral
      case _ => false
    }

    /**
      * Returns an expression that compares the two given expressions `e1` and `e2` for equality.
      */
    def mkEqual(e1: SimplifiedAst.Expression, e2: SimplifiedAst.Expression, loc: SourceLocation): SimplifiedAst.Expression = {
      /*
       * Special Case 1: Unit
       */
      (e1.tpe, e2.tpe) match {
        case (Type.Cst(TypeConstructor.Unit), Type.Cst(TypeConstructor.Unit)) =>
          // Unit is always equal to itself.
          return SimplifiedAst.Expression.True
        case _ => // fallthrough
      }

      /*
       * Compute the semantic operator.
       */
      val sop = e1.tpe match {
        case Type.Cst(TypeConstructor.Bool) => SemanticOperator.BoolOp.Eq
        case Type.Cst(TypeConstructor.Char) => SemanticOperator.CharOp.Eq
        case Type.Cst(TypeConstructor.Float32) => SemanticOperator.Float32Op.Eq
        case Type.Cst(TypeConstructor.Float64) => SemanticOperator.Float64Op.Eq
        case Type.Cst(TypeConstructor.Int8) => SemanticOperator.Int8Op.Eq
        case Type.Cst(TypeConstructor.Int16) => SemanticOperator.Int16Op.Eq
        case Type.Cst(TypeConstructor.Int32) => SemanticOperator.Int32Op.Eq
        case Type.Cst(TypeConstructor.Int64) => SemanticOperator.Int64Op.Eq
        case Type.Cst(TypeConstructor.BigInt) => SemanticOperator.BigIntOp.Eq
        case Type.Cst(TypeConstructor.Str) => SemanticOperator.StringOp.Eq
        case t => throw InternalCompilerException(s"Unexpected type: '$t'.")
      }

      SimplifiedAst.Expression.Binary(sop, BinaryOperator.Equal, e1, e2, Type.Cst(TypeConstructor.Bool), loc)
    }

    /**
      * Eliminates pattern matching by translations to labels and jumps.
      */
    def patternMatchWithLabels(exp0: TypedAst.Expression, rules: List[TypedAst.MatchRule], tpe: Type, loc: SourceLocation): SimplifiedAst.Expression = {
      //
      // Given the code:
      //
      // match x with {
      //   case PATTERN_1 => BODY_1
      //   case PATTERN_2 => BODY_2
      //   ...
      //   case PATTERN_N => BODY_N
      // }
      //
      // The structure of the generated code is as follows:
      //
      // let matchVar = x ;
      //
      //   branch {
      //     jumpto label$1
      //
      //     label$1:
      //       ...
      //     label$2:
      //       ...
      //     default:
      //       MatchError
      //   }
      //

      // Generate a fresh variable to hold the result of the match expression.
      val matchVar = Symbol.freshVarSym("matchVar")

      // Translate the match expression.
      val matchExp = visitExp(exp0)

      // Generate a fresh label for the default fall through case.
      val defaultLab = Symbol.freshLabel("default")

      // Generate a label for each rule.
      val ruleLabels = rules.map(_ => Symbol.freshLabel("case"))

      // Construct a map from each label to the label of the next case.
      // The default label is the next label of the last case.
      val nextLabel = (ruleLabels zip (ruleLabels.drop(1) ::: defaultLab :: Nil)).toMap

      // Create a branch for each rule.
      val branches = (ruleLabels zip rules) map {
        // Process each (label, rule) pair.
        case (label, TypedAst.MatchRule(pat, guard, body)) =>
          // Retrieve the label of the next rule.
          // If this rule is the last, the next label is the default label.
          val next = nextLabel(label)

          // Success case: evaluate the match body.
          val success = visitExp(body)

          // Failure case: Jump to the next label.
          val failure = SimplifiedAst.Expression.JumpTo(next, tpe, loc)

          // Return the branch with its label.
          label -> patternMatchList(List(pat), List(matchVar), guard, success, failure
          )
      }

      // Construct the error branch.
      val errorBranch = defaultLab -> SimplifiedAst.Expression.MatchError(tpe, loc)

      // The initial expression simply jumps to the first label.
      val entry = SimplifiedAst.Expression.JumpTo(ruleLabels.head, tpe, loc)

      // Assemble all the branches together.
      val branch = SimplifiedAst.Expression.Branch(entry, branches.toMap + errorBranch, tpe, loc)

      // Wrap the branches inside a let-binding for the match variable.
      SimplifiedAst.Expression.Let(matchVar, matchExp, branch, tpe, loc)
    }


    /**
      * Returns an expression that matches the given list of patterns `xs` against the given list of variables `ys`.
      *
      * Checks the `guard` when all patterns have been matched.
      *
      * Evaluates `succ` on success and `fail` otherwise.
      */
    def patternMatchList(xs: List[TypedAst.Pattern], ys: List[Symbol.VarSym], guard: TypedAst.Expression, succ: SimplifiedAst.Expression, fail: SimplifiedAst.Expression): SimplifiedAst.Expression =
      ((xs, ys): @unchecked) match {
        /**
          * There are no more patterns and variables to match.
          *
          * The pattern was match successfully. Test the guard.
          */
        case (Nil, Nil) =>
          val g = visitExp(guard)
          SimplifiedAst.Expression.IfThenElse(g, succ, fail, succ.tpe, g.loc)

        /**
          * Matching a wildcard is guaranteed to succeed.
          *
          * We proceed by recursion on the remaining patterns and variables.
          */
        case (TypedAst.Pattern.Wild(tpe, loc) :: ps, v :: vs) =>
          patternMatchList(ps, vs, guard, succ, fail)

        /**
          * Matching a variable is guaranteed to succeed.
          *
          * We proceed by constructing a let-binding that binds the value
          * of the match variable `ident` to the variable `v`.
          * The body of the let-binding is computed by recursion on the
          * remaining patterns and variables.
          */
        case (TypedAst.Pattern.Var(sym, tpe, loc) :: ps, v :: vs) =>
          val exp = patternMatchList(ps, vs, guard, succ, fail)
          SimplifiedAst.Expression.Let(sym, SimplifiedAst.Expression.Var(v, tpe, loc), exp, succ.tpe, loc)

        /**
          * Matching a literal may succeed or fail.
          *
          * We generate a binary expression testing whether the literal `lit`
          * matches the variable `v` and then we generate an if-then-else
          * expression where the consequent expression is determined by
          * recursion on the remaining patterns and variables and the
          * alternative expression is `fail`.
          */
        case (lit :: ps, v :: vs) if isPatLiteral(lit) =>
          val exp = patternMatchList(ps, vs, guard, succ, fail)
          val cond = mkEqual(pat2exp(lit), SimplifiedAst.Expression.Var(v, lit.tpe, lit.loc), lit.loc)
          SimplifiedAst.Expression.IfThenElse(cond, exp, fail, succ.tpe, lit.loc)

        /**
          * Matching a tag may succeed or fail.
          *
          * We generate a binary expression testing whether the tag name `tag`
          * matches the tag extracted from the variable `v` and then we generate
          * an if-then-else expression where the consequent expression is determined
          * by recursion on the remaining patterns and variables together with the
          * nested pattern of the tag added in front and a new fresh variable holding
          * the value of the tag.
          */
        case (TypedAst.Pattern.Tag(sym, tag, pat, tpe, loc) :: ps, v :: vs) =>
          val cond = SimplifiedAst.Expression.Is(sym, tag, SimplifiedAst.Expression.Var(v, tpe, loc), loc)
          val freshVar = Symbol.freshVarSym("innerTag")
          val inner = patternMatchList(pat :: ps, freshVar :: vs, guard, succ, fail)
          val consequent = SimplifiedAst.Expression.Let(freshVar, SimplifiedAst.Expression.Untag(sym, tag, SimplifiedAst.Expression.Var(v, tpe, loc), pat.tpe, loc), inner, succ.tpe, loc)
          SimplifiedAst.Expression.IfThenElse(cond, consequent, fail, succ.tpe, loc)

        /**
          * Matching a tuple may succeed or fail.
          *
          * We generate a fresh variable and let-binding for each component of the
          * tuple and then we recurse on the nested patterns and freshly generated
          * variables.
          */
        case (TypedAst.Pattern.Tuple(elms, tpe, loc) :: ps, v :: vs) =>
          val freshVars = elms.map(_ => Symbol.freshVarSym("innerElm"))
          val zero = patternMatchList(elms ::: ps, freshVars ::: vs, guard, succ, fail)
          elms.zip(freshVars).zipWithIndex.foldRight(zero) {
            case (((pat, name), idx), exp) =>
              SimplifiedAst.Expression.Let(name, SimplifiedAst.Expression.Index(SimplifiedAst.Expression.Var(v, tpe, loc), idx, pat.tpe, loc), exp, succ.tpe, loc)
          }

        case p => throw InternalCompilerException(s"Unsupported pattern '$p'.")
      }


    //
    // Main computation.
    //
    val defns = root.defs.map { case (k, v) => k -> visitDef(v) }
    val effs = root.effs.map { case (k, v) => k -> visitEff(v) }
    val handlers = root.handlers.map { case (k, v) => k -> visitHandler(v) }
    val enums = root.enums.map {
      case (k, TypedAst.Enum(doc, mod, sym, tparams, cases0, enumType, loc)) =>
        val cases = cases0 map {
          case (tag, TypedAst.Case(enumSym, tagName, tagType, tagLoc)) => tag -> SimplifiedAst.Case(enumSym, tagName, tagType, tagLoc)
        }
        k -> SimplifiedAst.Enum(mod, sym, cases, enumType, loc)
    }
    val latticeComponents = root.latticeComponents.map { case (k, v) => k -> visitLatticeComponents(v) }
    val relations = root.relations.map { case (k, v) => k -> visitRelation(v) }
    val lattices = root.lattices.map { case (k, v) => k -> visitLattice(v) }
    val properties = root.properties.map { p => visitProperty(p) }
    val specialOps = root.specialOps
    val reachable = root.reachable

    SimplifiedAst.Root(defns ++ toplevel, effs, handlers, enums, relations, lattices, latticeComponents, properties, specialOps, reachable, root.sources).toSuccess
  }

  /**
    * Returns a copy of the given expression `exp0` where every variable symbol
    * has been replaced according to the given substitution `m`.
    */
  def copyExp(exp0: SimplifiedAst.Expression, m: Map[Symbol.VarSym, Symbol.VarSym]): SimplifiedAst.Expression = {

    def visitExp(e: SimplifiedAst.Expression): SimplifiedAst.Expression = e match {
      case SimplifiedAst.Expression.Unit => e
      case SimplifiedAst.Expression.True => e
      case SimplifiedAst.Expression.False => e
      case SimplifiedAst.Expression.Char(lit) => e
      case SimplifiedAst.Expression.Float32(lit) => e
      case SimplifiedAst.Expression.Float64(lit) => e
      case SimplifiedAst.Expression.Int8(lit) => e
      case SimplifiedAst.Expression.Int16(lit) => e
      case SimplifiedAst.Expression.Int32(lit) => e
      case SimplifiedAst.Expression.Int64(lit) => e
      case SimplifiedAst.Expression.BigInt(lit) => e
      case SimplifiedAst.Expression.Str(lit) => e
      case SimplifiedAst.Expression.Var(sym, tpe, loc) => m.get(sym) match {
        case None => SimplifiedAst.Expression.Var(sym, tpe, loc)
        case Some(replacement) => SimplifiedAst.Expression.Var(replacement, tpe, loc)
      }
      case SimplifiedAst.Expression.Def(sym, tpe, loc) => e
      case SimplifiedAst.Expression.Eff(sym, tpe, loc) => e
      case SimplifiedAst.Expression.Lambda(fparams, body, tpe, loc) =>
        SimplifiedAst.Expression.Lambda(fparams, visitExp(body), tpe, loc)
      case SimplifiedAst.Expression.Apply(exp, args, tpe, loc) =>
        SimplifiedAst.Expression.Apply(visitExp(exp), args.map(visitExp), tpe, loc)
      case SimplifiedAst.Expression.Unary(sop, op, exp, tpe, loc) =>
        SimplifiedAst.Expression.Unary(sop, op, visitExp(exp), tpe, loc)
      case SimplifiedAst.Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        SimplifiedAst.Expression.Binary(sop, op, visitExp(exp1), visitExp(exp2), tpe, loc)
      case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        SimplifiedAst.Expression.IfThenElse(visitExp(exp1), visitExp(exp2), visitExp(exp3), tpe, loc)
      case SimplifiedAst.Expression.Branch(exp, branches, tpe, loc) =>
        val e = visitExp(exp)
        val bs = branches map {
          case (sym, br) => sym -> br
        }
        SimplifiedAst.Expression.Branch(e, bs, tpe, loc)
      case SimplifiedAst.Expression.JumpTo(sym, tpe, loc) =>
        SimplifiedAst.Expression.JumpTo(sym, tpe, loc)
      case SimplifiedAst.Expression.Let(sym, exp1, exp2, tpe, loc) =>
        SimplifiedAst.Expression.Let(sym, visitExp(exp1), visitExp(exp2), tpe, loc)
      case SimplifiedAst.Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
        SimplifiedAst.Expression.LetRec(sym, visitExp(exp1), visitExp(exp2), tpe, loc)
      case SimplifiedAst.Expression.Is(sym, tag, exp, loc) =>
        SimplifiedAst.Expression.Is(sym, tag, visitExp(exp), loc)
      case SimplifiedAst.Expression.Tag(enum, tag, exp, tpe, loc) =>
        SimplifiedAst.Expression.Tag(enum, tag, visitExp(exp), tpe, loc)
      case SimplifiedAst.Expression.Untag(sym, tag, exp, tpe, loc) =>
        SimplifiedAst.Expression.Untag(sym, tag, visitExp(exp), tpe, loc)
      case SimplifiedAst.Expression.Index(exp, offset, tpe, loc) =>
        SimplifiedAst.Expression.Index(visitExp(exp), offset, tpe, loc)
      case SimplifiedAst.Expression.Tuple(elms, tpe, loc) =>
        SimplifiedAst.Expression.Tuple(elms.map(visitExp), tpe, loc)

      case SimplifiedAst.Expression.RecordEmpty(tpe, loc) =>
        SimplifiedAst.Expression.RecordEmpty(tpe, loc)

      case SimplifiedAst.Expression.RecordSelect(exp, label, tpe, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.RecordSelect(e, label, tpe, loc)

      case SimplifiedAst.Expression.RecordExtend(label, value, rest, tpe, loc) =>
        val v = visitExp(value)
        val r = visitExp(rest)
        SimplifiedAst.Expression.RecordExtend(label, v, r, tpe, loc)

      case SimplifiedAst.Expression.RecordRestrict(label, rest, tpe, loc) =>
        val r = visitExp(rest)
        SimplifiedAst.Expression.RecordRestrict(label, r, tpe, loc)

      case SimplifiedAst.Expression.ArrayLit(elms, tpe, loc) =>
        SimplifiedAst.Expression.ArrayLit(elms.map(visitExp), tpe, loc)
      case SimplifiedAst.Expression.ArrayNew(elm, len, tpe, loc) =>
        SimplifiedAst.Expression.ArrayNew(visitExp(elm), visitExp(len), tpe, loc)
      case SimplifiedAst.Expression.ArrayLoad(base, index, tpe, loc) =>
        SimplifiedAst.Expression.ArrayLoad(visitExp(base), visitExp(index), tpe, loc)
      case SimplifiedAst.Expression.ArrayStore(base, index, elm, tpe, loc) =>
        SimplifiedAst.Expression.ArrayStore(visitExp(base), visitExp(index), visitExp(elm), tpe, loc)
      case SimplifiedAst.Expression.ArrayLength(base, tpe, loc) =>
        SimplifiedAst.Expression.ArrayLength(visitExp(base), tpe, loc)
      case SimplifiedAst.Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
        SimplifiedAst.Expression.ArraySlice(visitExp(base), visitExp(startIndex), visitExp(endIndex), tpe, loc)
      case SimplifiedAst.Expression.Ref(exp, tpe, loc) =>
        SimplifiedAst.Expression.Ref(visitExp(exp), tpe, loc)
      case SimplifiedAst.Expression.Deref(exp, tpe, loc) =>
        SimplifiedAst.Expression.Deref(visitExp(exp), tpe, loc)
      case SimplifiedAst.Expression.Assign(exp1, exp2, tpe, loc) =>
        SimplifiedAst.Expression.Assign(visitExp(exp1), visitExp(exp2), tpe, loc)
      case SimplifiedAst.Expression.HandleWith(exp, bindings, tpe, loc) =>
        val e = copyExp(exp, m)
        val bs = bindings map {
          case SimplifiedAst.HandlerBinding(sym, handler) => SimplifiedAst.HandlerBinding(sym, copyExp(handler, m))
        }
        SimplifiedAst.Expression.HandleWith(e, bs, tpe, loc)
      case SimplifiedAst.Expression.Existential(params, exp, loc) =>
        SimplifiedAst.Expression.Existential(params, visitExp(exp), loc)
      case SimplifiedAst.Expression.Universal(params, exp, loc) =>
        SimplifiedAst.Expression.Universal(params, visitExp(exp), loc)

      case SimplifiedAst.Expression.TryCatch(exp, rules, tpe, loc) =>
        val e = visitExp(exp)
        val rs = rules map {
          case SimplifiedAst.CatchRule(sym, clazz, body) =>
            val b = visitExp(body)
            SimplifiedAst.CatchRule(sym, clazz, b)
        }
        SimplifiedAst.Expression.TryCatch(e, rs, tpe, loc)

      case SimplifiedAst.Expression.NativeConstructor(constructor, args, tpe, loc) =>
        val es = args map visitExp
        SimplifiedAst.Expression.NativeConstructor(constructor, es, tpe, loc)

      case SimplifiedAst.Expression.NativeField(field, tpe, loc) =>
        SimplifiedAst.Expression.NativeField(field, tpe, loc)

      case SimplifiedAst.Expression.NativeMethod(method, args, tpe, loc) =>
        val es = args map visitExp
        SimplifiedAst.Expression.NativeMethod(method, es, tpe, loc)

      case SimplifiedAst.Expression.NewChannel(exp, tpe, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.NewChannel(e, tpe, loc)

      case SimplifiedAst.Expression.GetChannel(exp, tpe, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.GetChannel(e, tpe, loc)

      case SimplifiedAst.Expression.PutChannel(exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        SimplifiedAst.Expression.PutChannel(e1, e2, tpe, loc)

      case SimplifiedAst.Expression.SelectChannel(rules, default, tpe, loc) =>
        val rs = rules map {
          case SimplifiedAst.SelectChannelRule(sym, chan, exp) =>
            val c = visitExp(chan)
            val e = visitExp(exp)
            SimplifiedAst.SelectChannelRule(sym, c, e)
        }

        val d = default.map(visitExp(_))

        SimplifiedAst.Expression.SelectChannel(rs, d, tpe, loc)

      case SimplifiedAst.Expression.Spawn(exp, tpe, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.Spawn(e, tpe, loc)

      case SimplifiedAst.Expression.Sleep(exp, tpe, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.Sleep(e, tpe, loc)

      case SimplifiedAst.Expression.FixpointConstraint(c0, tpe, loc) =>
        val c = visitConstraint(c0)
        SimplifiedAst.Expression.FixpointConstraint(c, tpe, loc)

      case SimplifiedAst.Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        SimplifiedAst.Expression.FixpointCompose(e1, e2, tpe, loc)

      case SimplifiedAst.Expression.FixpointSolve(exp, tpe, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Expression.FixpointSolve(e, tpe, loc)

      case SimplifiedAst.Expression.FixpointProject(pred, exp, tpe, loc) =>
        val p = visitPredicateWithParam(pred)
        val e = visitExp(exp)
        SimplifiedAst.Expression.FixpointProject(p, e, tpe, loc)

      case SimplifiedAst.Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        SimplifiedAst.Expression.FixpointEntails(e1, e2, tpe, loc)

      case SimplifiedAst.Expression.HoleError(sym, tpe, loc) => e
      case SimplifiedAst.Expression.MatchError(tpe, loc) => e
      case SimplifiedAst.Expression.SwitchError(tpe, loc) => e

      case SimplifiedAst.Expression.Closure(ref, freeVars, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case SimplifiedAst.Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case SimplifiedAst.Expression.ApplyClo(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case SimplifiedAst.Expression.ApplyDef(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case SimplifiedAst.Expression.ApplyEff(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case SimplifiedAst.Expression.ApplyCloTail(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case SimplifiedAst.Expression.ApplyDefTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case SimplifiedAst.Expression.ApplyEffTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case SimplifiedAst.Expression.ApplySelfTail(name, formals, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    }

    def visitConstraint(c0: SimplifiedAst.Constraint): SimplifiedAst.Constraint = c0 match {
      case SimplifiedAst.Constraint(cparams, head0, body0) =>
        val head = visitHeadPred(head0)
        val body = body0.map(visitBodyPred)
        SimplifiedAst.Constraint(cparams, head, body)
    }

    def visitHeadPred(h0: SimplifiedAst.Predicate.Head): SimplifiedAst.Predicate.Head = h0 match {
      case SimplifiedAst.Predicate.Head.Atom(pred, terms, tpe, loc) =>
        val p = visitPredicateWithParam(pred)
        val ts = terms.map(visitHeadTerm)
        SimplifiedAst.Predicate.Head.Atom(p, ts, tpe, loc)
    }

    def visitBodyPred(b0: SimplifiedAst.Predicate.Body): SimplifiedAst.Predicate.Body = b0 match {
      case SimplifiedAst.Predicate.Body.Atom(pred, polarity, terms, tpe, loc) =>
        val p = visitPredicateWithParam(pred)
        val ts = terms.map(visitBodyTerm)
        SimplifiedAst.Predicate.Body.Atom(p, polarity, ts, tpe, loc)

      case SimplifiedAst.Predicate.Body.Filter(sym, terms, loc) =>
        val ts = terms.map(visitBodyTerm)
        SimplifiedAst.Predicate.Body.Filter(sym, ts, loc)

      case SimplifiedAst.Predicate.Body.Functional(sym, term, loc) =>
        val t = visitHeadTerm(term)
        SimplifiedAst.Predicate.Body.Functional(sym, t, loc)
    }

    def visitHeadTerm(t0: SimplifiedAst.Term.Head): SimplifiedAst.Term.Head = t0 match {
      case SimplifiedAst.Term.Head.QuantVar(sym, tpe, loc) => t0

      case SimplifiedAst.Term.Head.CapturedVar(sym, tpe, loc) =>
        val s = m.getOrElse(sym, sym)
        SimplifiedAst.Term.Head.CapturedVar(s, tpe, loc)

      case SimplifiedAst.Term.Head.Lit(exp, tpe, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Term.Head.Lit(e, tpe, loc)

      case SimplifiedAst.Term.Head.App(sym, args, tpe, loc) => t0
    }

    def visitBodyTerm(t0: SimplifiedAst.Term.Body): SimplifiedAst.Term.Body = t0 match {
      case SimplifiedAst.Term.Body.Wild(tpe, loc) => t0

      case SimplifiedAst.Term.Body.QuantVar(sym, tpe, loc) =>
        val s = m.getOrElse(sym, sym)
        SimplifiedAst.Term.Body.QuantVar(s, tpe, loc)

      case SimplifiedAst.Term.Body.CapturedVar(sym, tpe, loc) => t0

      case SimplifiedAst.Term.Body.Lit(exp, tpe, loc) =>
        val e = visitExp(exp)
        SimplifiedAst.Term.Body.Lit(e, tpe, loc)
    }

    def visitPredicateWithParam(p0: SimplifiedAst.PredicateWithParam): SimplifiedAst.PredicateWithParam = p0 match {
      case SimplifiedAst.PredicateWithParam(sym, exp) =>
        val e = visitExp(exp)
        SimplifiedAst.PredicateWithParam(sym, e)
    }

    visitExp(exp0)
  }

}
