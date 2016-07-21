/*
 * Copyright 2015-2016 Luqman Aden, Magnus Madsen
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

import ca.uwaterloo.flix.language.ast.SimplifiedAst.Definition.Constant
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression._
import ca.uwaterloo.flix.language.ast.SimplifiedAst.{Expression, ExpressionFolder, FreeVar, Root}
import ca.uwaterloo.flix.language.ast.{BinaryOperator, Name, SourceLocation, Type}
import ca.uwaterloo.flix.util.InternalCompilerException

object Optimizer {

  def optimize(root: Root): Root = {
    val constants = root.constants.map {
      case (name, defn) =>
        var fn = defn

        // Do simple copy propagation to get rid of targets of direct assignment
        fn = CopyPropagation.optimize(fn)

        // Clean up by simplifying some expressions and removing dead code
        fn = ConstantFolding.optimize(fn)

        name -> fn
    }

    root.copy(constants = constants)
  }

  object ConstantFolding extends ExpressionFolder {

    override def foldBinary(op: BinaryOperator,
                            exp1: Expression,
                            exp2: Expression,
                            tpe: Type,
                            loc: SourceLocation): Expression = {

      op match {
        // Replace an equality check on the Unit type with just True
        // We need not check that exp2 is also of type Unit since
        // the type checker wil guarantee that exp1.tpe == exp2.tpe.
        case BinaryOperator.Equal =>
          if (exp1.tpe == Unit.tpe) {
            True
          } else {
            super.foldBinary(op, exp1, exp2, tpe, loc)
          }

        // Simplify arithmetic operations where we already know exp1 and exp2
        case BinaryOperator.Plus =>
          val fExp1 = foldExpression(exp1)
          val fExp2 = foldExpression(exp2)

          (fExp1, fExp2) match {
            case (Expression.Float32(a), Expression.Float32(b)) => Expression.Float32(a + b)
            case (Expression.Float64(a), Expression.Float64(b)) => Expression.Float64(a + b)
            case (Expression.Int8(a), Expression.Int8(b)) => Expression.Int8((a + b).toByte)
            case (Expression.Int16(a), Expression.Int16(b)) => Expression.Int16((a + b).toShort)
            case (Expression.Int32(a), Expression.Int32(b)) => Expression.Int32(a + b)
            case (Expression.Int64(a), Expression.Int64(b)) => Expression.Int64(a + b)
            case (Expression.BigInt(a), Expression.BigInt(b)) => Expression.BigInt(a.add(b))
            case _ => Expression.Binary(op, fExp1, fExp2, tpe, loc)
          }

        case BinaryOperator.Minus =>
          val fExp1 = foldExpression(exp1)
          val fExp2 = foldExpression(exp2)

          (fExp1, fExp2) match {
            case (Expression.Float32(a), Expression.Float32(b)) => Expression.Float32(a - b)
            case (Expression.Float64(a), Expression.Float64(b)) => Expression.Float64(a - b)
            case (Expression.Int8(a), Expression.Int8(b)) => Expression.Int8((a - b).toByte)
            case (Expression.Int16(a), Expression.Int16(b)) => Expression.Int16((a - b).toShort)
            case (Expression.Int32(a), Expression.Int32(b)) => Expression.Int32(a - b)
            case (Expression.Int64(a), Expression.Int64(b)) => Expression.Int64(a - b)
            case (Expression.BigInt(a), Expression.BigInt(b)) => Expression.BigInt(a.subtract(b))
            case _ => Expression.Binary(op, fExp1, fExp2, tpe, loc)
          }

        case BinaryOperator.Times =>
          val fExp1 = foldExpression(exp1)
          val fExp2 = foldExpression(exp2)

          (fExp1, fExp2) match {
            case (Expression.Float32(a), Expression.Float32(b)) => Expression.Float32(a * b)
            case (Expression.Float64(a), Expression.Float64(b)) => Expression.Float64(a * b)
            case (Expression.Int8(a), Expression.Int8(b)) => Expression.Int8((a * b).toByte)
            case (Expression.Int16(a), Expression.Int16(b)) => Expression.Int16((a * b).toShort)
            case (Expression.Int32(a), Expression.Int32(b)) => Expression.Int32(a * b)
            case (Expression.Int64(a), Expression.Int64(b)) => Expression.Int64(a * b)
            case (Expression.BigInt(a), Expression.BigInt(b)) => Expression.BigInt(a.multiply(b))
            case _ => Expression.Binary(op, fExp1, fExp2, tpe, loc)
          }

        case BinaryOperator.Divide =>
          val fExp1 = foldExpression(exp1)
          val fExp2 = foldExpression(exp2)

          (fExp1, fExp2) match {
            case (Expression.Float32(a), Expression.Float32(b)) =>
              if (b == 0) {
                throw InternalCompilerException("Tried to divide by 0.")
              } else {
                Expression.Float32(a / b)
              }
            case (Expression.Float64(a), Expression.Float64(b)) =>
              if (b == 0) {
                throw InternalCompilerException("Tried to divide by 0.")
              } else {
                Expression.Float64(a / b)
              }
            case (Expression.Int8(a), Expression.Int8(b)) =>
              if (b == 0) {
                throw InternalCompilerException("Tried to divide by 0.")
              } else {
                Expression.Int8((a / b).toByte)
              }
            case (Expression.Int16(a), Expression.Int16(b)) =>
              if (b == 0) {
                throw InternalCompilerException("Tried to divide by 0.")
              } else {
                Expression.Int16((a / b).toShort)
              }
            case (Expression.Int32(a), Expression.Int32(b)) =>
              if (b == 0) {
                throw InternalCompilerException("Tried to divide by 0.")
              } else {
                Expression.Int32(a / b)
              }
            case (Expression.Int64(a), Expression.Int64(b)) =>
              if (b == 0) {
                throw InternalCompilerException("Tried to divide by 0.")
              } else {
                Expression.Int64(a / b)
              }
            case (Expression.BigInt(a), Expression.BigInt(b)) =>
              if (b == 0) {
                throw InternalCompilerException("Tried to divide by 0.")
              } else {
                Expression.BigInt(a.divide(b))
              }
            case _ => Expression.Binary(op, fExp1, fExp2, tpe, loc)
          }

        case _ => super.foldBinary(op, exp1, exp2, tpe, loc)
      }
    }

    override def foldIfThenElse(cond: Expression,
                                thenExp: Expression,
                                elseExp: Expression,
                                tpe: Type,
                                loc: SourceLocation): Expression = {
      val fCond = super.foldExpression(cond)
      fCond match {
        // Condition known to be true, so replace whole expression with just the consequent expression
        case Expression.True => super.foldExpression(thenExp)

        // Condition known to be false, so replace whole expression with just the alternative expression
        case Expression.False => super.foldExpression(elseExp)

        case _ => Expression.IfThenElse(fCond,
                                        super.foldExpression(thenExp),
                                        super.foldExpression(elseExp),
                                        tpe,
                                        loc)
      }
    }

    def optimize(f: Constant): Constant = {
      f.copy(exp = super.foldExpression(f.exp))
    }

  }

  object CopyPropagation {

    def optimize(f: Constant): Constant = {

      /**
        * Replaces any references to `oldVar` with `newVar`.
        */
      class ReplaceVarInExp(oldVar: Name.Ident,
                            newVar: Name.Ident,
                            newOffset: scala.Int,
                            newLoc: SourceLocation) extends ExpressionFolder {
        override def foldVar(ident: Name.Ident,
                             offset: scala.Int,
                             tpe: Type,
                             loc: SourceLocation): Expression = {
          if (ident.name == oldVar.name) {
            Var(newVar, newOffset, tpe, newLoc)
          } else {
            Var(ident, offset, tpe, loc)
          }
        }

        override def foldMkClosure(lambda: Lambda,
                                   freeVars: List[FreeVar],
                                   tpe: Type.Lambda,
                                   loc: SourceLocation): Expression = throw InternalCompilerException("MkClosure should have been replaced by MkClosureRef after lambda lifting.")

        override def foldMkClosureRef(ref: Ref,
                                      freeVars: List[FreeVar],
                                      tpe: Type.Lambda,
                                      loc: SourceLocation): Expression = {
          val fFreeVars = freeVars.map {
            case f @ FreeVar(ident, _, t) =>
              if (ident.name == oldVar.name) {
                FreeVar(newVar, newOffset, t)
              } else {
                f
              }
          }
          MkClosureRef(ref, fFreeVars, tpe, loc)
        }
      }

      object PropagateSimpleBindings extends ExpressionFolder {
        override def foldLet(ident: Name.Ident,
                             offset: scala.Int,
                             rhs: Expression,
                             body: Expression,
                             tpe: Type,
                             loc: SourceLocation): Expression = rhs match {
          // If we have something like
          //    Let x = y in body
          // We can transform that to just
          //    body
          // where we've replaced all references to `x` with `y` in `body`.
          case Var(bIdent, bOffset, _, bLoc) => new ReplaceVarInExp(ident, bIdent, bOffset, bLoc).foldExpression(body)

          // Otherwise don't change anything
          case _ => Let(ident, offset, rhs, body, tpe, loc)
        }
      }

      f.copy(exp = PropagateSimpleBindings.foldExpression(f.exp))
    }
  }

}
