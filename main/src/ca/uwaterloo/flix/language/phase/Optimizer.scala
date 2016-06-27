/*
 * Copyright 2015-2016 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.SimplifiedAst.{Expression, ExpressionFolder, Root}
import ca.uwaterloo.flix.language.ast.{BinaryOperator, SourceLocation, Type}

object Optimizer {

  def optimize(root: Root): Root = {
    val constants = root.constants.map {
      case (name, defn) =>
        var fn = defn

        // Do simple copy propagation to get rid of targets of direct assignment
        fn = CopyPropagation.optimize(fn)

        // Replace Unit == Unit checks
        fn = EliminateUnitChecks.optimize(fn)

        // Clean up by removing code
        fn = DeadCodeElimination.optimize(fn)

        name -> fn
    }

    root.copy(constants = constants)
  }

  object EliminateUnitChecks {

    def optimize(f: Constant): Constant = {

      object ReplaceUnitCheckWithTrue extends ExpressionFolder {
        override def foldBinary(op: BinaryOperator,
                                exp1: Expression,
                                exp2: Expression,
                                tpe: Type,
                                loc: SourceLocation): Expression = {
          // Replace an equality check on the Unit type with just True
          // We need not check that exp2 is also of type Unit since
          // the type checker wil guarantee that exp1.tpe == exp2.tpe.
          if (op == BinaryOperator.Equal && exp1.tpe == Unit.tpe) {
            True
          } else {
            super.foldBinary(op, exp1, exp2, tpe, loc)
          }
        }
      }

      f.copy(exp = ReplaceUnitCheckWithTrue.foldExpression(f.exp))
    }

  }

  object DeadCodeElimination {

    def optimize(f: Constant): Constant = {

      object RemoveKnownConditional extends ExpressionFolder {
        override def foldIfThenElse(cond: Expression,
                                    thenExp: Expression,
                                    elseExp: Expression,
                                    tpe: Type,
                                    loc: SourceLocation): Expression = cond match {
          // Condition known to be true, so replace whole expression with just the consequent expression
          case Expression.True => super.foldExpression(thenExp)

          // Condition known to be false, so replace whole expression with just the alternative expression
          case Expression.False => super.foldExpression(elseExp)

          case _ => super.foldIfThenElse(cond, thenExp, elseExp, tpe, loc)
        }
      }

      f.copy(exp = RemoveKnownConditional.foldExpression(f.exp))
    }

  }

  object CopyPropagation {

    def optimize(f: Constant): Constant = f

  }

}
