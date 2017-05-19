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
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.ast.TypedAst.Expression
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object Effects extends Phase[TypedAst.Root, TypedAst.Root] {

  /**
    * Performs effect inference on the given AST `root`.
    */
  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, CompilationError] = {
    // TODO
    root.toSuccess
  }

  object Expressions {

    /**
      * Infers the effects of the given expression `exp0`.
      */
    def infer(exp0: Expression, root: TypedAst.Root): Expression = {
      /**
        * Local visitor.
        */
      def visit(e0: Expression): Expression = exp0 match {

        // TODO: Add all other cases.

        case Expression.Apply(lambda, args, tpe, _, loc) =>
          val e = visit(lambda)
          val es = args.map(visit)

          // The effect is the effect of the individual arguments and the latent effect of the lambda.
          val eff = lub(e.eff.latent, lub(es.map(_.eff)))
          Expression.Apply(e, es, tpe, eff, loc)

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, _, loc) =>
          val e1 = visit(exp1)
          val e2 = visit(exp2)
          val e3 = visit(exp3)
          val eff = lub(e1.eff, e2.eff, e3.eff)
          Expression.IfThenElse(e1, e2, e3, tpe, eff, loc)

        case _ => ??? // TODO: Remove once all cases have been added.

      }

      visit(exp0)
    }

  }

  private def lub(eff1: Eff, eff2: Eff, effs: Eff*): Eff = ???

  private def lub(effs: List[Eff]): Eff = ???

}
