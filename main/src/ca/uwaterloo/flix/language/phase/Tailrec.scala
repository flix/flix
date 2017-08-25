/*
 * Copyright 2016 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.SimplifiedAst
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

/**
  * The Tailrec phase identifies function calls that are in tail recursive position.
  *
  * Specifically, it replaces `ApplyRef` AST nodes with `ApplyTail` AST nodes
  * when the `ApplyRef` node calls the same function and occurs in tail position.
  */
object Tailrec extends Phase[SimplifiedAst.Root, SimplifiedAst.Root] {

  /**
    * Identifies tail recursive calls in the given AST `root`.
    */
  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = {
    val b = System.nanoTime()

    val defns = root.defs.map {
      case (sym, defn) => sym -> tailrec(defn)
    }

    val e = System.nanoTime() - b

    root.copy(defs = defns, time = root.time.copy(tailrec = e)).toSuccess
  }

  /**
    * Identifies tail recursive calls in the given definition `defn`.
    */
  private def tailrec(defn: SimplifiedAst.Def): SimplifiedAst.Def = {
    /**
      * Introduces tail recursive calls in the given expression `exp0`.
      *
      * Replaces every `ApplyRef`, which calls the same function and occurs in tail position, with `ApplyTail`.
      */
    def visit(exp0: SimplifiedAst.Expression): SimplifiedAst.Expression = exp0 match {
      /*
       * Let: The body expression is in tail position.
       * (The value expression is *not* in tail position).
       */
      case SimplifiedAst.Expression.Let(sym, exp1, exp2, tpe, loc) =>
        val e2 = visit(exp2)
        SimplifiedAst.Expression.Let(sym, exp1, e2, tpe, loc)

      /*
       * If-Then-Else: Consequent and alternative are both in tail position.
       * (The condition is *not* in tail position).
       */
      case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        val e2 = visit(exp2)
        val e3 = visit(exp3)
        SimplifiedAst.Expression.IfThenElse(exp1, e2, e3, tpe, loc)

      /*
       * ApplyRef: Check if the `ApplyRef` is a tail recursive call.
       */
      case SimplifiedAst.Expression.ApplyDef(name, args, tpe, loc) =>
        if (defn.sym == name) {
          // Case 1: Tail-recursive call. Replace node.
          SimplifiedAst.Expression.ApplySelfTail(name, defn.formals, args, tpe, loc)
        } else {
          // Case 2: Non-tail recursive call.
          exp0
        }

      /*
       * Other expression: No calls in tail position.
       */
      case _ => exp0
    }

    defn.copy(exp = visit(defn.exp))
  }

}
