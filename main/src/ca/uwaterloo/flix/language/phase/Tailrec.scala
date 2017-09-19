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
import ca.uwaterloo.flix.language.ast.SimplifiedAst._
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

/**
  * The Tailrec phase identifies function calls that are in tail recursive position.
  *
  * Specifically, it replaces `ApplyRef` AST nodes with `ApplyTail` AST nodes
  * when the `ApplyRef` node calls the same function and occurs in tail position.
  */
object Tailrec extends Phase[Root, Root] {

  /**
    * Identifies tail recursive calls in the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = {
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
  private def tailrec(defn: Def): Def = {
    /**
      * Introduces tail recursive calls in the given expression `exp0`.
      *
      * Replaces every `ApplyRef`, which calls the same function and occurs in tail position, with `ApplyTail`.
      */
    def visit(exp0: Expression): Expression = exp0 match {
      /*
       * Let: The body expression is in tail position.
       * (The value expression is *not* in tail position).
       */
      case Expression.Let(sym, exp1, exp2, tpe, loc) =>
        val e2 = visit(exp2)
        Expression.Let(sym, exp1, e2, tpe, loc)

      /*
       * If-Then-Else: Consequent and alternative are both in tail position.
       * (The condition is *not* in tail position).
       */
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        val e2 = visit(exp2)
        val e3 = visit(exp3)
        Expression.IfThenElse(exp1, e2, e3, tpe, loc)

      /*
       * Branch: Each branch is in tail position.
       */
      case Expression.Branch(e0, br0, tpe, loc) =>
        val br = br0 map {
          case (sym, exp) => sym -> visit(exp)
        }
        Expression.Branch(e0, br, tpe, loc)

      /*
       * ApplyClo.
       */
      case Expression.ApplyClo(exp, args, tpe, loc) =>
        Expression.ApplyCloTail(exp, args, tpe, loc)

      /*
       * ApplyDef.
       */
      case Expression.ApplyDef(sym, args, tpe, loc) =>
        // Check whether this is a self recursive call.
        if (defn.sym != sym) {
          // Case 1: Tail recursive call.
          Expression.ApplyDefTail(sym, args, tpe, loc)
        } else {
          // Case 2: Self recursive call.
          Expression.ApplySelfTail(sym, defn.fparams, args, tpe, loc)
        }

      /*
       * Other expression: No calls in tail position.
       */
      case _ => exp0
    }

    defn.copy(exp = visit(defn.exp))
  }

}
