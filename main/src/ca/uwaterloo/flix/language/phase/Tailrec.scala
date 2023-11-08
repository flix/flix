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
import ca.uwaterloo.flix.language.ast.Ast
import ca.uwaterloo.flix.language.ast.LiftedAst._
import ca.uwaterloo.flix.util.ParOps

/**
  * The Tailrec phase identifies function calls that are in tail recursive position.
  *
  * Specifically, it replaces `ApplyRef` AST nodes with `ApplyTail` AST nodes
  * when the `ApplyRef` node calls the same function and occurs in tail position.
  */
object Tailrec {

  /**
    * Identifies tail recursive calls in the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Root = flix.phase("Tailrec") {
    val defns = ParOps.parMapValues(root.defs)(visitDef)

    root.copy(defs = defns)
  }

  /**
    * Identifies tail recursive calls in the given definition `defn`.
    */
  private def visitDef(defn: Def): Def = {
    /**
      * Introduces tail recursive calls in the given expression `exp0`.
      *
      * Replaces every `ApplyRef`, which calls the same function and occurs in tail position, with `ApplyTail`.
      */
    def visitExp(exp0: Expr): Expr = exp0 match {
      /*
       * Let: The body expression is in tail position.
       * (The value expression is *not* in tail position).
       */
      case Expr.Let(sym, exp1, exp2, tpe, purity, loc) =>
        val e2 = visitExp(exp2)
        Expr.Let(sym, exp1, e2, tpe, purity, loc)

      /*
       * If-Then-Else: Consequent and alternative are both in tail position.
       * (The condition is *not* in tail position).
       */
      case Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
        val e2 = visitExp(exp2)
        val e3 = visitExp(exp3)
        Expr.IfThenElse(exp1, e2, e3, tpe, purity, loc)

      /*
       * Branch: Each branch is in tail position.
       */
      case Expr.Branch(e0, br0, tpe, purity, loc) =>
        val br = br0 map {
          case (sym, exp) => sym -> visitExp(exp)
        }
        Expr.Branch(e0, br, tpe, purity, loc)

      /*
       * ApplyClo.
       */
      case Expr.ApplyClo(exp, exps, _, tpe, purity, loc) =>
        Expr.ApplyClo(exp, exps, Ast.CallType.TailCall, tpe, purity, loc)

      /*
       * ApplyDef.
       */
      case Expr.ApplyDef(sym, exps, _, tpe, purity, loc) =>
        // Check whether this is a self recursive call.
        if (defn.sym != sym) {
          // Case 1: Tail recursive call.
          Expr.ApplyDef(sym, exps, Ast.CallType.TailCall, tpe, purity, loc)
        } else {
          // Case 2: Self recursive call.
          Expr.ApplySelfTail(sym, defn.cparams ++ defn.fparams, exps, tpe, purity, loc)
        }

      /*
       * Other expression: No calls in tail position.
       */
      case _ => exp0
    }

    defn.copy(exp = visitExp(defn.exp))
  }

}
