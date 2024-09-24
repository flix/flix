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
import ca.uwaterloo.flix.language.ast.LiftedAst._
import ca.uwaterloo.flix.language.ast.{AtomicOp, Symbol}
import ca.uwaterloo.flix.language.dbg.AstPrinter._
import ca.uwaterloo.flix.util.ParOps

/**
  * The Tree Shaking phase removes all unused function definitions.
  *
  * A function is considered reachable if it:
  *
  * (a) The main function is always reachable.
  * (b) A function marked with @test is reachable.
  * (c) Appears in a function which itself is reachable.
  *
  */
object TreeShaker2 {

  /** Performs tree shaking on the given AST `root`. */
  def run(root: Root)(implicit flix: Flix): Root = flix.phase("TreeShaker2") {
    // Compute the symbols that are always reachable.
    val initReach = root.reachable

    // Compute the symbols that are transitively reachable.
    val allReachable = ParOps.parReach(initReach, visitSym(_, root))

    // Filter the reachable definitions.
    val newDefs = root.defs.filter {
      case (sym, _) => allReachable.contains(sym)
    }

    // Reassemble the AST.
    root.copy(defs = newDefs)
  }

  /** Returns the symbols reachable from the given symbol `sym`. */
  private def visitSym(sym: Symbol.DefnSym, root: Root): Set[Symbol.DefnSym] = root.defs.get(sym) match {
    case None => Set.empty
    case Some(defn) => visitExp(defn.exp)
  }

  /** Returns the function symbols reachable from the given expression `e0`. */
  private def visitExp(e0: Expr): Set[Symbol.DefnSym] = e0 match {
    case Expr.Cst(_, _, _) =>
      Set.empty

    case Expr.Var(_, _, _) =>
      Set.empty

    case Expr.ApplyAtomic(op, exps, _, _, _) =>
      visitAtomicOp(op) ++ visitExps(exps)

    case Expr.ApplyClo(exp, exps, _, _, _) =>
      visitExp(exp) ++ visitExps(exps)

    case Expr.ApplyDef(sym, exps, _, _, _) =>
      Set(sym) ++ visitExps(exps)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expr.Branch(exp, branches, _, _, _) =>
      visitExp(exp) ++ visitExps(branches.values.toList)

    case Expr.JumpTo(_, _, _, _) =>
      Set.empty

    case Expr.Let(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.LetRec(_, _, _, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Scope(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.TryCatch(exp, rules, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp))

    case Expr.TryWith(exp, _, rules, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp))

    case Expr.Do(_, exps, _, _, _) =>
      visitExps(exps)

    case Expr.NewObject(_, _, _, _, methods, _) =>
      visitExps(methods.map(_.clo))

  }

  /** Returns the function symbols reachable from the given [[AtomicOp]] `op`. */
  private def visitAtomicOp(op: AtomicOp): Set[Symbol.DefnSym] = op match {
    case AtomicOp.Closure(sym) => Set(sym)
    case _ => Set.empty
  }

  /** Returns the function symbols reachable from `es`. */
  private def visitExps(es: List[Expr]): Set[Symbol.DefnSym] = es.map(visitExp).fold(Set())(_ ++ _)

}
