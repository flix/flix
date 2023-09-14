/*
 * Copyright 2017 Magnus Madsen, 2022 Jakob Schneider Villumsen
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
import ca.uwaterloo.flix.language.ast.LoweredAst._
import ca.uwaterloo.flix.language.ast.{LoweredAst, Symbol}
import ca.uwaterloo.flix.util.ParOps

/**
  * The Tree Shaking phase removes all unused function definitions.
  *
  * A function is considered reachable if it:
  *
  * (a) The main function is always reachable.
  *
  * (b) A function marked with @Benchmark or @Test is reachable.
  *
  * (c) Appears in a function which itself is reachable.
  *
  * (d) Is an instance of a class whose signature(s) appear in a reachable function.
  * Monomorph will erase unused instances so this phase must check all instances
  * for the monomorph to work.
  *
  */
object EarlyTreeShaker {

  /**
    * Performs tree shaking on the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Root = flix.phase("EarlyTreeShaker") {
    // Compute the symbols that are always reachable.
    val initReach = initReachable(root)

    // Compute the symbols that are transitively reachable.
    val allReachable = ParOps.parReachable(initReach, visitSym(_, root))

    // Filter the reachable definitions.
    val reachableDefs = root.defs.filter {
      case (sym, _) => allReachable.contains(ReachableSym.DefnSym(sym))
    }

    // Reassemble the AST.
    root.copy(defs = reachableDefs)
  }

  /**
    * Returns the symbols that are always reachable.
    */
  private def initReachable(root: Root): Set[ReachableSym] = {
    // A set used to collect the symbols of reachable functions.
    var reachable: Set[ReachableSym] = Set.empty

    //
    // (a) The main function is always reachable (if it exists).
    //
    reachable = reachable ++ root.entryPoint.map(ReachableSym.DefnSym)

    //
    // (b) A function annotated with @Benchmark or @Test is always reachable.
    //
    for ((sym, defn) <- root.defs) {
      if (isBenchmark(defn) || isTest(defn)) {
        reachable = reachable + ReachableSym.DefnSym(sym)
      }
    }
    reachable
  }

  /**
    * Returns `true` if `defn` is annotated with `@Benchmark`
    */
  private def isBenchmark(defn: LoweredAst.Def): Boolean = defn.spec.ann.isBenchmark

  /**
    * Returns `true` if `defn` is annotated with `@Test`
    */
  private def isTest(defn: LoweredAst.Def): Boolean = defn.spec.ann.isTest

  /**
    * Returns the symbols reachable from the given symbol `sym`.
    *
    * This includes three types of symbols:
    *
    * (a) The function or signature symbols in the implementation / body expression of a reachable function symbol
    *
    * (b) The class symbol of a reachable sig symbol.
    *
    * (c)Every expression in a class instance of a reachable class symbol is reachable.
    *
    */
  private def visitSym(sym: ReachableSym, root: Root): Set[ReachableSym] = sym match {
    case ReachableSym.DefnSym(defnSym) =>
      visitExp(root.defs(defnSym).impl.exp)

    case ReachableSym.SigSym(sigSym) =>
      val sig = root.sigs(sigSym)
      Set(ReachableSym.ClassSym(sig.sym.clazz)) ++
        sig.impl.map(_.exp).map(visitExp).getOrElse(Set.empty)

    case ReachableSym.ClassSym(classSym) =>
      root.instances(classSym).foldLeft(Set.empty[ReachableSym]) {
        case (acc, s) => visitExps(s.defs.map(_.impl.exp)) ++ acc
      }
  }

  /**
    * Returns the function and signature symbols reachable from the given expression `e0`.
    */
  private def visitExp(e0: Expr): Set[ReachableSym] = e0 match {
    case Expr.Cst(_, _, _) =>
      Set.empty

    case Expr.Var(_, _, _) =>
      Set.empty

    case Expr.Def(sym, _, _) =>
      Set(ReachableSym.DefnSym(sym))

    case Expr.Sig(sym, _, _) =>
      Set(ReachableSym.SigSym(sym))

    case Expr.Lambda(_, exp, _, _) =>
      visitExp(exp)

    case Expr.Apply(exp, exps, _, _, _) =>
      visitExp(exp) ++ visitExps(exps)

    case Expr.ApplyAtomic(_, exps, _, _, _) =>
      visitExps(exps)

    case Expr.Let(_, _, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.LetRec(_, _, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Scope(_, _, exp, _, _, _) =>
      visitExp(exp)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expr.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Discard(exp, _, _) =>
      visitExp(exp)

    case Expr.Match(exp, rules, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp)) ++ visitExps(rules.flatMap(_.guard))

    case Expr.TypeMatch(exp, rules, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp))

    case Expr.RelationalChoose(exps, rules, _, _, _) =>
      visitExps(exps) ++ visitExps(rules.map(_.exp))

    case Expr.VectorLit(exps, exp, _, _) =>
      visitExps(exps)

    case Expr.VectorLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.VectorLength(exp, _) =>
      visitExp(exp)

    case Expr.Ascribe(exp, _, _, _) =>
      visitExp(exp)

    case Expr.Cast(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expr.TryCatch(exp, rules, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp))

    case Expr.NewObject(_, _, _, _, methods, _) =>
      visitExps(methods.map(_.exp))

    case Expr.Do(_, exps, _, _, _) =>
      visitExps(exps)

    case Expr.Resume(exp, _, _) =>
      visitExp(exp)

    case Expr.TryWith(exp, _, rules, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp))
  }

  /**
    * Returns the function symbols reachable from `exps`.
    */
  private def visitExps(exps: List[Expr]): Set[ReachableSym] = exps.map(visitExp).fold(Set())(_ ++ _)


  /**
    * A common super-type for reachable symbols (defs, classes, sigs)
    */
  sealed trait ReachableSym

  object ReachableSym {

    case class DefnSym(sym: Symbol.DefnSym) extends ReachableSym

    case class ClassSym(sym: Symbol.ClassSym) extends ReachableSym

    case class SigSym(sym: Symbol.SigSym) extends ReachableSym

  }

}
