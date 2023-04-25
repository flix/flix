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
  * (b) A function marked with @benchmark or @test is reachable.
  *
  * (c) Appears in a function which itself is reachable.
  *
  * (d) Is an instance of a class whose signature(s) appear in a reachable function.
  * Monomorph will erase erase unused instances so this phase must check all instances
  * for the monomorph to work.
  *
  */
object EarlyTreeShaker {

  /**
    * The name of the phase.
    */
  val phaseName = "EarlyTreeShaker"

  /**
    * Performs tree shaking on the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Root = flix.phase(phaseName) {
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
    // (b) A function annotated with @benchmark or @test is always reachable.
    //
    for ((sym, defn) <- root.defs) {
      if (isBenchmark(defn) || isTest(defn)) {
        reachable = reachable + ReachableSym.DefnSym(sym)
      }
    }
    reachable
  }

  /**
    * Returns `true` if `defn` is annotated with `@benchmark`
    */
  private def isBenchmark(defn: LoweredAst.Def): Boolean = defn.spec.ann.isBenchmark

  /**
    * Returns `true` if `defn` is annotated with `@test`
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
  private def visitExp(e0: Expression): Set[ReachableSym] = e0 match {
    case Expression.Cst(_, _, _) =>
      Set.empty

    case Expression.Wild(_, _) =>
      Set.empty

    case Expression.Var(_, _, _) =>
      Set.empty

    case Expression.Def(sym, _, _) =>
      Set(ReachableSym.DefnSym(sym))

    case Expression.Sig(sym, _, _) =>
      Set(ReachableSym.SigSym(sym))

    case Expression.Hole(_, _, _) =>
      Set.empty

    case Expression.Lambda(_, exp, _, _) =>
      visitExp(exp)

    case Expression.Apply(exp, exps, _, _, _, _) =>
      visitExp(exp) ++ visitExps(exps)

    case Expression.Unary(_, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.Binary(_, exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Let(_, _, exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.LetRec(_, _, exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Region(_, _) =>
      Set.empty

    case Expression.Scope(_, _, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.ScopeExit(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.Stm(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Discard(exp, _, _, _) =>
      visitExp(exp)

    case Expression.Match(exp, rules, _, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp)) ++ visitExps(rules.flatMap(_.guard))

    case Expression.TypeMatch(exp, rules, _, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp))

    case Expression.RelationalChoose(exps, rules, _, _, _, _) =>
      visitExps(exps) ++ visitExps(rules.map(_.exp))

    case Expression.Tag(_, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.Tuple(elms, _, _, _, _) =>
      visitExps(elms)

    case Expression.RecordEmpty(_, _) =>
      Set.empty

    case Expression.RecordSelect(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expression.RecordExtend(_, value, rest, _, _, _, _) =>
      visitExp(value) ++ visitExp(rest)

    case Expression.RecordRestrict(_, rest, _, _, _, _) =>
      visitExp(rest)

    case Expression.ArrayLit(exps, exp, _, _, _, _) =>
      visitExps(exps) ++ visitExp(exp)

    case Expression.ArrayNew(exp1, exp2, exp3, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.ArrayLoad(base, index, _, _, _, _) =>
      visitExp(base) ++ visitExp(index)

    case Expression.ArrayLength(base, _, _, _) =>
      visitExp(base)

    case Expression.ArrayStore(base, index, elm, _, _, _) =>
      visitExp(base) ++ visitExp(index) ++ visitExp(elm)

    case Expression.VectorLit(exps, exp, _, _, _) =>
      visitExps(exps)

    case Expression.VectorLoad(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.VectorLength(exp, _) =>
      visitExp(exp)

    case Expression.Ref(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Deref(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.Assign(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Ascribe(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.InstanceOf(exp, _, _) =>
      visitExp(exp)

    case Expression.Cast(exp, _, _, _, _, _, _, _) =>
      visitExp(exp)

    case Expression.TryCatch(exp, rules, _, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp))

    case Expression.InvokeConstructor(_, args, _, _, _, _) =>
      visitExps(args)

    case Expression.InvokeMethod(_, exp, args, _, _, _, _) =>
      visitExp(exp) ++ visitExps(args)

    case Expression.InvokeStaticMethod(_, args, _, _, _, _) =>
      visitExps(args)

    case Expression.GetField(_, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.PutField(_, exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.GetStaticField(_, _, _, _, _) =>
      Set.empty

    case Expression.PutStaticField(_, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.NewObject(_, _, _, _, _, methods, _) =>
      visitExps(methods.map(_.exp))

    case Expression.Spawn(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Lazy(exp, _, _) =>
      visitExp(exp)

    case Expression.Force(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.Do(_, exps, _, _, _) =>
      visitExps(exps)

    case Expression.Resume(exp, _, _) =>
      visitExp(exp)

    case Expression.TryWith(exp, _, rules, _, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp))

    case Expression.Without(exp, _, _, _, _, _) =>
      visitExp(exp)
  }

  /**
    * Returns the function symbols reachable from `exps`.
    */
  private def visitExps(exps: List[Expression]): Set[ReachableSym] = exps.map(visitExp).fold(Set())(_ ++ _)


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
