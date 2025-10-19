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
import ca.uwaterloo.flix.language.ast.LoweredAst.*
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugLoweredAst
import ca.uwaterloo.flix.util.ParOps

/**
  * The Tree Shaking phase removes all unused function definitions.
  *
  * A function is considered reachable if it:
  *   - Is an entry point (main / test / export).
  *   - Appears in a function which itself is reachable.
  *   - Is an instance of a trait whose signature(s) appear in a reachable function.
  */
object TreeShaker1 {

  /** Performs tree shaking on `root`. */
  def run(root: Root)(implicit flix: Flix): Root = flix.phase("TreeShaker1") {
    val initReach: Set[ReachableSym] = root.entryPoints.map(ReachableSym.DefnSym.apply)

    // Compute the symbols that are transitively reachable.
    val allReachable = ParOps.parReach(initReach, visitSym(_, root))

    // Filter the reachable definitions.
    val reachableDefs = root.defs.filter {
      case (sym, _) => allReachable.contains(ReachableSym.DefnSym(sym))
    }

    root.copy(defs = reachableDefs)
  }

  /** Returns the symbols reachable from `sym`. */
  private def visitSym(sym: ReachableSym, root: Root): Set[ReachableSym] = sym match {
    case ReachableSym.DefnSym(defnSym) =>
      val defn = root.defs(defnSym)
      visitExp(defn.exp)

    case ReachableSym.SigSym(sigSym) =>
      val sig = root.sigs(sigSym)
      Set(ReachableSym.TraitSym(sig.sym.trt)) ++
        sig.exp.map(visitExp).getOrElse(Set.empty)

    case ReachableSym.TraitSym(traitSym) =>
      root.instances(traitSym).foldLeft(Set.empty[ReachableSym]) {
        case (acc, s) => visitExps(s.defs.map(_.exp)) ++ acc
      }
  }

  /** Returns the symbols reachable from `e0`. */
  private def visitExp(e0: Exp): Set[ReachableSym] = e0 match {
    case Exp.Cst(_, _, _) =>
      Set.empty

    case Exp.Var(_, _, _) =>
      Set.empty

    case Exp.Lambda(_, exp, _, _) =>
      visitExp(exp)

    case Exp.ApplyClo(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Exp.ApplyDef(sym, exps, _, _, _, _, _) =>
      Set(ReachableSym.DefnSym(sym)) ++ visitExps(exps)

    case Exp.ApplyLocalDef(_, exps, _, _, _) =>
      visitExps(exps)

    case Exp.ApplyOp(_, exps, _, _, _) =>
      visitExps(exps)

    case Exp.ApplySig(sym, exps, _, _, _, _, _, _) =>
      Set(ReachableSym.SigSym(sym)) ++ visitExps(exps)

    case Exp.ApplyAtomic(_, exps, _, _, _) =>
      visitExps(exps)

    case Exp.Let(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Exp.LocalDef(_, _, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Exp.Region(_, _, exp, _, _, _) =>
      visitExp(exp)

    case Exp.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Exp.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Exp.Discard(exp, _, _) =>
      visitExp(exp)

    case Exp.Match(exp, rules, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp)) ++ visitExps(rules.flatMap(_.guard))

    case Exp.ExtMatch(exp, rules, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp))

    case Exp.TypeMatch(exp, rules, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp))

    case Exp.VectorLit(exps, _, _, _) =>
      visitExps(exps)

    case Exp.VectorLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Exp.VectorLength(exp, _) =>
      visitExp(exp)

    case Exp.Ascribe(exp, _, _, _) =>
      visitExp(exp)

    case Exp.Cast(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Exp.TryCatch(exp, rules, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp))

    case Exp.NewObject(_, _, _, _, methods, _) =>
      visitExps(methods.map(_.exp))

    case Exp.RunWith(exp, _, rules, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp))
  }

  /** Returns the symbols reachable from `exps`. */
  private def visitExps(exps: List[Exp]): Set[ReachableSym] =
    exps.map(visitExp).fold(Set())(_ ++ _)


  /** Reachable symbols (defs, traits, sigs). */
  private sealed trait ReachableSym

  private object ReachableSym {

    case class DefnSym(sym: Symbol.DefnSym) extends ReachableSym

    case class TraitSym(sym: Symbol.TraitSym) extends ReachableSym

    case class SigSym(sym: Symbol.SigSym) extends ReachableSym

  }

}
