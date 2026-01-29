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
import ca.uwaterloo.flix.language.ast.TypedAst.*
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugTypedAst
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

/**
  * The Tree Shaking phase removes all unused function definitions.
  *
  * A function is considered reachable if it:
  *   - Is an entry point (main / test / export).
  *   - Appears in a function which itself is reachable.
  *   - Is an instance of a trait whose signature(s) appear in a reachable function.
  *   - Is annotated with `@LoweringTarget`.
  */
object TreeShaker1 {

  /** Performs tree shaking on `root`. */
  def run(root: Root)(implicit flix: Flix): Root = flix.phase("TreeShaker1") {
    val initReach: Set[ReachableSym] = root.entryPoints.map(ReachableSym.DefnSym.apply)

    val defaultHandlers = root.defaultHandlers.map(handler => ReachableSym.DefnSym(handler.handlerSym)).toSet

    val loweringTargets: Set[ReachableSym] = root.defs.foldLeft(Set[ReachableSym]()) {
      case (acc, (_, defn)) if defn.spec.ann.isLoweringTarget => acc + ReachableSym.DefnSym(defn.sym)
      case (acc, _) => acc
    }

    // Compute the symbols that are transitively reachable.
    val allReachable = ParOps.parReach(initReach ++ loweringTargets ++ defaultHandlers, visitSym(_, root))

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
  private def visitExp(e0: Expr): Set[ReachableSym] = e0 match {
    case Expr.Cst(_, _, _) =>
      Set.empty

    case Expr.Var(_, _, _) =>
      Set.empty

    case Expr.Hole(_, _, _, _, _) =>
      Set.empty

    case Expr.HoleWithExp(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.OpenAs(_, exp, _, _) =>
      visitExp(exp)

    case Expr.Use(_, _, exp, _) =>
      visitExp(exp)

    case Expr.Lambda(_, exp, _, _) =>
      visitExp(exp)

    case Expr.ApplyClo(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.ApplyDef(bnd, exps, _, _, _, _, _) =>
      Set(ReachableSym.DefnSym(bnd.sym)) ++ visitExps(exps)

    case Expr.ApplyLocalDef(_, exps, _, _, _, _) =>
      visitExps(exps)

    case Expr.ApplyOp(_, exps, _, _, _) =>
      visitExps(exps)

    case Expr.ApplySig(bnd, exps, _, _, _, _, _, _) =>
      Set(ReachableSym.SigSym(bnd.sym)) ++ visitExps(exps)

    case Expr.Unary(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Let(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.LocalDef(_, _, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Region(_, _, exp, _, _, _) =>
      visitExp(exp)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expr.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Discard(exp, _, _) =>
      visitExp(exp)

    case Expr.Match(exp, rules, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp)) ++ visitExps(rules.flatMap(_.guard))

    case Expr.RestrictableChoose(_, exp, rules, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp))

    case Expr.ExtMatch(exp, rules, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp))

    case Expr.Tag(_, exps, _, _, _) =>
      visitExps(exps)

    case Expr.RestrictableTag(_, exps, _, _, _) =>
      visitExps(exps)

    case Expr.ExtTag(_, exps, _, _, _) =>
      visitExps(exps)

    case Expr.Tuple(exps, _, _, _) =>
      visitExps(exps)

    case Expr.RecordSelect(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.RecordExtend(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.RecordRestrict(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.ArrayLit(exps, exp, _, _, _) =>
      visitExps(exps) ++ visitExp(exp)

    case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expr.ArrayLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.ArrayLength(exp, _, _) =>
      visitExp(exp)

    case Expr.ArrayStore(exp1, exp2, exp3, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expr.StructNew(_, fields, region, _, _, _) =>
      visitExps(fields.map(_._2)) ++ region.map(visitExp).getOrElse(Set.empty)

    case Expr.StructGet(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.StructPut(exp1, _, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.TypeMatch(exp, rules, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp))

    case Expr.VectorLit(exps, _, _, _) =>
      visitExps(exps)

    case Expr.VectorLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.VectorLength(exp, _) =>
      visitExp(exp)

    case Expr.Ascribe(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expr.InstanceOf(exp, _, _) =>
      visitExp(exp)

    case Expr.CheckedCast(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.UncheckedCast(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expr.Unsafe(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expr.Without(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.TryCatch(exp, rules, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp))

    case Expr.Throw(exp, _, _, _) =>
      visitExp(exp)

    case Expr.Handler(_, rules, _, _, _, _, _) =>
      visitExps(rules.map(_.exp))

    case Expr.InvokeConstructor(_, exps, _, _, _) =>
      visitExps(exps)

    case Expr.InvokeMethod(_, exp, exps, _, _, _) =>
      visitExp(exp) ++ visitExps(exps)

    case Expr.InvokeStaticMethod(_, exps, _, _, _) =>
      visitExps(exps)

    case Expr.GetField(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.PutField(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.GetStaticField(_, _, _, _) =>
      Set.empty

    case Expr.PutStaticField(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.NewObject(_, _, _, _, methods, _) =>
      visitExps(methods.map(_.exp))

    case Expr.RunWith(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.NewChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expr.GetChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expr.PutChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Spawn(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.SelectChannel(selects, optExp, _, _, _) =>
      visitExps(selects.map(_.exp)) ++ visitExps(selects.map(_.chan)) ++ optExp.map(visitExp).getOrElse(Set.empty)

    case Expr.ParYield(frags, exp, _, _, _) =>
      visitExps(frags.map(_.exp)) ++ visitExp(exp)

    case Expr.Lazy(exp, _, _) =>
      visitExp(exp)

    case Expr.Force(exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointConstraintSet(cs, _, _) =>
      cs.map(visitConstraint).fold(Set.empty)(_ ++ _)

    case Expr.FixpointLambda(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointMerge(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.FixpointQueryWithProvenance(exps, select, _, _, _, _) =>
      visitExps(exps) ++ visitHead(select)

    case Expr.FixpointQueryWithSelect(exps, queryExp, selects, from0, where0, _, _, _, _) =>
      visitExps(exps) ++
        visitExp(queryExp) ++
        visitExps(selects) ++
        visitBodies(from0) ++
        visitExps(where0)

    case Expr.FixpointSolveWithProject(exps0, _, _, _, _, _) =>
      visitExps(exps0)

    case Expr.FixpointInjectInto(exps, _, _, _, _) =>
      visitExps(exps)

    case Expr.Error(m, _, _) =>
      throw InternalCompilerException(s"Unexpected error expression near", m.loc)
  }

  /** Returns the symbols reachable from `exps`. */
  private def visitExps(exps: List[Expr]): Set[ReachableSym] =
    exps.map(visitExp).fold(Set.empty)(_ ++ _)

  /** Returns the symbols reachable from `cs`. */
  private def visitConstraint(cs: Constraint): Set[ReachableSym] = cs match {
    case Constraint(_, head, bodies, _) =>
      visitHead(head) ++ visitBodies(bodies)
  }

  /** Returns the symbols reachable from `bodies`. */
  private def visitBodies(bodies: List[Predicate.Body]): Set[ReachableSym] =
    bodies.map(visitBody).fold(Set.empty)(_ ++ _)

  /** Returns the symbols reachable from `head`. */
  private def visitHead(head: Predicate.Head): Set[ReachableSym] = head match {
    case Head.Atom(_, _, exps, _, _) => visitExps(exps)
  }

  /** Returns the symbols reachable from `body`. */
  private def visitBody(body: Predicate.Body): Set[ReachableSym] = body match {
    case Body.Atom(_, _, _, _, _, _, _) => Set.empty
    case Body.Functional(_, exp, _) => visitExp(exp)
    case Body.Guard(exp, _) => visitExp(exp)
  }

  /** Reachable symbols (defs, traits, sigs). */
  private sealed trait ReachableSym

  private object ReachableSym {

    case class DefnSym(sym: Symbol.DefnSym) extends ReachableSym

    case class TraitSym(sym: Symbol.TraitSym) extends ReachableSym

    case class SigSym(sym: Symbol.SigSym) extends ReachableSym

  }

}
