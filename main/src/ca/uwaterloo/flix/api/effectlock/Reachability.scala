/*
 * Copyright 2025 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.language.ast.shared.SymUse

/**
  * Computes the set of reachable missing symbols in the program.
  *
  * For effect locking, the caller can filter the program down to
  * contain only definitions from the local program and find the
  * reachable library functions by calling [[run]].
  */
object Reachability {

  case class ReachableSyms(defs: Set[Symbol.DefnSym], sigs: Set[Symbol.SigSym])

  /**
    * Computes the set of reachable symbols not present in `root`.
    */
  def run(root: TypedAst.Root): ReachableSyms = {
    val initDefnSyms = root.defs.keys.map(ReachableSym.DefnSym.apply).toSet
    val initTraitSyms = root.traits.keys.map(ReachableSym.TraitSym.apply)
    val initSigSym = root.sigs.keys.map(ReachableSym.SigSym.apply)
    val init: Set[ReachableSym] = initDefnSyms ++ initTraitSyms ++ initSigSym

    var reach = init
    var delta = reach

    while (delta.nonEmpty) {
      val newReach = delta.flatMap(visitSym(_, root))
      delta = newReach -- reach
      reach = reach ++ delta
    }

    val defnSyms = reach.collect { case x: ReachableSym.DefnSym => x }.map(_.sym)
    val sigSyms = reach.collect { case x: ReachableSym.SigSym => x }.map(_.sym)
    ReachableSyms(defnSyms, sigSyms)

  }

  /** Returns the symbols reachable from `sym0`. */
  private def visitSym(sym0: ReachableSym, root: TypedAst.Root): Set[ReachableSym] = sym0 match {
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

  /** Returns the symbols reachable from `exp0`. */
  private def visitExp(exp0: Expr): Set[ReachableSym] = exp0 match {
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

    case Expr.ApplyDef(SymUse.DefSymUse(sym, _), exps, _, _, _, _, _) =>
      Set(ReachableSym.DefnSym(sym)) ++ visitExps(exps)

    case Expr.ApplyLocalDef(_, exps, _, _, _, _) =>
      visitExps(exps)

    case Expr.ApplyOp(_, exps, _, _, _) =>
      visitExps(exps)

    case Expr.ApplySig(SymUse.SigSymUse(sym, _), exps, _, _, _, _, _, _) =>
      Set(ReachableSym.SigSym(sym)) ++ visitExps(exps)

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

    case Expr.ExtMatch(exp, rules, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp))

    case Expr.TypeMatch(exp, rules, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp))

    case Expr.RestrictableChoose(_, exp, rules, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(r => r.exp))

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

    case Expr.StructNew(_, fields, exp, _, _, _) =>
      visitExps(fields.map { case (_, e) => e }) ++ exp.map(visitExp).getOrElse(Set.empty)

    case Expr.StructGet(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.StructPut(exp1, _, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

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
      visitExps(rules.map(r => r.exp))

    case Expr.RunWith(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

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

    case Expr.NewChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expr.GetChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expr.PutChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.SelectChannel(selects, optExp, _, _, _) =>
      visitExps(selects.map(_.exp)) ++ visitExps(selects.map(_.chan)) ++ optExp.map(visitExp).getOrElse(Set.empty)

    case Expr.Spawn(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.ParYield(frags, exp, _, _, _) =>
      visitExps(frags.map(f => f.exp)) ++ visitExp(exp)

    case Expr.Lazy(exp, _, _) =>
      visitExp(exp)

    case Expr.Force(exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointConstraintSet(constrs, _, _) =>
      constrs.flatMap(visitFixpointConstraint).toSet

    case Expr.FixpointLambda(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointMerge(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.FixpointQueryWithProvenance(exps, select, _, _, _, _) =>
      visitExps(exps) ++ visitPredicateHead(select)

    case Expr.FixpointQueryWithSelect(exps1, exp, exps2, from, exps3, _, _, _, _) =>
      visitExps(exps1) ++ visitExp(exp) ++ visitExps(exps2) ++ from.flatMap(visitPredicateBody) ++ visitExps(exps3)

    case Expr.FixpointSolveWithProject(exps, _, _, _, _, _) =>
      visitExps(exps)

    case Expr.FixpointInjectInto(exps, _, _, _, _) =>
      visitExps(exps)

    case Expr.Error(_, _, _) =>
      Set.empty

  }

  /** Returns the symbols reachable from `exps`. */
  private def visitExps(exps: List[Expr]): Set[ReachableSym] =
    exps.map(visitExp).fold(Set())(_ ++ _)

  /** Returns the symbols reachable from `head0`. */
  private def visitFixpointConstraint(constr0: TypedAst.Constraint): Set[ReachableSym] = constr0 match {
    case TypedAst.Constraint(_, head, body, _) =>
      visitPredicateHead(head) ++ body.flatMap(visitPredicateBody)
  }

  /** Returns the symbols reachable from `head0`. */
  private def visitPredicateHead(head0: TypedAst.Predicate.Head): Set[ReachableSym] = head0 match {
    case TypedAst.Predicate.Head.Atom(_, _, exps, _, _) => visitExps(exps)
  }

  /** Returns the symbols reachable from `body0`. */
  private def visitPredicateBody(body0: TypedAst.Predicate.Body): Set[ReachableSym] = body0 match {
    case TypedAst.Predicate.Body.Atom(_, _, _, _, _, _, _) => Set.empty
    case TypedAst.Predicate.Body.Functional(_, exp, _) => visitExp(exp)
    case TypedAst.Predicate.Body.Guard(exp, _) => visitExp(exp)
  }

  /** Reachable symbols (defs, traits, sigs). */
  private sealed trait ReachableSym

  private object ReachableSym {

    case class DefnSym(sym: Symbol.DefnSym) extends ReachableSym

    case class SigSym(sym: Symbol.SigSym) extends ReachableSym

    case class TraitSym(sym: Symbol.TraitSym) extends ReachableSym

  }

}
