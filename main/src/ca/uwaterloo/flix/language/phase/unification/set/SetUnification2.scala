/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.unification.set

import ca.uwaterloo.flix.language.phase.unification.EffUnification3
import ca.uwaterloo.flix.language.phase.unification.set.SetFormula.*
import ca.uwaterloo.flix.language.phase.unification.shared.{BoolUnificationException, CofiniteIntSet, SveAlgorithm, SveMode}
import ca.uwaterloo.flix.language.phase.unification.zhegalkin.{Zhegalkin, ZhegalkinAlgebra, ZhegalkinExpr}

import scala.collection.immutable.IntMap

/**
  * A solver for systems of set equations built around a single rule.
  *
  * Every equation `f1 ~ f2` is read as `q ~ ∅`. Solving `q ~ ∅` for a variable `x` is one step of
  * successive variable elimination (SVE):
  *
  * {{{
  *   a = q[x := ∅]        t = q[x := 𝓤]
  *   binding   x ↦ a ∪ (x ∩ ¬t)
  *   residual  a ∩ t ~ ∅
  * }}}
  *
  * The step is free when the residual is `∅`. The solver takes the free steps on set formulas,
  * where they are linear, and hands what remains to SVE on Zhegalkin polynomials, where the
  * residuals are. It runs in phases:
  *
  *   1. Alias and define: `x ~ y` and `x ~ f` with `x` not in `f` bind `x`. These run on the
  *      original equalities so that ranges stay small and a variable leaves the system in one step.
  *      The equations are taken smallest first (see [[sortBySize]]).
  *
  *   2. Inclusions: every other equation `f1 ~ f2` becomes the constraints `f1 ∩ ¬f2 ~ ∅` and
  *      `¬f1 ∩ f2 ~ ∅`. Complements sit at the atoms (see [[SetFormula.mkCompl]]), so every
  *      constraint is an intersection of members equated to `∅`.
  *
  *   3. Constraints are processed from a worklist. A constraint `x ∩ g ~ ∅` or `¬x ∩ g ~ ∅` whose
  *      variable `x` occurs in no member of `g` is a free step: it is solved by `x ↦ x ∩ ¬g` or
  *      `x ↦ x ∪ g`. A ground constraint holds or is in conflict. Otherwise, a union among the
  *      members distributes out, `(g1 ∪ g2) ∩ h ~ ∅` becoming `g1 ∩ h ~ ∅` and `g2 ∩ h ~ ∅`, which
  *      exposes the variables inside it. Distribution is bounded by a budget on the constraints it
  *      creates. A constraint past the budget, or without a union, is left whole.
  *
  *   4. SVE: the constraints left whole are solved by [[SveAlgorithm]] at the granularity given by
  *      the mode.
  *
  * Complexity. Let `n` be the number of equations and `v` the number of variables. Phase 1 makes at
  * most `v + 1` passes of `n` substitutions. Phase 2 creates at most `2n` constraints. Phase 3 sees
  * at most `2n` plus the split budget many constraints, each once, and applies every binding to the
  * constraints that remain: quadratic in the number of constraints, which is linear in `n`. The
  * ground emptiness test goes through a Zhegalkin polynomial and can be exponential in the rigid
  * constants it mentions. Phase 4 is exponential in the variables of the constraints it receives,
  * each of which is checked against [[SetUnification.MaxVars]].
  */
object SetUnification2 {

  /**
    * The budget on constraints created by distributing unions, as a multiple of the number of
    * input equations plus a constant. Past the budget, constraints are left whole for SVE.
    */
  private val SplitBudgetPerEquation: Int = 8
  private val SplitBudgetBase: Int = 64

  private implicit val alg: ZhegalkinAlgebra[CofiniteIntSet] = EffUnification3.Algebra

  /** A simplified constraint `f ~ ∅` derived from the input equation `origin`. */
  private final case class Constraint(f: SetFormula, origin: Equation) {
    /** Returns `this` with `s` applied. A formula that changes is rebuilt simplified. */
    def apply(s: SetSubstitution): Constraint = {
      val g = s(f)
      if (g eq f) this else Constraint(g, origin)
    }
  }

  /** The mutable state of one call to [[solve]]. */
  private final class State(var budget: Int) {
    /** The substitution built so far. */
    var subst: SetSubstitution = SetSubstitution.empty
    /** The constraints to process. */
    var pending: List[Constraint] = Nil
    /** The constraints left whole for SVE. */
    var leftovers: List[Constraint] = Nil

    /**
      * Records the binding `x ↦ f` on top of the substitution built so far, and applies it to
      * every constraint.
      *
      * A binding is applied to a constraint exactly once, here: a binding `x ↦ x ∪ g` leaves `x`
      * in the formula as its own parameter, so applying it twice yields `(x ∪ g) ∪ g`, and a part
      * split off along that union would be split again on every visit.
      */
    def bind(x: Int, f: SetFormula): Unit = {
      val s = SetSubstitution.singleton(x, f)
      subst = s @@ subst
      pending = pending.map(_.apply(s))
      leftovers = leftovers.map(_.apply(s))
    }
  }

  /**
    * Attempts to solve the equation system `l` to find the most general substitution.
    *
    * Returns the equations that could not be solved, marked [[Equation.Status.Unsolvable]] or
    * [[Equation.Status.Timeout]], and the substitution. If the returned list is empty, the
    * substitution is the most general solution of `l`. Otherwise the substitution is empty, and the
    * returned equations are the input equations the failures derive from, with the substitution
    * found before the failure applied (see [[SetUnification.solve]]).
    */
  def solve(l: List[Equation], mode: SveMode): (List[Equation], SetSubstitution) = {
    val state = new State(SplitBudgetPerEquation * l.length + SplitBudgetBase)
    for (eq <- aliasAndDefine(sortBySize(l), state)) {
      state.pending = inclusionsOf(eq) ::: state.pending
    }

    while (state.pending.nonEmpty) {
      val c = state.pending.head
      state.pending = state.pending.tail
      val ms = c.f match {
        case Inter(members) => members.toList
        case f => f :: Nil
      }
      freeStepOf(ms) match {
        case Some((x, range)) =>
          state.bind(x, range)

        case None if c.f.isGround =>
          if (!isEmpty(c.f)) {
            // A conflict. Every unsolved equation is reported.
            return unsolvable(c :: state.pending ::: state.leftovers, state.subst)
          }

        case None => ms.collectFirst { case u: Union => u } match {
          case Some(u) if state.budget > 0 =>
            // (g1 ∪ g2 ∪ ..) ∩ h ~ ∅ holds exactly when every gi ∩ h ~ ∅ holds.
            state.budget -= u.l.length
            val others = ms.filterNot(_ eq u)
            state.pending = u.l.toList.map(g => Constraint(mkInterAll(g :: others), c.origin)) ::: state.pending
          case _ =>
            // The budget is spent, or every variable sits inside a symmetric difference.
            state.leftovers = c :: state.leftovers
        }
      }
    }

    sve(state.leftovers, state.subst, mode)
  }

  /**
    * Returns `eqs` sorted by size, smallest first. Equations of equal size keep their order.
    *
    * The order decides which of several definitions `x ~ f1`, `x ~ f2`, .. of a variable binds it
    * in [[aliasAndDefine]]: the first binds `x`, and the others become inclusion constraints. With
    * the smallest first, the binding copies the smallest formula into every occurrence of `x`,
    * and the larger ones are solved as constraints. In the opposite order, a large range is copied
    * everywhere and the small equations become constraints over it, whose free steps nest the
    * previous ranges inside the next.
    */
  private def sortBySize(eqs: List[Equation]): List[Equation] =
    eqs.map(eq => (eq.size, eq)).sortBy(_._1).map(_._2)

  /**
    * Solves the aliases `x ~ y` and definitions `x ~ f`, where `x` does not occur in `f`, among
    * `eqs`, and returns the other equations with the resulting substitution applied.
    *
    * Runs passes over the equations until a pass binds no variable. Every binding removes its
    * variable from the system, so there is at most one pass per variable. An equation `f ~ f`
    * holds and is dropped.
    */
  private def aliasAndDefine(eqs: List[Equation], state: State): List[Equation] = {
    var work = eqs
    var progress = true
    while (progress) {
      progress = false
      var kept: List[Equation] = Nil
      for (eq0 <- work) {
        val eq = state.subst(eq0)
        if (eq.f1 != eq.f2) {
          definitionOf(eq) match {
            case Some((x, f)) =>
              state.bind(x, f)
              progress = true
            case None =>
              kept = eq0 :: kept
          }
        }
      }
      work = kept.reverse
    }
    work.map(state.subst.apply)
  }

  /**
    * Returns the binding that solves `eq`, if `eq` is an alias or a definition:
    *
    *   - `x ~ y` becomes `[x -> y]`, binding the lower variable to the higher
    *   - `x ~ f` where `f` does not contain `x` becomes `[x -> f]`
    */
  private def definitionOf(eq: Equation): Option[(Int, SetFormula)] = (eq.f1, eq.f2) match {
    case (Var(x), Var(y)) => if (x < y) Some((x, eq.f2)) else Some((y, eq.f1))
    case (v@Var(x), f) if !f.contains(v) => Some((x, simplify(f)))
    case (f, v@Var(x)) if !f.contains(v) => Some((x, simplify(f)))
    case _ => None
  }

  /**
    * Returns `eq` as constraints `f ~ ∅`: the equation `f1 ~ f2` holds exactly when the inclusions
    * `f1 ∩ ¬f2 ~ ∅` and `¬f1 ∩ f2 ~ ∅` hold. An inclusion that simplifies to `∅` holds and is
    * dropped, which leaves one constraint for an equation with `∅` or `𝓤` on one side.
    */
  private def inclusionsOf(eq: Equation): List[Constraint] = {
    val Equation(f1, f2, _, _) = eq
    List(mkInter(f1, mkCompl(f2)), mkInter(mkCompl(f1), f2)).map(simplify).filter(_ != Empty).map(Constraint(_, eq))
  }

  /**
    * Returns the free step of the constraint `m1 ∩ .. ∩ mk ~ ∅` with the simplified members `ms`,
    * if it has one: a member `x` or `¬x` whose variable occurs in no other member. The step is
    * the SVE step for `x`, whose residual is `∅`:
    *
    *   - `x ∩ g ~ ∅` becomes `[x -> x ∩ ¬g]`
    *   - `¬x ∩ g ~ ∅` becomes `[x -> x ∪ g]`
    *
    * where `g` is the intersection of the other members, or `𝓤` if there are none. Every solution
    * of the first has `x ⊆ ¬g`, and `x ∩ ¬g` with `x` free ranges over exactly those sets; every
    * solution of the second has `g ⊆ x`, and `x ∪ g` with `x` free ranges over exactly those sets.
    */
  private def freeStepOf(ms: List[SetFormula]): Option[(Int, SetFormula)] = {
    ms.iterator.flatMap {
      case v@Var(x) if !ms.exists(m => (m ne v) && m.contains(v)) =>
        val g = mkInterAll(ms.filterNot(_ eq v))
        Some((x, simplify(mkInter(v, mkCompl(g)))))
      case c@Compl(v@Var(x)) if !ms.exists(m => (m ne c) && m.contains(v)) =>
        val g = mkInterAll(ms.filterNot(_ eq c))
        Some((x, simplify(mkUnion2(v, g))))
      case _ => None
    }.nextOption()
  }

  /** Returns `true` if the ground formula `f` denotes the empty set. */
  private def isEmpty(f: SetFormula): Boolean =
    f == Empty || ZhegalkinExpr.isEmpty(Zhegalkin.toZhegalkin(f)(alg, CofiniteIntSet.LatticeOps))

  /**
    * Solves the constraints `cs` by SVE, and returns the result of the whole solve: the unsolved
    * equations and the substitution.
    */
  private def sve(cs: List[Constraint], subst: SetSubstitution, mode: SveMode): (List[Equation], SetSubstitution) = {
    if (cs.isEmpty) {
      return (Nil, subst)
    }

    for (c <- cs) {
      val n = c.f.varsOf.size
      if (n > SetUnification.MaxVars) {
        val msg = s"Unification too complex: The equation contains $n variables which exceeds the limit of ${SetUnification.MaxVars}."
        return (List(subst(c.origin).toTimeout(msg)), SetSubstitution.empty)
      }
    }

    try {
      val s = SveAlgorithm.sveAll(cs.map(c => (Zhegalkin.toZhegalkin(c.f)(alg, CofiniteIntSet.LatticeOps), alg.zero)), mode)
      val m = s.m.foldLeft(IntMap.empty[SetFormula]) {
        case (acc, (x, e)) => acc.updated(x, Zhegalkin.toSetFormula(e))
      }
      (Nil, SetSubstitution(m) @@ subst)
    } catch {
      case _: BoolUnificationException => unsolvable(cs, subst)
    }
  }

  /** Reports the origins of `cs` as unsolvable, with the substitution found so far applied. */
  private def unsolvable(cs: List[Constraint], subst: SetSubstitution): (List[Equation], SetSubstitution) =
    (cs.map(c => subst(c.origin).toUnsolvable).distinct, SetSubstitution.empty)

}
