/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.unification.set

import ca.uwaterloo.flix.language.phase.unification.EffUnification3
import ca.uwaterloo.flix.language.phase.unification.set.SetFormula.*
import ca.uwaterloo.flix.language.phase.unification.set.SetUnification.Phase
import ca.uwaterloo.flix.language.phase.unification.shared.{BoolUnificationException, CofiniteIntSet, SveAlgorithm, SveMode}
import ca.uwaterloo.flix.language.phase.unification.zhegalkin.{Zhegalkin, ZhegalkinAlgebra, ZhegalkinExpr}

import scala.collection.immutable.{IntMap, SortedSet}
import scala.collection.mutable

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
  *      The equations are taken smallest first, independent of their order (see [[sortBySize]]).
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
  *      creates; past the budget a constraint is left whole for SVE. A constraint without a union
  *      whose variables all sit inside symmetric differences is stuck, and is revisited when one
  *      of its variables is bound.
  *
  *   4. SVE: the stuck constraints and the constraints left whole are solved by [[SveAlgorithm]]
  *      at the granularity given by the mode.
  *
  * The substitution is applied lazily: a constraint records how many bindings its formula has
  * seen, and the bindings made since are applied when it is taken off the worklist (see
  * [[State.update]]). The phases are component-local without an explicit partition, since a
  * binding of `x` only changes the constraints that mention `x`.
  *
  * Complexity. Let `n` be the number of distinct equations and `v` the number of variables. Phase 1
  * makes at most `v + 1` passes of `n` substitutions. Phase 2 creates at most `2n` constraints.
  * Phase 3 sees at most `2n` plus the split budget many constraints, and every step closes, splits
  * or parks the constraint it takes up. A free step closes a constraint, so there are at most that
  * many bindings, and a stuck constraint is revisited at most once per binding: quadratic in the
  * number of constraints, which is linear in `n`. The ground emptiness test goes through a
  * Zhegalkin polynomial and can be exponential in the rigid constants it mentions. Phase 4 is
  * exponential in the variables of the constraints it receives, each of which is checked against
  * [[SetUnification.MaxVars]].
  */
object SetUnification2 {

  /**
    * The budget on constraints created by distributing unions, as a multiple of the number of
    * input equations plus a constant. Past the budget, constraints are left whole for SVE.
    */
  private val SplitBudgetPerEquation: Int = 8
  private val SplitBudgetBase: Int = 64

  private implicit val alg: ZhegalkinAlgebra[CofiniteIntSet] = EffUnification3.Algebra

  /**
    * A constraint `f ~ ∅` derived from the input equation `origin`.
    *
    * The formula has the first `version` bindings of the solve applied (see [[State.update]]).
    */
  private final case class Constraint(f: SetFormula, origin: Equation, version: Int)

  /** The outcome of [[assignmentOf]] on one equation. */
  private sealed trait Assignment

  private object Assignment {
    /** The equation is neither an alias nor a definition. */
    case object NotApplicable extends Assignment

    /** The equation holds trivially. */
    case object Trivial extends Assignment

    /** The equation is solved by `[x -> f]`. */
    final case class Bind(x: Int, f: SetFormula, phase: Phase) extends Assignment
  }

  /** The mutable state of one call to [[solve]]. */
  private final class State(val splitBudget: Int) {
    /** The substitution built so far. */
    var subst: SetSubstitution = SetSubstitution.empty
    /** The bindings made so far, in order. */
    val bindings: mutable.ArrayBuffer[(Int, SetFormula)] = mutable.ArrayBuffer.empty
    /** The constraints to process. */
    var pending: List[Constraint] = Nil
    /** The constraints without a free step or a union. Revisited when a variable they mention is bound. */
    var stuck: List[Constraint] = Nil
    /** The constraints left whole because the split budget was exhausted. */
    var leftovers: List[Constraint] = Nil
    /** The origins of the constraints found to be in conflict, most recent first. */
    var conflicts: List[Equation] = Nil
    /** The number of constraints created by distributing unions. */
    var splits: Int = 0

    /** The number of bindings made so far. */
    def version: Int = bindings.length

    /** Records the binding `x ↦ f` on top of the substitution built so far. */
    def bind(x: Int, f: SetFormula): Unit = {
      subst = SetSubstitution.singleton(x, f) @@ subst
      bindings += ((x, f))
    }

    /**
      * Returns the formula of `c` with the bindings made since its creation applied, in order.
      *
      * Applying the bindings one at a time, in order, to a formula that has seen the earlier ones
      * gives the same result as applying the composed substitution to the original formula.
      *
      * The bindings already applied must not be applied again: a binding `x ↦ x ∪ g` leaves `x`
      * in the formula as its own parameter, so applying it twice yields `(x ∪ g) ∪ g`, and a part
      * split off along that union would be split again on every visit.
      */
    def update(c: Constraint): SetFormula = {
      var f = c.f
      var i = c.version
      while (i < bindings.length) {
        val (x, r) = bindings(i)
        f = SetSubstitution.singleton(x, r)(f)
        i += 1
      }
      f
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
    // Equations are compared without their locations, so that the same constraint from different
    // sites (e.g. the cases of a handler) counts as a duplicate.
    val eqs = sortBySize(l.distinctBy(eq => (eq.f1, eq.f2)))
    val state = new State(SplitBudgetPerEquation * eqs.length + SplitBudgetBase)

    val rest = aliasAndDefine(eqs, state)
    for (eq <- rest) {
      state.pending = inclusionsOf(eq, state.version) ::: state.pending
    }
    processConstraints(state)
    sve(state, mode)
  }

  /**
    * Returns `eqs` sorted by size, smallest first, with ties broken by the structural hash of the
    * equation, so that the order of `eqs` matters only among equations of equal size and hash.
    *
    * The order decides which of several definitions `x ~ f1`, `x ~ f2`, .. of a variable binds it
    * in [[aliasAndDefine]]: the first binds `x`, and the others become inclusion constraints. With
    * the smallest first, the binding copies the smallest formula into every occurrence of `x`,
    * and the larger ones are solved as constraints. In the opposite order, a large range is copied
    * everywhere and the small equations become constraints over it, whose free steps nest the
    * previous ranges inside the next.
    */
  private def sortBySize(eqs: List[Equation]): List[Equation] =
    eqs.map(eq => ((eq.f1.size + eq.f2.size, (eq.f1, eq.f2).hashCode), eq)).sortBy(_._1).map(_._2)

  /**
    * Solves the aliases `x ~ y` and definitions `x ~ f`, where `x` does not occur in `f`, among
    * `eqs`, and returns the other equations with the resulting substitution applied.
    *
    * Runs passes over the equations until a pass binds no variable. Every binding removes its
    * variable from the system, so there is at most one pass per variable.
    */
  private def aliasAndDefine(eqs: List[Equation], state: State): List[Equation] = {
    var work = eqs
    var progress = true
    while (progress) {
      progress = false
      var kept: List[Equation] = Nil
      for (eq0 <- work) {
        val eq = state.subst(eq0)
        assignmentOf(eq) match {
          case Assignment.Trivial =>
            progress = true
            stat(Phase.Trivial, 1, 0)
          case Assignment.Bind(x, f, phase) =>
            state.bind(x, f)
            progress = true
            stat(phase, 1, 1)
          case Assignment.NotApplicable =>
            kept = eq0 :: kept
        }
      }
      work = kept.reverse
    }
    work.map(state.subst.apply)
  }

  /**
    * Matches `eq` against the shapes solved by [[aliasAndDefine]]:
    *
    *   - `f ~ f` holds trivially
    *   - `x ~ y` becomes `[x -> y]`, binding the lower variable to the higher
    *   - `x ~ f` where `f` does not contain `x` becomes `[x -> f]`
    */
  private def assignmentOf(eq: Equation): Assignment = {
    val Equation(f1, f2, _, _) = eq
    (f1, f2) match {
      case _ if f1 == f2 => Assignment.Trivial
      case (Var(x), Var(y)) =>
        if (x < y) Assignment.Bind(x, f2, Phase.Alias) else Assignment.Bind(y, f1, Phase.Alias)
      case (v@Var(x), f) if !f.contains(v) => Assignment.Bind(x, simplify(f), Phase.Define)
      case (f, v@Var(x)) if !f.contains(v) => Assignment.Bind(x, simplify(f), Phase.Define)
      case _ => Assignment.NotApplicable
    }
  }

  /**
    * Returns `eq` as constraints `f ~ ∅` at the given `version`: the equation `f1 ~ f2` holds
    * exactly when the inclusions `f1 ∩ ¬f2 ~ ∅` and `¬f1 ∩ f2 ~ ∅` hold. An equation with `∅` or
    * `𝓤` on one side is one constraint.
    */
  private def inclusionsOf(eq: Equation, version: Int): List[Constraint] = {
    val Equation(f1, f2, _, _) = eq
    val fs = (f1, f2) match {
      case (Empty, f) => f :: Nil
      case (f, Empty) => f :: Nil
      case (Univ, f) => mkCompl(f) :: Nil
      case (f, Univ) => mkCompl(f) :: Nil
      case _ => mkInter(f1, mkCompl(f2)) :: mkInter(mkCompl(f1), f2) :: Nil
    }
    fs.map(f => Constraint(simplify(f), eq, version))
  }

  /** Processes the pending constraints of `state` until none are left or a conflict is found. */
  private def processConstraints(state: State): Unit = {
    while (state.pending.nonEmpty && state.conflicts.isEmpty) {
      val c = state.pending.head
      state.pending = state.pending.tail
      // Variables of the constraint may have been bound since it was created.
      val f = simplify(state.update(c))
      processConstraint(f, c.origin, state)
    }
  }

  /**
    * Processes the constraint `f ~ ∅`, derived from `origin`, where `f` is simplified and has the
    * current substitution applied.
    */
  private def processConstraint(f: SetFormula, origin: Equation, state: State): Unit = f match {
    case Empty =>
      stat(Phase.Trivial, 1, 0)

    case Univ =>
      state.conflicts = origin :: state.conflicts

    case _ =>
      val members = f match {
        case Inter(l) => l.toList
        case _ => f :: Nil
      }
      foldElements(members) match {
        case None =>
          // The element sets of the members are disjoint, so the intersection is empty.
          stat(Phase.Trivial, 1, 0)

        case Some(ms) => freeStepOf(ms) match {
          case Some((x, range)) =>
            state.bind(x, range)
            stat(Phase.Bound, 1, 0)
            // The stuck constraints that mention x may now have a free step.
            val v = Var(x)
            val (touched, rest) = state.stuck.partition(_.f.contains(v))
            state.stuck = rest
            state.pending = touched ::: state.pending

          case None =>
            if (ms.forall(_.isGround)) {
              if (isEmpty(mkInterAll(ms))) {
                stat(Phase.Ground, 1, 0)
              } else {
                state.conflicts = origin :: state.conflicts
              }
            } else {
              ms.collectFirst { case u: Union => u } match {
                case Some(u) =>
                  if (state.splits >= state.splitBudget) {
                    state.leftovers = Constraint(mkInterAll(ms), origin, state.version) :: state.leftovers
                  } else {
                    // (g1 ∪ g2 ∪ ..) ∩ h ~ ∅ holds exactly when every gi ∩ h ~ ∅ holds.
                    state.splits += u.l.length
                    val others = ms.filterNot(_ eq u)
                    val parts = u.l.toList.map(g => Constraint(mkInterAll(g :: others), origin, state.version))
                    state.pending = parts ::: state.pending
                  }
                case None =>
                  state.stuck = Constraint(mkInterAll(ms), origin, state.version) :: state.stuck
              }
            }
        }
      }
  }

  /**
    * Folds the element sets among the simplified `members` of an intersection into one positive
    * element set, using `e1 ∩ e2 ∩ ¬e3 = (e1 ∩ e2) - e3`, or into one complemented element set if
    * there is no positive one.
    *
    * Returns [[None]] if the folded element set is empty, so that the intersection is empty.
    */
  private def foldElements(members: List[SetFormula]): Option[List[SetFormula]] = {
    var pos: Option[SortedSet[Int]] = None
    var neg: SortedSet[Int] = SortedSet.empty
    var rest: List[SetFormula] = Nil
    for (m <- members) m match {
      case ElemSet(s) => pos = Some(pos.fold(s)(_ intersect s))
      case Compl(ElemSet(s)) => neg = neg union s
      case _ => rest = m :: rest
    }
    pos match {
      case Some(s) =>
        val d = s diff neg
        if (d.isEmpty) None else Some(ElemSet(d) :: rest.reverse)
      case None =>
        if (neg.isEmpty) Some(members) else Some(Compl(ElemSet(neg)) :: rest.reverse)
    }
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
    ZhegalkinExpr.isEmpty(Zhegalkin.toZhegalkin(f)(alg, CofiniteIntSet.LatticeOps))

  /**
    * Solves the stuck and leftover constraints of `state` by SVE, and returns the result of the
    * whole solve: the unsolved equations and the substitution.
    */
  private def sve(state: State, mode: SveMode): (List[Equation], SetSubstitution) = {
    val subst = state.subst

    /** Reports `origins` as unsolvable, with the substitution found so far applied. */
    def unsolvable(origins: List[Equation]): (List[Equation], SetSubstitution) =
      (origins.map(o => subst(o).toUnsolvable).distinct, SetSubstitution.empty)

    // Bring every remaining constraint up to date with the bindings made since its creation. The
    // pending constraints are those left when a conflict stopped the processing.
    val eqs = (state.stuck ::: state.pending ::: state.leftovers).map(c => Constraint(simplify(state.update(c)), c.origin, state.version))

    if (state.conflicts.nonEmpty) {
      // A conflict was found. Every unsolved equation is reported.
      return unsolvable(state.conflicts.reverse ::: eqs.map(_.origin))
    }

    if (eqs.isEmpty) {
      return (Nil, subst)
    }

    for (c <- eqs) {
      val n = c.f.varsOf.size
      if (n > SetUnification.MaxVars) {
        val msg = s"Unification too complex: The equation contains $n variables which exceeds the limit of ${SetUnification.MaxVars}."
        return (List(subst(c.origin).toTimeout(msg)), SetSubstitution.empty)
      }
    }

    stat(Phase.SuccessiveVariableElimination, eqs.length, eqs.map(_.f.varsOf.size).sum)

    val queries = eqs.map(c => (Zhegalkin.toZhegalkin(c.f)(alg, CofiniteIntSet.LatticeOps), alg.zero))
    try {
      val s = SveAlgorithm.sveAll(queries, mode)
      val m = s.m.foldLeft(IntMap.empty[SetFormula]) {
        case (acc, (x, e)) => acc.updated(x, Zhegalkin.toSetFormula(e))
      }
      (Nil, SetSubstitution(m) @@ subst)
    } catch {
      case _: BoolUnificationException =>
        // SVE failed. Every equation handed to it is reported.
        unsolvable(eqs.map(_.origin))
    }
  }

  /** Records that `phase` closed `eqs` equations or constraints and eliminated `vars` variables. */
  private def stat(phase: Phase, eqs: Int, vars: Int): Unit = {
    if (SetUnification.EnableStats) {
      SetUnification.synchronized {
        SetUnification.ElimPerRule.update(phase, SetUnification.ElimPerRule.getOrElse(phase, 0) + eqs)
        SetUnification.VarElimPerRule.update(phase, SetUnification.VarElimPerRule.getOrElse(phase, 0) + vars)
      }
    }
  }

}
