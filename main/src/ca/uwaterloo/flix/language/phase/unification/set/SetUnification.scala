/*
 * Copyright 2024 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.unification.set

import ca.uwaterloo.flix.language.phase.unification.set.SetFormula.*

object SetUnification {

  /**
    * The static parameters of set unification.
    *
    * @param sveRecSizeThreshold if positive, [[sve]] will give up on formulas beyond this size.
    */
  private final case class Options(sveRecSizeThreshold: Int)

  /** A class to track progress during set unification. */
  private final class Progress() {

    /** Is `true` if [[markProgress]] has been called. */
    private var hasMadeProgress: Boolean = false

    /** Mark that progress has been made. */
    def markProgress(): Unit = hasMadeProgress = true

    /** Returns `true` if [[markProgress]] has been called. */
    def hasProgressed: Boolean = hasMadeProgress
  }

  /**
    * Solves equations that trivially hold (like `univ ~ univ`) and marks trivially unsolvable
    * equations (like `univ ~ empty`). Always returns an empty substitution.
    *
    * Otherwise returns `eq` directly.
    *
    *   - `univ ~ univ` becomes `({}, [])`
    *   - `univ ~ empty` becomes `({univ ~error empty}, [])`
    *   - `x1 ~ x1` becomes `({}, [])`
    *   - `x2 ~ univ` becomes `({x2 ~ univ}, [])`
    */
  private def trivial(eq: Equation)(implicit p: Progress): (List[Equation], SetSubstitution) = {
    val Equation(t1, t2, _, _) = eq

    def error(): (List[Equation], SetSubstitution) = {
      p.markProgress()
      (List(eq.toUnsolvable), SetSubstitution.empty)
    }

    def success(): (List[Equation], SetSubstitution) = {
      p.markProgress()
      (Nil, SetSubstitution.empty)
    }

    (t1, t2) match {
      // Equations that are solved.
      case (Univ, Univ) => success()
      case (Empty, Empty) => success()
      case (Cst(c1), Cst(c2)) if c1 == c2 => success()
      case (Var(x1), Var(x2)) if x1 == x2 => success()
      case (ElemSet(e1), ElemSet(e2)) if e1 == e2 => success()
      // Equations that are Unsolvable.
      case (Univ, Empty) => error()
      case (Univ, ElemSet(_)) => error()
      case (Univ, Cst(_)) => error()
      case (Empty, Univ) => error()
      case (Empty, ElemSet(_)) => error()
      case (Empty, Cst(_)) => error()
      case (ElemSet(_), Univ) => error()
      case (ElemSet(e), Empty) if e.nonEmpty => error()
      case (ElemSet(i1), ElemSet(i2)) if i1 != i2 => error()
      case (ElemSet(_), Cst(_)) => error()
      case (Cst(_), Univ) => error()
      case (Cst(_), Empty) => error()
      case (Cst(_), ElemSet(_)) => error()
      case (Cst(c1), Cst(c2)) if c1 != c2 => error()
      // Equations that are not trivial.
      case _ => (List(eq), SetSubstitution.empty) // Cannot do anything.
    }
  }

  /**
    * Solves equations of ground assignments to variables (e.g. `x ~ c1 ∪ e2`).
    *
    *   - `x ~ f` where [[SetFormula.isGround]] on `f` is true, becomes `({}, [x -> f])`
    *   - `!x ~ f` where [[SetFormula.isGround]] on `f` is true, becomes `({}, [x -> !f])`
    *   - `t1 ∩ t2 ∩ .. ~ univ` becomes `({t1 ~ univ, t2 ~ univ, ..}, [])`
    *   - `t1 ∪ t2 ∪ .. ~ empty` becomes `({t1 ~ empty, t2 ~ empty, ..}, [])`
    *   - `f1 ~ f2` where [[SetFormula.isGround]] is true on both sides, becomes `({}, [])` if it
    *     holds or `({f1 ~error f2}, [])` if it does not.
    *
    * This also applies to the symmetric equations.
    */
  private def constantAssignment(eq: Equation)(implicit p: Progress): (List[Equation], SetSubstitution) = {
    val Equation(f1, f2, _, loc) = eq
    (f1, f2) match {
      // x ~ f, where f is ground
      // ---
      // {},
      // [x -> f]
      case (Var(x), f) if f.isGround =>
        p.markProgress()
        (Nil, SetSubstitution.singleton(x, f))

      // Symmetric case.
      case (f, Var(x)) if f.isGround =>
        p.markProgress()
        (Nil, SetSubstitution.singleton(x, f))

      // !x ~ f, where f is ground
      // ---
      // {},
      // [x -> !f]
      case (Compl(Var(x)), f) if f.isGround =>
        p.markProgress()
        (Nil, SetSubstitution.singleton(x, mkCompl(f)))

      // Symmetric case.
      case (f, Compl(Var(x))) if f.isGround =>
        p.markProgress()
        (Nil, SetSubstitution.singleton(x, mkCompl(f)))

      // f1 ∩ f2 ∩ .. ~ univ
      // ---
      // {f1 ~ univ, f2 ~ univ, ..},
      // []
      case (inter@Inter(_, _, _, _, _, _, _), Univ) =>
        p.markProgress()
        val eqs = inter.mapSubformulas(Equation.mk(_, Univ, loc))
        (eqs, SetSubstitution.empty)

      // Symmetric case.
      case (Univ, inter@Inter(_, _, _, _, _, _, _)) =>
        p.markProgress()
        val eqs = inter.mapSubformulas(Equation.mk(_, Univ, loc))
        (eqs, SetSubstitution.empty)

      // f1 ∪ f2 ∪ .. ~ empty
      // ---
      // {f1 ~ empty, f2 ~ empty, ..},
      // []
      case (union@Union(_, _, _, _, _, _, _), Empty) =>
        p.markProgress()
        val eqs = union.mapSubformulas(Equation.mk(_, Empty, loc))
        (eqs, SetSubstitution.empty)

      // Symmetric Case.
      case (Empty, union@Union(_, _, _, _, _, _, _)) =>
        p.markProgress()
        val eqs = union.mapSubformulas(Equation.mk(_, Empty, loc))
        (eqs, SetSubstitution.empty)

      // f1 ~ f2, where f1 and f2 are ground
      // ---
      // {}, [] if solved
      // {f1 ~error f2}, [] if unsolvable
      case (f1, f2) if f1.isGround && f2.isGround =>
        p.markProgress()
        if (isEquivalent(f1, f2)) (Nil, SetSubstitution.empty)
        else (List(eq.toUnsolvable), SetSubstitution.empty)

      case _ =>
        // Cannot do anything.
        (List(eq), SetSubstitution.empty)
    }
  }

  /**
    * Solves variable alias equations (e.g. `x1 ~ x2`).
    *
    *   - `x1 ~ x1` becomes `({}, [])`
    *   - `x1 ~ x2` becomes `({}, [x1 -> x2])`
    *   - `!x1 ~ !x1` becomes `({}, [])`
    *   - `!x1 ~ !x2` becomes `({}, [x1 -> x2])`
    *
    * There is a binding-bias towards lower variables, such that `x1 ~ x2` and `x2 ~ x1` both
    * become `({}, [x1 -> x2])`.
    */
  private def variableAlias(eq: Equation)(implicit p: Progress): (List[Equation], SetSubstitution) = {
    val Equation(t1, t2, _, _) = eq
    (t1, t2) match {
      // x1 ~ x1
      // ---
      // {},
      // []
      case (Var(x), Var(y)) if x == y =>
        p.markProgress()
        (Nil, SetSubstitution.empty)

      // x1 ~ x2
      // ---
      // {},
      // [x1 -> x2]
      case (x0@Var(_), y0@Var(_)) =>
        p.markProgress()
        // Make this rule stable on symmetric equations.
        val (x, y) = if (x0.x < y0.x) (x0, y0) else (y0, x0)
        (Nil, SetSubstitution.singleton(x.x, y))

      // !x1 ~ !x1
      // ---
      // {},
      // []
      case (Compl(Var(x)), Compl(Var(y))) if x == y =>
        p.markProgress()
        (Nil, SetSubstitution.empty)

      // !x1 ~ !x2
      // ---
      // {},
      // [x1 -> x2]
      case (Compl(x0@Var(_)), Compl(y0@Var(_))) =>
        p.markProgress()
        // Make this rule stable on symmetric equations.
        val (x, y) = if (x0.x < y0.x) (x0, y0) else (y0, x0)
        (Nil, SetSubstitution.singleton(x.x, y))

      case _ =>
        // Cannot do anything.
        (List(eq), SetSubstitution.empty)
    }
  }

  /**
    * Solves non-recursive variable assignments (e.g. `x1 ~ x2 ∪ c4`).
    *
    *   - `x ~ f` where `f` does not contain `x` becomes `({}, [x -> f])`
    *   - `!x ~ f` where `f` does not contain `x` becomes `({}, [x -> !f])`
    *
    * This also applies to the symmetric equations.
    */
  private def variableAssignment(eq: Equation)(implicit p: Progress): (List[Equation], SetSubstitution) = {
    val Equation(t1, t2, _, _) = eq
    (t1, t2) match {
      // x ~ f, where f does not contain x
      // ---
      // {},
      // [x -> f]
      case (v@Var(x), f) if !f.contains(v) =>
        p.markProgress()
        (Nil, SetSubstitution.singleton(x, f))

      // Symmetric case.
      case (f, v@Var(x)) if !f.contains(v) =>
        p.markProgress()
        (Nil, SetSubstitution.singleton(x, f))

      // !x ~ f, where f does not contain x
      // ---
      // {},
      // [x -> !f]
      case (Compl(v@Var(x)), f) if !f.contains(v) =>
        p.markProgress()
        (Nil, SetSubstitution.singleton(x, mkCompl(f)))

      // Symmetric case.
      case (f, Compl(v@Var(x))) if !f.contains(v) =>
        p.markProgress()
        (Nil, SetSubstitution.singleton(x, mkCompl(f)))

      case _ =>
        // Cannot do anything.
        (List(eq), SetSubstitution.empty)
    }
  }

  /**
    * Solves equations using successive-variable-elimination, i.e. exhaustive instantiation.
    *
    * Always returns no equations or `eq` marked as [[Equation.Status.Unsolvable]] or
    * [[Equation.Status.Timeout]].
    */
  private def sve(eq: Equation)(implicit p: Progress, opts: Options): (List[Equation], SetSubstitution) = {
    p.markProgress()
    val query = mkEmptyQuery(eq.f1, eq.f2)
    val fvs = query.variables.toList
    try {
      val subst = successiveVariableElimination(query, fvs)
      (Nil, subst)
    } catch {
      case NoSolutionException() => (List(eq.toUnsolvable), SetSubstitution.empty)
      case ComplexException(msg) => (List(eq.toTimeout(msg)), SetSubstitution.empty)
    }
  }

  /**
    * The Successive Variable Elimination algorithm.
    *
    * Returns the most-general unifier of the equation `f ~ empty` where `fvs` is the free
    * variables in `f`. If there is no unifier then [[NoSolutionException]] is thrown.
    *
    * Eliminates variables recursively from `fvs`.
    *
    * If the formula that is recursively built is ever larger than `recSizeThreshold` then
    * [[ComplexException]] is thrown. If `recSizeThreshold` is non-positive then there is no
    * checking.
    */
  private def successiveVariableElimination(f: SetFormula, fvs: List[Int])(implicit opts: Options): SetSubstitution = fvs match {
    case Nil =>
      // `fvs` is empty so `f` has no variables.
      // The remaining constants are rigid so `f` has to be empty no matter their instantiation.
      // Return the empty substitution if `f` is equivalent to `empty`.
      if (isEmptyEquivalent(f)) SetSubstitution.empty
      else throw NoSolutionException()

    case x :: xs =>
      val f0 = SetSubstitution.singleton(x, Empty)(f)
      val f1 = SetSubstitution.singleton(x, Univ)(f)
      val recFormula = propagation(mkInter(f0, f1))
      assertSveSize(recFormula)
      val se = successiveVariableElimination(recFormula, xs)
      val xFormula = propagation(mkUnion(se(f0), mkDifference(Var(x), se(f1))))
      // We can safely use `unsafeExtend` because `xFormula` contains no variables and we only add
      // each variable of `fvs` once (which is assumed to have no duplicates).
      // `se`, `x`, and `xFormula` therefore have disjoint variables.
      se.unsafeExtend(x, xFormula)
  }

  /** Throws [[ComplexException]] if `f` is larger than [[Options.sveRecSizeThreshold]]. */
  private def assertSveSize(f: SetFormula)(implicit opts: Options): Unit = {
    if (opts.sveRecSizeThreshold > 0) {
      val fSize = f.size
      if (fSize > opts.sveRecSizeThreshold) throw ComplexException(
        s"SetFormula size ($fSize) is over recursive SVE threshold ($opts.sveRecSizeThreshold)."
      )
    }
  }
}
