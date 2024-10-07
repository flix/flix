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

import SetFormula.*

object SetUnification {

  /** A class to track progress during set unification. */
  private class Progress() {

    /** Is `true` if [[markProgress]] has been called. */
    private var hasMadeProgress: Boolean = false

    /** Mark that progress has been made. */
    def markProgress(): Unit = hasMadeProgress = true

    /** Returns `true` if [[markProgress]] has been called. */
    def hasProgressed: Boolean = hasMadeProgress
  }

  /** A collection of rules that can be used to solve individual [[Equation]]. */
  private object Rules {

    /**
      * Solves equations that trivially hold (like `univ ~ univ`) and marks trivially unsolvable
      * equations (like `univ ~ empty`). Always returns an empty substitution.
      *
      * Otherwise returns `eq` directly.
      *
      *   - `univ ~ univ` becomes `({}, [])`
      *   - `univ ~ empty` becomes `({univ !~ empty}, [])`
      *   - `x1 ~ x1` becomes `({}, [])`
      *   - `x2 ~ univ` becomes `({x2 ~ univ}, [])`
      */
    def trivial(eq: Equation)(implicit p: Progress): (List[Equation], SetSubstitution) = {
      val Equation(t1, t2, _, _) = eq

      def error(): (List[Equation], SetSubstitution) = {
        p.markProgress()
        (List(eq.toUnsolvable), SetSubstitution.empty)
      }

      def success(): (List[Equation], SetSubstitution) = {
        p.markProgress()
        (Nil, SetSubstitution.empty)
      }

      // TODO: decide on Equation ordering method.

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
      *   - `x ~ f` where [[SetFormula.isGround]] on `f` becomes `({}, [x -> f])`
      *   - `!x ~ f` where [[SetFormula.isGround]] on `f` becomes `({}, [x -> !f])`
      *   - `t1 ∩ t2 ∩ .. ~ univ` becomes `({t1 ~ univ, t2 ~ univ, ..}, [])`
      *   - `t1 ∪ t2 ∪ .. ~ empty` becomes `({t1 ~ empty, t2 ~ empty, ..}, [])`
      *
      * All of these are applied to their symmetric equations.
      */
    def constantAssignment(eq: Equation)(implicit p: Progress): (List[Equation], SetSubstitution) = {
      val Equation(f01, f02, _, loc) = eq
      // TODO: decide on Equation ordering method.

      // Reorder formulas to avoid symmetric matches below.
      //   - `x` and `!x` to the left
      //   - `empty` and `univ` to the right
      val (f1, f2) = (f01, f02) match {
        case (_, Var(_)) => (f02, f01)
        case (_, Compl(Var(_))) => (f02, f01)
        case (Empty, _) => (f02, f01)
        case (Univ, _) => (f02, f01)
        case _ => (f01, f02)
      }

      (f1, f2) match {
        // x ~ f, where f is ground
        // ---
        // {},
        // [x -> f]
        case (Var(x), f) if f.isGround =>
          p.markProgress()
          (Nil, SetSubstitution.singleton(x, f))

        // !x ~ f, where f is ground
        // ---
        // {},
        // [x -> !f]
        case (Compl(Var(x)), f) if f.isGround =>
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

        // f1 ∪ f2 ∪ .. ~ empty
        // ---
        // {f1 ~ empty, f2 ~ empty, ..},
        // []
        case (union@Union(_, _, _, _, _, _, _), Empty) =>
          p.markProgress()
          val eqs = union.mapSubformulas(Equation.mk(_, Empty, loc))
          (eqs, SetSubstitution.empty)

        // f1 ~ f2, where f1 and f2 are ground
        // ---
        // {}, [] if solved
        // {f1 !~ f2}, [] if unsolvable
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
      */
    def variableAlias(eq: Equation)(implicit p: Progress): (List[Equation], SetSubstitution) = {
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
        case (Var(x), y@Var(_)) =>
          p.markProgress()
          // TODO: decide on Equation ordering method.
          //       how to bias this symmetric equation
          (Nil, SetSubstitution.singleton(x, y))

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
        case (Compl(Var(x)), Compl(y@Var(_))) =>
          p.markProgress()
          // TODO: decide on Equation ordering method.
          //       how to bias this symmetric equation
          (Nil, SetSubstitution.singleton(x, y))

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
      */
    def variableAssignment(eq: Equation)(implicit p: Progress): (List[Equation], SetSubstitution) = {
      val Equation(t1, t2, _, _) = eq
      // TODO: decide on Equation ordering method.
      (t1, t2) match {
        // x ~ f, where f does not contain x
        // ---
        // {},
        // [x -> f]
        case (v@Var(x), f) if !f.contains(v) =>
          p.markProgress()
          (Nil, SetSubstitution.singleton(x, f))

        // !x ~ f, where f does not contain x
        // ---
        // {},
        // [x -> !f]
        case (Compl(v@Var(x)), f) if !f.contains(v) =>
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
    def sve(recSizeThreshold: Int)(eq: Equation)(implicit p: Progress): (List[Equation], SetSubstitution) = {
      p.markProgress()
      val query = mkEquivalenceTestToEmpty(eq.f1, eq.f2)
      val fvs = query.variables.toList
      try {
        val subst = successiveVariableElimination(query, fvs, recSizeThreshold)
        (Nil, subst)
      } catch {
        case NoSolutionException() => (List(eq.toUnsolvable), SetSubstitution.empty)
        case ComplexException(msg) => (List(eq.toTimeout(msg)), SetSubstitution.empty)
      }
    }
  }

}
