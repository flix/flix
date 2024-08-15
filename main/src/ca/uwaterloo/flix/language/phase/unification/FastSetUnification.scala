/*
 *  Copyright 2024 Magnus Madsen
 *  Copyright 2024 Jonathan Lindegaard Starup
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.{Formatter, InternalCompilerException, Result}

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.collection.mutable

/**
  *  Fast Type Inference with Systems of Set Unification [[Equation]]s of [[Term]]s.
  *
  * A set unification solver based on the following ideas:
  *   - Work on all the equations as one whole system.
  *   - Assume all equations has been put into normal form. This avoids the need to mirror a
  *     lot of cases ([[Equation.mk]] for details).
  *   - Instead of applying rules ad-hoc, work in phases ([[Solver.solve]] for details).
  *   - Represent n-ary operations with seperated parts for easy simplification
  *     ([[Term]] for details]]).
  *   - Use rewriting term constructors to keep formulas structured
  *     ([[Term.mkCompl]], [[Term.mkUnionAll]], and [[Term.mkInterAll]] for details).
  *   - Assume an open-world, i.e. the universe of elements is infinite.
  * */
object FastSetUnification {

  import ca.uwaterloo.flix.language.phase.unification.FastSetUnification.Phases.Phase
  import ca.uwaterloo.flix.language.phase.unification.FastSetUnification.Rules.Output

  object Solver {

    /**
      * @param sizeThreshold the upper limit of the amount of connectives in the substitution
      * @param complexThreshold the upper limit of mappings in the substitution
      * @param permutationLimit the number of permutations given to SVE
      * @param debugging prints information to terminal during solving
      * @param rerun if a system couldn't be solved, rerun it with `debugging = true`
      * @param verifySubst verify that the solution substitution is a solution (VERY SLOW)
      * @param verifySize verify that the solution substitution is less than `sizeThreshold`
      */
    case class RunOptions(
                           sizeThreshold: Int,
                           complexThreshold: Int,
                           permutationLimit: Int,
                           debugging: Boolean,
                           rerun: Boolean,
                           verifySubst: Boolean,
                           verifySize: Boolean
                         )

    object RunOptions {
      val default: RunOptions = RunOptions(
        sizeThreshold = 800,
        complexThreshold = 10,
        permutationLimit = 10,
        debugging = false,
        rerun = false,
        verifySubst = false,
        verifySize = true
      )
    }

    /**
      * Represents the running state of the solver
      *   - eqs: the remaining equations to solve
      *   - subst: the current substitution, which has already been applied to `eqs`
      *   - lastPhase: the name of the last phase that was run and made progress
      *   - phase: the number of the last phase that was run and made progress
      */
    private class State(var eqs: List[Equation]) {
      var subst: SetSubstitution = SetSubstitution.empty
      var lastPhase: Option[String] = None
      var phase: Option[Int] = None
    }

    /**
      * Attempts to solve the equation system that this solver was instantiated with.
      *
      * Returns `Result.Ok(s)` with a most-general substitution which solves the equations system.
      *
      * Returns `Result.Err((ex, l, s))` where `c` is a conflict, `l` is a list of unsolved equations, and `s` is a partial substitution.
      */
    def solve(l: List[Equation])(implicit opts: RunOptions): (Result[SetSubstitution, (FastBoolUnificationException, List[Equation], SetSubstitution)], (Option[String], Option[Int])) = {
      import FastSetUnification.{Phases => P}
      val state = new State(l)

      def checkAndSimplify(s: State): Unit = runPhase(
        "Check and Simplify",
        "trivial correct and incorrect equations",
        P.filteringPhase(P.checkAndSimplify)
      )(s)(opts.copy(debugging = false))

      debugState(state)
      try {
        runPhase(
          "Constant Propagation",
          "resolves all equations of the form: x = c where x is a var and c is univ/empty/constant/element)",
          P.propagateConstants
        )(state)
        checkAndSimplify(state)
        runPhase(
          "Variable Propagation",
          "resolves all equations of the form: x = y where x and y are vars)",
          P.propagateVars
        )(state)
        checkAndSimplify(state)
        runPhase(
          "Variable Assignment",
          "resolves all equations of the form: x = t where x is free in t",
          P.varAssignment
        )(state)
        checkAndSimplify(state)
        runPhase(
          "Eliminate Trivial and Redundant Equations",
          "eliminates equations of the form X = X and duplicated equations",
          P.filteringPhase(P.eliminateTrivialAndRedundant)
        )(state)
        checkAndSimplify(state)
        runPhase(
          "Set Unification",
          "resolves all remaining equations using SVE.",
          P.completePhase(P.setUnifyAllPickSmallest(opts.complexThreshold, opts.permutationLimit))
        )(state)
        if (opts.verifySubst) verifySubst(state.subst, l)
        if (opts.verifySize) verifySubstSize(state.subst)
        assert(state.eqs.isEmpty)
        val res = Result.Ok(state.subst)
        (res, (state.lastPhase, state.phase))
      } catch {
        case _ if !opts.debugging && opts.rerun =>
          // rerun with debugging
          solve(l)(opts.copy(debugging = true, rerun = false))
        case ex: ConflictException =>
          val res = Result.Err((ex, state.eqs, state.subst))
          (res, (state.lastPhase, state.phase))
        case ex: TooComplexException =>
          val res = Result.Err((ex, state.eqs, state.subst))
          (res, (state.lastPhase, state.phase))
      }

    }

    /**
      * Runs the given [[Phase]] on the given [[State]], debugging information if required by [[RunOptions]].
      */
    private def runPhase(name: String, description: String, phase: Phase)(state: State)(implicit opts: RunOptions): Unit = {
      if (state.eqs.isEmpty) return
      val phaseNumber = state.phase.getOrElse(0) + 1
      state.phase = Some(phaseNumber)
      state.lastPhase = Some(name)
      debugPhase(phaseNumber, name, description)
      phase(state.eqs) match {
        case Some((eqs, subst)) =>
          state.eqs = eqs
          state.subst = subst @@ state.subst
        case None =>
          ()
      }
      debugState(state)
    }

    /**
      * Verifies that `subst` is a solution to `eqs`.
      *
      * Throws [[InternalCompilerException]] if the solution is incorrect.
      *
      * Note: Does not verify that `subst` is the most general solution.
      *
      * Note: This function is very slow since [[Term.emptyEquivalent]] is very slow.
      */
    def verifySubst(subst: SetSubstitution, eqs: List[Equation]): Unit = {
      // Apply the substitution to every equation and check that it is solved.
      for (e <- eqs) {
        // We want to check that `s(t1) == s(t2)`
        val t1 = subst.apply(e.t1)
        val t2 = subst.apply(e.t2)
        if (!Term.equivalent(t1, t2)) {
          println(s"  Original  : ${e.t1} ~ ${e.t2}")
          println(s"  with Subst: $t1 ~ $t2")
          throw InternalCompilerException(s"Incorrectly solved equation", SourceLocation.Unknown)
        }
      }
    }

    private def verifySubstSize(subst: SetSubstitution)(implicit opts: RunOptions): Unit = {
      val size = subst.size
      if (size > opts.sizeThreshold) {
        throw TooComplexException(s"Too large a substitution (threshold: ${opts.sizeThreshold}, found: $size)")
      }
    }

    private def debugPhase(number: Int, name: String, description: String)(implicit opts: RunOptions): Unit = {
      debugln("-".repeat(80))
      debugln(s"--- Phase $number: $name")
      debugln(s"    ($description)")
      debugln("-".repeat(80))
    }

    private def debugState(state: State)(implicit opts: RunOptions): Unit = {
      debugln(s"Equations (${state.eqs.size}):")
      debugln(format(state.eqs))
      debugln(s"Substitution (${state.subst.numberOfBindings}):")
      debugln(state.subst.toString)
      debugln("")
    }

    // Note: By-name to ensure that we do not compute expensive strings.
    private def debugln(s: => String)(implicit opts: RunOptions): Unit = {
      if (opts.debugging) Console.println(s)
    }

  }

  //
  // A general rule is of type Equation => (List[Equation], Substitution).
  //
  // The returned list might be an Option or just an Equation..
  // The returned substitution might be omitted
  //
  private object Rules {

    type Output = Option[(List[Equation], SetSubstitution)]
    type Rule = Equation => Output

    def triviallyHolds(eq: Equation): Boolean = {
      val Equation(t1, t2, _) = eq
      (t1, t2) match {
        case (Term.Univ, Term.Univ) => true
        case (Term.Empty, Term.Empty) => true
        case (Term.Cst(c1), Term.Cst(c2)) if c1 == c2 => true
        case (Term.Var(x1), Term.Var(x2)) if x1 == x2 => true
        case (Term.ElemSet(s1), Term.ElemSet(s2)) if s1 == s2 => true
        case _ => false
      }
    }

    def triviallyWrong(eq: Equation): Option[ConflictException] = {
      // Possible additions
      // - compare elements/constants to intersections
      val Equation(t1, t2, loc) = eq

      @inline
      def error: Option[ConflictException] = Some(ConflictException(t1, t2, loc))

      (t1, t2) match {
        case (Term.Univ, Term.Empty) => error
        case (Term.Univ, Term.ElemSet(_)) => error
        case (Term.Univ, Term.Cst(_)) => error
        case (Term.Univ, inter: Term.Inter) if inter.triviallyNonUniv => error
        case (Term.Empty, Term.Univ) => error
        case (Term.Empty, Term.ElemSet(_)) => error
        case (Term.Empty, Term.Cst(_)) => error
        case (Term.Empty, union: Term.Union) if union.triviallyNonEmpty => error
        case (Term.ElemSet(_), Term.Univ) => error
        case (Term.ElemSet(_), Term.Empty) => error
        case (Term.ElemSet(i1), Term.ElemSet(i2)) if i1 != i2 => error
        case (Term.ElemSet(_), Term.Cst(_)) => error
        case (Term.Cst(_), Term.Univ) => error
        case (Term.Cst(_), Term.Empty) => error
        case (Term.Cst(_), Term.ElemSet(_)) => error
        case (Term.Cst(c1), Term.Cst(c2)) if c1 != c2 => error
        case (inter: Term.Inter, Term.Univ) if inter.triviallyNonUniv => error
        case (union: Term.Union, Term.Empty) if union.triviallyNonEmpty => error
        case _ => None
      }
    }

    def constantAssignment(eq: Equation): Output = {
      val Equation(t1, t2, loc) = eq
      (t1, t2) match {
        // x ~ t, where t has no variables
        // ---
        // [],
        // [x -> t]
        case (Term.Var(x), t) if t.noFreeVars =>
          Some((Nil, SetSubstitution.singleton(x, Term.propagation(t))))

        // x ∩ y ∩ !z ∩ ... ∩ rest_i ~ univ
        // ---
        // [rest_i ~ univ],
        // [x -> univ, y -> univ, z -> empty, ...]
        case (inter@Term.Inter(_, _, posVars, _, _, negVars, rest), Term.Univ) if inter.mightBeUniv =>
          // note: posVars and negVars are guaranteed to be disjoint
          var subst = SetSubstitution.empty
          var changed = false
          // x -> univ
          for (Term.Var(x) <- posVars) {
            subst = subst.extended(x, Term.Univ, loc)
            changed = true
          }
          // z -> empty
          for (Term.Var(x) <- negVars) {
            subst = subst.extended(x, Term.Empty, loc)
            changed = true
          }
          if (rest.nonEmpty) {
            val constraints = rest.map(Equation.mk(_, Term.Univ, loc))
            Some((constraints, subst))
          } else if (changed) {
            Some((Nil, subst))
          } else {
            None
          }

        // x ∪ y ∪ !z ∪ ... ∪ rest ~ empty
        // ---
        // [rest_i ~ empty],
        // [x -> empty, y -> empty, z -> univ, ...]
        case (union@Term.Union(_, _, posVars, _, _, negVars, rest), Term.Empty) if union.mightBeEmpty =>
          // note: posVars and negVars are guaranteed to be disjoint
          var subst = SetSubstitution.empty
          var changed = false
          // x -> empty
          for (Term.Var(x) <- posVars) {
            subst = subst.extended(x, Term.Empty, loc)
            changed = true
          }
          // z -> univ
          for (Term.Var(x) <- negVars) {
            subst = subst.extended(x, Term.Univ, loc)
            changed = true
          }
          if (rest.nonEmpty) {
            val constraints = rest.map(Equation.mk(_, Term.Empty, loc))
            Some((constraints, subst))
          } else if (changed) {
            Some((Nil, subst))
          } else {
            None
          }

        case _ => None
      }
    }

    def variableAlias(eq: Equation): Option[SetSubstitution] = {
      val Equation(t1, t2, _) = eq
      (t1, t2) match {
        case (Term.Var(x), y@Term.Var(_)) =>
          Some(SetSubstitution.singleton(x, y))
        case _ =>
          None
      }
    }

    def variableAssignment(eq: Equation): Option[SetSubstitution] = {
      val Equation(t1, t2, _) = eq
      (t1, t2) match {
        case (v@Term.Var(x), rhs) if !rhs.freeVarsContains(v) =>
          // We have found an equation: `x ~ t` where `x` is not free in `t`.
          // Construct a singleton substitution `[x -> y]`.
          // Apply propagation to simplify the rhs before it goes into the substitution.
          Some(SetSubstitution.singleton(x, Term.propagation(rhs)))
        case _ =>
          None
      }
    }

    /**
      * Set unification using the SVE algorithm.
      *
      * Returns a most-general unifier that solves the given equation `e`.
      *
      * Throws a [[ConflictException]] if the equation cannot be solved.
      */
    def setUnifyOne(e: Equation): Result[SetSubstitution, ConflictException] = try {
      // The set expression we want to show is empty.
      val query = Term.mkXor(e.t1, e.t2)

      // Determine the order in which to eliminate the variables.
      val fvs = query.freeVars.toList

      // Eliminate all variables one by one in the order specified by fvs.
      Result.Ok(Term.successiveVariableElimination(query, fvs))
    } catch {
      case _: BoolUnificationException => Result.Err(ConflictException(e.t1, e.t2, e.loc))
    }
  }

  private object Phases {

    type Phase = List[Equation] => Output

    def filteringPhase(f: List[Equation] => Option[List[Equation]]): Phase = {
      eqs0 => f(eqs0).map(eqs1 => (eqs1, SetSubstitution.empty))
    }

    def completePhase(f: List[Equation] => SetSubstitution): Phase = {
      eqs0 => Some((Nil, f(eqs0)))
    }

    /**
      * Returns a list of non-trivial unification equations computed from the given list `l`.
      *
      * Throws a [[ConflictException]] if an unsolvable equation is encountered.
      *
      * A trivial equation is one of:
      * -     univ ~ univ
      * -    empty ~ empty
      * -        e ~ e        (same element)
      * -        c ~ c        (same constant)
      * -        x ~ x        (same variable)
      */
    def checkAndSimplify(l: List[Equation]): Option[List[Equation]] = {
      var changed = false
      // unwrap to check triviallyWrong no matter if trivial did anything
      val res0 = runEqRule(Rules.triviallyHolds)(l) match {
        case Some(eqs) =>
          changed = true
          eqs
        case None =>
          l
      }
      runErrRule(Rules.triviallyWrong)(res0)
      if (changed) Some(res0) else None
    }


    /**
      * Propagates constants and truth values through the equation system.
      *
      * The implementation saturates the system, i.e. it computes a fixpoint.
      *
      * The implementation uses five rewrite rules:
      *
      * - `x ~ univ` becomes `[x -> univ]`.
      * - `x ~ c` becomes `[x -> c]`.
      * - `x ~ e` becomes `[x -> e]`.
      * - `x ∩ y ∩ !z ∩ ... ∩ rest ~ univ` becomes `[x -> univ, y -> univ, z -> empty, ...]` and `rest ~ univ`.
      * - `x ∪ y ∪ !z ∪ ... ∪ rest ~ empty` becomes `[x -> empty, y -> empty, z -> univ, ...]` and `rest ~ empty`.
      *
      * For example, if the equation system is:
      *
      * {{{
      *     c1794221043 ~ (x55062 ∩ x55050 ∩ x55046 ∩ x55060 ∩ x55066 ∩ x55040 ∩ x55075 ∩ x55042 ∩ x55058 ∩ x55078)
      *     x55078 ~ x112431
      *     c1794221043 ~ x55040
      *     x55042 ~ x112433
      *     x55044 ~ x112437
      *     x55046 ~ (x112435 ∩ x55044)
      *     x55048 ~ univ
      *     x55050 ~ (x112439 ∩ x55048)
      *     x55052 ~ x112443
      *     x55055 ~ univ
      *     x55058 ~ (x55052 ∩ x112441 ∩ x55055)
      *     x55060 ~ x112446
      *     x55062 ~ x112448
      *     x55066 ~ univ
      *     x55075 ~ x112453
      * }}}
      *
      * then after constant propagation it is:
      *
      * {{{
      *     c1794221043 ~ (c1794221043 ∩ x55062 ∩ x55050 ∩ x55046 ∩ x55060 ∩ x55075 ∩ x55042 ∩ x55058 ∩ x55078)
      *     x55078 ~ x112431
      *     x55042 ~ x112433
      *     x55044 ~ x112437
      *     x55046 ~ (x112435 ∩ x55044)
      *     x112439 ~ x55050
      *     x55052 ~ x112443
      *     x55058 ~ (x55052 ∩ x112441)
      *     x55060 ~ x112446
      *     x55062 ~ x112448
      *     x55075 ~ x112453
      * }}}
      *
      * with the substitution:
      *
      * {{{
      *     x55048 -> univ
      *     x55055 -> univ
      *     x55066 -> univ
      *     x55040 -> c1794221043
      * }}}
      *
      * Note that several equations were simplified.
      *
      * Note: We use `subst.extended` to check for conflicts. For example, if we already know that `s = [x -> c17]` and we
      * learn that `x -> univ` then we will try to extend s with the new binding which will raise a [[ConflictException]].
      */
    def propagateConstants(l: List[Equation]): Output = {
      runRule(Rules.constantAssignment, selfFeeding = true, substInduced = true)(l)
    }

    /**
      * Propagates variables through the equation system. Cannot fail.
      *
      * The observation is that we if we have simple equations of the form: `x ~ y`
      * then we can create a substitution that binds `[x -> y]`. Every time we create
      * a binding, we must be careful to update both unsolved and pending equations.
      *
      * For example, if the equation system is:
      *
      * {{{
      *     x78914 ~ (c1498 ∩ c1500 ∩ c1501)
      *     x78914 ~ (x78926 ∩ x78923 ∩ x78917)
      *     x78917 ~ x127244
      *     x78921 ~ x127251
      *     x78923 ~ (x127249 ∩ x127247 ∩ x127248)
      *     x78926 ~ (x127254 ∩ x127252)
      * }}}
      *
      * then after variable propagation it is:
      *
      * {{{
      *     x78914 ~ (c1498 ∩ c1500 ∩ c1501)
      *     x78914 ~ (x78926 ∩ x78923 ∩ x127244)
      *     x78923 ~ (x127249 ∩ x127247 ∩ x127248)
      *     x78926 ~ (x127254 ∩ x127252)
      * }}}
      *
      * with the substitution:
      *
      * {{{
      *      x78917 -> x127244
      *     x127251 -> x78921
      * }}}
      *
      * Returns a list of unsolved equations and a partial substitution.
      */
    def propagateVars(l: List[Equation]): Output = {
      runSubstRule(Rules.variableAlias)(l)
    }


    /**
      * Assigns variables. Given a unification equation `x ~ t` we can assign `[x -> t]` if `x` does not occur in `t`.
      *
      * For example, given the equation system:
      *
      * {{{
      *    x78914 ~ (c1498 ∩ c1500 ∩ c1501)
      *    x78914 ~ (x78926 ∩ x78923 ∩ x127244)
      *    x78923 ~ (x127249 ∩ x127247 ∩ x127248)
      *    x78926 ~ (x127254 ∩ x127252)
      * }}}
      *
      * We compute the substitution:
      *
      * {{{
      *     x78926 -> (x127254 ∩ x127252)
      *     x78914 -> (c1498 ∩ c1500 ∩ c1501)
      *     x78923 -> (x127249 ∩ x127247 ∩ x127248)
      * }}}
      *
      * and we have the remaining equations:
      *
      * {{{
      *     (c1498 ∩ c1500 ∩ c1501) ~ (x127248 ∩ x127244 ∩ x127254 ∩ x127252 ∩ x127249 ∩ x127247)
      * }}}
      */
    def varAssignment(l: List[Equation]): Output = {
      runSubstRule(Rules.variableAssignment)(l)
    }

    /**
      * Eliminates trivial and redundant equations.
      *
      * An equation is trivial if its LHS and RHS are the same.
      *
      * An equation is redundant if it appears multiple times in the list of equations.
      *
      * For example, given the equation system:
      *
      * {{{
      *   (c9 ∩ c0) ~ (c9 ∩ c0)
      *         c10 ~ c11
      * }}}
      *
      * We return the new equation system:
      *
      * {{{
      *   c10 ~ c11
      * }}}
      *
      * For example, given the equation system:
      *
      * {{{
      *   (c1 ∩ c3) ~ (c1 ∩ c3)
      *   (c1 ∩ c3) ~ (c1 ∩ c3)
      * }}}
      *
      * We return the new equation system:
      *
      * {{{
      *   (c1 ∩ c3) ~ (c1 ∩ c3)
      * }}}
      *
      * We only remove equations, hence the returned substitution is always empty.
      *
      * Note: We only consider *syntactic equality*.
      */
    def eliminateTrivialAndRedundant(l: List[Equation]): Option[List[Equation]] = {
      var result = List.empty[Equation]
      val seen = mutable.Set.empty[Equation]
      var changed = false

      // We rely on equations and terms having correct equals and hashCode functions.
      // Note: We are purely working with *syntactic equality*, not *semantic equality*.
      for (eq <- l) {
        if (!seen.contains(eq)) {
          // The equation has not been seen before.
          if (eq.t1 != eq.t2) {
            // The LHS and RHS are different.
            seen += eq
            result = eq :: result
          } else {
            changed = true
          }
        } else {
          changed = true
        }
      }

      if (changed) Some(result.reverse) else None
    }

    /**
      * Given a unification equation system `l` computes a most-general unifier for all its equations.
      *
      * If multiple equations are involved then we try to solve them in different order to find a small substitution.
      */
    def setUnifyAllPickSmallest(complexThreshold: Int, permutationLimit: Int)(l: List[Equation]): SetSubstitution = {
      // Case 1: We have at most one equation to solve: just solve immediately.
      if (l.length <= 1) {
        return setUnifyAll(l)
      }

      // Case 2: Check that there are not too many complex equations.
      if (l.length > complexThreshold) {
        throw TooComplexException(s"Too many complex equations (threshold: $complexThreshold, found: ${l.length})")
      }

      // Case 3: We solve the first [[PermutationLimit]] permutations and pick the one that gives rise to the smallest substitution.
      val results = l.permutations.take(permutationLimit).toList.map {
        p => (p, setUnifyAll(p))
      }.sortBy {
        case (_, s) => s.size
      }

      // Pick the smallest substitution.
      results.head._2
    }


    /**
      * Computes the most-general unifier of all the given equations `l`.
      *
      * Throws a [[ConflictException]] if an equation in `l` cannot be solved.
      *
      * Note: We assume that any existing substitution has already been applied to the equations in `l`.
      *
      * Note: We assume that any existing substitution will be composed with the substitution returned by this function.
      */
    def setUnifyAll(l: List[Equation]): SetSubstitution = {
      runSubstResRule(Rules.setUnifyOne)(l)
    }

    // Converting Rule into Phase

    /**
      * @param rule a solving rule that optionally produces constraints, cs, and a substitution subst.
      * @param selfFeeding can the rule potentially solve any constrains in cs?
      * @param substInduced can the rule potentially discover new solvable constraints via any subst it returns?
      * @param l the constraints to run the rule on
      * @return the remaining constraints and the found substitution.
      */
    private def runRule(rule: Equation => Output, selfFeeding: Boolean, substInduced: Boolean)(l: List[Equation]): Output = {
      // TODO optimization:
      // - Sometimes the @@ can be ++
      // - Sometimes the outer loop can be skipped (aren't all rules discoverable by subst?)
      var subst = SetSubstitution.empty
      var workList = l
      var leftover: List[Equation] = Nil
      var changed = true // has there been progress in the inner loop
      var overallChanged = false // has there been progress at any point in the loops
      var flipped = false // is the `workList` list flipped compared to the initial order
      while (changed) {
        flipped = !flipped
        changed = false
        while (workList != Nil) workList match {
          case eq0 :: tail =>
            val eq = subst.apply(eq0)
            workList = tail
            rule(eq) match {
              case Some((eqs, s)) =>
                changed = true
                overallChanged = true
                // add the new eqs to `workList` if declared applicable
                if (selfFeeding) workList = eqs ++ workList
                else leftover = eqs ++ leftover
                subst = s @@ subst
              case None =>
                leftover = eq :: leftover
            }
          case Nil => ()
        }
        workList = leftover
        leftover = Nil
        if (!substInduced) changed = false // stop the outer loop is it is declared unnecessary
      }
      if (overallChanged) {
        val correctedTodo = if (flipped) workList.reverse else workList
        Some(correctedTodo.map(subst.apply), subst)
      } else {
        None
      }
    }

    /** If the rule returns `true` that means that the constraint should be removed */
    private def runEqRule(rule: Equation => Boolean)(l: List[Equation]): Option[List[Equation]] = {
      val rule1 = (eq0: Equation) => if (rule(eq0)) Some((Nil, SetSubstitution.empty)) else None
      runRule(rule1, selfFeeding = false, substInduced = false)(l).map(_._1)
    }

    private def runSubstRule(rule: Equation => Option[SetSubstitution])(l: List[Equation]): Output = {
      val rule1 = (eq0: Equation) => rule(eq0).map(subst => (Nil, subst))
      runRule(rule1, selfFeeding = false, substInduced = true)(l)
    }

    private def runSubstResRule(rule: Equation => Result[SetSubstitution, Throwable])(l: List[Equation]): SetSubstitution = {
      val rule1 = (eq0: Equation) => rule(eq0) match {
        case Result.Ok(subst: SetSubstitution) => Some((Nil: List[Equation], subst))
        case Result.Err(ex: Throwable) => throw ex
      }
      val res = runRule(rule1, selfFeeding = false, substInduced = false)(l)
      res.map {
        case (eqs, subst) =>
          assert(eqs.isEmpty)
          subst
      }.getOrElse(SetSubstitution.empty)
    }

    private def runErrRule(rule: Equation => Option[Throwable])(l: List[Equation]): Unit = {
      val rule1 = (eq0: Equation) => rule(eq0).map[(List[Equation], SetSubstitution)](ex => throw ex)
      val res = runRule(rule1, selfFeeding = false, substInduced = false)(l)
      assert(res.isEmpty)
    }

  }

  /**
    * Represents a finite or co-finite set with infinite domain.
    * All sets are either a concrete set or the complement of a concrete set.
    * No concrete set is ever equivalent to universe.
    */
  sealed trait SetEval {

    import SetEval.{Compl, Set}

    /**
      * Returns `true` if this set is universe.
      */
    def isUniv: Boolean = this match {
      case Compl(s) if s.isEmpty => true
      case Set(_) => false
      case Compl(_) => false
    }

    /**
      * Returns `true` if this set is empty.
      */
    def isEmpty: Boolean = this match {
      case Set(s) if s.isEmpty => true
      case Set(_) => false
      case Compl(_) => false
    }
  }

  private object SetEval {
    private case class Set(s: SortedSet[Int]) extends SetEval

    private case class Compl(s: SortedSet[Int]) extends SetEval

    val empty: SetEval = Set(SortedSet.empty)

    val univ: SetEval = Compl(SortedSet.empty)

    /**
      * Returns the set of `s`.
      */
    def set(s: SortedSet[Int]): SetEval = Set(s)

    /**
      * Returns the union of two sets.
      */
    def union(s1: SetEval, s2: SetEval): SetEval = (s1, s2) match {
      case (Set(x), Set(y)) =>
        // x ∪ y
        Set(x.union(y))
      case (Set(x), Compl(y)) =>
        // x ∪ !y =
        // !(!x ∩ y) =
        // !(y ∩ !x) =
        // !(y - x)
        Compl(y.diff(x))
      case (Compl(x), Set(y)) =>
        // !x ∪ y =
        // !(x ∩ !y) =
        // !(x - y)
        Compl(x.diff(y))
      case (Compl(x), Compl(y)) =>
        // !x ∪ !y =
        // !(x ∩ y)
        Compl(x.intersect(y))
    }

    /**
      * Returns the intersection of two sets.
      */
    def intersection(s1: SetEval, s2: SetEval): SetEval = (s1, s2) match {
      case (Set(x), Set(y)) =>
        // x ∩ y
        Set(x.intersect(y))
      case (Set(x), Compl(y)) =>
        // x ∩ !y =
        // x - y
        Set(x.diff(y))
      case (Compl(x), Set(y)) =>
        // !x ∩ y =
        // y ∩ !x =
        // y - x
        Set(y.diff(x))
      case (Compl(x), Compl(y)) =>
        // !x ∩ !y =
        // !(x ∪ y)
        Compl(x.union(y))
    }

    /**
      * Returns the complement of the set.
      */
    def complement(s: SetEval): SetEval = s match {
      case Set(s) =>
        // !s
        Compl(s)
      case Compl(s) =>
        // !!s =
        // s
        Set(s)
    }
  }

  /** Companion object for [[Equation]]. */
  object Equation {
    /**
      * Returns a unification equation  `t1 ~ t2` between the terms `t1` and `t2`.
      *
      * The smart constructor performs normalization:
      * - We move univ, empty to the rhs.
      * - We move a single variable to the lhs.
      * - We reorder constant/variables so that the smaller constant/variable is on the lhs.
      *
      * Examples:
      * -     univ ~ x7 ==> x7 ~ univ
      * -       c3 ~ c2 ==> c2 ~ c3
      * -       x7 ~ x5 ==> x5 ~ x7
      * - x3 ∩ x7 ~ x4 ==> x4 ~ x3 ∩ x7
      */
    def mk(t1: Term, t2: Term, loc: SourceLocation): Equation = (t1, t2) match {
      case (Term.Cst(c1), Term.Cst(c2)) => if (c1 <= c2) Equation(t1, t2, loc) else Equation(t2, t1, loc)
      case (Term.Var(x1), Term.Var(x2)) => if (x1 <= x2) Equation(t1, t2, loc) else Equation(t2, t1, loc)
      case (Term.Univ, _) => Equation(t2, t1, loc)
      case (Term.Empty, _) => Equation(t2, t1, loc)
      case (Term.ElemSet(_), _) => Equation(t2, t1, loc)
      case (Term.Cst(_), _) => Equation(t2, t1, loc)
      case (_, Term.Var(_)) => Equation(t2, t1, loc)
      case _ => Equation(t1, t2, loc)
    }
  }

  /**
    * Represents a unification equation `t1 ~ t2` between the terms `t1` and `t2`.
    *
    * WARNING: Equations should be normalized. Use the smart constructor [[Equation.mk]] to create a new equation.
    */
  case class Equation private(t1: Term, t2: Term, loc: SourceLocation) {
    /**
      * Returns the size of this equation which is the sum of its lhs and rhs.
      */
    final def size: Int = t1.size + t2.size

    /**
      * Returns a human-readable representation of `this` unification equation.
      */
    override final def toString: String = s"$t1 ~ $t2"
  }

  /**
    * A common super-type for set terms.
    *
    * Note: We assume that the integers used for constants and variables are disjoint.
    */
  sealed trait Term {

    /**
      * Syntactic sugar for [[Term.reconstructInter]].
      */
    def inter(that: Term): Term = Term.mkInter(this, that)

    /**
      * Syntactic sugar for [[Term.mkUnionAll]].
      */
    def union(that: Term): Term = Term.mkUnion(this, that)

    /**
      * Syntactic sugar for [[Term.mkCompl]]
      */
    def compl(): Term = Term.mkCompl(this)

    /**
      * Syntactic sugar for [[Equation.mk]]
      */
    final def ~(that: Term)(implicit loc: SourceLocation): Equation = Equation.mk(this, that, loc)

    /**
      * Returns all variables that occur in `this` term.
      *
      * Note: use [[freeVarsContains]] or [[noFreeVars]] when possible.
      */
    final def freeVars: SortedSet[Int] = this match {
      case Term.Univ => SortedSet.empty
      case Term.Empty => SortedSet.empty
      case Term.Cst(_) => SortedSet.empty
      case Term.ElemSet(_) => SortedSet.empty
      case Term.Var(x) => SortedSet(x)
      case Term.Compl(t) => t.freeVars
      case Term.Inter(_, _, posVars, _, _, negVars, rest) =>
        SortedSet.empty[Int] ++ posVars.map(_.x) ++ negVars.map(_.x) ++ rest.flatMap(_.freeVars)
      case Term.Union(_, _, posVars, _, _, negVars, rest) =>
        SortedSet.empty[Int] ++ posVars.map(_.x) ++ negVars.map(_.x) ++ rest.flatMap(_.freeVars)
    }

    /**
      * Returns `true` if [[freeVars]] contains `v`.
      */
    final def freeVarsContains(v: Term.Var): Boolean = this match {
      case Term.Univ => false
      case Term.Empty => false
      case Term.Cst(_) => false
      case Term.ElemSet(_) => false
      case Term.Var(_) => this == v
      case Term.Compl(t) => t.freeVarsContains(v)
      case Term.Inter(_, _, posVars, _, _, negVars, rest) =>
        posVars.contains(v) || negVars.contains(v) || rest.exists(t => t.freeVarsContains(v))
      case Term.Union(_, _, posVars, _, _, negVars, rest) =>
        posVars.contains(v) || negVars.contains(v) || rest.exists(t => t.freeVarsContains(v))
    }

    /**
      * Returns all variables and constants that occur in `this` term.
      *
      * OBS: [[Term.Cst]] and [[Term.Var]] must use disjoint integers.
      */
    final def freeUnknowns: SortedSet[Int] = this match {
      case Term.Univ => SortedSet.empty
      case Term.Empty => SortedSet.empty
      case Term.Cst(c) => SortedSet(c)
      case Term.ElemSet(_) => SortedSet.empty
      case Term.Var(x) => SortedSet(x)
      case Term.Compl(t) => t.freeUnknowns
      case Term.Inter(_, posCsts, posVars, _, negCsts, negVars, rest) =>
        SortedSet.empty[Int] ++ posCsts.map(_.c) ++ posVars.map(_.x) ++ negCsts.map(_.c) ++ negVars.map(_.x) ++ rest.flatMap(_.freeUnknowns)
      case Term.Union(_, posCsts, posVars, _, negCsts, negVars, rest) =>
        SortedSet.empty[Int] ++ posCsts.map(_.c) ++ posVars.map(_.x) ++ negCsts.map(_.c) ++ negVars.map(_.x) ++ rest.flatMap(_.freeUnknowns)
    }

    /**
      * Returns true if there is no variables in this term.
      */
    final def noFreeVars: Boolean = this match {
      case Term.Univ => true
      case Term.Empty => true
      case Term.Cst(_) => true
      case Term.Var(_) => false
      case Term.ElemSet(_) => true
      case Term.Compl(t) => t.noFreeVars
      case Term.Inter(_, _, posVars, _, _, negVars, rest) =>
        posVars.isEmpty && negVars.isEmpty && rest.forall(_.noFreeVars)
      case Term.Union(_, _, posVars, _, _, negVars, rest) =>
        posVars.isEmpty && negVars.isEmpty && rest.forall(_.noFreeVars)
    }

    /**
      * Returns the number of connectives in `this` term.
      *
      * For example, `size(x) = 0`, `size(x ∩ y) = 1`, and `size(x ∩ !y) = 2`.
      */
    final def size: Int = this match {
      case Term.Univ => 0
      case Term.Empty => 0
      case Term.Cst(_) => 0
      case Term.ElemSet(_) => 0
      case Term.Var(_) => 0
      case Term.Compl(t) => t.size + 1
      case Term.Inter(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
        // We need a connective for each constant, variable, and term minus one.
        // We then add the size of all the sub-terms in `rest`.
        ((posElem.size + posCsts.size + posVars.size + negElem.size + negCsts.size + negVars.size + rest.size) - 1) + rest.map(_.size).sum
      case Term.Union(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
        ((posElem.size + posCsts.size + posVars.size + negElem.size + negCsts.size + negVars.size + rest.size) - 1) + rest.map(_.size).sum
    }

    override def toString: String = toFormattedString(Formatter.NoFormatter)

    /**
      * Returns a human-readable representation of `this` term.
      */
    def toFormattedString(formatter: Formatter): String = this match {
      case Term.Univ => formatter.red("univ")
      case Term.Empty => formatter.red("empty")
      case Term.Cst(c) => formatter.blue(s"c$c")
      case Term.ElemSet(s) if s.sizeIs == 1 => formatter.green(s"e${s.head}")
      case Term.ElemSet(s) => formatter.green(s"e${s.mkString(".")}")
      case Term.Var(x) => s"x$x"
      case Term.Compl(f) => f match {
        case Term.Univ => formatter.red(s"!$f")
        case Term.Empty => formatter.red(s"!$f")
        case Term.Cst(_) => formatter.blue(s"!$f")
        case Term.ElemSet(_) => formatter.green(s"!$f")
        case Term.Var(_) => s"!$f"
        case _ => s"!($f)"
      }
      case Term.Inter(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
        val terms = posElem.toList ++ negElem.map(Term.mkCompl(_)) ++ posCsts ++ negCsts.map(Term.mkCompl(_)) ++ posVars ++ negVars.map(Term.mkCompl(_)) ++ rest
        s"(${terms.mkString(" ∩ ")})"
      case Term.Union(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
        val terms = posElem.toList ++ negElem.map(Term.mkCompl(_)) ++ posCsts ++ negCsts.map(Term.mkCompl(_)) ++ posVars ++ negVars.map(Term.mkCompl(_)) ++ rest
        s"(${terms.mkString(" ∪ ")})"
    }

  }

  object Term {

    /**
      * The UNIV symbol.
      */
    case object Univ extends Term

    /**
      * The EMPTY symbol.
      */
    case object Empty extends Term

    /**
      * Represents an uninterpreted constant ("rigid variable").
      *
      * Note: We assume that constants and variables are disjoint.
      */
    case class Cst(c: Int) extends Term

    /**
      * Represents a set variable ("flexible variable").
      *
      * Note: We assume that constants and variables are disjoint.
      */
    case class Var(x: Int) extends Term

    /**
      * Represents a concrete set of elements.
      *
      * Note: `s` must be non-empty
      */
    case class ElemSet(s: SortedSet[Int]) extends Term {
      assert(s.nonEmpty)
    }

    /**
      * Represents the complement of the term `t` (`!`).
      *
      * Should NEVER be build outside of [[mkCompl]].
      */
    case class Compl(t: Term) extends Term

    /**
      * Represents a intersection of terms (`∩`). An empty intersection is univ.
      *
      * Should NEVER be build outside of [[reconstructInter]] methods.
      *
      * We use a clever representation where we have a intersection of elements, constants, variables, and then sub-terms.
      *
      * For example, the intersection: `x7 ∩ !x2 ∩ c1 ∩ x4 ∩ (e1 ∪ x9)` is represented as:
      *
      * `None, Set(c1), Set(x4, x7), Set(), Set(), Set(x2), List(e1 ∪ x9)`.
      */
    case class Inter private(posElem: Option[Term.ElemSet], posCsts: Set[Term.Cst], posVars: Set[Term.Var], negElem: Option[Term.ElemSet], negCsts: Set[Term.Cst], negVars: Set[Term.Var], rest: List[Term]) extends Term {
      assert(posVars.intersect(negVars).isEmpty, this.toString)
      assert(posCsts.intersect(negCsts).isEmpty, this.toString)

      /** Returns true if any elements or constants exist in the outer intersection */
      def triviallyNonUniv: Boolean = posElem.isDefined || posCsts.nonEmpty || negElem.isDefined || negCsts.nonEmpty

      /** Returns false if any elements or constants exist in the outer intersection */
      def mightBeUniv: Boolean = !triviallyNonUniv
    }

    /**
      * A union of the terms `ts` (`∪`). An empty union is empty.
      *
      * Should NEVER be build outside of [[mkUnionAll]] methods.
      *
      * Represented similarly to [[Inter]].
      */
    case class Union(posElem: Option[Term.ElemSet], posCsts: Set[Term.Cst], posVars: Set[Term.Var], negElem: Option[Term.ElemSet], negCsts: Set[Term.Cst], negVars: Set[Term.Var], rest: List[Term]) extends Term {
      assert(posVars.intersect(negVars).isEmpty, this.toString)
      assert(posCsts.intersect(negCsts).isEmpty, this.toString)

      /** Returns true if any elements or constants exist in the outer union */
      def triviallyNonEmpty: Boolean = posElem.isDefined || posCsts.nonEmpty || negElem.isDefined || negCsts.nonEmpty

      /** Returns false if any elements or constants exist in the outer union */
      def mightBeEmpty: Boolean = !triviallyNonEmpty
    }

    final def mkElemSet(i: Int): ElemSet = {
      Term.ElemSet(SortedSet(i))
    }

    final def mkElemSet(i: SortedSet[Int]): Term = {
      if (i.isEmpty) Term.Empty
      else Term.ElemSet(i)
    }

    /**
      * Smart constructor for complement (`!`).
      */
    final def mkCompl(t: Term): Term = t match {
      case Univ => Empty
      case Empty => Univ
      case Cst(_) => Compl(t)
      case Var(_) => Compl(t)
      case ElemSet(_) => Compl(t)
      case Compl(t0) => t0
      case Inter(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
        reconstructUnion(negElem, negCsts, negVars, posElem, posCsts, posVars, rest.map(mkCompl))
      case Union(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
        reconstructInter(negElem, negCsts, negVars, posElem, posCsts, posVars, rest.map(mkCompl))
    }

    /**
      * Smart constructor for intersection (`∩`).
      */
    final def mkInter(t1: Term, t2: Term): Term = (t1, t2) match {
      case (Empty, _) => Empty
      case (_, Empty) => Empty
      case (Univ, _) => t2
      case (_, Univ) => t1
      case _ => mkInterAll(List(t1, t2))
    }

    /**
      * Smart constructor for union (`∪`).
      */
    final def mkUnion(t1: Term, t2: Term): Term = (t1, t2) match {
      case (Univ, _) => Univ
      case (_, Univ) => Univ
      case (Empty, _) => t2
      case (_, Empty) => t1
      case _ => mkUnionAll(List(t1, t2))
    }

    /**
      * Smart constructor for intersection (`∩`).
      *
      * A lot of heavy lifting occurs here. In particular, we must partition `ts` into
      * (a) pos/neg elements (b) pos/neg constants, (c) pos/neg variables, and (d) other sub-terms.
      * Moreover, we look into those sub-terms and flatten any intersections we find within.
      */
    final def mkInterAll(ts: List[Term]): Term = {
      // Mutable data structures to hold elements, constants, variables, and other sub-terms.
      var posElemTerm0: Option[SortedSet[Int]] = None // None represents univ
      val posCstTerms = mutable.Set.empty[Term.Cst]
      val posVarTerms = mutable.Set.empty[Term.Var]
      var negElemTerm0: SortedSet[Int] = SortedSet.empty
      val negCstTerms = mutable.Set.empty[Term.Cst]
      val negVarTerms = mutable.Set.empty[Term.Var]
      val restTerms = mutable.ListBuffer.empty[Term]
      for (t <- ts) {
        t match {
          case Univ => // NOP - We do not have to include Univ in a intersection.
          case Empty => return Empty // If the intersection contains EMPTY then whole intersection is EMPTY.

          // Add atoms if found and check for their negation to short-circuit
          // Element cancelling is done afterwards
          case x@Term.Cst(_) =>
            if (negCstTerms.contains(x)) return Empty
            posCstTerms += x
          case Term.Compl(x@Term.Cst(_)) =>
            if (posCstTerms.contains(x)) return Empty
            negCstTerms += x
          case Term.ElemSet(s) =>
            posElemTerm0 = posElemTerm0.map(_.intersect(s)).orElse(Some(s))
            if (posElemTerm0.exists(_.isEmpty)) return Empty
          case Term.Compl(Term.ElemSet(s)) =>
            negElemTerm0 = negElemTerm0.union(s)
          case x@Term.Var(_) =>
            if (negVarTerms.contains(x)) return Empty
            posVarTerms += x
          case Term.Compl(x@Term.Var(_)) =>
            if (posVarTerms.contains(x)) return Empty
            negVarTerms += x

          // A nested intersection
          case Inter(posElem0, posCsts0, posVars0, negElem0, negCsts0, negVars0, rest0) =>
            // Add its atoms while checking negated occurrences.
            // Element cancelling is done afterwards
            posElem0 match {
              case Some(e) =>
                posElemTerm0 = posElemTerm0.map(_.intersect(e.s)).orElse(Some(e.s))
                if (posElemTerm0.exists(_.isEmpty)) return Empty
              case None => () // nothing to add
            }
            for (x <- posCsts0) {
              if (negCstTerms.contains(x)) return Empty
              posCstTerms += x
            }
            for (x <- posVars0) {
              if (negVarTerms.contains(x)) return Empty
              posVarTerms += x
            }
            for (e <- negElem0) {
              negElemTerm0 = negElemTerm0.union(e.s)
            }
            for (x <- negCsts0) {
              if (posCstTerms.contains(x)) return Empty
              negCstTerms += x
            }
            for (x <- negVars0) {
              if (posVarTerms.contains(x)) return Empty
              negVarTerms += x
            }
            // Iterate through the nested sub-terms of the nested intersection to
            // flatten the structure one layer.
            for (t0 <- rest0) {
              t0 match {
                case Univ => // NOP - We do not have to include Univ in a intersection.
                case Empty => return Empty // If the intersection contains EMPTY then whole intersection is EMPTY.
                case x@Term.Cst(_) =>
                  if (negCstTerms.contains(x)) return Empty
                  posCstTerms += x
                case Term.Compl(x@Term.Cst(_)) =>
                  if (posCstTerms.contains(x)) return Empty
                  negCstTerms += x
                case Term.ElemSet(s) =>
                  posElemTerm0 = posElemTerm0.map(_.intersect(s)).orElse(Some(s))
                  if (posElemTerm0.exists(_.isEmpty)) return Empty
                case Term.Compl(Term.ElemSet(s)) =>
                  negElemTerm0 = negElemTerm0.union(s)
                case x@Term.Var(_) =>
                  if (negVarTerms.contains(x)) return Empty
                  posVarTerms += x
                case Term.Compl(x@Term.Var(_)) =>
                  if (posVarTerms.contains(x)) return Empty
                  negVarTerms += x
                case _ => restTerms += t0
              }
            }
          case _ => restTerms += t // We found some other sub-term.
        }
      }
      // remove redundant elements that are known not to be included
      val (posElemTerm, negElemTerm) = posElemTerm0 match {
        case Some(s) =>
          // es1 inter !es2
          // es1 - es2
          val posElems = s.diff(negElemTerm0)
          val pos = if (posElems.isEmpty) return Empty else Some(Term.ElemSet(posElems))
          (pos, None)
        case None =>
          val neg = if (negElemTerm0.isEmpty) None else Some(Term.ElemSet(negElemTerm0))
          (None, neg)
      }


      // Eliminate intersections with zero or one subterm
      val elms = posElemTerm.size + posCstTerms.size + posVarTerms.size + negElemTerm.size + negCstTerms.size + negVarTerms.size + restTerms.size
      if (elms == 0) {
        return Term.Univ
      } else if (elms == 1) {
        for (any <- posElemTerm) return any
        for (any <- posCstTerms) return any
        for (any <- posVarTerms) return any
        for (any <- negElemTerm) return Term.mkCompl(any)
        for (any <- negCstTerms) return Term.mkCompl(any)
        for (any <- negVarTerms) return Term.mkCompl(any)
        for (any <- restTerms) return any
      }
      // posElemTerms should never be over size 1 since that should short-circuit to Empty
      Inter(posElemTerm, posCstTerms.toSet, posVarTerms.toSet, negElemTerm, negCstTerms.toSet, negVarTerms.toSet, restTerms.toList)
    }

    /**
      * Smart constructor for intersection (`∩`).
      *
      * More efficient than the other constructors when building an intersection
      * based on an existing intersection.
      *
      * OBS: must be composed of collections form existing intersections/unions
      * to preserve invariants.
      */
    final def reconstructInter(posElem: Option[Term.ElemSet], posCsts: Set[Term.Cst], posVars: Set[Term.Var], negElem: Option[Term.ElemSet], negCsts: Set[Term.Cst], negVars: Set[Term.Var], rest: List[Term]): Term = {
      val maintain = Term.Inter(posElem, posCsts, posVars, negElem, negCsts, negVars, Nil)
      Term.mkInterAll(maintain :: rest)
    }

    /**
      * Smart constructor for union (`∪`).
      */
    final def mkUnionAll(ts: List[Term]): Term = {
      // Mutable data structures to hold elements, constants, variables, and other sub-terms.
      var posElemTerm0: SortedSet[Int] = SortedSet.empty
      val posCstTerms = mutable.Set.empty[Term.Cst]
      val posVarTerms = mutable.Set.empty[Term.Var]
      var negElemTerm0: Option[SortedSet[Int]] = None
      val negCstTerms = mutable.Set.empty[Term.Cst]
      val negVarTerms = mutable.Set.empty[Term.Var]
      val restTerms = mutable.ListBuffer.empty[Term]
      for (t <- ts) {
        t match {
          case Empty => // NOP - We do not have to include Empty in a union.
          case Univ => return Univ // If the union contains UNIV then whole union is UNIV.

          // Add atoms if found and check for their negation to short-circuit
          case x@Term.Cst(_) =>
            if (negCstTerms.contains(x)) return Univ
            posCstTerms += x
          case Term.Compl(x@Term.Cst(_)) =>
            if (posCstTerms.contains(x)) return Univ
            negCstTerms += x
          case Term.ElemSet(s) =>
            posElemTerm0 = posElemTerm0.union(s)
          case Term.Compl(Term.ElemSet(s)) =>
            negElemTerm0 = negElemTerm0.map(_.intersect(s)).orElse(Some(s))
            if (negElemTerm0.exists(_.isEmpty)) return Univ
          case x@Term.Var(_) =>
            if (negVarTerms.contains(x)) return Univ
            posVarTerms += x
          case Term.Compl(x@Term.Var(_)) =>
            if (posVarTerms.contains(x)) return Univ
            negVarTerms += x

          // A nested union
          case Union(posElem0, posCsts0, posVars0, negElem0, negCsts0, negVars0, rest0) =>
            // Add its atoms while checking negated occurrences.
            // Element cancelling is done afterwards
            for (x <- posElem0) {
              posElemTerm0 = posElemTerm0.union(x.s)
            }
            for (x <- posCsts0) {
              if (negCstTerms.contains(x)) return Univ
              posCstTerms += x
            }
            for (x <- posVars0) {
              if (negVarTerms.contains(x)) return Univ
              posVarTerms += x
            }
            negElem0 match {
              case Some(e) =>
                negElemTerm0 = negElemTerm0.map(_.intersect(e.s)).orElse(Some(e.s))
                if (negElemTerm0.exists(_.isEmpty)) return Univ
              case None => () // nothing to add
            }
            for (x <- negCsts0) {
              if (posCstTerms.contains(x)) return Univ
              negCstTerms += x
            }
            for (x <- negVars0) {
              if (posVarTerms.contains(x)) return Univ
              negVarTerms += x
            }
            // Iterate through the nested sub-terms of the nested intersection to
            // flatten the structure one layer.
            for (t0 <- rest0) {
              t0 match {
                case Empty => // NOP - We do not have to include Empty in a union.
                case Univ => return Univ // If the union contains UNIV then whole union is UNIV.
                case x@Term.Cst(_) =>
                  if (negCstTerms.contains(x)) return Univ
                  posCstTerms += x
                case Term.Compl(x@Term.Cst(_)) =>
                  if (posCstTerms.contains(x)) return Univ
                  negCstTerms += x
                case Term.ElemSet(s) =>
                  posElemTerm0 = posElemTerm0.union(s)
                case Term.Compl(Term.ElemSet(s)) =>
                  negElemTerm0 = negElemTerm0.map(_.intersect(s)).orElse(Some(s))
                  if (negElemTerm0.exists(_.isEmpty)) return Univ
                case x@Term.Var(_) =>
                  if (negVarTerms.contains(x)) return Univ
                  posVarTerms += x
                case Term.Compl(x@Term.Var(_)) =>
                  if (posVarTerms.contains(x)) return Univ
                  negVarTerms += x
                case _ => restTerms += t0
              }
            }
          case _ => restTerms += t // We found some other sub-term.
        }
      }
      // remove redundant elements that are known to be included
      val (posElemTerm, negElemTerm) = negElemTerm0 match {
        case Some(s) =>
          // es1 union !es2
          // !(!es1 inter es2)
          // !(es2 - es1)
          val negElems = s.diff(posElemTerm0)
          val neg = if (negElems.isEmpty) return Term.Univ else Some(Term.ElemSet(negElems))
          (None, neg)
        case None =>
          val pos = if (posElemTerm0.isEmpty) None else Some(Term.ElemSet(posElemTerm0))
          (pos, None)
      }

      // Eliminate unions with zero or one subterm
      val elms = posElemTerm.size + posCstTerms.size + posVarTerms.size + negElemTerm.size + negCstTerms.size + negVarTerms.size + restTerms.size
      if (elms == 0) {
        return Term.Empty
      } else if (elms == 1) {
        for (any <- posElemTerm) return any
        for (any <- posCstTerms) return any
        for (any <- posVarTerms) return any
        for (any <- negElemTerm) return Term.mkCompl(any)
        for (any <- negCstTerms) return Term.mkCompl(any)
        for (any <- negVarTerms) return Term.mkCompl(any)
        for (any <- restTerms) return any
      }

      Union(posElemTerm, posCstTerms.toSet, posVarTerms.toSet, negElemTerm, negCstTerms.toSet, negVarTerms.toSet, restTerms.toList)
    }

    /**
      * Smart constructor for union (`∪`).
      *
      * More efficient than the other constructors when building a union based on an existing union.
      */
    final def reconstructUnion(posElem: Option[Term.ElemSet], posCsts: Set[Term.Cst], posVars: Set[Term.Var], negElem: Option[Term.ElemSet], negCsts: Set[Term.Cst], negVars: Set[Term.Var], rest: List[Term]): Term = {
      val maintain = Term.Union(posElem, posCsts, posVars, negElem, negCsts, negVars, Nil)
      Term.mkUnionAll(maintain :: rest)
    }

    /**
      * Returns the Xor of `x` and `y`. Implemented by de-sugaring: `(x - y) ∪ (y - x)`.
      */
    final def mkXor(x: Term, y: Term): Term = mkUnion(mkMinus(x, y), mkMinus(y, x))

    /**
      * Returns the Minus of `x` and `y` (`-`). Implemented by de-sugaring: `x ∩ !y`.
      */
    final private def mkMinus(x: Term, y: Term): Term = mkInter(x, mkCompl(y))


    /**
      * The Successive Variable Elimination algorithm.
      *
      * Computes the most-general unifier of the given term `t ~ empty` where `fvs` is the list of free variables in `t`.
      *
      * Eliminates variables one-by-one from the given list `fvs`.
      *
      * Throws a [[BoolUnificationException]] if there is no solution.
      */
    def successiveVariableElimination(t: Term, fvs: List[Int]): SetSubstitution = fvs match {
      case Nil =>
        // [[emptyEquivalent]] now treats the remaining constants as variables and
        // goes through all assignments of those
        if (emptyEquivalent(t))
          SetSubstitution.empty
        else
          throw BoolUnificationException()

      case x :: xs =>
        val t0 = SetSubstitution.singleton(x, Term.Empty)(t)
        val t1 = SetSubstitution.singleton(x, Term.Univ)(t)
        val se = successiveVariableElimination(Term.propagation(Term.mkInter(t0, t1)), xs)

        val f1 = Term.propagation(Term.mkUnion(se(t0), Term.mkMinus(Term.Var(x), se(t1))))
        val st = SetSubstitution.singleton(x, f1)
        st ++ se
    }

    /**
      * Returns `true` if the given term `t` is equivalent to empty.
      * Exponential time in the number of unknowns.
      *
      * OBS: [[Term.Cst]] and [[Term.Var]] must use disjoint integers (see [[Term.emptyEquivalent]]).
      */
    def emptyEquivalent(t: Term): Boolean = t match {
      case Term.Univ => false
      case Term.Cst(_) => false
      case Term.Var(_) => false
      case Term.ElemSet(_) => false
      case Term.Empty => true
      case Term.Union(posElem, posCsts, posVars, negElem, negCsts, negVars, _)
        if posElem.nonEmpty || posCsts.nonEmpty || posVars.nonEmpty || negElem.nonEmpty || negCsts.nonEmpty || negVars.nonEmpty => false
      case _ =>
        /** Try all instantiations of [[Term.Cst]] and [[Term.Var]] and check that those formulas are empty */
        emptyEquivalentExhaustive(t)
    }

    /**
      * Returns `true` if the given term `t` is equivalent to empty.
      * Exponential time in the number of unknowns.
      *
      * OBS: [[Term.Cst]] and [[Term.Var]] must use disjoint integers (see [[Term.emptyEquivalent]]).
      */
    private def emptyEquivalentExhaustive(t: Term): Boolean = {
      /**
        * Checks that all possible instantiations of unknowns result in an empty set.
        * The unknowns not present either set is implicitly instantiated to [[Term.Empty]].
        * @param unknowns the unknowns in `t` that has not yet been instantiated
        * @param univUnknowns the unknowns that are instantiated to [[Term.Univ]]
        */
      def loop(unknowns: List[Int], univUnknowns: SortedSet[Int]): Boolean = unknowns match {
        case Nil =>
          // All unknowns are bound. Compute the truth value.
          evaluate(t, univUnknowns).isEmpty
        case x :: xs =>
          // `t` is empty equivalent if and only if
          // both `t[x -> univ]` and `t[x -> empty]` are empty equivalent.
          loop(xs, univUnknowns + x) && loop(xs, univUnknowns)
      }
      loop(t.freeUnknowns.toList, SortedSet.empty)
    }

    /**
      * Returns `true` if `t1` and `t2` are equivalent formulas.
      */
    def equivalent(t1: Term, t2: Term): Boolean = {
      if (t1 == Term.Empty) emptyEquivalent(t2)
      else if (t2 == Term.Empty) emptyEquivalent(t1)
      else Term.emptyEquivalent(Term.mkXor(t1, t2))
    }

    /**
      * Returns the evaluation of the formula assuming that unknowns in `univUnknowns` are univ
      * and unknowns not in `univUnknowns` are empty.
      *
      * OBS: This function requires that variables and constants use disjoint integers.
      */
    private def evaluate(t: Term, univUnknowns: SortedSet[Int]): SetEval = t match {
      case Term.Univ => SetEval.univ
      case Term.Empty => SetEval.empty
      case Term.Cst(c) => if (univUnknowns.contains(c)) SetEval.univ else SetEval.empty
      case Term.ElemSet(s) => SetEval.set(s)
      case Term.Var(x) => if (univUnknowns.contains(x)) SetEval.univ else SetEval.empty
      case Term.Compl(t) => SetEval.complement(evaluate(t, univUnknowns))

      case Term.Inter(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
        // Evaluate the sub-terms, exiting early in case the running set is empty
        var running = SetEval.univ
        for (t <- posElem.iterator ++ posCsts.iterator ++ posVars.iterator) {
          running = SetEval.intersection(running, evaluate(t, univUnknowns))
          if (running.isEmpty) return running
        }
        for (t <- negElem.iterator ++ negCsts.iterator ++ negVars.iterator) {
          // Note that these are negated implicitly, so evaluate complement.
          running = SetEval.intersection(running, SetEval.complement(evaluate(t, univUnknowns)))
          if (running.isEmpty) return running
        }
        for (t <- rest) {
          running = SetEval.intersection(running, evaluate(t, univUnknowns))
          if (running.isEmpty) return running
        }
        running

      case Term.Union(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
        // Evaluate the sub-terms, exiting early in case the running set is univ
        var running = SetEval.empty
        for (t <- posElem.iterator ++ posCsts.iterator ++ posVars.iterator) {
          running = SetEval.union(running, evaluate(t, univUnknowns))
          if (running.isUniv) return running
        }
        for (t <- negElem.iterator ++ negCsts.iterator ++ negVars.iterator) {
          // Note that these are negated implicitly, so evaluate complement.
          running = SetEval.union(running, SetEval.complement(evaluate(t, univUnknowns)))
          if (running.isUniv) return running
        }
        for (t <- rest) {
          running = SetEval.union(running, evaluate(t, univUnknowns))
          if (running.isUniv) return running
        }
        running
    }

    private def setElem(elem: Option[Term.ElemSet], target: Term): SortedMap[Int, Term] = elem match {
      case Some(e) => setElem(e, target)
      case _ => SortedMap.empty[Int, Term]
    }

    private def setElem(elem: Term.ElemSet, target: Term): SortedMap[Int, Term] = {
      if (elem.s.sizeIs == 1) SortedMap(elem.s.head -> target)
      else SortedMap.empty[Int, Term]
    }

    /**
      * Use the four rewrites
      *
      * * `x ∩ f == x ∩ f[x -> Univ]`
      * * `!x ∩ f == !x ∩ f[x -> Empty]`
      * * `x ∪ f == x ∪ f[x -> Empty]`
      * * `!x ∪ f == !x ∪ f[x -> Univ]`
      *
      * where `x` can be an element, a constant, or a variable.
      */
    def propagation(t: Term): Term = {
      // instantiates elements, returning either Univ, Empty, or Term.ElemSet
      def instElemSet(s0: SortedSet[Int], setElems: SortedMap[Int, Term]): Term = {
        if (setElems.nonEmpty && s0.exists(i => setElems.get(i).contains(Term.Univ))) {
          // if any of the elements are univ, the whole term is univ
          Term.Univ
        } else {
          // remove elements set to empty
          val s = s0.filterNot(i => setElems.get(i).contains(Term.Empty))
          if (s eq s0) t else Term.mkElemSet(s)
        }
      }

      // `setX` assigns elements/constants/variables to univ or empty, where
      // elements/constants/variables not in the map are just themselves.
      def visit(t: Term, setElems: SortedMap[Int, Term], setCsts: SortedMap[Int, Term], setVars: SortedMap[Int, Term]): Term = t match {
        case Term.Univ => t
        case Term.Empty => t
        case Term.Cst(c) => setCsts.getOrElse(c, t) // use mapping, if present
        case Term.Var(x) => setVars.getOrElse(x, t) // use mapping, if present
        case Term.ElemSet(s) => instElemSet(s, setElems)

        case Term.Compl(t0) =>
          val compl0 = visit(t0, setElems, setCsts, setVars)
          // avoid reconstruction if `visit` did nothing
          if (compl0 eq t0) t else Term.mkCompl(compl0)

        case Term.Inter(posElem0, posCsts0, posVars0, negElem0, negCsts0, negVars0, rest0) =>
          // check for trivial cases where elements/constants/variables are empty
          val instPosElem = posElem0.map(e => instElemSet(e.s, setElems))
          val posElem = instPosElem match {
            case Some(e) => e match {
              case Term.Univ => None // redundant
              case Term.Empty => return Term.Empty
              case e@Term.ElemSet(_) => Some(e)
              case _ => ??? // unreachable
            }
            case None => None
          }
          if (setCsts.nonEmpty && posCsts0.exists(c => setCsts.get(c.c).contains(Term.Empty))) return Term.Empty
          if (setVars.nonEmpty && posVars0.exists(x => setVars.get(x.x).contains(Term.Empty))) return Term.Empty
          val instNegElem = negElem0.map(e => instElemSet(e.s, setElems))
          val negElem = instNegElem match {
            case Some(e) => e match {
              case Term.Univ => return Term.Empty
              case Term.Empty => None // redundant
              case e@Term.ElemSet(_) => Some(e)
              case _ => ??? // unreachable
            }
            case None => None
          }
          if (setCsts.nonEmpty && negCsts0.exists(c => setCsts.get(c.c).contains(Term.Univ))) return Term.Empty
          if (setVars.nonEmpty && negVars0.exists(x => setVars.get(x.x).contains(Term.Univ))) return Term.Empty

          // Compute new constant and variable sets by removing constants and variables that are univ (redundant).
          val posCsts = if (setCsts.isEmpty) posCsts0 else posCsts0.filterNot(c => setCsts.get(c.c).contains(Term.Univ))
          val posVars = if (setVars.isEmpty) posVars0 else posVars0.filterNot(x => setVars.get(x.x).contains(Term.Univ))
          val negCsts = if (setCsts.isEmpty) negCsts0 else negCsts0.filterNot(c => setCsts.get(c.c).contains(Term.Empty))
          val negVars = if (setVars.isEmpty) negVars0 else negVars0.filterNot(x => setVars.get(x.x).contains(Term.Empty))

          // Add the new propagated elements/constants/variables
          var currentElems = setElems ++ setElem(posElem0, Term.Univ) ++ setElem(negElem0, Term.Empty)
          var currentCsts = setCsts ++
            posCsts0.iterator.map(_.c -> Term.Univ) ++
            negCsts0.iterator.map(_.c -> Term.Empty)
          var currentVars = setVars ++
            posVars0.iterator.map(_.x -> Term.Univ) ++
            negVars0.iterator.map(_.x -> Term.Empty)

          // Recurse on the sub-terms with the updated maps.
          // If a sub-term becomes a elements/constants/variables after visiting,
          // add it to the propagated atoms.
          val rest = rest0.map { t =>
            val res = visit(t, currentElems, currentCsts, currentVars)
            res match {
              case Term.Univ => res
              case Term.Empty => res
              case Term.Cst(c) =>
                currentCsts += (c -> Term.Univ)
                res
              case Term.Compl(Term.Cst(c)) =>
                currentCsts += (c -> Term.Empty)
                res
              case Term.Var(x) =>
                currentVars += (x -> Term.Univ)
                res
              case Term.Compl(Term.Var(x)) =>
                currentVars += (x -> Term.Empty)
                res
              case e@Term.ElemSet(_) =>
                currentElems ++= setElem(e, Term.Univ)
                res
              case Term.Compl(e@Term.ElemSet(_)) =>
                currentElems ++= setElem(e, Term.Empty)
                res
              case Term.Compl(_) => res
              case Term.Inter(_, _, _, _, _, _, _) => res
              case Term.Union(_, _, _, _, _, _, _) => res
            }
          }

          Term.reconstructInter(posElem, posCsts, posVars, negElem, negCsts, negVars, rest)

        case Term.Union(posElem0, posCsts0, posVars0, negElem0, negCsts0, negVars0, rest0) =>
          // check for trivial cases
          val instPosElem = posElem0.map(e => instElemSet(e.s, setElems))
          val posElem = instPosElem match {
            case Some(e) => e match {
              case Term.Univ => return Term.Univ
              case Term.Empty => None // redundant
              case e@Term.ElemSet(_) => Some(e)
              case _ => ??? // unreachable
            }
            case None => None
          }
          if (setCsts.nonEmpty && posCsts0.exists(c => setCsts.get(c.c).contains(Term.Univ))) return Term.Univ
          if (setVars.nonEmpty && posVars0.exists(x => setVars.get(x.x).contains(Term.Univ))) return Term.Univ
          val instNegElem = negElem0.map(e => instElemSet(e.s, setElems))
          val negElem = instNegElem match {
            case Some(e) => e match {
              case Term.Univ => None // redundant
              case Term.Empty => return Term.Univ
              case e@Term.ElemSet(_) => Some(e)
              case _ => ??? // unreachable
            }
            case None => None
          }
          if (setCsts.nonEmpty && negCsts0.exists(c => setCsts.get(c.c).contains(Term.Empty))) return Term.Univ
          if (setVars.nonEmpty && negVars0.exists(x => setVars.get(x.x).contains(Term.Empty))) return Term.Univ

          // Compute new constant and variable sets by removing constants and variables that hold.
          val posCsts = if (setCsts.isEmpty) posCsts0 else posCsts0.filterNot(c => setCsts.get(c.c).contains(Term.Empty))
          val posVars = if (setVars.isEmpty) posVars0 else posVars0.filterNot(x => setVars.get(x.x).contains(Term.Empty))
          val negCsts = if (setCsts.isEmpty) negCsts0 else negCsts0.filterNot(c => setCsts.get(c.c).contains(Term.Univ))
          val negVars = if (setVars.isEmpty) negVars0 else negVars0.filterNot(x => setVars.get(x.x).contains(Term.Univ))

          var currentElems = setElems ++ setElem(posElem0, Term.Empty) ++ setElem(negElem0, Term.Univ)
          var currentCsts = setCsts ++
            posCsts0.iterator.map(_.c -> Term.Empty) ++
            negCsts0.iterator.map(_.c -> Term.Univ)
          var currentVars = setVars ++
            posVars0.iterator.map(_.x -> Term.Empty) ++
            negVars0.iterator.map(_.x -> Term.Univ)

          // Recurse on the sub-terms with the updated maps.
          val rest = rest0.map { t =>
            val res = visit(t, currentElems, currentCsts, currentVars)
            res match {
              case Term.Univ => res
              case Term.Empty => res
              case Term.Cst(c) =>
                currentCsts += (c -> Term.Empty)
                res
              case Term.Compl(Term.Cst(c)) =>
                currentCsts += (c -> Term.Univ)
                res
              case Term.Var(x) =>
                currentVars += (x -> Term.Empty)
                res
              case Term.Compl(Term.Var(x)) =>
                currentVars += (x -> Term.Univ)
                res
              case e@Term.ElemSet(_) =>
                currentElems ++= setElem(e, Term.Empty)
                res
              case Term.Compl(e@Term.ElemSet(_)) =>
                currentElems ++= setElem(e, Term.Univ)
                res
              case Term.Compl(_) => res
              case Term.Inter(_, _, _, _, _, _, _) => res
              case Term.Union(_, _, _, _, _, _, _) => res
            }
          }

          Term.reconstructUnion(posElem, posCsts, posVars, negElem, negCsts, negVars, rest)
      }

      visit(t, SortedMap.empty, SortedMap.empty, SortedMap.empty)
    }
  }

  /**
    * Companion object of [[SetSubstitution]].
    */
  private object SetSubstitution {
    /**
      * The empty substitution.
      */
    val empty: SetSubstitution = SetSubstitution(Map.empty)

    /**
      * Returns a singleton substitution where the variable `x` is bound to the term `t`.
      */
    def singleton(x: Int, t: Term): SetSubstitution = SetSubstitution(Map(x -> t))
  }

  /**
    * Represents a substitution from set variables (represented as integers) to set terms.
    *
    * A substitution is a partial map from variables to terms. Every substitution induces a total function
    * `s: Term -> Term` that replaces every occurrence in the input term, which occurs in the domain of the
    * substitution, with its corresponding term from the co-domain.
    *
    * We will often write a substitution as `[x -> t1, y -> t2]` and so forth. We write that `x -> t1` is a binding.
    *
    * Note: constants and variables are a separate syntactic category. A substitution will never replace any constants.
    */
  case class SetSubstitution(m: Map[Int, Term]) {

    /**
      * Applies `this` substitution to the given term `t`.
      *
      * We must use the smart constructors from [[Term]] to ensure that the constructed term is normalized.
      */
    def apply(t: Term): Term = {
      if (m.isEmpty) t else t match {
        case Term.Univ => t
        case Term.Empty => t
        case Term.Cst(_) => t
        case Term.ElemSet(_) => t

        case Term.Var(x) => m.get(x) match {
          case None => t // Case 1: The substitution has a binding for `x`. Return the bound term.
          case Some(t0) => t0 // Case 2: The substitution has no binding for `x`. Return the original term.
        }

        case Term.Compl(t0) =>
          val app = apply(t0)
          // reuse objects when apply did no change
          if (app eq t0) t else Term.mkCompl(app)

        case Term.Inter(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
          val ts = mutable.ListBuffer.empty[Term]
          for (x <- posVars) ts += apply(x)
          for (x <- negVars) ts += Term.mkCompl(apply(x))
          for (t <- rest) ts += apply(t)
          Term.reconstructInter(posElem, posCsts, Set.empty, negElem, negCsts, Set.empty, ts.toList)

        case Term.Union(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
          val ts = mutable.ListBuffer.empty[Term]
          for (x <- posVars) ts += apply(x)
          for (x <- negVars) ts += Term.mkCompl(apply(x))
          for (t <- rest) ts += apply(t)
          Term.reconstructUnion(posElem, posCsts, Set.empty, negElem, negCsts, Set.empty, ts.toList)
      }
    }

    /**
      * Applies `this` substitution to the given equation `e`.
      *
      * Intuitively, applies `this` to the lhs and rhs of `e` and reconstructs the equation.
      *
      * Applying the substitution and reconstructing the equation may "flip" the lhs and rhs. For example:
      *
      * If `s = [x -> y]` and `e = univ ~ x and y` then `s(e) = y ~ univ` which has flipped lhs and rhs.
      */
    def apply(e: Equation): Equation = e match {
      case Equation(t1, t2, loc) =>
        val app1 = apply(t1)
        val app2 = apply(t2)
        if ((app1 eq t1) && (app2 eq t2)) e else Equation.mk(app1, app2, loc)
    }

    /**
      * Applies `this` substitution to the given list of equations `l`.
      */
    def apply(l: List[Equation]): List[Equation] = {
      if (m.isEmpty) l else l.map(apply)
    }

    /**
      * Returns the number of bindings in `this` substitution.
      */
    def numberOfBindings: Int = m.size

    /**
      * Returns the size of `this` substitution.
      *
      * The size of a substitution is the sum of the sizes of all terms in its co-domain.
      */
    def size: Int = m.values.map(_.size).sum

    /**
      * Extends `this` substitution with a new binding from the variable `x` to the term `t`.
      *
      * Throws a [[ConflictException]] if `x` is already bound to a term different from `t`.
      */
    def extended(x: Int, t: Term, loc: SourceLocation): SetSubstitution = m.get(x) match {
      case None => SetSubstitution(m + (x -> t))
      case Some(t1) =>
        // Note: If t == t1 we can just return the same substitution.
        if (t == t1)
          this
        else
          throw ConflictException(t, t1, loc)
    }

    /**
      * Merges `this` substitution with `that` substitution.
      *
      * The returned substitution has all bindings from `this` and `that` substitution.
      *
      * The variables in the two substitutions must not overlap.
      */
    def ++(that: SetSubstitution): SetSubstitution = {
      val intersection = this.m.keySet.intersect(that.m.keySet)
      if (intersection.nonEmpty) {
        throw InternalCompilerException(s"Substitutions are not disjoint on: '${intersection.mkString(",")}'.", SourceLocation.Unknown)
      }

      SetSubstitution(this.m ++ that.m)
    }

    /**
      * Composes `this` substitution with `that` substitution.
      *
      * Conceptually `this` is a function: `b -> c` and `that` is a function `a -> b`.
      *
      * We want to compute `a -> c` which we get by computing `x -> this(that(x))`.
      */
    def @@(that: SetSubstitution): SetSubstitution = {
      // Case 1: Return `that` if `this` is empty.
      if (this.m.isEmpty) {
        return that
      }

      // Case 2: Return `this` if `that` is empty.
      if (that.m.isEmpty) {
        return this
      }

      // Case 3: Merge the two substitutions.
      val result = mutable.Map.empty[Int, Term]

      // Add all bindings in `that`. (Applying the current substitution).
      for ((x, t) <- that.m) {
        result.update(x, Term.propagation(this.apply(t)))
      }

      // Add all bindings in `this` that are not in `that`.
      for ((x, t) <- this.m) {
        if (!that.m.contains(x)) {
          result.update(x, t)
        }
      }

      SetSubstitution(result.toMap)
    }

    /**
      * Returns a human-readable representation of `this` substitution.
      */
    override def toString: String = {
      val indent = 4

      val sb = new StringBuilder()
      // We sort the bindings by (size, name).
      for ((x, t) <- m.toList.sortBy(kv => (kv._2.size, kv._1))) {
        sb.append(" ".repeat(indent))
        sb.append(s"x$x")
        sb.append(" -> ")
        sb.append(t)
        sb.append("\n")
      }
      sb.toString()
    }
  }

  /**
    * Returns a human-readable representations of the given list of unification equations `l`.
    */
  private def format(l: List[Equation], indent: Int = 4): String = {
    val sb = new StringBuilder()
    for (Equation(t1, t2, _) <- l) {
      sb.append(" ".repeat(indent))
      sb.append(t1.toString)
      sb.append(" ~ ")
      sb.append(t2.toString)
      sb.append("\n")
    }
    sb.toString()
  }

  /**
    * A common super-type for exceptions throw by the solver.
    */
  sealed trait FastBoolUnificationException extends RuntimeException

  /**
    * Represents a set unification failure between the two terms: `x` and `y`.
    */
  case class ConflictException(x: Term, y: Term, loc: SourceLocation) extends FastBoolUnificationException

  /**
    * Represents a solution that is too complex.
    *
    * @param msg the specific error message.
    */
  case class TooComplexException(msg: String) extends FastBoolUnificationException

}
