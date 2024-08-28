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

import scala.annotation.nowarn
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Set as MutSet}

/**
  * Fast Type Inference with Systems of Set Unification [[Equation]]s of [[Term]]s.
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

  import ca.uwaterloo.flix.language.phase.unification.FastSetUnification.Phases.{DescribedPhase, Phase}
  import ca.uwaterloo.flix.language.phase.unification.FastSetUnification.Rules.Output

  object Solver {

    /**
      * @param sizeThreshold    the upper limit of the amount of connectives in the substitution,
      *                         a non-positive number disables checking
      * @param complexThreshold the upper limit of mappings in the substitution,
      *                         a non-positive number disables checking
      * @param permutationLimit the number of permutations given to SVE,
      *                         a non-positive number uses all permutations
      * @param debugging        prints information to terminal during solving based on the option
      * @param verifySubst      verify that the solution substitution is a solution (VERY SLOW)
      */
    case class RunOptions(
                           sizeThreshold: Int,
                           complexThreshold: Int,
                           permutationLimit: Int,
                           debugging: RunOptions.Debugging,
                           verifySubst: Boolean
                         )

    object RunOptions {
      sealed trait Debugging

      object Debugging {
        case object Nothing extends Debugging

        case object DebugAll extends Debugging

        case object RerunDebugOnCrash extends Debugging
      }

      val default: RunOptions = RunOptions(
        sizeThreshold = 1800,
        complexThreshold = 10,
        permutationLimit = 10,
        debugging = Debugging.Nothing,
        verifySubst = false
      )
    }

    /**
      * Represents the running state of the solver
      *   - eqs: the remaining equations to solve
      *   - subst: the current substitution, which has already been applied to `eqs`
      *   - lastProgressPhase: the name of the last phase that was run and made progress
      *   - currentPhase: the number of the last phase that was run
      */
    private class State(var eqs: List[Equation]) {
      var subst: SetSubstitution = SetSubstitution.empty
      var lastProgressPhase: Option[(String, Int)] = None
      var currentPhase: Int = -1
    }

    /**
      * Attempts to solve the equation system `eqs`.
      *
      *   - Returns `Result.Ok(s)` with a most-general substitution that solves the equations system
      *     , or
      *   - Returns `Result.Err((ex, rest, s))` where `ex` is the error, `rest` is a list of
      *     unsolved equations, and `s` is a partial substitution.
      */
    def solve(eqs: List[Equation], opts: RunOptions = RunOptions.default): Result[SetSubstitution, (FastBoolUnificationException, List[Equation], SetSubstitution)] =
      solveWithInfo(eqs, opts)._1

    /**
      * Attempts to solve the equation system `eqs`, like [[solve]].
      *
      * Additionally returns the name and number of the last phase to make progress, if any.
      */
    def solveWithInfo(l: List[Equation], opts: RunOptions = RunOptions.default): (Result[SetSubstitution, (FastBoolUnificationException, List[Equation], SetSubstitution)], Option[(String, Int)]) = {
      import FastSetUnification.Phases as P
      implicit val implOpts: RunOptions = opts
      val noDebug = opts.copy(debugging = RunOptions.Debugging.Nothing)

      val state = new State(l)

      debugState(state)
      try {
        runPhase(P.propagateConstantsDescr)(state)
        runPhase(P.checkAndSimplifyDescr)(state)(noDebug)
        runPhase(P.propagateVarsDescr)(state)
        runPhase(P.checkAndSimplifyDescr)(state)(noDebug)
        runPhase(P.varAssignmentDescr)(state)
        runPhase(P.checkAndSimplifyDescr)(state)(noDebug)
        runPhase(P.eliminateTrivialAndRedundantDescr)(state)
        runPhase(P.checkAndSimplifyDescr)(state)(noDebug)
        runPhase(P.setUnifyPickSmallestDescr(complexThreshold = opts.complexThreshold, permutationLimit = opts.permutationLimit))(state)

        // SVE can solves everything or throws, so eqs is always empty
        assert(state.eqs.isEmpty)
        if (opts.verifySubst) verifySubst(state.subst, l)
        if (opts.sizeThreshold > 0) verifySubstSize(state.subst)
        (Result.Ok(state.subst), state.lastProgressPhase)
      } catch {
        case _: ConflictException | _: TooComplexException if opts.debugging == RunOptions.Debugging.RerunDebugOnCrash =>
          solveWithInfo(l, opts.copy(debugging = RunOptions.Debugging.DebugAll))
        case ex: ConflictException =>
          (Result.Err((ex, state.eqs, state.subst)), state.lastProgressPhase)
        case ex: TooComplexException =>
          (Result.Err((ex, state.eqs, state.subst)), state.lastProgressPhase)
      }

    }

    /**
      * Runs the given [[Phase]] on the given [[State]], debugging information if required
      * by [[RunOptions]].
      */
    private def runPhase(phaseDescr: DescribedPhase)(state: State)(implicit opts: RunOptions): Unit = {
      if (state.eqs.isEmpty) return
      val DescribedPhase(name, description, phase) = phaseDescr

      state.currentPhase += 1
      debugPhase(state.currentPhase, name, description)

      phase(state.eqs) match {
        case Some((eqs, subst)) =>
          state.lastProgressPhase = Some((name, state.currentPhase))
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
      * Note: This function is very slow since [[Term.equivalent]] is very slow.
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

    /**
      * Checks that [[SetSubstitution.size]] is less than [[RunOptions.sizeThreshold]].
      *
      * @throws TooComplexException if the substitution is too big
      */
    private def verifySubstSize(subst: SetSubstitution)(implicit opts: RunOptions): Unit = {
      val size = subst.size
      if (opts.sizeThreshold > 0 && size > opts.sizeThreshold) {
        throw TooComplexException(
          s"Summed term sizes in substitution ($size) is over the threshold (${opts.sizeThreshold})."
        )
      }
    }

    /** Prints the phase number, name, and description if enabled by [[RunOptions]]. */
    private def debugPhase(number: Int, name: String, description: String)(implicit opts: RunOptions): Unit =
      if (opts.debugging == RunOptions.Debugging.DebugAll) {
        Console.println("-".repeat(80))
        Console.println(s"--- Phase $number: $name")
        Console.println(s"    ($description)")
        Console.println("-".repeat(80))
      }

    /** Prints the state equations and substitution if enabled by [[RunOptions]]. */
    private def debugState(state: State)(implicit opts: RunOptions): Unit =
      if (opts.debugging == RunOptions.Debugging.DebugAll) {
        Console.println(s"Equations (${state.eqs.size}):")
        Console.println(format(state.eqs))
        Console.println(s"Substitution (${state.subst.numberOfBindings}):")
        Console.println(state.subst.toString)
        Console.println("")
      }

  }

  /** A collection of [[Rule]]s that can be used to solve [[Equation]]s. */
  private object Rules {

    /**
      * The output of a rule is a list of new equations to solve, and the
      * substitution that solved the given equation.
      */
    type Output = Option[(List[Equation], SetSubstitution)]

    /** A rule takes an equation and might produce a solution that might give additional equations. */
    type Rule = Equation => Output

    /**
      * Returns `true` if `eq` trivially holds (e.g. `univ ~ univ`).
      *
      * Returns `false` if it is unknown whether `eq` holds or not.
      */
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

    /**
      * Returns [[Some]] if `eq` can never hold (e.g. `empty ~ univ`).
      *
      * Returns [[None]] if it is unknown whether `eq` holds or not.
      */
    def triviallyWrong(eq: Equation): Option[ConflictException] = {
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

    /**
      * Solves equations of ground assignments to variables (e.g. `x ~ c1 ∪ e2`).
      *
      *   - `x ~ t` where [[Term.noFreeVars]] on `t` becomes `([], {x -> t})`
      *   - `t1 ∩ t2 ∩ ... ~ univ` becomes `([t1 ~ univ, t2 ~ univ, ...], {})`
      *   - `t1 ∪ t2 ∪ ... ~ univ` becomes `([t1 ~ empty, t2 ~ empty, ...], {})`
      *   - `!t1 ~ t2` where [[Term.noFreeVars]] on `t2` becomes `([t1 ~ !t2], {})`
      */
    def constantAssignment(eq: Equation): Output = {
      val Equation(t1, t2, loc) = eq
      (t1, t2) match {
        // x ~ t, where t has no variables
        // ---
        // [],
        // {x -> t}
        case (Term.Var(x), t) if t.noFreeVars =>
          Some((Nil, SetSubstitution.singleton(x, t)))

        // t1 ∩ t2 ∩ ... ~ univ
        // ---
        // [t1 ~ univ, t2 ~ univ, ...],
        // {}
        case (Term.Inter(posElem, posCsts, posVars, negElem, negCsts, negVars, rest), Term.Univ) =>
          var ts: List[Term] = rest
          for (e <- posElem) ts ::= e
          for (c <- posCsts) ts ::= c
          for (v <- posVars) ts ::= v
          for (e <- negElem) ts ::= Term.mkCompl(e)
          for (c <- negCsts) ts ::= Term.mkCompl(c)
          for (v <- negVars) ts ::= Term.mkCompl(v)
          Some((ts.map(Equation.mk(_, Term.Univ, loc)), SetSubstitution.empty))

        // t1 ∪ t2 ∪ ... ~ empty
        // ---
        // [t1 ~ empty, t2 ~ empty, ...],
        // {}
        case (Term.Union(posElem, posCsts, posVars, negElem, negCsts, negVars, rest), Term.Empty) =>
          var ts: List[Term] = rest
          for (e <- posElem) ts ::= e
          for (c <- posCsts) ts ::= c
          for (v <- posVars) ts ::= v
          for (e <- negElem) ts ::= Term.mkCompl(e)
          for (c <- negCsts) ts ::= Term.mkCompl(c)
          for (v <- negVars) ts ::= Term.mkCompl(v)
          Some((ts.map(Equation.mk(_, Term.Empty, loc)), SetSubstitution.empty))

        // t1 ~ t2, where t1 and t2 has no variables
        // ---
        // None
        // We have this case such that the two following doesn't loop.
        case (t1, t2) if t1.noFreeVars && t2.noFreeVars =>
          // TODO: We could check if `t1 ~ t2` since there is no variables.
          //       But if that answer is no, we need to give an error, which we
          //       can't with the current signature. Leaving it untouched if it
          //       doesn't hold would mean that we could compute Term.equivalent
          //       again and again.
          None

        // !t1 ~ t2, where t2 has no variables
        // ---
        // [t1 ~ !t2],
        // {}
        case (Term.Compl(xt1), t) if t.noFreeVars =>
          Some((List(Equation.mk(xt1, Term.mkCompl(t), loc)), SetSubstitution.empty))

        // t1 ~ !t2, where t1 has no variables
        // ---
        // [!t1 ~ t2],
        // {}
        case (t, Term.Compl(xt2)) if t.noFreeVars =>
          Some((List(Equation.mk(Term.mkCompl(t), xt2, loc)), SetSubstitution.empty))

        case _ => None
      }
    }

    /**
      * Solves equations variable alias equations (e.g. `x1 ~ x2`).
      *
      *   - `x1 ~ x2` becomes `([], {x1 -> x2})`
      */
    def variableAlias(eq: Equation): Option[SetSubstitution] = {
      val Equation(t1, t2, _) = eq
      (t1, t2) match {
        // x1 ~ x2
        // ---
        // [],
        // {x1 -> x2}
        case (Term.Var(x), y@Term.Var(_)) =>
          Some(SetSubstitution.singleton(x, y))

        case _ =>
          None
      }
    }

    /**
      * Solves non-recursive variable assignments (e.g. `x1 ~ x2 ∪ c4`).
      *
      *   - `x ~ t` where [[Term.freeVarsContains]] on `t` is false in regards to `x`.
      *     This becomes `([], {x -> t})`
      */
    def variableAssignment(eq: Equation): Option[SetSubstitution] = {
      val Equation(t1, t2, _) = eq
      (t1, t2) match {
        // x ~ t, where x is not in t
        // ---
        // [],
        // {x -> t}
        case (v@Term.Var(x), rhs) if !rhs.freeVarsContains(v) =>
          Some(SetSubstitution.singleton(x, rhs))

        case _ =>
          None
      }
    }

    /**
      * Solves all solvable equations using successive-variable-elimination,
      * i.e. exhaustive instantiation.
      *
      * Returns a [[ConflictException]] if the equation cannot be solved.
      */
    def setUnifyOne(e: Equation): Result[SetSubstitution, ConflictException] = try {
      val query = Term.emptyTest(e.t1, e.t2)
      val fvs = query.freeVars.toList
      Result.Ok(Term.successiveVariableElimination(query, fvs))
    } catch {
      case _: BoolUnificationException => Result.Err(ConflictException(e.t1, e.t2, e.loc))
    }
  }

  private object Phases {

    type Phase = List[Equation] => Output

    case class DescribedPhase(name: String, description: String, phase: Phase)

    def filteringPhase(f: List[Equation] => Option[List[Equation]]): Phase = {
      eqs0 => f(eqs0).map(eqs1 => (eqs1, SetSubstitution.empty))
    }

    def completePhase(f: List[Equation] => SetSubstitution): Phase = {
      eqs0 => Some((Nil, f(eqs0)))
    }

    /**
      * Returns a list of non-trivial unification equations computed from `eqs`.
      *
      * Throws a [[ConflictException]] if an unsolvable equation is encountered.
      *
      * A trivial equation is one of:
      *   - `univ ~ univ`
      *   - `empty ~ empty`
      *   - `e ~ e`      (same element)
      *   - `c ~ c`      (same constant)
      *   - `x ~ x`      (same variable)
      */
    def checkAndSimplify(eqs: List[Equation]): Option[List[Equation]] = {
      var changed = false
      // unwrap to check triviallyWrong no matter if trivial did anything
      val res0 = runFilterNotRule(Rules.triviallyHolds)(eqs) match {
        case Some(eqs) =>
          changed = true
          eqs
        case None =>
          eqs
      }
      runErrRule(Rules.triviallyWrong)(res0)
      if (changed) Some(res0) else None
    }

    def checkAndSimplifyDescr: DescribedPhase = DescribedPhase(
      "Check and Simplify",
      "trivially correct and incorrect equations",
      filteringPhase(checkAndSimplify)
    )

    def propagateConstants(l: List[Equation]): Output = {
      runRule(Rules.constantAssignment, simpleSubst = true)(l)
    }

    def propagateConstantsDescr: DescribedPhase = DescribedPhase(
      "Constant Propagation",
      "solves equations: `x ~ f` where f is a formula without variables",
      propagateConstants
    )

    def propagateVars(l: List[Equation]): Output = {
      runSubstRule(Rules.variableAlias)(l)
    }

    def propagateVarsDescr: DescribedPhase = DescribedPhase(
      "Variable Propagation",
      "solves equations: `x ~ y`",
      propagateVars
    )


    def varAssignment(l: List[Equation]): Output = {
      runSubstRule(Rules.variableAssignment)(l)
    }

    def varAssignmentDescr: DescribedPhase = DescribedPhase(
      "Variable Assignment",
      "solves equations: `x ~ t` where `t` does not contain `x`",
      varAssignment
    )

    def eliminateTrivialAndRedundant(eqs: List[Equation]): Option[List[Equation]] = {
      var result = List.empty[Equation]
      val seen = mutable.Set.empty[Equation]
      var changed = false

      // We rely on equations and terms having correct equals and hashCode functions.
      // Note: We are purely working with *syntactic equality*, not *semantic equality*.
      for (eq <- eqs) {
        if (eq.t1 == eq.t2 || seen.contains(eq)) {
          // dont add to result
          changed = true
        } else {
          // add to result, thus not making a change
          seen += eq
          result = eq :: result
        }
      }

      if (changed) Some(result.reverse) else None
    }

    def eliminateTrivialAndRedundantDescr: DescribedPhase = DescribedPhase(
      "Eliminate Trivial and Redundant Equations",
      "eliminates equations of the form X = X and duplicated equations",
      filteringPhase(eliminateTrivialAndRedundant)
    )

    /**
      * Returns a most-general unifier for `eqs`, trying multiple permutations
      * to minimize substitution size.
      *
      * @param complexThreshold throws [[TooComplexException]] if `eqs` is longer,
      *                         a non-positive number omits the check.
      * @param permutationLimit a limit on the number of permutations to try,
      *                         a non-positive number will try all permutations.
      */
    def setUnifyAllPickSmallest(complexThreshold: Int, permutationLimit: Int)(eqs: List[Equation]): SetSubstitution = {
      if (eqs.length <= 1) {
        return setUnifyAll(eqs)
      }

      if (complexThreshold > 0 && eqs.length > complexThreshold) {
        throw TooComplexException(
          s"Amount of complex equations in substitution (${eqs.length}) is over the threshold ($complexThreshold)."
        )
      }

      // We solve the first [[PermutationLimit]] permutations and pick the one that gives rise to the smallest substitution.
      val permutations = if (permutationLimit > 0) eqs.permutations.take(permutationLimit) else eqs.permutations
      val results = permutations.toList.map {
        // TODO: stop early for sufficiently small substitutions
        p => (p, setUnifyAll(p))
      }.sortBy {
        case (_, s) => s.size
      }

      // Pick the smallest substitution.
      results.head._2
    }

    def setUnifyPickSmallestDescr(complexThreshold: Int, permutationLimit: Int): DescribedPhase = DescribedPhase(
      "Set Unification",
      "resolves all remaining equations using SVE, trying multiple permutations to minimize the solution",
      completePhase(setUnifyAllPickSmallest(complexThreshold, permutationLimit))
    )

    def setUnifyAll(eqs: List[Equation]): SetSubstitution = {
      runSubstResRule(Rules.setUnifyOne)(eqs)
    }

    def setUnifyAllDescr: DescribedPhase = DescribedPhase(
      "Set Unification",
      "resolves all remaining equations using SVE.",
      completePhase(setUnifyAll)
    )

    // Converting Rule into Phase

    /**
      * @param rule         a solving rule that optionally produces constraints, cs, and a substitution subst.
      * @param selfFeeding  can the rule potentially solve any constrains in cs?
      * @param substInduced can the rule potentially discover new solvable constraints via any subst it returns?
      * @param l            the constraints to run the rule on
      * @return the remaining constraints and the found substitution.
      */
    private def runRule(rule: Equation => Output, selfFeeding: Boolean = true, substInduced: Boolean = true, simpleSubst: Boolean = false)(l: List[Equation]): Output = {
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
                subst = if (simpleSubst) s ++ subst else s @@ subst
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

    /** If the rule returns `true` that means that the constraint is solved by the empty substitution */
    private def runFilterNotRule(rule: Equation => Boolean)(l: List[Equation]): Option[List[Equation]] = {
      val rule1 = (eq0: Equation) => if (rule(eq0)) Some((Nil, SetSubstitution.empty)) else None
      runRule(rule1, selfFeeding = false, substInduced = false, simpleSubst = true)(l).map(_._1)
    }

    /** If the rule returns Some(s), then the equation is solved with s */
    private def runSubstRule(rule: Equation => Option[SetSubstitution])(l: List[Equation]): Output = {
      val rule1 = (eq0: Equation) => rule(eq0).map(subst => (Nil, subst))
      runRule(rule1, selfFeeding = false)(l)
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
      val res = runRule(rule1, selfFeeding = false, substInduced = false, simpleSubst = true)(l)
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
    *
    * the `@nowarn` annotation is required for Scala 3 compatiblity, since the derived `copy` method is private
    * in Scala 3 due to the private constructor. In Scala 2 the `copy` method is still public.
    * However, we do not use the `copy` method anywhere for [[Equation]], so this is fine.
    */
  @nowarn
  case class Equation private(t1: Term, t2: Term, loc: SourceLocation) {
    /** Returns the size of this equation which is the sum of its lhs and rhs. */
    final def size: Int = t1.size + t2.size

    /** Returns a human-readable representation of `this` unification equation. */
    override final def toString: String = s"$t1 ~ $t2"
  }

  /**
    * A common super-type for set terms.
    *
    * OBS: [[Term.Cst]] and [[Term.Var]] must use disjoint integers (see [[Term.freeUnknowns]] and [[Term.propagation]]).
    */
  sealed trait Term {

    def vars: SortedSet[Int]

    /**
      * Syntactic sugar for [[Term.mkInter]].
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
    final def freeVars: SortedSet[Int] = this.vars

    /**
      * Returns `true` if [[freeVars]] contains `v`.
      */
    final def freeVarsContains(v: Term.Var): Boolean = this.vars.contains(v.x)

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

    /** Returns true if [[vars]] is empty. */
    final def noFreeVars: Boolean = this.vars.isEmpty

    /** Returns the number of connectives in `this` term. */
    final def size: Int = this match {
      case Term.Univ => 0
      case Term.Empty => 0
      case Term.Cst(_) => 0
      case Term.Var(_) => 0
      case Term.ElemSet(_) => 0
      case Term.Compl(t) => t.size + 1
      case Term.Inter(_, _, _, _, _, _, _) => Term.sizes(List(this))
      case Term.Union(_, _, _, _, _, _, _) => Term.sizes(List(this))
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

    /** The full universe set (`univ`). */
    case object Univ extends Term {
      override val vars: SortedSet[Int] = SortedSet.empty
    }

    /** The empty set (`empty`). */
    case object Empty extends Term {
      override val vars: SortedSet[Int] = SortedSet.empty
    }

    /**
      * Represents an uninterpreted constant, i.e. a rigid variable (`c42`).
      *
      * OBS: [[Term.Cst]] and [[Term.Var]] must use disjoint integers.
      */
    case class Cst(c: Int) extends Term {
      override val vars: SortedSet[Int] = SortedSet.empty
    }

    /**
      * Represents a set variable, i.e. a flexible variable (`x42`).
      *
      * OBS: [[Term.Cst]] and [[Term.Var]] must use disjoint integers.
      */
    case class Var(x: Int) extends Term {
      override val vars: SortedSet[Int] = SortedSet(x)
    }

    /**
      * Represents a concrete set of elements, i.e. a union of elements (`e42.43` or `e42 ∪ e43`).
      *
      * The set `s` is never empty.
      *
      * The `@nowarn` annotation is required for Scala 3 compatibility, since the derived `copy`
      * method is private in Scala 3 due to the private constructor. In Scala 2 the `copy` method is
      * still public. However, we do not use the `copy` method anywhere for [[ElemSet]], so this is
      * fine.
      */
    @nowarn
    case class ElemSet private(s: SortedSet[Int]) extends Term {
      assert(s.nonEmpty)

      override val vars: SortedSet[Int] = SortedSet.empty
    }

    /**
      * Represents the complement of the term `t` (`!`).
      *
      * The `@nowarn` annotation is required for Scala 3 compatibility, since the derived `copy`
      * method is private in Scala 3 due to the private constructor. In Scala 2 the `copy` method is
      * still public. However, we do not use the `copy` method anywhere for [[Compl]], so this is
      * fine.
      */
    @nowarn
    case class Compl private(t: Term) extends Term {
      override val vars: SortedSet[Int] = t.vars
    }

    /**
      * Represents a intersection of terms (`∩`).
      *
      * The intersection
      * {{{
      * posElem = Some({e1, e2}),
      * posCsts =  Set( c1, c2),
      * posVars =  Set( x1, x2),
      * negElem = Some({e3, e4}),
      * negCsts =  Set( c3, c4),
      * negVars =  Set( x3, x4),
      * rest    = List( e1 ∪ x9)
      * }}}
      * represents the formula
      * {{{ (e1 ∪ e2) ∩ c1 ∩ c2 ∩ x1 ∩ x2 ∩ !(e3 ∪ e4) ∩ !c3 ∩ !c4 ∩ !x3 ∩ !x4 ∩ (e1 ∪ x9) }}}
      *
      * Invariants and Properties
      *   - An empty intersection is [[Term.Univ]].
      *   - `posElem` and `negElem` are disjoint
      *   - [[vars]] is precomputed on construction
      *
      * The `@nowarn` annotation is required for Scala 3 compatibility, since the derived `copy`
      * method is private in Scala 3 due to the private constructor. In Scala 2 the `copy` method is
      * still public. However, we do not use the `copy` method anywhere for [[Inter]], so this is
      * fine.
      */
    @nowarn
    case class Inter private(posElem: Option[Term.ElemSet], posCsts: Set[Term.Cst], posVars: Set[Term.Var], negElem: Option[Term.ElemSet], negCsts: Set[Term.Cst], negVars: Set[Term.Var], rest: List[Term]) extends Term {
      assert(!posVars.exists(negVars.contains), this.toString)
      assert(!posCsts.exists(negCsts.contains), this.toString)

      /** Returns `true` if any elements or constants exist in the outer intersection. */
      def triviallyNonUniv: Boolean = posElem.isDefined || posCsts.nonEmpty || negElem.isDefined || negCsts.nonEmpty

      /** The set of variables in `this`. */
      override val vars: SortedSet[Int] = SortedSet.from(posVars.map(_.x)) ++ negVars.map(_.x) ++ rest.flatMap(_.vars)
    }

    /**
      * Represents a union of terms (`∪`).
      *
      * The union
      * {{{
      * posElem = Some({e1, e2}),
      * posCsts =  Set( c1, c2),
      * posVars =  Set( x1, x2),
      * negElem = Some({e3, e4}),
      * negCsts =  Set( c3, c4),
      * negVars =  Set( x3, x4),
      * rest    = List( e1 ∩ x9)
      * }}}
      * represents the formula
      * {{{ (e1 ∪ e2) ∪ c1 ∪ c2 ∪ x1 ∪ x2 ∪ !(e3 ∪ e4) ∪ !c3 ∪ !c4 ∪ !x3 ∪ !x4 ∪ (e1 ∩ x9) }}}
      *
      * Invariants and Properties
      *   - An empty union is [[Term.Empty]].
      *   - `posElem` and `negElem` are disjoint
      *   - [[vars]] is precomputed on construction
      *
      * The `@nowarn` annotation is required for Scala 3 compatibility, since the derived `copy`
      * method is private in Scala 3 due to the private constructor. In Scala 2 the `copy` method is
      * still public. However, we do not use the `copy` method anywhere for [[Union]], so this is
      * fine.
      */
    @nowarn
    case class Union private(posElem: Option[Term.ElemSet], posCsts: Set[Term.Cst], posVars: Set[Term.Var], negElem: Option[Term.ElemSet], negCsts: Set[Term.Cst], negVars: Set[Term.Var], rest: List[Term]) extends Term {
      assert(!posVars.exists(negVars.contains), this.toString)
      assert(!posCsts.exists(negCsts.contains), this.toString)

      /** Returns `true` if any elements or constants exist in the outer union. */
      def triviallyNonEmpty: Boolean = posElem.isDefined || posCsts.nonEmpty || negElem.isDefined || negCsts.nonEmpty

      /** The set of variables in `this`. */
      override val vars: SortedSet[Int] = SortedSet.from(posVars.map(_.x)) ++ negVars.map(_.x) ++ rest.flatMap(_.vars)
    }

    /** Returns a singleton [[Term.ElemSet]]. */
    final def mkElemSet(i: Int): Term.ElemSet = {
      Term.ElemSet(SortedSet(i))
    }

    /**
      * Returns [[Term.ElemSet]] if `i` is nonEmpty.
      *
      * Returns [[Term.Empty]] if `i` is empty.
      */
    final def mkElemSet(i: SortedSet[Int]): Term =
      if (i.isEmpty) Term.Empty else Term.ElemSet(i)

    /**
      * Returns [[Term.ElemSet]] if `i` is nonEmpty.
      *
      * Returns `None` if `i` is empty.
      */
    final def mkElemSetOpt(i: SortedSet[Int]): Option[ElemSet] =
      if (i.isEmpty) None else Some(Term.ElemSet(i))

    /** Returns the complement of `t` (`!t`). */
    final def mkCompl(t: Term): Term = t match {
      case Univ => Empty
      case Empty => Univ
      case Cst(_) => Compl(t)
      case Var(_) => Compl(t)
      case ElemSet(_) => Compl(t)
      case Compl(t0) => t0
      case Inter(posElem, posCsts, posVars, negElem, negCsts, negVars, Nil) =>
        Term.Union(negElem, negCsts, negVars, posElem, posCsts, posVars, Nil)
      case Inter(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
        reconstructUnion(negElem, negCsts, negVars, posElem, posCsts, posVars, rest.map(mkCompl))
      case Union(posElem, posCsts, posVars, negElem, negCsts, negVars, Nil) =>
        Term.Inter(negElem, negCsts, negVars, posElem, posCsts, posVars, Nil)
      case Union(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
        reconstructInter(negElem, negCsts, negVars, posElem, posCsts, posVars, rest.map(mkCompl))
    }

    /** Returns the intersection of `t1` and `t2` (`t1 ∩ t2`). */
    final def mkInter(t1: Term, t2: Term): Term = (t1, t2) match {
      case (Empty, _) => Empty
      case (_, Empty) => Empty
      case (Univ, _) => t2
      case (_, Univ) => t1
      case _ => mkInterAll(List(t1, t2))
    }

    /** Returns the union of `t1` and `t2` (`t1 ∪ t2`). */
    final def mkUnion(t1: Term, t2: Term): Term = (t1, t2) match {
      case (Univ, _) => Univ
      case (_, Univ) => Univ
      case (Empty, _) => t2
      case (_, Empty) => t1
      case _ => mkUnionAll(List(t1, t2))
    }

    /** Returns the intersection of `ts` (`ts1 ∩ ts2 ∩ ..`). */
    final def mkInterAll(ts: List[Term]): Term = {
      // A) separate terms into specific buckets
      // B) flatten one level of intersections

      @inline
      def interOpt(s1Opt: Option[SortedSet[Int]], s2: SortedSet[Int]): SortedSet[Int] = s1Opt match {
        case Some(s1) => s1.intersect(s2)
        case None => s2
      }

      @inline
      def interOptOpt(s1Opt: Option[SortedSet[Int]], s2Opt: Option[SortedSet[Int]]): Option[SortedSet[Int]] = (s1Opt, s2Opt) match {
        case (Some(s1), Some(s2)) => Some(s1.intersect(s2))
        case (s@Some(_), None) => s
        case (None, s@Some(_)) => s
        case (None, None) => None
      }

      var posElem0: Option[SortedSet[Int]] = None // `None` represents `univ`
      val posCsts = MutSet.empty[Term.Cst]
      val posVars = MutSet.empty[Term.Var]
      var negElem0 = SortedSet.empty[Int]
      val negCsts = MutSet.empty[Term.Cst]
      val negVars = MutSet.empty[Term.Var]
      val rest = ListBuffer.empty[Term]

      var workList = ts
      while (workList.nonEmpty) {
        val t :: next = workList
        workList = next
        t match {
          case Univ =>
            ()
          case Empty =>
            return Empty
          case c@Term.Cst(_) =>
            if (negCsts.contains(c)) return Empty
            posCsts += c
          case Term.Compl(c@Term.Cst(_)) =>
            if (posCsts.contains(c)) return Empty
            negCsts += c
          case Term.ElemSet(s) =>
            val inter = interOpt(posElem0, s)
            if (inter.isEmpty) return Empty
            posElem0 = Some(inter)
          case Term.Compl(Term.ElemSet(s)) =>
            negElem0 = negElem0.union(s)
          case x@Term.Var(_) =>
            if (negVars.contains(x)) return Empty
            posVars += x
          case Term.Compl(x@Term.Var(_)) =>
            if (posVars.contains(x)) return Empty
            negVars += x
          case Inter(posElem1, posCsts1, posVars1, negElem1, negCsts1, negVars1, rest1) =>
            posElem0 = interOptOpt(posElem1.map(_.s), posElem0)
            if (posElem0.contains(SortedSet.empty[Int])) return Empty
            for (x <- posCsts1) {
              if (negCsts.contains(x)) return Empty
              posCsts += x
            }
            for (x <- posVars1) {
              if (negVars.contains(x)) return Empty
              posVars += x
            }
            for (e <- negElem1) {
              negElem0 = negElem0.union(e.s)
            }
            for (x <- negCsts1) {
              if (posCsts.contains(x)) return Empty
              negCsts += x
            }
            for (x <- negVars1) {
              if (posVars.contains(x)) return Empty
              negVars += x
            }
            workList = rest1 ++ workList
          case union@Term.Union(_, _, _, _, _, _, _) =>
            rest += union
          case compl@Term.Compl(_) =>
            rest += compl
        }
      }
      // Check overlaps in the element sets (could be done inline)
      val posElem = posElem0 match {
        case Some(s) =>
          // s ∩ !neg ∩ .. = (s - neg) ∩ !neg ∩ ..
          val posElemSet = s.diff(negElem0)
          if (posElemSet.isEmpty) return Empty
          else Some(Term.ElemSet(posElemSet))
        case None => None
      }
      val negElem = Term.mkElemSetOpt(negElem0)

      // Eliminate intersections with zero or one subterm
      val elms = posElem.size + posCsts.size + posVars.size + negElem.size + negCsts.size + negVars.size + rest.size
      if (elms == 0) {
        return Term.Univ
      } else if (elms == 1) {
        for (any <- posElem) return any
        for (any <- posCsts) return any
        for (any <- posVars) return any
        for (any <- negElem) return Term.mkCompl(any)
        for (any <- negCsts) return Term.mkCompl(any)
        for (any <- negVars) return Term.mkCompl(any)
        for (any <- rest) return any
      }
      Inter(posElem, posCsts.toSet, posVars.toSet, negElem, negCsts.toSet, negVars.toSet, rest.toList)
    }

    /**
      * Reconstructor for intersection (`∩`).
      *
      * More efficient than the other constructors when building an intersection
      * based on an existing intersection/union.
      *
      * OBS: must be called with collections from existing intersections/unions.
      */
    final def reconstructInter(posElem: Option[Term.ElemSet], posCsts: Set[Term.Cst], posVars: Set[Term.Var], negElem: Option[Term.ElemSet], negCsts: Set[Term.Cst], negVars: Set[Term.Var], rest: List[Term]): Term = {
      val maintain = Term.Inter(posElem, posCsts, posVars, negElem, negCsts, negVars, Nil)
      Term.mkInterAll(maintain :: rest)
    }

    /** Returns the union of `ts` (`ts1 ∪ ts2 ∪ ..`). */
    final def mkUnionAll(ts: List[Term]): Term = {
      // A) separate terms into specific buckets
      // B) flatten one level of intersections

      @inline
      def unionNegOpt(s1Opt: Option[SortedSet[Int]], s2: SortedSet[Int]): SortedSet[Int] = s1Opt match {
        case Some(s1) => s1.intersect(s2)
        case None => s2
      }

      @inline
      def unionNegOptOpt(s1Opt: Option[SortedSet[Int]], s2Opt: Option[SortedSet[Int]]): Option[SortedSet[Int]] = (s1Opt, s2Opt) match {
        case (Some(s1), Some(s2)) => Some(s1.intersect(s2))
        case (s@Some(_), None) => s
        case (None, s@Some(_)) => s
        case (None, None) => None
      }

      var posElem0 = SortedSet.empty[Int]
      val poscsts = MutSet.empty[Term.Cst]
      val posVars = MutSet.empty[Term.Var]
      var negElem0: Option[SortedSet[Int]] = None // `None` represents `empty`
      val negCsts = MutSet.empty[Term.Cst]
      val negVars = MutSet.empty[Term.Var]
      val rest = ListBuffer.empty[Term]

      var workList = ts
      while (workList.nonEmpty) {
        val t :: next = workList
        workList = next
        t match {
          case Empty =>
            ()
          case Univ =>
            return Univ
          case x@Term.Cst(_) =>
            if (negCsts.contains(x)) return Univ
            poscsts += x
          case Term.Compl(x@Term.Cst(_)) =>
            if (poscsts.contains(x)) return Univ
            negCsts += x
          case Term.ElemSet(s) =>
            posElem0 = posElem0.union(s)
          case Term.Compl(Term.ElemSet(s)) =>
            val union = unionNegOpt(negElem0, s)
            if (union.isEmpty) return Univ
            negElem0 = Some(union)
          case x@Term.Var(_) =>
            if (negVars.contains(x)) return Univ
            posVars += x
          case Term.Compl(x@Term.Var(_)) =>
            if (posVars.contains(x)) return Univ
            negVars += x
          case Union(posElem1, posCsts1, posVars1, negElem1, negCsts1, negVars1, rest1) =>
            for (x <- posElem1) {
              posElem0 = posElem0.union(x.s)
            }
            for (x <- posCsts1) {
              if (negCsts.contains(x)) return Univ
              poscsts += x
            }
            for (x <- posVars1) {
              if (negVars.contains(x)) return Univ
              posVars += x
            }
            negElem0 = unionNegOptOpt(negElem1.map(_.s), negElem0)
            if (negElem0.contains(SortedSet.empty[Int])) return Univ
            for (x <- negCsts1) {
              if (poscsts.contains(x)) return Univ
              negCsts += x
            }
            for (x <- negVars1) {
              if (posVars.contains(x)) return Univ
              negVars += x
            }
            workList = rest1 ++ workList
          case inter@Term.Inter(_, _, _, _, _, _, _) =>
            rest += inter
          case compl@Term.Compl(_) =>
            rest += compl
        }
      }
      // Check overlaps in the element sets (could be done inline)
      val (posElem, negElem) = negElem0 match {
        case Some(s) =>
          // pos ∪ !s ∪ .. = !(!pos ∩ s) ∪ .. = !(s - pos) ∪ ..
          val negElemSet = s.diff(posElem0)
          if (negElemSet.isEmpty) return Term.Univ
          else (None, Term.mkElemSetOpt(negElemSet))
        case None =>
          (Term.mkElemSetOpt(posElem0), None)
      }

      // Eliminate unions with zero or one subterm
      val elms = posElem.size + poscsts.size + posVars.size + negElem.size + negCsts.size + negVars.size + rest.size
      if (elms == 0) {
        return Term.Empty
      } else if (elms == 1) {
        for (any <- posElem) return any
        for (any <- poscsts) return any
        for (any <- posVars) return any
        for (any <- negElem) return Term.mkCompl(any)
        for (any <- negCsts) return Term.mkCompl(any)
        for (any <- negVars) return Term.mkCompl(any)
        for (any <- rest) return any
      }
      Union(posElem, poscsts.toSet, posVars.toSet, negElem, negCsts.toSet, negVars.toSet, rest.toList)
    }

    /**
      * Reconstructor for union (`∪`).
      *
      * More efficient than the other constructors when building an union
      * based on an existing intersection/union.
      *
      * OBS: must be called with collections from existing intersections/unions.
      */
    final def reconstructUnion(posElem: Option[Term.ElemSet], posCsts: Set[Term.Cst], posVars: Set[Term.Var], negElem: Option[Term.ElemSet], negCsts: Set[Term.Cst], negVars: Set[Term.Var], rest: List[Term]): Term = {
      val maintain = Term.Union(posElem, posCsts, posVars, negElem, negCsts, negVars, Nil)
      Term.mkUnionAll(maintain :: rest)
    }

    /** Returns the Xor of `x` and `y` with the formula `(x - y) ∪ (y - x)`. */
    final def mkXor(x: Term, y: Term): Term = mkUnion(mkMinus(x, y), mkMinus(y, x))

    /** Returns the Minus of `x` and `y` (`-`) with the formula `x ∩ !y`. */
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

        val st = SetSubstitution.singleton(x, Term.mkUnion(se(t0), Term.mkMinus(Term.Var(x), se(t1))))
        st ++ se
    }

    /**
      * Returns `true` if the given term `t` is equivalent to empty.
      * Exponential time in the number of unknowns.
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

    /** Returs a Term that is empty-equivalent if and only if `t1` and `t2` are equivalent. */
    def emptyTest(t1: Term, t2: Term): Term = {
      if (t1 == Term.Empty) t2
      if (t2 == Term.Empty) t1
      else Term.mkXor(t1, t2)
    }

    /**
      * Returns `true` if the given term `t` is equivalent to empty.
      * Exponential time in the number of unknowns.
      */
    private def emptyEquivalentExhaustive(t: Term): Boolean = {
      /**
        * Checks that all possible instantiations of unknowns result in an empty set.
        * The unknowns not present either set is implicitly instantiated to [[Term.Empty]].
        *
        * @param unknowns     the unknowns in `t` that has not yet been instantiated
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
    def equivalent(t1: Term, t2: Term): Boolean =
      Term.emptyEquivalent(Term.emptyTest(t1, t2))

    /**
      * Returns the [[SetEval]] evaluation of `t`, interpreting unknowns in `univUnknowns` as [[Term.Univ]]
      * and unknowns not in `univUnknowns` as [[Term.Empty]].
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

    /** Returns a point-wise substitution for `{e1 ∪ e2 ∪ .. -> target}`` if possible, otherwise an empty map. */
    private def setElemOne(elem: Option[Term.ElemSet], target: Term): SortedMap[Int, Term] = elem match {
      case Some(e) => setElemOne(e, target)
      case _ => SortedMap.empty[Int, Term]
    }

    /**
      * Returns a point-wise substitution for `{e1 ∪ e2 ∪ .. -> target}`` if possible, otherwise an empty map.
      *
      *   - `{e1 -> target}` becomes `Map(e1 -> target)`
      *   - `{e1 ∪ e2 ∪ .. -> empty` becomes `Map(e1 -> empty, e2 -> empty, ..)`
      *   - otherwise `empty`
      */
    private def setElemOne(elem: Term.ElemSet, target: Term): SortedMap[Int, Term] = {
      if (elem.s.sizeIs == 1) SortedMap(elem.s.head -> target)
      else if (target == Term.Empty) setElemPointwise(elem, target)
      else SortedMap.empty[Int, Term]
    }

    /** Returns a map with `e -> target` for each `e` in `elem`. */
    private def setElemPointwise(elem: Option[Term.ElemSet], target: Term): SortedMap[Int, Term] = elem match {
      case Some(e) => setElemPointwise(e, target)
      case _ => SortedMap.empty[Int, Term]
    }

    /** Returns a map with `e -> target` for each `e` in `elem`. */
    private def setElemPointwise(elem: Term.ElemSet, target: Term): SortedMap[Int, Term] = {
      elem.s.foldLeft(SortedMap.empty[Int, Term]) { case (acc, i) => acc + (i -> target) }
    }

    /**
      * Use the four rewrites
      *
      *   - `x ∩ f == x ∩ f[x -> Univ]`
      *   - `!x ∩ f == !x ∩ f[x -> Empty]`
      *   - `x ∪ f == x ∪ f[x -> Empty]`
      *   - `!x ∪ f == !x ∪ f[x -> Univ]`
      *
      * where `x` can be an element, a constant, or a variable.
      *
      * OBS: [[Term.Cst]] and [[Term.Var]] must use disjoint integers.
      *
      */
    def propagation(t: Term): Term = {
      /**
        * Instantiates unions of elements, returning either [[Term.Univ]], [[Term.Empty]], or
        * [[Term.ElemSet]].
        */
      def instElemSet(e: Term.ElemSet, setElems: SortedMap[Int, Term]): Term = {
        if (setElems.isEmpty) e else {
          if (e.s.exists(i => setElems.get(i).contains(Term.Univ))) {
            // instElemSet(e1 ∪ e2 ∪ e3, {e2 -> univ, ..}) = e1 ∪ univ ∪ e3 = univ
            Term.Univ
          } else {
            // instElemSet(e1 ∪ e2 ∪ e3, {e2 -> empty}) = e1 ∪ empty ∪ e3 = e1 ∪ e3
            val s1 = e.s.filterNot(i => setElems.get(i).contains(Term.Empty))
            if (s1 eq e.s) e else Term.mkElemSet(s1)
          }
        }
      }

      // `setX` assigns elements/constants/variables to univ or empty, where
      // elements/constants/variables not in the map are just themselves.
      def visit(t: Term, setElems: SortedMap[Int, Term], setUnknowns: SortedMap[Int, Term]): Term = t match {
        case Term.Univ => t
        case Term.Empty => t
        case Term.Cst(c) => setUnknowns.getOrElse(c, t) // use mapping, if present
        case Term.Var(x) => setUnknowns.getOrElse(x, t) // use mapping, if present
        case Term.ElemSet(_) if setElems.isEmpty => t
        case e@Term.ElemSet(_) => instElemSet(e, setElems)

        case Term.Compl(t0) =>
          val compl0 = visit(t0, setElems, setUnknowns)
          // avoid reconstruction if `visit` did nothing
          if (compl0 eq t0) t else Term.mkCompl(compl0)

        case Term.Inter(posElem0, posCsts0, posVars0, negElem0, negCsts0, negVars0, rest0) =>
          // check for trivial cases where elements/constants/variables are empty
          val instPosElem = posElem0.map(instElemSet(_, setElems))
          val posElem = instPosElem match {
            case Some(e) => e match {
              case Term.Univ => None // redundant
              case Term.Empty => return Term.Empty
              case e@Term.ElemSet(_) => Some(e)
              case _ => ??? // unreachable
            }
            case None => None
          }
          if (setUnknowns.nonEmpty && posCsts0.exists(c => setUnknowns.get(c.c).contains(Term.Empty))) return Term.Empty
          if (setUnknowns.nonEmpty && posVars0.exists(x => setUnknowns.get(x.x).contains(Term.Empty))) return Term.Empty
          val instNegElem = negElem0.map(instElemSet(_, setElems))
          val negElem = instNegElem match {
            case Some(e) => e match {
              case Term.Univ => return Term.Empty
              case Term.Empty => None // redundant
              case e@Term.ElemSet(_) => Some(e)
              case _ => ??? // unreachable
            }
            case None => None
          }
          if (setUnknowns.nonEmpty && negCsts0.exists(c => setUnknowns.get(c.c).contains(Term.Univ))) return Term.Empty
          if (setUnknowns.nonEmpty && negVars0.exists(x => setUnknowns.get(x.x).contains(Term.Univ))) return Term.Empty

          // Compute new constant and variable sets by removing constants and variables that are univ (redundant).
          val posCsts = if (setUnknowns.isEmpty) posCsts0 else posCsts0.filterNot(c => setUnknowns.get(c.c).contains(Term.Univ))
          val posVars = if (setUnknowns.isEmpty) posVars0 else posVars0.filterNot(x => setUnknowns.get(x.x).contains(Term.Univ))
          val negCsts = if (setUnknowns.isEmpty) negCsts0 else negCsts0.filterNot(c => setUnknowns.get(c.c).contains(Term.Empty))
          val negVars = if (setUnknowns.isEmpty) negVars0 else negVars0.filterNot(x => setUnknowns.get(x.x).contains(Term.Empty))

          // Add the new propagated elements/constants/variables
          var currentElems = setElems ++ setElemOne(posElem, Term.Univ) ++ setElemOne(negElem, Term.Empty)
          var currentUnknowns = setUnknowns ++
            posCsts.iterator.map(_.c -> Term.Univ) ++
            negCsts.iterator.map(_.c -> Term.Empty) ++
            posVars.iterator.map(_.x -> Term.Univ) ++
            negVars.iterator.map(_.x -> Term.Empty)

          // Recurse on the sub-terms with the updated maps.
          // If a sub-term becomes a elements/constants/variables after visiting,
          // add it to the propagated atoms.
          val rest = rest0.map { t =>
            val res = visit(t, currentElems, currentUnknowns)
            res match {
              case Term.Univ => res
              case Term.Empty => res
              case Term.Cst(c) =>
                currentUnknowns += (c -> Term.Univ)
                res
              case Term.Compl(Term.Cst(c)) =>
                currentUnknowns += (c -> Term.Empty)
                res
              case Term.Var(x) =>
                currentUnknowns += (x -> Term.Univ)
                res
              case Term.Compl(Term.Var(x)) =>
                currentUnknowns += (x -> Term.Empty)
                res
              case e@Term.ElemSet(_) =>
                currentElems ++= setElemOne(e, Term.Univ)
                res
              case Term.Compl(e@Term.ElemSet(_)) =>
                currentElems ++= setElemOne(e, Term.Empty)
                res
              case Term.Compl(_) => res
              case Term.Inter(_, _, _, _, _, _, _) => res
              case Term.Union(_, _, _, _, _, _, _) => res
            }
          }

          Term.reconstructInter(posElem, posCsts, posVars, negElem, negCsts, negVars, rest)

        case Term.Union(posElem0, posCsts0, posVars0, negElem0, negCsts0, negVars0, rest0) =>
          // check for trivial cases
          val instPosElem = posElem0.map(instElemSet(_, setElems))
          val posElem = instPosElem match {
            case Some(e) => e match {
              case Term.Univ => return Term.Univ
              case Term.Empty => None // redundant
              case e@Term.ElemSet(_) => Some(e)
              case _ => ??? // unreachable
            }
            case None => None
          }
          if (setUnknowns.nonEmpty && posCsts0.exists(c => setUnknowns.get(c.c).contains(Term.Univ))) return Term.Univ
          if (setUnknowns.nonEmpty && posVars0.exists(x => setUnknowns.get(x.x).contains(Term.Univ))) return Term.Univ
          val instNegElem = negElem0.map(instElemSet(_, setElems))
          val negElem = instNegElem match {
            case Some(e) => e match {
              case Term.Univ => None // redundant
              case Term.Empty => return Term.Univ
              case e@Term.ElemSet(_) => Some(e)
              case _ => ??? // unreachable
            }
            case None => None
          }
          if (setUnknowns.nonEmpty && negCsts0.exists(c => setUnknowns.get(c.c).contains(Term.Empty))) return Term.Univ
          if (setUnknowns.nonEmpty && negVars0.exists(x => setUnknowns.get(x.x).contains(Term.Empty))) return Term.Univ

          // Compute new constant and variable sets by removing constants and variables that hold.
          val posCsts = if (setUnknowns.isEmpty) posCsts0 else posCsts0.filterNot(c => setUnknowns.get(c.c).contains(Term.Empty))
          val posVars = if (setUnknowns.isEmpty) posVars0 else posVars0.filterNot(x => setUnknowns.get(x.x).contains(Term.Empty))
          val negCsts = if (setUnknowns.isEmpty) negCsts0 else negCsts0.filterNot(c => setUnknowns.get(c.c).contains(Term.Univ))
          val negVars = if (setUnknowns.isEmpty) negVars0 else negVars0.filterNot(x => setUnknowns.get(x.x).contains(Term.Univ))

          var currentElems = setElems ++ setElemOne(posElem, Term.Empty) ++ setElemOne(negElem, Term.Univ)
          var currentUnknowns = setUnknowns ++
            posCsts.iterator.map(_.c -> Term.Empty) ++
            negCsts.iterator.map(_.c -> Term.Univ) ++
            posVars.iterator.map(_.x -> Term.Empty) ++
            negVars.iterator.map(_.x -> Term.Univ)

          // Recurse on the sub-terms with the updated maps.
          val rest = rest0.map { t =>
            val res = visit(t, currentElems, currentUnknowns)
            res match {
              case Term.Univ => res
              case Term.Empty => res
              case Term.Cst(c) =>
                currentUnknowns += (c -> Term.Empty)
                res
              case Term.Compl(Term.Cst(c)) =>
                currentUnknowns += (c -> Term.Univ)
                res
              case Term.Var(x) =>
                currentUnknowns += (x -> Term.Empty)
                res
              case Term.Compl(Term.Var(x)) =>
                currentUnknowns += (x -> Term.Univ)
                res
              case e@Term.ElemSet(_) =>
                currentElems ++= setElemOne(e, Term.Empty)
                res
              case Term.Compl(e@Term.ElemSet(_)) =>
                currentElems ++= setElemOne(e, Term.Univ)
                res
              case Term.Compl(_) => res
              case Term.Inter(_, _, _, _, _, _, _) => res
              case Term.Union(_, _, _, _, _, _, _) => res
            }
          }

          Term.reconstructUnion(posElem, posCsts, posVars, negElem, negCsts, negVars, rest)
      }

      visit(t, SortedMap.empty, SortedMap.empty)
    }

    /** Returns the number of connectives in the terms `ts`. */
    def sizes(ts: List[Term]): Int = {
      var workList = ts
      var counter = 0

      @inline
      def countTerms(posElem: Option[Term.ElemSet], posCsts: Set[Term.Cst], posVars: Set[Term.Var], negElem: Option[Term.ElemSet], negCsts: Set[Term.Cst], negVars: Set[Term.Var], rest: List[Term]): Unit = {
        val negElemSize = negElem.size
        val negCstsSize = negCsts.size
        val negVarsSize = negVars.size
        val terms = posElem.size + posCsts.size + posVars.size + negElemSize + negCstsSize + negVarsSize + rest.size
        val connectives = negElemSize + negCstsSize + negVarsSize
        counter += (terms - 1) + connectives
        workList = rest ++ workList
      }

      while (workList.nonEmpty) {
        val t :: next = workList
        workList = next
        t match {
          case Term.Univ => ()
          case Term.Empty => ()
          case Term.Cst(_) => ()
          case Term.Var(_) => ()
          case Term.ElemSet(_) => ()
          case Term.Compl(t) =>
            counter += 1
            workList = t :: workList
          case Term.Inter(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
            countTerms(posElem, posCsts, posVars, negElem, negCsts, negVars, rest)
          case Term.Union(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
            countTerms(posElem, posCsts, posVars, negElem, negCsts, negVars, rest)
        }
      }
      counter
    }
  }

  private object SetSubstitution {
    val empty: SetSubstitution = SetSubstitution(Map.empty)

    /** Returns the singleton substitution `[x -> t]`. */
    def singleton(x: Int, t: Term): SetSubstitution = SetSubstitution(Map(x -> Term.propagation(t)))
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

    /** Applies `this` substitution to the given term `t`. */
    def apply(t: Term): Term = {
      if (m.isEmpty || t.vars.isEmpty || !t.vars.exists(m.contains)) t else Term.propagation(applyInternal(t))
    }

    /**
      * Applies `this` substitution to the term `t0` without early exit checks.
      *
      * Maintain and exploit reference equality for performance.
      */
    private def applyInternal(t0: Term): Term = t0 match {
      case Term.Univ => t0
      case Term.Empty => t0
      case Term.Cst(_) => t0
      case Term.ElemSet(_) => t0

      case Term.Var(x) => m.get(x) match {
        case None => t0
        case Some(t1) => t1
      }

      case Term.Compl(t1) =>
        val t1Applied = applyInternal(t1)
        if (t1Applied eq t1) t0 else Term.mkCompl(t1Applied)

      case Term.Inter(_, _, posVars, _, _, negVars, Nil)
        if posVars.forall(v => !this.m.contains(v.x)) && negVars.forall(v => !this.m.contains(v.x)) =>
        t0

      case Term.Inter(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
        val ts = ListBuffer.empty[Term]
        for (x <- posVars) {
          val x1 = applyInternal(x)
          if (x1 == Term.Empty) return x1
          ts += x1
        }
        for (x <- negVars) {
          val x1 = applyInternal(x)
          if (x1 == Term.Univ) return Term.Empty
          ts += Term.mkCompl(x1)
        }
        for (t <- rest) {
          val t1 = applyInternal(t)
          if (t1 == Term.Empty) return t1
          ts += t1
        }
        Term.reconstructInter(posElem, posCsts, Set.empty, negElem, negCsts, Set.empty, ts.toList)

      case Term.Union(_, _, posVars, _, _, negVars, Nil)
        if posVars.forall(v => !this.m.contains(v.x)) && negVars.forall(v => !this.m.contains(v.x)) =>
        t0

      case Term.Union(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
        val ts = ListBuffer.empty[Term]
        for (x <- posVars) {
          val x1 = applyInternal(x)
          if (x1 == Term.Univ) return x1
          ts += x1
        }
        for (x <- negVars) {
          val x1 = applyInternal(x)
          if (x1 == Term.Empty) return Term.Univ
          ts += Term.mkCompl(x1)
        }
        for (t <- rest) {
          val t1 = applyInternal(t)
          if (t1 == Term.Univ) return t1
          ts += t1
        }
        Term.reconstructUnion(posElem, posCsts, Set.empty, negElem, negCsts, Set.empty, ts.toList)
    }

    /**
      * Applies `this` substitution to the equation `eq`.
      *
      * The equation may be "flipped", for example:
      * {{{
      *   {x -> y}.apply(univ ~ x ∩ y) = y ~ univ
      * }}}
      */
    def apply(eq: Equation): Equation = eq match {
      case Equation(t1, t2, loc) =>
        val app1 = apply(t1)
        val app2 = apply(t2)
        if ((app1 eq t1) && (app2 eq t2)) eq else Equation.mk(app1, app2, loc)
    }

    /** Applies `this` substitution to the list of equations `l`. */
    def apply(l: List[Equation]): List[Equation] =
      if (m.isEmpty) l else l.map(apply)

    /** Returns the number of bindings in `this` substitution. */
    def numberOfBindings: Int = m.size

    /** Returns the sum of the sizes of the terms in this substitution. */
    def size: Int = Term.sizes(m.values.toList)

    /**
      * Extends `this` substitution with a new binding from the variable `x` to the term `t`.
      *
      * Throws a [[ConflictException]] if `x` is already bound to a term syntactically different from `t`.
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
      *
      * It is faster to call `smaller ++ larger` than to call `larger ++ smaller`.
      */
    def ++(that: SetSubstitution): SetSubstitution = {
      if (this.m.isEmpty) that
      else if (that.m.isEmpty) this
      else {
        val notDisjoint = this.m.keySet.exists(that.m.keySet.contains)
        if (notDisjoint) {
          val intersection = this.m.keySet.intersect(that.m.keySet)
          throw InternalCompilerException(s"Substitutions are not disjoint on: '${intersection.mkString(",")}'.", SourceLocation.Unknown)
        }

        SetSubstitution(that.m ++ this.m)
      }
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
        val t1 = this.apply(t)
        result.update(x, if (t1 eq t) t else Term.propagation(t1))
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

  /** A substitution is too large or the equation system is too complex. */
  case class TooComplexException(msg: String) extends FastBoolUnificationException

}
