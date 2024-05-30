/*
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

import scala.annotation.tailrec
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.collection.mutable

///
/// Fast Type Inference with Systems of Boolean Unification Equations
///
/// Work smarter, not harder -- Proverb
///
/// A Fast Boolean Unification Solver based on the following ideas:
///
/// - We work on all the equations as one whole system.
/// - We assume the equation system has been put into normal form. This avoids the need to mirror a lot of cases.
///   - We move univ and empty to the rhs.
///   - We move a single variable to the lhs.
///   - See the definition of `Equation.mk` for details.
/// - We progress in the following order:
///   1. We propagate ground terms (univ/empty/constant) in a fixpoint.
///   2. We propagate variables (i.e. resolving constraints of the form x = y).
///   3. We perform trivial assignments where the left-hand variables does not occur in the RHS.
///   4. We eliminate (now) trivial and redundant constraints.
///   5. We do full-blown Boolean unification with SVE.
/// - We represent a intersection with n >= 2 terms.
///   - We group the terms into three: a set of constants, a set of variables, and a list of sub-terms.
///   - We flatten intersections at least one level per call to `mkInter`.
///
/// Written under open-world assumption, i.e. the universe is infinite.
///
/// In the future, we could:
/// - Explore change of basis.
/// - Explore use slack variables (i.e. variables marked don't care, and use SAT).
/// - Explore in detail how constraints are generated.
/// - Explore whether to run Phase 1-3 after _ONE_ SVE computation.
///
object FastSetUnification {

  /**
    * The threshold for when a solution is considered too complex.
    *
    * The threshold is specified as an upper bound on the permitted connectives in a substitution.
    *
    * The threshold must be at least 600 for the test suite to pass.
    *
    * If a solution is too complex an [[TooComplexException]] exception is thrown.
    */
  private val SizeThreshold: Int = 600

  /**
    * The threshold for how many complex equation we will try to solve.
    *
    * If there are more than the threshold number of non-trivial equations an [[TooComplexException]] exception is thrown.
    */
  private val ComplexThreshold: Int = 10

  /**
    * The maximum number of permutations to try while solving a system of unification equations using SVE.
    *
    * Recall that for a list of length
    * 0, 1, 2, 3,  4,   5,   6,    7,      8
    * there are
    * 1, 1, 2, 6, 24, 120, 720, 5040, 40,320
    * permutations.
    *
    * If the PermutationLimit is 10 and there are 3 equations we will cover 6/6.
    *
    * If the PermutationLimit is 10 and there are 5 equations we will cover 10/120.
    */
  private val PermutationLimit: Int = 10

  /**
    * Enable debugging (prints information during Boolean unification).
    */
  private val Debugging: Boolean = false

  /**
    * Enable verification (i.e. check that the computed most-general unifier is a solution to the original equation system.)
    *
    * Note: Verification is _very expensive_ and may not terminate in reasonable time.
    */
  private val Verify: Boolean = false

  /**
    * Internal formatter. Used for debugging.
    */
  private val formatter: Formatter = Formatter.NoFormatter

  /**
    * Attempts to solve all the given unification equations `l`.
    *
    * Returns `Ok(s)` where `s` is a most-general unifier for all equations.
    *
    * Returns `Err(c, l, s)` where `c` is a conflict, `l` is a list of unsolved equations, and `s` is a partial substitution.
    */
  def solveAll(l: List[Equation]): Result[BoolSubstitution, (FastBoolUnificationException, List[Equation], BoolSubstitution)] = {
    val solver = new Solver(l)
    solver.solve()
  }

  /**
    * A stateful phased solver for Boolean unification equations. The solver maintains two fields that change over time:
    *
    * - The current (pending) unification equations to solve.
    * - The current (partial) substitution which represents the solution.
    *
    * @param l The input list of Boolean unification equations to solve.
    */
  private class Solver(l: List[Equation]) {

    /**
      * The current (pending) equations to solve.
      *
      * The list of pending equations decrease as the solver progresses.
      *
      * Note that the pending equations is not a strict subset of the original equations because they may be simplified during computation.
      *
      * If Boolean unification is successful, the list of pending equations will become empty at the end of the computation.
      */
    private var currentEqns: List[Equation] = l

    /**
      * The current substitution. Initially empty, but grows during computation.
      */
    private var currentSubst: BoolSubstitution = BoolSubstitution.empty

    /**
      * Attempts to solve the equation system that this solver was instantiated with.
      *
      * Returns `Result.Ok(s)` with a most-general substitution which solves the equations system.
      *
      * Returns `Result.Err((ex, l, s))` where `c` is a conflict, `l` is a list of unsolved equations, and `s` is a partial substitution.
      */
    def solve(): Result[BoolSubstitution, (FastBoolUnificationException, List[Equation], BoolSubstitution)] = {
      try {
        phase0Init()
        phase1ConstantPropagation()
        phase2VarPropagation()
        phase3VarAssignment()
        phase4TrivialAndRedundant()
        phase5SVE()
        verifySolution()
        verifySolutionSize()

        Result.Ok(currentSubst)
      } catch {
        case ex: ConflictException => Result.Err((ex, currentEqns, currentSubst))
        case ex: TooComplexException => Result.Err((ex, currentEqns, currentSubst))
      }
    }

    private def phase0Init(): Unit = {
      debugln("-".repeat(80))
      debugln("--- Phase 0: Input")
      debugln("-".repeat(80))
      printEquations()
      debugln()
    }

    private def phase1ConstantPropagation(): Unit = {
      debugln("-".repeat(80))
      debugln("--- Phase 1: Constant Propagation")
      debugln("    (resolves all equations of the form: x = c where x is a var and c is const)")
      debugln("-".repeat(80))
      val s = propagateConstants(currentEqns)
      updateState(s)
      printEquations()
      printSubstitution()
      debugln()
    }

    private def phase2VarPropagation(): Unit = {
      debugln("-".repeat(80))
      debugln("--- Phase 2: Variable Propagation")
      debugln("    (resolves all equations of the form: x = y where x and y are vars)")
      debugln("-".repeat(80))
      val s = propagateVars(currentEqns)
      updateState(s)
      printEquations()
      printSubstitution()
      debugln()
    }

    private def phase3VarAssignment(): Unit = {
      debugln("-".repeat(80))
      debugln("--- Phase 3: Variable Assignment")
      debugln("    (resolves all equations of the form: x = t where x is free in t)")
      debugln("-".repeat(80))
      val s = varAssignment(currentEqns)
      updateState(s)
      printEquations()
      printSubstitution()
      debugln()
    }

    private def phase4TrivialAndRedundant(): Unit = {
      debugln("-".repeat(80))
      debugln("--- Phase 4: Eliminate Trivial and Redundant Equations")
      debugln("    (eliminates equations of the form X = X and duplicated equations)")
      debugln("-".repeat(80))
      val s = eliminateTrivialAndRedundant(currentEqns)
      updateState(s)
      printEquations()
      printSubstitution()
      debugln()
    }

    private def phase5SVE(): Unit = {
      debugln("-".repeat(80))
      debugln("--- Phase 5: Boolean Unification")
      debugln("    (resolves all remaining equations using SVE.)")
      debugln("-".repeat(80))
      val newSubst = boolUnifyAllPickSmallest(currentEqns)
      updateState(Nil, newSubst) // Note: We pass Nil because SVE will have solved all equations.
      printSubstitution()
      debugln()
    }

    /**
      * Updates the internal state with the given list of pending equations and partial substitution `l`.
      *
      * Simplifies the pending equations. Throws [[ConflictException]] if an unsolvable equation is detected.
      */
    private def updateState(l: (List[Equation], BoolSubstitution)): Unit = {
      val (nextEqns, nextSubst) = l
      currentEqns = checkAndSimplify(nextEqns)
      currentSubst = nextSubst @@ currentSubst
    }

    /**
      * Verifies that the current substitution is a valid solution to the original equations `l`.
      *
      * Note: Does not make sense to call before the equation system has been fully solved.
      *
      * Throws a [[ConflictException]] if an equation is not solved by the current substitution.
      */
    private def verifySolution(): Unit = {
      if (Verify) { // We only verify if the flag is set.
        verify(currentSubst, l)
      }
    }

    /**
      * Checks that current substitution has not grown too large according to the defined threshold.
      */
    private def verifySolutionSize(): Unit = {
      val size = currentSubst.size
      if (size > SizeThreshold) {
        println(currentSubst)
        throw TooComplexException(s"Too large a substitution (threshold: $SizeThreshold, found: $size)")
      }
    }

    private def printEquations(): Unit = {
      debugln(s"Equations (${currentEqns.size}):")
      debugln(format(currentEqns))
    }

    private def printSubstitution(): Unit = {
      debugln(s"Substitution (${currentSubst.numberOfBindings}):")
      debugln(currentSubst.toString)
    }

    private def debugln(): Unit = debugln("")

    // Note: By-name to ensure that we do not compute expensive strings.
    private def debugln(s: => String): Unit = {
      if (Debugging) {
        Console.println(s)
      }
    }

  }

  /**
    * Returns a list of non-trivial unification equations computed from the given list `l`.
    *
    * Throws a [[ConflictException]] if an unsolvable equation is encountered.
    *
    * A trivial equation is one of:
    * -     univ ~ univ
    * -    empty ~ empty
    * -        c ~ c        (same constant)
    * -        x ~ x        (same variable)
    *
    *
    * An unsolvable (conflicted) equation is one of:
    *
    * -      univ ~ empty   (and mirrored)
    * -       c_i ~ c_j     (different constants)
    * -        c ~ univ     (and mirrored)
    * -        c ~ empty    (and mirrored)
    */
  private def checkAndSimplify(l: List[Equation]): List[Equation] = l match {
    case Nil => Nil
    case Equation(t1, t2, loc) :: es => (t1, t2) match {
      // Trivial equations: skip them.
      case (Term.Univ, Term.Univ) => checkAndSimplify(es)
      case (Term.Empty, Term.Empty) => checkAndSimplify(es)
      case (Term.Cst(c1), Term.Cst(c2)) if c1 == c2 => checkAndSimplify(es)
      case (Term.Var(x1), Term.Var(x2)) if x1 == x2 => checkAndSimplify(es)
      case (Term.Elem(i1), Term.Elem(i2)) if i1 == i2 => checkAndSimplify(es)

      // Unsolvable (conflicted) equations: raise an exception.
      case (Term.Univ, Term.Empty) => throw ConflictException(t1, t2, loc)
      case (Term.Univ, Term.Elem(_)) => throw ConflictException(t1, t2, loc)
      case (Term.Empty, Term.Univ) => throw ConflictException(t1, t2, loc)
      case (Term.Empty, Term.Elem(_)) => throw ConflictException(t1, t2, loc)
      case (Term.Cst(c1), Term.Cst(c2)) if c1 != c2 => throw ConflictException(t1, t2, loc)
      // Note: A constraint with two different variables is of course solvable!
      case (Term.Cst(_), Term.Univ) => throw ConflictException(t1, t2, loc)
      case (Term.Cst(_), Term.Elem(_)) => throw ConflictException(t1, t2, loc)
      case (Term.Cst(_), Term.Empty) => throw ConflictException(t1, t2, loc)
      case (Term.Elem(_), Term.Univ) => throw ConflictException(t1, t2, loc)
      case (Term.Elem(_), Term.Empty) => throw ConflictException(t1, t2, loc)
      case (Term.Elem(_), Term.Cst(_)) => throw ConflictException(t1, t2, loc)
      case (Term.Elem(i1), Term.Elem(i2)) if i1 != i2 => throw ConflictException(t1, t2, loc)
      case (Term.Univ, Term.Cst(_)) => throw ConflictException(t1, t2, loc)
      case (Term.Empty, Term.Cst(_)) => throw ConflictException(t1, t2, loc)

      case (Term.Inter(posElem, _, _, negElems, _, _, _), Term.Univ) if posElem.isDefined || negElems.nonEmpty => throw ConflictException(t1, t2, loc)
      case (Term.Inter(Some(Term.Elem(i1)), _, _, _, _, _, _), Term.Elem(i2)) if i1 != i2 => throw ConflictException(t1, t2, loc)
      case (Term.Inter(_, _, _, negElems, _, _, _), Term.Elem(i)) if negElems.exists(_.i == i) => throw ConflictException(t1, t2, loc)
      case (Term.Inter(posElem, _, _, negElems, _, _, _), Term.Cst(_)) if posElem.isDefined || negElems.nonEmpty => throw ConflictException(t1, t2, loc)

      case (Term.Union(posElems, _, _, negElems, _, _, _), Term.Empty) if posElems.nonEmpty || negElems.nonEmpty => throw ConflictException(t1, t2, loc)
      case (Term.Union(posElems, _, _, negElems, _, _, _), Term.Cst(_)) if posElems.nonEmpty || negElems.nonEmpty => throw ConflictException(t1, t2, loc)

      // Non-trivial and non-conflicted equation: keep it.
      case _ => Equation(t1, t2, loc) :: checkAndSimplify(es)
    }
  }

  /**
    * Propagates constants and truth values through the equation system.
    *
    * The implementation saturates the system, i.e. it computes a fixpoint.
    *
    * The implementation uses three rewrite rules:
    *
    * - `x ~ univ` becomes `[x -> univ]`.
    * - `x ~ c` becomes `[x -> c]`.
    * - `x ∩ y ∩ ... = univ` becomes `[x -> univ, y -> univ, ...]`.
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
    * Note: We do not propagate empty. This extension can be added, if needed.
    *
    * Note: We use `subst.extended` to check for conflicts. For example, if we already know that `s = [x -> c17]` and we
    * learn that `x -> univ` then we will try to extend s with the new binding which will raise a [[ConflictException]].
    */
  private def propagateConstants(l: List[Equation]): (List[Equation], BoolSubstitution) = {
    var pending = l
    var subst = BoolSubstitution.empty

    // We iterate until no changes are detected.
    var changed = true
    while (changed) {
      changed = false

      var rest: List[Equation] = Nil
      // OBS: subst.extended checks for conflicting mappings
      for (e <- pending) {
        e match {
          // Case 1: x ~ univ
          case Equation(Term.Var(x), Term.Univ, loc) =>
            subst = subst.extended(x, Term.Univ, loc)
            changed = true

          // Case 2: x ~ c
          case Equation(Term.Var(x), Term.Cst(c), loc) =>
            subst = subst.extended(x, Term.Cst(c), loc)
            changed = true

          // Case 3: x ~ e
          case Equation(Term.Var(x), Term.Elem(i), loc) =>
            subst = subst.extended(x, Term.Elem(i), loc)
            changed = true

          // Case 4: x ∩ y ∩ !z ∩... ~ univ
          case Equation(Term.Inter(None, posCsts, posVars, negElems, negCsts, negVars, rest), Term.Univ, loc) if
            posCsts.isEmpty && negElems.isEmpty && negCsts.isEmpty && rest.isEmpty => {
              for (Term.Var(x) <- posVars) {
                subst = subst.extended(x, Term.Univ, loc)
                changed = true
              }
              for (Term.Var(x) <- negVars) {
                subst = subst.extended(x, Term.Empty, loc)
                changed = true
              }
            }

          // Case 5: x ∪ y ∪ !z ∪... ~ empty
          case Equation(Term.Union(posElems, posCsts, posVars, negElems, negCsts, negVars, rest), Term.Empty, loc) if
            posElems.isEmpty && posCsts.isEmpty && negElems.isEmpty && negCsts.isEmpty && rest.isEmpty => {
              for (Term.Var(x) <- posVars) {
                subst = subst.extended(x, Term.Empty, loc)
                changed = true
              }
              for (Term.Var(x) <- negVars) {
                subst = subst.extended(x, Term.Univ, loc)
                changed = true
              }
            }

          case _ =>
            rest = e :: rest
        }
      }
      // INVARIANT: We apply the current substitution to all remaining equations.
      pending = subst(rest)
    }

    // Reverse the unsolved equations to ensure they are returned in the original order.
    (pending.reverse, subst)
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
  private def propagateVars(l: List[Equation]): (List[Equation], BoolSubstitution) = {
    var pending = l
    var subst = BoolSubstitution.empty

    var unsolved: List[Equation] = Nil

    // INVARIANT: The current substitution has been applied to BOTH unsolved AND pending equations.
    while (pending.nonEmpty) {
      // Extract the next equation and update the pending equations.
      val e = pending.head
      pending = pending.tail

      e match {
        case Equation(Term.Var(x), y@Term.Var(_), _) =>
          // Case 1: We have found an equation: `x ~ y`.
          // Construct a singleton substitution `[x -> y]`.
          val singleton = BoolSubstitution.singleton(x, y)

          // Apply the singleton substitution to the pending equations.
          pending = singleton(pending)

          // Apply the singleton substitution to the unsolved equations.
          unsolved = singleton(unsolved)

          // Update the current substitution with the new binding.
          subst = singleton @@ subst

        case _ =>
          // Case 2: We have some other type of equation. Add it to the list of unsolved equations.
          // The invariant ensures that the current substitution has already been applied to the equation.
          unsolved = e :: unsolved
      }
    }

    // Reverse the unsolved equations to ensure they are returned in the original order.
    (unsolved.reverse, subst)
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
  private def varAssignment(l: List[Equation]): (List[Equation], BoolSubstitution) = {
    var pending = l
    var subst = BoolSubstitution.empty

    var unsolved: List[Equation] = Nil

    // INVARIANT: The current substitution has been applied to BOTH the unsolved AND pending equations.
    while (pending.nonEmpty) {
      // Extract the next equation and update the pending equations.
      val e = pending.head
      pending = pending.tail

      e match {
        case Equation(v@Term.Var(x), rhs, _) if !rhs.freeVarsContains(v) =>
          // Case 1: We have found an equation: `x ~ t` where `x` is not free in `t`.
          // Construct a singleton substitution `[x -> y]`.
          val singleton = BoolSubstitution.singleton(x, rhs)

          // Apply the singleton substitution to the unsolved equations.
          pending = singleton(pending)

          // Apply the singleton substitution to the unsolved equations.
          unsolved = singleton(unsolved)

          // Compose the new substitution with the current substitution.
          subst = singleton @@ subst

        case _ => unsolved = e :: unsolved
      }
    }

    // Reverse the unsolved equations to ensure they are returned in the original order.
    (unsolved.reverse, subst)
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
  private def eliminateTrivialAndRedundant(l: List[Equation]): (List[Equation], BoolSubstitution) = {
    var result = List.empty[Equation]
    val seen = mutable.Set.empty[Equation]

    // We rely on equations and terms having correct equals and hashCode functions.
    // Note: We are purely working with *syntactic equality*, not *semantic equality*.
    for (eqn <- l) {
      if (!seen.contains(eqn)) {
        // The equation has not been seen before.
        if (eqn.t1 != eqn.t2) {
          // The LHS and RHS are different.
          seen += eqn
          result = eqn :: result
        }
      }
    }

    // We reverse the list of equations to preserve the initial order.
    (result.reverse, BoolSubstitution.empty)
  }

  /**
    * Given a unification equation system `l` computes a most-general unifier for all its equations.
    *
    * If multiple equations are involved then we try to solve them in different order to find a small substitution.
    */
  private def boolUnifyAllPickSmallest(l: List[Equation]): BoolSubstitution = {
    // Case 1: We have at most one equation to solve: just solve immediately.
    if (l.length <= 1) {
      return boolUnifyAll(l)
    }

    // Case 2: Check that there are not too many complex equations.
    if (l.length > ComplexThreshold) {
      throw TooComplexException(s"Too many complex equations (threshold: $ComplexThreshold, found: ${l.length})")
    }

    // Case 3: We solve the first [[PermutationLimit]] permutations and pick the one that gives rise to the smallest substitution.
    val results = l.permutations.take(PermutationLimit).toList.map {
      case p => (p, boolUnifyAll(p))
    }.sortBy {
      case (_, s) => s.size
    }

    //    println("Permutations:")
    //    for ((p, s) <- results) {
    //      println(s"  $p -- ${s.size}")
    //    }

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
  private def boolUnifyAll(l: List[Equation]): BoolSubstitution = {
    var subst = BoolSubstitution.empty
    var pending = l
    while (pending.nonEmpty) {
      val e = pending.head
      pending = pending.tail

      // Compute the most-general unifier for the current equation `e`.
      val newSubst = boolUnifyOne(e)

      // Apply the computed substitution to all pending equations.
      pending = newSubst(pending)

      // Compose the new substitution with the current substitution.
      subst = newSubst @@ subst
    }

    subst
  }

  /**
    * Boolean unification using the SVE algorithm.
    *
    * Returns a most-general unifier that solves the given equation `e`.
    *
    * Throws a [[ConflictException]] if the equation cannot be solved.
    */
  private def boolUnifyOne(e: Equation): BoolSubstitution = try {
    // The boolean expression we want to show is empty.
    val query = Term.mkXor(e.t1, e.t2)

    // Determine the order in which to eliminate the variables.
    val fvs = query.freeVars.toList

    // Eliminate all variables one by one in the order specified by fvs.
    successiveVariableElimination(query, fvs)
  } catch {
    case _: BoolUnificationException => throw ConflictException(e.t1, e.t2, e.loc)
  }

  /**
    * The Successive Variable Elimination algorithm.
    *
    * Computes the most-general unifier of the given term `t ~ empty` where `fvs` is the list of free variables in `t`.
    *
    * Eliminates variables one-by-one from the given list `fvs`.
    *
    * Throws a [[BoolUnificationException]] if there is no solution.
    */
  private def successiveVariableElimination(t: Term, fvs: List[Int]): BoolSubstitution = fvs match {
    case Nil =>
      // Determine if f is unsatisfiable when all constants are made into (flexible) variables.
      if (emptyEquivalent(t))
        BoolSubstitution.empty
      else
        throw BoolUnificationException()

    case x :: xs =>
      val t0 = BoolSubstitution.singleton(x, Term.Empty)(t)
      val t1 = BoolSubstitution.singleton(x, Term.Univ)(t)
      val se = successiveVariableElimination(repeatedPropagation(Term.mkInter(t0, t1), 1), xs)

      val f1 = repeatedPropagation(Term.mkUnion(se(t0), Term.mkMinus(Term.Var(x), se(t1))), 1)
      val st = BoolSubstitution.singleton(x, f1)
      st ++ se
  }

  /**
    * Returns `true` if the given term `t` is equivalent to empty, i.e. if all assignments to its free variables
    * makes the whole term evaluate to empty.
    *
    * Note that a constant can never be empty equivalent because we do not know if it is univ or empty.
    *
    * A smarter implementation would use a full-blown SAT solver, but since this function is rarely called and typically
    * called with a small term, we use a very naive implementation.
    */
  private def emptyEquivalent(t: Term): Boolean = t match {
    case Term.Univ => false
    case Term.Cst(_) => false
    case Term.Var(_) => false
    case Term.Elem(_) => false
    case Term.Empty => true
    case _ => evaluateAll(t, t.freeUnknowns.toList, SortedSet.empty) // Evaluate t on all its valuations.
  }

  /**
    * Returns `true` if `t` is equivalent to empty under the assumption that `fvs` is the vars of
    * `t` and that vars in `trueVars` are set to `univ`.
    *
    * It is assumed that there is no constants
    *
    * For each variable `x`, the function recursively explores both when `x` is univ and when `x` is empty.
    */
  private def evaluateAll(t: Term, fvs: List[Int], trueVars: SortedSet[Int]): Boolean = fvs match {
    case Nil =>
      // All variables are bound. Compute the truth value.
      evaluate(t, trueVars).isEmpty
    case x :: xs =>
      // Recurse on two cases: x = univ and x = empty.
      evaluateAll(t, xs, trueVars + x) && evaluateAll(t, xs, trueVars)
  }

  /**
    * Returns the evaluation of the formula.
    */
  private def evaluate(t: Term, trueVars: SortedSet[Int]): SetEval = t match {
    case Term.Univ => SetEval.univ
    case Term.Empty => SetEval.empty
    case Term.Cst(c) => if (trueVars.contains(c)) SetEval.univ else SetEval.empty
    case Term.Elem(i) => SetEval.single(i)
    case Term.Var(x) => if (trueVars.contains(x)) SetEval.univ else SetEval.empty
    case Term.Compl(t) => evaluate(t, trueVars).compl()

    case Term.Inter(posElem, posCsts, posVars, negElems, negCsts, negVars, rest) =>
      var running = SetEval.univ
      for (t <- posElem.iterator ++ posCsts.iterator ++ posVars.iterator) {
        running = running.intersect(evaluate(t, trueVars))
         if (running.isEmpty) return running
      }
      for (t <- negElems.iterator ++ negCsts.iterator ++ negVars.iterator) {
        running = running.intersect(evaluate(t, trueVars).compl())
        if (running.isEmpty) return running
      }
      for (t <- rest) {
        running = running.intersect(evaluate(t, trueVars))
        if (running.isEmpty) return running
      }
      running

    case Term.Union(posElems, posCsts, posVars, negElems, negCsts, negVars, rest) =>
      var running = SetEval.empty
      for (t <- posElems.iterator ++ posCsts.iterator ++ posVars.iterator) {
        running = running.union(evaluate(t, trueVars))
        if (running.isUniv) return running
      }
      for (t <- negElems.iterator ++ negCsts.iterator ++ negVars.iterator) {
        running = running.union(evaluate(t, trueVars).compl())
        if (running.isUniv) return running
      }
      for (t <- rest) {
        running = running.union(evaluate(t, trueVars))
        if (running.isUniv) return running
      }
      running
  }

  sealed trait SetEval {

    import SetEval.{Compl, Set}

    def union(s: SetEval): SetEval = (this, s) match {
      case (Set(s1), Set(s2)) =>
        Set(s1.union(s2))
      case (Set(s1), Compl(s2)) =>
        // s1 u ^s2
        // ^(^s1 inter s2)
        // ^(s2 - s1)
        Compl(s2.diff(s1))
      case (Compl(_), Set(_)) =>
        s.union(this)
      case (Compl(s1), Compl(s2)) =>
        // ^s1 u ^s2
        Compl(s1.intersect(s2))
    }

    def intersect(s: SetEval): SetEval = (this, s) match {
      case (Set(s1), Set(s2)) =>
        Set(s1.intersect(s2))
      case (Set(s1), Compl(s2)) =>
        // s1 inter ^s2
        Set(s1.diff(s2))
      case (Compl(_), Set(_)) =>
        s.intersect(this)
      case (Compl(s1), Compl(s2)) =>
        // ^s1 inter ^s2
        Compl(s1.union(s2))
    }

    def compl(): SetEval = this match {
      case Set(s) =>
        Compl(s)
      case Compl(s) =>
        Set(s)
    }

    def isUniv: Boolean = this match {
      case Compl(s) if s.isEmpty => true
      case Set(_) => false
      case Compl(_) => false
    }

    def isEmpty: Boolean = this match {
      case Set(s) if s.isEmpty => true
      case Set(_) => false
      case Compl(_) => false
    }
  }

  object SetEval {
    private case class Set(s: SortedSet[Int]) extends SetEval

    private case class Compl(s: SortedSet[Int]) extends SetEval

    val empty: SetEval = Set(SortedSet.empty)
    val univ: SetEval = Compl(SortedSet.empty)

    def single(i: Int): SetEval = Set(SortedSet(i))
  }

  @tailrec
  private def repeatedPropagation(t: Term, k: Int): Term = {
    if (k <= 0) t else repeatedPropagation(propagation(t), k - 1)
  }

  /**
    * Use the two rewrites `x ∩ f === x ∩ f[x -> Univ]` and `x ∪ f === x ∪ f[x -> Empty]`.
    *
    * We use this for elements, constants, and variables.
    */
  private def propagation(t: Term): Term = {
    def visit(t: Term, setElems: SortedMap[Int, Term], setCsts: SortedMap[Int, Term], setVars: SortedMap[Int, Term]): Term = t match {
      case Term.Univ => t
      case Term.Empty => t
      case Term.Cst(c) => setCsts.getOrElse(c, t)
      case Term.Var(x) => setVars.getOrElse(x, t)
      case Term.Elem(i) => setElems.getOrElse(i, t)
      case Term.Compl(t0) =>
        val compl0 = visit(t0, setElems, setCsts, setVars)
        if (compl0 eq t0) t else Term.mkCompl(compl0)
      case Term.Inter(posElem0, posCsts0, posVars0, negElems0, negCsts0, negVars0, rest0) =>
        // check for trivial cases
        if (setElems.nonEmpty && posElem0.exists(e => setElems.get(e.i).contains(Term.Empty))) return Term.Empty
        if (setCsts.nonEmpty && posCsts0.exists(c => setCsts.get(c.c).contains(Term.Empty))) return Term.Empty
        if (setVars.nonEmpty && posVars0.exists(x => setVars.get(x.x).contains(Term.Empty))) return Term.Empty
        if (setElems.nonEmpty && negElems0.exists(e => setElems.get(e.i).contains(Term.Univ))) return Term.Empty
        if (setCsts.nonEmpty && negCsts0.exists(c => setCsts.get(c.c).contains(Term.Univ))) return Term.Empty
        if (setVars.nonEmpty && negVars0.exists(x => setVars.get(x.x).contains(Term.Univ))) return Term.Empty

        // Compute new constant and variable sets by removing constants and variables that hold.
        val posElem = if (setElems.isEmpty) posElem0 else posElem0.filterNot(e => setElems.get(e.i).contains(Term.Univ))
        val posCsts = if (setCsts.isEmpty) posCsts0 else posCsts0.filterNot(c => setCsts.get(c.c).contains(Term.Univ))
        val posVars = if (setVars.isEmpty) posVars0 else posVars0.filterNot(x => setVars.get(x.x).contains(Term.Univ))
        val negElems = if (setElems.isEmpty) negElems0 else negElems0.filterNot(e => setElems.get(e.i).contains(Term.Empty))
        val negCsts = if (setCsts.isEmpty) negCsts0 else negCsts0.filterNot(c => setCsts.get(c.c).contains(Term.Empty))
        val negVars = if (setVars.isEmpty) negVars0 else negVars0.filterNot(x => setVars.get(x.x).contains(Term.Empty))

        var currentElems = setElems ++ posElem0.toList.map(e => (e.i -> Term.Univ)) ++ negElems0.toList.map(e => (e.i -> Term.Empty))
        var currentCsts = setCsts ++ posCsts0.toList.map(c => (c.c -> Term.Univ)) ++ negCsts0.toList.map(c => (c.c -> Term.Empty))
        var currentVars = setVars ++ posVars0.toList.map(x => (x.x -> Term.Univ)) ++ negVars0.toList.map(x => (x.x -> Term.Empty))

        // Recurse on the sub-terms with the updated maps.
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
            case Term.Elem(i) =>
              currentElems += (i -> Term.Univ)
              res
            case Term.Compl(Term.Elem(i)) =>
              currentElems += (i -> Term.Empty)
              res
            case Term.Compl(_) => res
            case Term.Inter(_, _, _, _, _, _, _) => res
            case Term.Union(_, _, _, _, _, _, _) => res
          }
        }

        val maintain = Term.Inter(posElem, posCsts, posVars, negElems, negCsts, negVars, Nil)
        Term.mkInter(maintain :: rest)

      case Term.Union(posElems0, posCsts0, posVars0, negElems0, negCsts0, negVars0, rest0) =>
        // check for trivial cases
        if (setElems.nonEmpty && posElems0.exists(e => setElems.get(e.i).contains(Term.Univ))) return Term.Univ
        if (setCsts.nonEmpty && posCsts0.exists(c => setCsts.get(c.c).contains(Term.Univ))) return Term.Univ
        if (setVars.nonEmpty && posVars0.exists(x => setVars.get(x.x).contains(Term.Univ))) return Term.Univ
        if (setElems.nonEmpty && negElems0.exists(e => setElems.get(e.i).contains(Term.Empty))) return Term.Univ
        if (setCsts.nonEmpty && negCsts0.exists(c => setCsts.get(c.c).contains(Term.Empty))) return Term.Univ
        if (setVars.nonEmpty && negVars0.exists(x => setVars.get(x.x).contains(Term.Empty))) return Term.Univ

        // Compute new constant and variable sets by removing constants and variables that hold.
        val posElem = if (setElems.isEmpty) posElems0 else posElems0.filterNot(e => setElems.get(e.i).contains(Term.Empty))
        val posCsts = if (setCsts.isEmpty) posCsts0 else posCsts0.filterNot(c => setCsts.get(c.c).contains(Term.Empty))
        val posVars = if (setVars.isEmpty) posVars0 else posVars0.filterNot(x => setVars.get(x.x).contains(Term.Empty))
        val negElems = if (setElems.isEmpty) negElems0 else negElems0.filterNot(e => setElems.get(e.i).contains(Term.Univ))
        val negCsts = if (setCsts.isEmpty) negCsts0 else negCsts0.filterNot(c => setCsts.get(c.c).contains(Term.Univ))
        val negVars = if (setVars.isEmpty) negVars0 else negVars0.filterNot(x => setVars.get(x.x).contains(Term.Univ))

        var currentElems = setElems ++ posElems0.toList.map(e => (e.i -> Term.Empty)) ++ negElems0.toList.map(e => (e.i -> Term.Univ))
        var currentCsts = setCsts ++ posCsts0.toList.map(c => (c.c -> Term.Empty)) ++ negCsts0.toList.map(c => (c.c -> Term.Univ))
        var currentVars = setVars ++ posVars0.toList.map(x => (x.x -> Term.Empty)) ++ negVars0.toList.map(x => (x.x -> Term.Univ))

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
            case Term.Elem(i) =>
              currentElems += (i -> Term.Empty)
              res
            case Term.Compl(Term.Elem(i)) =>
              currentElems += (i -> Term.Univ)
              res
            case Term.Compl(_) => res
            case Term.Inter(_, _, _, _, _, _, _) => res
            case Term.Union(_, _, _, _, _, _, _) => res
          }
        }

        val maintain = Term.Union(posElem, posCsts, posVars, negElems, negCsts, negVars, Nil)
        Term.mkUnion(maintain :: rest)
    }

    visit(t, SortedMap.empty, SortedMap.empty, SortedMap.empty)
  }

  /**
    * Companion object for [[Equation]].
    */
  object Equation {
    /**
      * Returns a unification equation  `t1 ~ t2` between the terms `t1` and `t2`.
      *
      * The smart constructor performs normalization:
      * - We move univ and empty to the rhs.
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
      case (Term.Elem(i1), Term.Elem(i2)) => if (i1 <= i2) Equation(t1, t2, loc) else Equation(t2, t1, loc)
      case (Term.Univ, _) => Equation(t2, Term.Univ, loc)
      case (Term.Empty, _) => Equation(t2, Term.Empty, loc)
      case (Term.Elem(_), _) => Equation(t2, t1, loc)
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
    * A common super-type for Boolean terms.
    *
    * Note: We assume that the integers used for constants and variables are disjoint.
    */
  sealed trait Term {

    /**
      * Syntactic sugar for [[Term.mkInter]].
      */
    final def &(that: Term): Term = Term.mkInter(this, that)

    /**
      * Syntactic sugar for [[Term.mkUnion]].
      */
    final def |(that: Term): Term = Term.mkUnion(this, that)

    /**
      * Syntactic sugar for [[Equation.mk]]
      */
    final def ~(that: Term)(implicit loc: SourceLocation): Equation = Equation.mk(this, that, loc)

    /**
      * Returns all variables that occur in `this` term.
      */
    final def freeVars: SortedSet[Int] = this match {
      case Term.Univ => SortedSet.empty
      case Term.Empty => SortedSet.empty
      case Term.Cst(_) => SortedSet.empty
      case Term.Elem(_) => SortedSet.empty
      case Term.Var(x) => SortedSet(x)
      case Term.Compl(t) => t.freeVars
      case Term.Inter(_, _, posVars, _, _, negVars, rest) =>
        SortedSet.empty[Int] ++ posVars.map(_.x) ++ negVars.map(_.x) ++ rest.flatMap(_.freeVars)
      case Term.Union(_, _, posVars, _, _, negVars, rest) =>
        SortedSet.empty[Int] ++ posVars.map(_.x) ++ negVars.map(_.x) ++ rest.flatMap(_.freeVars)
    }

    /**
      * Returns all variables that occur in `this` term.
      */
    final def freeVarsContains(v: Term.Var): Boolean = this match {
      case Term.Univ => false
      case Term.Empty => false
      case Term.Cst(_) => false
      case Term.Elem(_) => false
      case Term.Var(_) => this == v
      case Term.Compl(t) => t.freeVarsContains(v)
      case Term.Inter(_, _, posVars, _, _, negVars, rest) =>
        posVars.contains(v) || negVars.contains(v) || rest.exists(t => t.freeVarsContains(v))
      case Term.Union(_, _, posVars, _, _, negVars, rest) =>
        posVars.contains(v) || negVars.contains(v) || rest.exists(t => t.freeVarsContains(v))
    }

    /**
      * Returns all variables and constants that occur in `this` term.
      */
    final def freeUnknowns: SortedSet[Int] = this match {
      case Term.Univ => SortedSet.empty
      case Term.Empty => SortedSet.empty
      case Term.Cst(c) => SortedSet(c)
      case Term.Elem(_) => SortedSet.empty
      case Term.Var(x) => SortedSet(x)
      case Term.Compl(t) => t.freeUnknowns
      case Term.Inter(_, posCsts, posVars, _, negCsts, negVars, rest) =>
        SortedSet.empty[Int] ++ posCsts.map(_.c) ++ posVars.map(_.x) ++ negCsts.map(_.c) ++ negVars.map(_.x) ++ rest.flatMap(_.freeUnknowns)
      case Term.Union(_, posCsts, posVars, _, negCsts, negVars, rest) =>
        SortedSet.empty[Int] ++ posCsts.map(_.c) ++ posVars.map(_.x) ++ negCsts.map(_.c) ++ negVars.map(_.x) ++ rest.flatMap(_.freeUnknowns)
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
      case Term.Elem(_) => 0
      case Term.Var(_) => 0
      case Term.Compl(t) => t.size + 1
      case Term.Inter(posElem, posCsts, posVars, negElems, negCsts, negVars, rest) =>
        // We need a connective for each constant, variable, and term minus one.
        // We then add the size of all the sub-terms in `rest`.
        ((posElem.size + posCsts.size + posVars.size + negElems.size + negCsts.size + negVars.size + rest.size) - 1) + rest.map(_.size).sum
      case Term.Union(posElems, posCsts, posVars, negElems, negCsts, negVars, rest) =>
        ((posElems.size + posCsts.size + posVars.size + negElems.size + negCsts.size + negVars.size + rest.size) - 1) + rest.map(_.size).sum
    }

    /**
      * Returns a human-readable representation of `this` term.
      */
    override def toString: String = this match {
      case Term.Univ => formatter.red("univ")
      case Term.Empty => formatter.red("empty")
      case Term.Cst(c) => formatter.blue(s"c$c")
      case Term.Elem(i) => formatter.green(s"e$i")
      case Term.Var(x) => s"x$x"
      case Term.Compl(f) => f match {
        case Term.Univ => formatter.red("!univ")
        case Term.Empty => formatter.red("!empty")
        case Term.Cst(c) => formatter.blue(s"!c$c")
        case Term.Elem(i) => formatter.green(s"!e$i")
        case Term.Var(x) => s"!x$x"
        case _ => s"!($f)"
      }
      case Term.Inter(posElem, posCsts, posVars, negElems, negCsts, negVars, rest) =>
        val terms = posElem.toList ++ negElems.map(Term.Compl(_)) ++ posCsts ++ negCsts.map(Term.Compl(_)) ++ posVars ++ negVars.map(Term.Compl(_)) ++ rest
        s"(${terms.mkString(" ∩ ")})"
      case Term.Union(posElems, posCsts, posVars, negElems, negCsts, negVars, rest) =>
        val terms = posElems.toList ++ negElems.map(Term.Compl(_)) ++ posCsts ++ negCsts.map(Term.Compl(_)) ++ posVars ++ negVars.map(Term.Compl(_)) ++ rest
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
      * Represents a Boolean variable ("flexible variable").
      *
      * Note: We assume that constants and variables are disjoint.
      */
    case class Var(x: Int) extends Term

    case class Elem(i: Int) extends Term

    /**
      * Represents the complement of the term `t` (`!`).
      */
    case class Compl(t: Term) extends Term

    /**
      * Represents a intersection of terms (`∩`).
      *
      * We use a clever representation where we have a intersection of constants, variables, and then sub-terms.
      *
      * For example, the intersection: `x7 ∩ !x2 ∩ c1 ∩ x4` is represented as: `Set(c1), Set(x4, x7), List(!x2)`.
      *
      * This representation is key to efficiency because the equations we solve are heavy on intersections.
      */
    case class Inter(posElem: Option[Term.Elem], posCsts: Set[Term.Cst], posVars: Set[Term.Var], negElems: Set[Term.Elem], negCsts: Set[Term.Cst], negVars: Set[Term.Var], rest: List[Term]) extends Term {
      // We ensure that `rest` cannot contain constants and variables.
      // Once the code is better tested, we can remove these assertions.
      assert(!rest.exists(_.isInstanceOf[Term.Cst]))
      assert(!rest.exists(_.isInstanceOf[Term.Var]))
      assert(!rest.exists(_.isInstanceOf[Term.Elem]))
    }

    /**
      * A union of the terms `ts` (`∪`).
      *
      * Note: We do not use currently use any clever representation of unions (because there has been no need).
      */
    case class Union(posElems: Set[Term.Elem], posCsts: Set[Term.Cst], posVars: Set[Term.Var], negElems: Set[Term.Elem], negCsts: Set[Term.Cst], negVars: Set[Term.Var], rest: List[Term]) extends Term {
      // We ensure that `rest` cannot contain constants and variables.
      // Once the code is better tested, we can remove these assertions.
      assert(!rest.exists(_.isInstanceOf[Term.Cst]))
      assert(!rest.exists(_.isInstanceOf[Term.Var]))
      assert(!rest.exists(_.isInstanceOf[Term.Elem]))
    }

    /**
      * Smart constructor for complement (`!`).
      */
    final def mkCompl(t: Term): Term = t match {
      case Univ => Empty
      case Empty => Univ
      case Cst(_) => Compl(t)
      case Var(_) => Compl(t)
      case Elem(_) => Compl(t)
      case Compl(t0) => t0
      case Inter(posElem, posCsts, posVars, negElems, negCsts, negVars, rest) =>
        val maintain = Union(negElems, negCsts, negVars, posElem.toSet, posCsts, posVars, Nil)
        mkUnion(maintain :: rest.map(mkCompl))
      case Union(posElems, posCsts, posVars, negElems, negCsts, negVars, rest) =>
        if (negElems.sizeIs > 1) return Term.Empty
        val maintain = Inter(negElems.headOption, negCsts, negVars, posElems, posCsts, posVars, Nil)
        mkInter(maintain :: rest.map(mkCompl))
    }

    /**
      * Smart constructor for intersection (`∩`).
      */
    final def mkInter(t1: Term, t2: Term): Term = (t1, t2) match {
      case (Empty, _) => Empty
      case (_, Empty) => Empty
      case (Univ, _) => t2
      case (_, Univ) => t1
      case _ => mkInter(List(t1, t2))
    }

    /**
      * Smart constructor for union (`∪`).
      */
    final def mkUnion(t1: Term, t2: Term): Term = (t1, t2) match {
      case (Univ, _) => Univ
      case (_, Univ) => Univ
      case (Empty, _) => t2
      case (_, Empty) => t1
      case _ => mkUnion(List(t1, t2))
    }

    /**
      * Smart constructor for intersection (`∩`).
      *
      * A lot of heavy lifting occurs here. In particular, we must partition `ts` into (a) constants, (b) variables,
      * and (c) other sub-terms. Moreover, we look into those sub-terms and flatten any intersections we find within.
      */
    final def mkInter(ts: List[Term]): Term = {
      ts match {
        case Nil => return Term.Univ
        // OBS: do not do this - some inter creation relies on the re-computation
        // case single :: Nil => return single
        case _ => ()
      }
      // Mutable data structures to hold constants, variables, and other sub-terms.
      val posElemTerms = mutable.Set.empty[Term.Elem]
      val posCstTerms = mutable.Set.empty[Term.Cst]
      val posVarTerms = mutable.Set.empty[Term.Var]
      val negElemTerms = mutable.Set.empty[Term.Elem]
      val negCstTerms = mutable.Set.empty[Term.Cst]
      val negVarTerms = mutable.Set.empty[Term.Var]
      val restTerms = mutable.ListBuffer.empty[Term]
      for (t <- ts) {
        t match {
          case Univ => // NOP - We do not have to include Univ in a intersection.
          case Empty => return Empty // If the intersection contains EMPTY then whole intersection is EMPTY.
          case x@Term.Cst(_) =>
            if (negCstTerms.contains(x)) return Empty
            posCstTerms += x
          case Term.Compl(x@Term.Cst(_)) =>
            if (posCstTerms.contains(x)) return Empty
            negCstTerms += x
          case x@Term.Elem(_) =>
            if (negElemTerms.contains(x)) return Empty
            posElemTerms += x
            if (posElemTerms.size > 1) return Empty
          case Term.Compl(x@Term.Elem(_)) =>
            if (posElemTerms.contains(x)) return Empty
            negElemTerms += x
          case x@Term.Var(_) =>
            if (negVarTerms.contains(x)) return Empty
            posVarTerms += x
          case Term.Compl(x@Term.Var(_)) =>
            if (posVarTerms.contains(x)) return Empty
            negVarTerms += x
          case Inter(posElem0, posCsts0, posVars0, negElems0, negCsts0, negVars0, rest0) =>
            posElemTerms ++= posElem0.toList
            if (posElemTerms.size > 1) return Empty
            // We have found a nested intersection. We can immediately add _its_ constants and variables.
            posCstTerms ++= posCsts0
            posVarTerms ++= posVars0
            negElemTerms ++= negElems0
            negCstTerms ++= negCsts0
            negVarTerms ++= negVars0
            for (t0 <- rest0) {
              // We then iterate through the nested sub-terms of the nested intersection.
              t0 match {
                case Univ => // NOP - We do not have to include Univ in a intersection.
                case Empty => return Empty // If the intersection contains EMPTY then whole intersection is EMPTY.
                case x@Term.Cst(_) =>
                  if (negCstTerms.contains(x)) return Empty
                  posCstTerms += x
                case Term.Compl(x@Term.Cst(_)) =>
                  if (posCstTerms.contains(x)) return Empty
                  negCstTerms += x
                case x@Term.Elem(_) =>
                  if (negElemTerms.contains(x)) return Empty
                  posElemTerms += x
                  if (posElemTerms.size > 1) return Empty
                case Term.Compl(x@Term.Elem(_)) =>
                  if (posElemTerms.contains(x)) return Empty
                  negElemTerms += x
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
      assert(posElemTerms.size <= 1)

      val elms = posElemTerms.size + posCstTerms.size + posVarTerms.size + negElemTerms.size + negCstTerms.size + negVarTerms.size + restTerms.size
      if (elms == 0) {
        return Term.Univ
      } else if (elms == 1) {
        for (any <- posElemTerms) return any
        for (any <- posCstTerms) return any
        for (any <- posVarTerms) return any
        for (any <- negElemTerms) return Term.Compl(any)
        for (any <- negCstTerms) return Term.Compl(any)
        for (any <- negVarTerms) return Term.Compl(any)
        for (any <- restTerms) return any
      }
      Inter(posElemTerms.headOption, posCstTerms.toSet, posVarTerms.toSet, negElemTerms.toSet, negCstTerms.toSet, negVarTerms.toSet, restTerms.toList)
    }

    /**
      * Smart constructor for union (`∪`).
      */
    final def mkUnion(ts: List[Term]): Term = {
      ts match {
        case Nil => return Term.Empty
        // OBS: do not do this - some inter creation relies on the re-computation
        // case single :: Nil => return single
        case _ => ()
      }
      // Mutable data structures to hold constants, variables, and other sub-terms.
      val posElemTerms = mutable.Set.empty[Term.Elem]
      val posCstTerms = mutable.Set.empty[Term.Cst]
      val posVarTerms = mutable.Set.empty[Term.Var]
      val negElemTerms = mutable.Set.empty[Term.Elem]
      val negCstTerms = mutable.Set.empty[Term.Cst]
      val negVarTerms = mutable.Set.empty[Term.Var]
      val restTerms = mutable.ListBuffer.empty[Term]
      for (t <- ts) {
        t match {
          case Empty => // NOP - We do not have to include Empty in a union.
          case Univ => return Univ // If the union contains UNIV then whole union is UNIV.
          case x@Term.Cst(_) =>
            if (negCstTerms.contains(x)) return Univ
            posCstTerms += x
          case Term.Compl(x@Term.Cst(_)) =>
            if (posCstTerms.contains(x)) return Univ
            negCstTerms += x
          case x@Term.Elem(_) =>
            if (negElemTerms.contains(x)) return Univ
            posElemTerms += x
          case Term.Compl(x@Term.Elem(_)) =>
            if (posElemTerms.contains(x)) return Univ
            negElemTerms += x
          case x@Term.Var(_) =>
            if (negVarTerms.contains(x)) return Univ
            posVarTerms += x
          case Term.Compl(x@Term.Var(_)) =>
            if (posVarTerms.contains(x)) return Univ
            negVarTerms += x
          case Union(posElems0, posCsts0, posVars0, negElems0, negCsts0, negVars0, rest0) =>
            // We have found a nested intersection. We can immediately add _its_ constants and variables.
            posElemTerms ++= posElems0
            posCstTerms ++= posCsts0
            posVarTerms ++= posVars0
            negElemTerms ++= negElems0
            negCstTerms ++= negCsts0
            negVarTerms ++= negVars0
            for (t0 <- rest0) {
              // We then iterate through the nested sub-terms of the nested intersection.
              t0 match {
                case Empty => // NOP - We do not have to include Empty in a union.
                case Univ => return Univ // If the union contains UNIV then whole union is UNIV.
                case x@Term.Cst(_) =>
                  if (negCstTerms.contains(x)) return Univ
                  posCstTerms += x
                case Term.Compl(x@Term.Cst(_)) =>
                  if (posCstTerms.contains(x)) return Univ
                  negCstTerms += x
                case x@Term.Elem(_) =>
                  if (negElemTerms.contains(x)) return Univ
                  posElemTerms += x
                case Term.Compl(x@Term.Elem(_)) =>
                  if (posElemTerms.contains(x)) return Univ
                  negElemTerms += x
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

      val elms = posElemTerms.size + posCstTerms.size + posVarTerms.size + negElemTerms.size + negCstTerms.size + negVarTerms.size + restTerms.size
      if (elms == 0) {
        return Term.Empty
      } else if (elms == 1) {
        for (any <- posElemTerms) return any
        for (any <- posCstTerms) return any
        for (any <- posVarTerms) return any
        for (any <- negElemTerms) return Term.Compl(any)
        for (any <- negCstTerms) return Term.Compl(any)
        for (any <- negVarTerms) return Term.Compl(any)
        for (any <- restTerms) return any
      }

      Union(posElemTerms.toSet, posCstTerms.toSet, posVarTerms.toSet, negElemTerms.toSet, negCstTerms.toSet, negVarTerms.toSet, restTerms.toList)
    }

    /**
      * Returns the Xor of `x` and `y`. Implemented by desugaring.
      */
    final def mkXor(x: Term, y: Term): Term = mkUnion(mkMinus(x, y), mkMinus(y, x))

    /**
      * Returns the Minus of `x` and `y` (`-`). Implemented by desugaring.
      */
    final def mkMinus(x: Term, y: Term): Term = mkInter(x, mkCompl(y))
  }

  /**
    * Companion object of [[BoolSubstitution]].
    */
  object BoolSubstitution {
    /**
      * The empty substitution.
      */
    val empty: BoolSubstitution = BoolSubstitution(Map.empty)

    /**
      * Returns a singleton substitution where the variable `x` is bound to the term `t`.
      */
    def singleton(x: Int, t: Term): BoolSubstitution = BoolSubstitution(Map(x -> t))
  }

  /**
    * Represents a substitution from Boolean variables (represented as integers) to Boolean terms.
    *
    * A substitution is a partial map from variables to terms. Every substitution induces a total function
    * `s: Term -> Term` that replaces every occurrence in the input term, which occurs in the domain of the
    * substitution, with its corresponding term from the co-domain.
    *
    * We will often write a substitution as `[x -> t1, y -> t2]` and so forth. We write that `x -> t1` is a binding.
    *
    * Note: constants and variables are a separate syntactic category. A substitution will never replace any constants.
    */
  case class BoolSubstitution(m: Map[Int, Term]) {

    /**
      * Applies `this` substitution to the given term `t`.
      *
      * We must use the smart constructors from [[Term]] to ensure that the constructed term is normalized.
      */
    def apply(t: Term): Term = t match {
      case Term.Univ => t
      case Term.Empty => t
      case Term.Cst(_) => t
      case Term.Elem(_) => t

      case Term.Var(x) => m.get(x) match {
        case None => t // Case 1: The substitution has a binding for `x`. Return the bound term.
        case Some(t0) => t0 // Case 2: The substitution has no binding for `x`. Return the original term.
      }

      case Term.Compl(t0) =>
        val app = apply(t0)
        if (app eq t0) t else Term.mkCompl(app)

      case Term.Inter(posElem, posCsts, posVars, negElems, negCsts, negVars, rest) =>
        val maintain = Term.Inter(posElem, posCsts, Set.empty, negElems, negCsts, Set.empty, Nil)
        val ts = mutable.ListBuffer.empty[Term]
        for (x <- posVars) ts += apply(x)
        for (x <- negVars) ts += Term.mkCompl(apply(x))
        for (t <- rest) ts += apply(t)
        Term.mkInter(maintain :: ts.toList)

      case Term.Union(posElems, posCsts, posVars, negElems, negCsts, negVars, rest) =>
        val maintain = Term.Union(posElems, posCsts, Set.empty, negElems, negCsts, Set.empty, Nil)
        val ts = mutable.ListBuffer.empty[Term]
        for (x <- posVars) ts += apply(x)
        for (x <- negVars) ts += Term.mkCompl(apply(x))
        for (t <- rest) ts += apply(t)
        Term.mkUnion(maintain :: ts.toList)
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
    def apply(l: List[Equation]): List[Equation] = l.map(apply)

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
    def extended(x: Int, t: Term, loc: SourceLocation): BoolSubstitution = m.get(x) match {
      case None => BoolSubstitution(m + (x -> t))
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
      * The domains of the two substitutions must not overlap.
      */
    def ++(that: BoolSubstitution): BoolSubstitution = {
      val intersection = this.m.keySet.intersect(that.m.keySet)
      if (intersection.nonEmpty) {
        throw InternalCompilerException(s"Substitutions are not disjoint on: '${intersection.mkString(",")}'.", SourceLocation.Unknown)
      }

      BoolSubstitution(this.m ++ that.m)
    }

    /**
      * Composes `this` substitution with `that` substitution.
      *
      * Conceptually `this` is a function: `b -> c` and `that` is a function `a -> b`.
      *
      * We want to compute `a -> c` which we get by computing `x -> this(that(x))`.
      */
    def @@(that: BoolSubstitution): BoolSubstitution = {
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
        result.update(x, this.apply(t))
      }

      // Add all bindings in `this` that are not in `that`.
      for ((x, t) <- this.m) {
        if (!that.m.contains(x)) {
          result.update(x, t)
        }
      }

      BoolSubstitution(result.toMap)
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
    * Verifies that `s` is a solution to the given Boolean unification equations.
    *
    * Throws an exception if the solution is incorrect.
    *
    * Note: The function verifies that it is _a solution_, not that it is _the most general_.
    *
    * Note: Unfortunately this function is very slow since the SAT solver is very slow.
    */
  def verify(s: BoolSubstitution, l: List[Equation]): Unit = {
    // Apply the substitution to every equation and check that it is solved.
    for (e <- l) {
      // We want to check that `s(t1) == s(t2)`. In other words that both sides are either univ or both sides are empty.
      // If we can find a situation where one side is univ and the other side is empty then the equation does not hold.
      // We can look for such a situation by checking whether `s(t1) xor s(t2)` is satisfiable. If it is then we have
      // found an equation that is not solved.
      val t1 = s(e.t1)
      val t2 = s(e.t2)
      val query = Term.mkXor(t1, t2)
      if (!emptyEquivalent(query)) {
        println(s"  Original  : ${e.t1} ~ ${e.t2}")
        println(s"  with Subst: $t1 ~ $t2")
        throw InternalCompilerException(s"Incorrectly solved equation", SourceLocation.Unknown)
      }
    }
  }

  /**
    * A common super-type for exceptions throw by the solver.
    */
  sealed trait FastBoolUnificationException extends RuntimeException

  /**
    * Represents a Boolean unification failure between the two terms: `x` and `y`.
    */
  case class ConflictException(x: Term, y: Term, loc: SourceLocation) extends FastBoolUnificationException

  /**
    * Represents a solution that is too complex.
    *
    * @param msg the specific error message.
    */
  case class TooComplexException(msg: String) extends FastBoolUnificationException

}
