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

import scala.collection.immutable.SortedSet
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
///   1. We propagate simple ground terms (univ/empty/constant) in a fixpoint.
///   2. We propagate variables (i.e. resolving constraints of the form x = y).
///   3. We perform trivial assignments where the left-hand variables does not occur in the RHS.
///   4. We eliminate (now) trivial and redundant constraints.
///   5. We do full-blown unification with SVE.
/// - We represent a union with n >= 2 terms.
///   - We group the terms into three: a set of constants, a set of variables, and a list of sub-terms.
///   - We flatten unions at least one level per call to `mkUnion`.
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
    * TODO: update number
    * The threshold must be at least 600 for the test suite to pass.
    *
    * If a solution is too complex an [[TooComplexException]] exception is thrown.
    */
  private val SizeThreshold: Int = 1_000

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
    * Enable debugging (prints information during unification).
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
  def solveAll(l: List[Equation]): Result[BoolSubstitution, (FastSetUnificationException, List[Equation], BoolSubstitution)] = {
    val solver = new Solver(l)
    solver.solve()
  }

  /**
    * A stateful phased solver for unification equations. The solver maintains two fields that change over time:
    *
    * - The current (pending) unification equations to solve.
    * - The current (partial) substitution which represents the solution.
    *
    * @param l The input list of unification equations to solve.
    */
  private class Solver(l: List[Equation]) {

    /**
      * The current (pending) equations to solve.
      *
      * The list of pending equations decrease as the solver progresses.
      *
      * Note that the pending equations are not a strict subset of the original equations because they may be simplified during computation.
      *
      * If unification is successful, the list of pending equations will become empty at the end of the computation.
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
    def solve(): Result[BoolSubstitution, (FastSetUnificationException, List[Equation], BoolSubstitution)] = {
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
      debugln("--- Phase 5: Unification")
      debugln("    (resolves all remaining equations using SVE.)")
      debugln("-".repeat(80))
      val newSubst = unifyAllPickSmallest(currentEqns)
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
    * -        c ~ false    (and mirrored)
    */
  private def checkAndSimplify(l: List[Equation]): List[Equation] = l match {
    case Nil => Nil
    case Equation(t1, t2, loc) :: es => (t1, t2) match {
      // Trivial equations: skip them.
      case (Term.Univ, Term.Univ) => checkAndSimplify(es)
      case (Term.Empty, Term.Empty) => checkAndSimplify(es)
      case (Term.Cst(c1), Term.Cst(c2)) if c1 == c2 => checkAndSimplify(es)
      case (Term.Var(x1), Term.Var(x2)) if x1 == x2 => checkAndSimplify(es)

      // Unsolvable (conflicted) equations: raise an exception.
      case (Term.Univ, Term.Empty) => throw ConflictException(t1, t2, loc)
      case (Term.Empty, Term.Univ) => throw ConflictException(t1, t2, loc)
      case (Term.Cst(c1), Term.Cst(c2)) if c1 != c2 => throw ConflictException(t1, t2, loc)
      // Note: A constraint with two different variables is of course solvable!
      case (Term.Cst(_), Term.Univ) => throw ConflictException(t1, t2, loc)
      case (Term.Cst(_), Term.Empty) => throw ConflictException(t1, t2, loc)
      case (Term.Univ, Term.Cst(_)) => throw ConflictException(t1, t2, loc)
      case (Term.Empty, Term.Cst(_)) => throw ConflictException(t1, t2, loc)

      // Non-trivial and non-conflicted equation: keep it.
      case _ => Equation(t1, t2, loc) :: checkAndSimplify(es) // TODO: tail-rec
    }
  }

  /**
    * Propagates constants and truth values through the equation system.
    *
    * The implementation saturates the system, i.e. it computes a fixpoint.
    *
    * The implementation uses three rewrite rules:
    *
    * - `x ~ empty` becomes `[x -> empty]`.
    * - `x ~ univ` becomes `[x -> univ]`.
    * - `x ~ c` becomes `[x -> c]`.
    * - `x ∪ y ∪ ... = empty` becomes `[x -> empty, y -> empty, ...]`.
    * - `x ∩ y ∩ ... = univ` becomes `[x -> univ, y -> univ, ...]`.
    *
    * For example, if the equation system is:
    *
    * {{{
    *     c1794221043 ~ (x55062 ∪ x55050 ∪ x55046 ∪ x55060 ∪ x55066 ∪ x55040 ∪ x55075 ∪ x55042 ∪ x55058 ∪ x55078)
    *     x55078 ~ x112431
    *     c1794221043 ~ x55040
    *     x55042 ~ x112433
    *     x55044 ~ x112437
    *     x55046 ~ (x112435 ∪ x55044)
    *     x55048 ~ empty
    *     x55050 ~ (x112439 ∪ x55048)
    *     x55052 ~ x112443
    *     x55055 ~ empty
    *     x55058 ~ (x55052 ∪ x112441 ∪ x55055)
    *     x55060 ~ x112446
    *     x55062 ~ x112448
    *     x55066 ~ empty
    *     x55075 ~ x112453
    * }}}
    *
    * then after constant propagation it is:
    *
    * {{{
    *     c1794221043 ~ (c1794221043 ∪ x55062 ∪ x55050 ∪ x55046 ∪ x55060 ∪ x55075 ∪ x55042 ∪ x55058 ∪ x55078)
    *     x55078 ~ x112431
    *     x55042 ~ x112433
    *     x55044 ~ x112437
    *     x55046 ~ (x112435 ∪ x55044)
    *     x112439 ~ x55050
    *     x55052 ~ x112443
    *     x55058 ~ (x55052 ∪ x112441)
    *     x55060 ~ x112446
    *     x55062 ~ x112448
    *     x55075 ~ x112453
    * }}}
    *
    * with the substitution:
    *
    * {{{
    *     x55048 -> empty
    *     x55055 -> empty
    *     x55066 -> empty
    *     x55040 -> c1794221043
    * }}}
    *
    * Note that several equations were simplified.
    *
    * Note: We use `subst.extended` to check for conflicts. For example, if we already know that `s = [x -> c17]` and we
    * learn that `x -> empty` then we will try to extend s with the new binding which will raise a [[ConflictException]].
    */
  private def propagateConstants(l: List[Equation]): (List[Equation], BoolSubstitution) = {
    var pending = l
    var subst = BoolSubstitution.empty

    // We iterate until no changes are detected.
    var changed = true
    while (changed) {
      changed = false

      var rest: List[Equation] = Nil
      for (e <- pending) {
        e match {
          // Case 1: x ~ empty
          case Equation(Term.Var(x), Term.Empty, loc) =>
            subst = subst.extended(x, Term.Empty, loc) // Note: the extended function will check that `x` is not already mapped to another constant.
            changed = true

          // Case 2: x ~ univ
          case Equation(Term.Var(x), Term.Univ, loc) =>
            subst = subst.extended(x, Term.Univ, loc) // Note: the extended function will check that `x` is not already mapped to another constant.
            changed = true

          // Case 3: x ~ c
          case Equation(Term.Var(x), Term.Cst(c), loc) =>
            subst = subst.extended(x, Term.Cst(c), loc) // Note: the extended function will check that `x` is not already mapped to another constant.
            changed = true

          // TODO: add for non-variable union/intersection?
          // Case 4: x ∪ y ∪ z ∪... ~ empty
          case Equation(Term.Union(posCsts, negCsts, posVars, negVars, rest), Term.Empty, loc) if posCsts.isEmpty && negCsts.isEmpty && rest.isEmpty =>
            for (Term.Var(x) <- posVars) {
              subst = subst.extended(x, Term.Empty, loc) // Note: the extended function will check that `x` is not already mapped to another constant.
              changed = true
            }
            for (Term.Var(x) <- negVars) {
              subst = subst.extended(x, Term.Univ, loc) // Note: the extended function will check that `x` is not already mapped to another constant.
              changed = true
            }

          // Case 5: x ∩ y ∩ z ∩... ~ univ
          case Equation(Term.Union(posCsts, negCsts, posVars, negVars, rest), Term.Univ, loc) if posCsts.isEmpty && negCsts.isEmpty && rest.isEmpty =>
            for (Term.Var(x) <- posVars) {
              subst = subst.extended(x, Term.Univ, loc) // Note: the extended function will check that `x` is not already mapped to another constant.
              changed = true
            }
            for (Term.Var(x) <- negVars) {
              subst = subst.extended(x, Term.Empty, loc) // Note: the extended function will check that `x` is not already mapped to another constant.
              changed = true
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
    *     x78914 ~ (c1498 ∪ c1500 ∪ c1501)
    *     x78914 ~ (x78926 ∪ x78923 ∪ x78917)
    *     x78917 ~ x127244
    *     x78921 ~ x127251
    *     x78923 ~ (x127249 ∪ x127247 ∪ x127248)
    *     x78926 ~ (x127254 ∪ x127252)
    * }}}
    *
    * then after variable propagation it is:
    *
    * {{{
    *     x78914 ~ (c1498 ∪ c1500 ∪ c1501)
    *     x78914 ~ (x78926 ∪ x78923 ∪ x127244)
    *     x78923 ~ (x127249 ∪ x127247 ∪ x127248)
    *     x78926 ~ (x127254 ∪ x127252)
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
        case Equation(Term.Var(x), Term.Var(y), _) =>
          // Case 1: We have found an equation: `x ~ y`.
          // Construct a singleton substitution `[x -> y]`.
          val singleton = BoolSubstitution.singleton(x, Term.Var(y))

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
    *    x78914 ~ (c1498 ∪ c1500 ∪ c1501)
    *    x78914 ~ (x78926 ∪ x78923 ∪ x127244)
    *    x78923 ~ (x127249 ∪ x127247 ∪ x127248)
    *    x78926 ~ (x127254 ∪ x127252)
    * }}}
    *
    * We compute the substitution:
    *
    * {{{
    *     x78926 -> (x127254 ∪ x127252)
    *     x78914 -> (c1498 ∪ c1500 ∪ c1501)
    *     x78923 -> (x127249 ∪ x127247 ∪ x127248)
    * }}}
    *
    * and we have the remaining equations:
    *
    * {{{
    *     (c1498 ∪ c1500 ∪ c1501) ~ (x127248 ∪ x127244 ∪ x127254 ∪ x127252 ∪ x127249 ∪ x127247)
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
        case Equation(Term.Var(x), rhs, _) if !rhs.freeVars.contains(x) =>
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
    *   (c9 ∪ c0) ~ (c9 ∪ c0)
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
    *   (c1 ∪ c3) ~ (c1 ∪ c3)
    *   (c1 ∪ c3) ~ (c1 ∪ c3)
    * }}}
    *
    * We return the new equation system:
    *
    * {{{
    *   (c1 ∪ c3) ~ (c1 ∪ c3)
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
  private def unifyAllPickSmallest(l: List[Equation]): BoolSubstitution = {
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
    case ex: SetUnificationException => println(ex.t); throw ConflictException(e.t1, e.t2, e.loc)
  }

  /**
    * The Successive Variable Elimination algorithm.
    *
    * Computes the most-general unifier of the given term `t ~ empty` where `fvs` is the list of free variables in `t`.
    *
    * Eliminates variables one-by-one from the given list `fvs`.
    *
    * Throws a [[SetUnificationException]] if there is no solution.
    */
  private def successiveVariableElimination(t: Term, fvs: List[Int]): BoolSubstitution = fvs match {
    case Nil =>
      // we want to know if `t = empty`.
      // `t` contains unknown constants, so we cannot just evaluate it.
      // we instead try all values of those constants (flexify makes them variables)
      // forall v: v(t) = empty (by straight forward evaluation)
      if (emptyEquivalent(flexify(t)))
        BoolSubstitution.empty
      else
        throw SetUnificationException(t)

    case x :: xs =>
      val t0 = BoolSubstitution.singleton(x, Term.Empty)(t)
      val t1 = BoolSubstitution.singleton(x, Term.Univ)(t)
      val se = successiveVariableElimination(booleanAbsorption(Term.mkInter(t0, t1)), xs)

      val f1 = booleanAbsorption(Term.mkUnion(se(t0), Term.mkMinus(Term.Var(x), se(t1))))
      val st = BoolSubstitution.singleton(x, f1)
      st ++ se
  }

  /**
    * Returns `t` where every constant has been made into a variable.
    *
    * Note: We use the constant name as the variable. This assume that constants and variables are disjoint.
    */
  private def flexify(t: Term): Term = t match {
    case Term.Univ => Term.Univ
    case Term.Empty => Term.Empty
    case Term.Cst(c) => Term.Var(c) // We use the constant name as the variable name.
    case Term.Elem(i) => Term.Elem(i)
    case Term.Var(x) => Term.Var(x)
    case Term.Compl(t) => Term.mkCompl(flexify(t))
    case Term.Union(posCsts, negCsts, posVars, negVars, rest) =>
      // Translate every constant into a variable:
      Term.mkUnion(posCsts.map(c => Term.Var(c.c)).toList ++ negCsts.map(c => Term.Compl(Term.Var(c.c))) ++ posVars ++ negVars.map(Term.Compl(_)) ++ rest.map(flexify))
    case Term.Inter(posCsts, negCsts, posVars, negVars, rest) =>
      // Translate every constant into a variable:
      Term.mkInter(posCsts.map(c => Term.Var(c.c)).toList ++ negCsts.map(c => Term.Compl(Term.Var(c.c))) ++ posVars ++ negVars.map(Term.Compl(_)) ++ rest.map(flexify))
  }

  /**
    * Returns `true` for the given term `t` if all assignments to its free variables makes the term
    * evaluate to universe.
    */
  private def emptyEquivalent(t: Term): Boolean = t match {
    case Term.Empty => true
    case Term.Univ => false
    case Term.Var(_) => false
    case _ => evaluateAll(t, t.freeVars.toList, SortedSet.empty)
  }

  /**
    * Returns `true` if `t` always evaluates to empty under the assumption that `fvs` are the free variables in `t`
    * (which can be assigned freely) and that `univVars` are variables assigned the UNIV value.
    *
    * For each variable `x`, the function recursively explores both when `x` is UNIV and when `x` is EMPTY.
    */
  private def evaluateAll(t: Term, fvs: List[Int], univVars: SortedSet[Int]): Boolean = fvs match {
    case Nil =>
      // All variables are bound. Compute the truth value.
      evaluate(t, univVars).isEmpty
    case x :: xs =>
      // Recurse on two cases: x = true and x = false.
      evaluateAll(t, xs, univVars + x) && evaluateAll(t, xs, univVars)
  }

  /**
    * Returns the evaluation of `t` assuming that it is without constants.
    */
  private def evaluate(t: Term, univVars: SortedSet[Int]): SetEval = t match {
    case Term.Univ => SetEval.univ
    case Term.Empty => SetEval.empty
    case Term.Cst(_) => ??? // unreachable
    case Term.Elem(i) => SetEval.single(i)
    case Term.Var(x) => if (univVars.contains(x)) SetEval.univ else SetEval.empty
    case Term.Compl(t) => evaluate(t, univVars).compl()

    case Term.Union(posCsts, negCsts, posVars, negVars, rest) =>
      assert(posCsts.isEmpty)
      assert(negCsts.isEmpty)
      // We must pay attention to performance here.

      // We first check if there is a variable x that is UNIV.
      for (x <- posVars.map(_.x)) {
        if (univVars.contains(x)) {
          return SetEval.univ
        }
      }
      for (x <- negVars.map(_.x)) {
        if (!univVars.contains(x)) {
          return SetEval.univ
        }
      }

      // All vars were empty. We evaluate each sub-term.
      var running = SetEval.empty
      for (t0 <- rest) {
        running = running.union(evaluate(t0, univVars))
        // exit early for UNIV
        if (running.isUniv) return running
      }
      running

    case Term.Inter(posCsts, negCsts, posVars, negVars, rest) =>
      assert(posCsts.isEmpty)
      assert(negCsts.isEmpty)
      // We must pay attention to performance here.

      // We first check if there is a variable x that is EMPTY.
      for (x <- posVars.map(_.x)) {
        if (!univVars.contains(x)) {
          return SetEval.empty
        }
      }
      for (x <- negVars.map(_.x)) {
        if (univVars.contains(x)) {
          return SetEval.empty
        }
      }

      // All vars were empty. We evaluate each sub-term.
      var running = SetEval.univ
      for (t0 <- rest) {
        running = running.intersect(evaluate(t0, univVars))
        // exit early for EMPTY
        if (running.isEmpty) return running
      }
      running

  }

  sealed trait SetEval {
    import SetEval.{Set,  Compl}
    def union(s: SetEval): SetEval = (this, s) match {
      case (Set(s1), Set(s2)) => Set(s1.union(s2))
      case (Set(s1), Compl(s2)) =>
        // s1 u ^s2
        Compl(s1.diff(s2))
      case (Compl(s1), Set(s2)) =>
        // s2 u ^s1
        Compl(s2.diff(s1))
      case (Compl(s1), Compl(s2)) =>
        // ^s1 u ^s2
        Compl(s1.intersect(s2))
    }
    def intersect(s: SetEval): SetEval = (this, s) match {
      case (Set(s1), Set(s2)) => Set(s1.intersect(s2))
      case (Set(s1), Compl(s2)) =>
        // s1 inter ^s2
        Set(s1.diff(s2))
      case (Compl(s1), Set(s2)) =>
        // s2 inter ^s1
        Set(s2.diff(s1))
      case (Compl(s1), Compl(s2)) =>
        // ^s1 inter ^s2
      Compl(s1.union(s2))
    }
    def compl(): SetEval = this match {
      case Set(s) =>
        Compl(s)
      case Compl(s) =>
        // U - (U - s) = s
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


  /**
    * Perform intersection-propagation on the given `term`.
    *
    * We use intersection-propagation to simplify terms during the Successive Variable Elimination (SVE) algorithm.
    *
    * For example, the term:
    *
    * {{{
    *   x1 inter x2 inter not (x7 inter x9 inter x1)
    * }}}
    *
    * is simplified to the term:
    *
    * {{{
    *   x1 inter x2 inter not (x7 inter x9 inter UNIV)
    * }}}
    *
    * The idea is that
    * x inter f(x) = x inter f(Univ)
    * ^x inter f(x) = ^x inter f(Empty)
    */
  private def booleanAbsorption(t: Term): Term = {
    def visit(t: Term, univCsts: SortedSet[Int], emptyCsts: SortedSet[Int], univVars: SortedSet[Int], emptyVars: SortedSet[Int]): Term = t match {
      case Term.Univ => Term.Univ
      case Term.Empty => Term.Empty
      case Term.Cst(c) => if (univCsts.contains(c)) Term.Univ else if (emptyCsts.contains(c)) Term.Empty else Term.Cst(c)
      case Term.Elem(i) => Term.Elem(i)
      case Term.Var(x) => if (univVars.contains(x)) Term.Univ else if (emptyVars.contains(x)) Term.Empty else Term.Var(x)
      case Term.Compl(t0) => Term.mkCompl(visit(t0, univCsts, emptyCsts, univVars, emptyVars))
      case Term.Inter(posCsts0, negCsts0, posVars0, negVars0, rest0) =>
        // Compute the constants and variables that _must_ hold for the whole conjunction to hold.
        val posTermCsts = posCsts0.map(_.c)
        val negTermCsts = negCsts0.map(_.c)
        val posTermVars = posVars0.map(_.x)
        val negTermVars = negVars0.map(_.x)

        // Extend trueCsts and trueVars.
        val currentPosCsts = univCsts ++ posTermCsts
        val currentNegCsts = emptyCsts ++ negTermCsts
        val currentPosVars = univVars ++ posTermVars
        val currentNegVars = emptyVars ++ negTermVars

        // Recurse on the sub-terms with the updated maps.
        val rest = rest0.collect {
          case t: Term.Compl => visit(t, currentPosCsts, currentNegCsts, currentPosVars, currentNegVars)
          case t: Term.Inter => visit(t, currentPosCsts, currentNegCsts, currentPosVars, currentNegVars)
          case t: Term.Union => visit(t, currentPosCsts, currentNegCsts, currentPosVars, currentNegVars)
          case t: Term.Elem => visit(t, currentPosCsts, currentNegCsts, currentPosVars, currentNegVars)
          case _: Term.Cst =>
            // Cannot happen because the invariant of [[Term.mkAnd]] ensures there are no constants in `rest`.
            throw InternalCompilerException("Unexpected constant", SourceLocation.Unknown)
          case _: Term.Var =>
            // Cannot happen because the invariant of [[Term.mkAnd]] ensures there are no variables in `rest`.
            throw InternalCompilerException("Unexpected variable", SourceLocation.Unknown)
        }

        // Compute new constant and variable sets by removing constants and variables that hold.
        val posCsts = posTermCsts -- univCsts
        val negCsts = negTermCsts -- emptyCsts
        val posVars = posTermVars -- univVars
        val negVars = negTermVars -- emptyVars

        // Recompose the conjunction. We use the smart constructor because some sets may have become empty.
        Term.mkInter(Term.Inter(posCsts.map(Term.Cst), negCsts.map(Term.Cst), posVars.map(Term.Var), negVars.map(Term.Var), Nil) :: rest)

      case Term.Union(posCsts0, negCsts0, posVars0, negVars0, rest0) =>
        // Compute the constants and variables that _must_ hold for the whole conjunction to hold.
        val posTermCsts = posCsts0.map(_.c)
        val negTermCsts = negCsts0.map(_.c)
        val posTermVars = posVars0.map(_.x)
        val negTermVars = negVars0.map(_.x)

        // Extend trueCsts and trueVars.
        val currentPosCsts = univCsts ++ negTermCsts
        val currentNegCsts = emptyCsts ++ posTermCsts
        val currentPosVars = univVars ++ negTermVars
        val currentNegVars = emptyVars ++ posTermVars

        // Recurse on the sub-terms with the updated maps.
        val rest = rest0.collect {
          case t: Term.Compl => visit(t, currentPosCsts, currentNegCsts, currentPosVars, currentNegVars)
          case t: Term.Inter => visit(t, currentPosCsts, currentNegCsts, currentPosVars, currentNegVars)
          case t: Term.Union => visit(t, currentPosCsts, currentNegCsts, currentPosVars, currentNegVars)
          case t: Term.Elem => visit(t, currentPosCsts, currentNegCsts, currentPosVars, currentNegVars)
          case _: Term.Cst =>
            // Cannot happen because the invariant of [[Term.mkAnd]] ensures there are no constants in `rest`.
            throw InternalCompilerException("Unexpected constant", SourceLocation.Unknown)
          case _: Term.Var =>
            // Cannot happen because the invariant of [[Term.mkAnd]] ensures there are no variables in `rest`.
            throw InternalCompilerException("Unexpected variable", SourceLocation.Unknown)
        }

        // Compute new constant and variable sets by removing constants and variables that hold.
        val posCsts = posTermCsts -- emptyCsts
        val negCsts = negTermCsts -- univCsts
        val posVars = posTermVars -- emptyVars
        val negVars = negTermVars -- univVars

        // Recompose the conjunction. We use the smart constructor because some sets may have become empty.
        Term.mkUnion(Term.Union(posCsts.map(Term.Cst), negCsts.map(Term.Cst), posVars.map(Term.Var), negVars.map(Term.Var), Nil) :: rest)
    }

    visit(t, SortedSet.empty, SortedSet.empty, SortedSet.empty, SortedSet.empty)
  }

  def boundAbsorption(t: Term): Term = {
    def visit(t: Term, atMostPosCsts: Set[Term.Cst], atMostNegCsts: Set[Term.Cst], atLeastPosCsts: Set[Term.Cst], atLeastNegCsts: Set[Term.Cst], atMostPosVars: Set[Term.Var], atMostNegVars: Set[Term.Var], atLeastPosVars: Set[Term.Var], atLeastNegVars: Set[Term.Var]): Term = t match {
      case Term.Univ => t
      case Term.Cst(_) => t
      case Term.Elem(_) => t
      case Term.Var(_) => t
      case Term.Compl(_) => t // TODO
      case Term.Union(posCsts0, negCsts0, posVars0, negVars0, rest0) =>
        // CASE: cst union (... union !cst union ... ) == cst union U
        if (atLeastPosCsts.intersect(negCsts0).nonEmpty) return Term.Univ
        // Case: !cst union (... union cst union ...) == cst union U
        if (atLeastNegCsts.intersect(posCsts0).nonEmpty) return Term.Univ
        // CASE: cst inter (... union cst union ... ) == cst inter cst
        val squashedPosCsts = atMostPosCsts.intersect(posCsts0)
        if (squashedPosCsts.nonEmpty) return squashedPosCsts.head
        // CASE: !cst inter (... union !cst union ... ) == !cst inter !cst
        val squashedNegCsts = atMostNegCsts.intersect(negCsts0)
        if (squashedNegCsts.nonEmpty) return squashedNegCsts.head
        // CASE: cst union (... union cst union ...) == cst union (... ...)
        // CASE: !cst inter (... union cst union ...) == !cst inter (... ...)
        val posCsts = posCsts0.diff(atLeastPosCsts).diff(atMostNegCsts)
        // CASE: !cst union (... union !cst union ...) == !cst union (... ...)
        // CASE: cst inter (... union !cst union ...) == cst inter (... ...)
        val negCsts = negCsts0.diff(atLeastNegCsts).diff(atMostPosCsts)
        // add to new lowerbounds
        val atLeastPosCsts1 = atLeastPosCsts.union(posCsts)
        val atLeastNegCsts1 = atLeastNegCsts.union(negCsts)

        // CASE: var union (... union !var union ... ) == var union U
        if (atLeastPosVars.intersect(negVars0).nonEmpty) return Term.Univ
        // Case: !var union (... union var union ...) == var union U
        if (atLeastNegVars.intersect(posVars0).nonEmpty) return Term.Univ
        // CASE: var inter (... union var union ... ) == var inter var
        val squashedPosVars = atMostPosVars.intersect(posVars0)
        if (squashedPosVars.nonEmpty) return squashedPosVars.head
        // CASE: !var inter (... union !var union ... ) == !var inter !var
        val squashedNegVars = atMostNegVars.intersect(negVars0)
        if (squashedNegVars.nonEmpty) return squashedNegVars.head
        // CASE: var union (... union var union ...) == var union (... ...)
        // CASE: !var inter (... union var union ...) == !var inter (... ...)
        val posVars = posVars0.diff(atLeastPosVars).diff(atMostNegVars)
        // CASE: !var union (... union !var union ...) == !var union (... ...)
        // CASE: var inter (... union !var union ...) == var inter (... ...)
        val negVars = negVars0.diff(atLeastNegVars).diff(atMostPosVars)
        // add to new lowerbounds
        val atLeastPosVars1 = atLeastPosVars.union(posVars)
        val atLeastNegVars1 = atLeastNegVars.union(negVars)

        val rest = rest0.map(visit(_, atMostPosCsts, atMostNegCsts, atLeastPosCsts1, atLeastNegCsts1, atMostPosVars, atMostNegVars, atLeastPosVars1, atLeastNegVars1))
        Term.mkUnion(Term.Union(posCsts, negCsts, posVars, negVars, Nil) :: rest)

      case Term.Inter(posCsts0, negCsts0, posVars0, negVars0, rest0) => t
    }

    visit(t, Set.empty, Set.empty, Set.empty, Set.empty, Set.empty, Set.empty, Set.empty, Set.empty)
  }

  /**
    * Companion object for [[Equation]].
    */
  object Equation {
    /**
      * Returns a unification equation  `t1 ~ t2` between the terms `t1` and `t2`.
      *
      * The smart constructor performs normalization:
      * - We move true and false to the rhs.
      * - We move a single variable to the lhs.
      * - We reorder constant/variables so that the smaller constant/variable is on the lhs.
      *
      * Examples:
      * -     true ~ x7 ==> x7 ~ true
      * -       c3 ~ c2 ==> c2 ~ c3
      * -       x7 ~ x5 ==> x5 ~ x7
      * - x3 ∪ x7 ~ x4 ==> x4 ~ x3 ∪ x7
      */
    def mk(t1: Term, t2: Term, loc: SourceLocation): Equation = (t1, t2) match {
      case (Term.Cst(c1), Term.Cst(c2)) => if (c1 <= c2) Equation(t1, t2, loc) else Equation(t2, t1, loc)
      case (Term.Var(x1), Term.Var(x2)) => if (x1 <= x2) Equation(t1, t2, loc) else Equation(t2, t1, loc)
      case (Term.Univ, _) => Equation(t2, Term.Univ, loc)
      case (Term.Empty, _) => Equation(t2, Term.Empty, loc)
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

    final def union(t: Term): Term = Term.mkUnion(this, t)

    final def inter(t: Term): Term = Term.mkInter(this, t)

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
      case Term.Union(_, _, posVars, negVars, rest) => SortedSet.empty[Int] ++ posVars.map(_.x) ++ negVars.map(_.x) ++ rest.flatMap(_.freeVars)
      case Term.Inter(_, _, posVars, negVars, rest) => SortedSet.empty[Int] ++ posVars.map(_.x) ++ negVars.map(_.x) ++ rest.flatMap(_.freeVars)
    }

    /**
      * Returns the number of connectives in `this` term.
      *
      * For example, `size(x) = 0`, `size(x ∪ y) = 1`, and `size(x ∪ not y) = 2`.
      */
    final def size: Int = this match {
      case Term.Univ => 0
      case Term.Empty => 0
      case Term.Cst(_) => 0
      case Term.Elem(_) => 0
      case Term.Var(_) => 0
      case Term.Compl(t) => t.size + 1
      case Term.Union(posCsts, negCsts, posVars, negVars, rest) =>
        // We need a connective for each constant, variable, and term minus one.
        // We then add the size of all the sub-terms in `rest`.
        ((posCsts.size + negCsts.size + posVars.size + negVars.size + rest.size) - 1) + rest.map(_.size).sum
      case Term.Inter(posCsts, negCsts, posVars, negVars, rest) =>
        ((posCsts.size + negCsts.size + posVars.size + negVars.size + rest.size) - 1) + rest.map(_.size).sum
    }

    /**
      * Returns a human-readable representation of `this` term.
      */
    override def toString: String = this match {
      case Term.Univ => formatter.red("U")
      case Term.Empty => formatter.red("Ø")
      case Term.Cst(c) => formatter.blue(s"c$c")
      case Term.Elem(i) => formatter.green(s"e$i")
      case Term.Var(x) => s"x$x"
      case Term.Compl(f) => f match {
        case Term.Var(x) => s"^x$x"
        case _ => s"^($f)"
      }
      case Term.Union(posCsts, negCsts, posVars, negVars, rest) => s"(${(posCsts.toList ++ negCsts.map(Term.Compl(_)).toList ++ posVars.toList ++ negVars.map(Term.Compl(_)).toList ++ rest).mkString(" ∪ ")})"
      case Term.Inter(posCsts, negCsts, posVars, negVars, rest) => s"(${(posCsts.toList ++ negCsts.map(Term.Compl(_)).toList ++ posVars.toList ++ negVars.map(Term.Compl(_)).toList ++ rest).mkString(" ∩ ")})"
    }

  }

  object Term {

    /**
      * The UNIV symbol (`U`).
      */
    case object Univ extends Term

    /**
      * The EMPTY term (`Ø`).
      */
    val Empty: Term = Union(Set.empty, Set.empty, Set.empty, Set.empty, Nil)

    /**
      * Represents an uninterpreted set constant ("rigid variable").
      *
      * Note: We assume that constants and variables are disjoint.
      */
    case class Cst(c: Int) extends Term

    /**
      * Represents an element of the set domain (like `Print`).
      */
    case class Elem(i: Int) extends Term

    /**
      * Represents a set variable ("flexible variable").
      *
      * Note: We assume that constants and variables are disjoint.
      */
    case class Var(x: Int) extends Term

    /**
      * Represents the complement of the term `t` (`^`).
      */
    case class Compl(t: Term) extends Term

    /**
      * Represents a union of terms (`∪`).
      *
      * We use a clever representation where we have a union of constants, variables, and then sub-terms.
      *
      * For example, the union: `x7 ∪ (^x2) ∪ c1 ∪ x4` is represented as: `Set(c1), Set(x4, x7), List(^x2)`.
      */
    case class Union(posCsts: Set[Term.Cst], negCsts: Set[Term.Cst], posVars: Set[Term.Var], negVars: Set[Term.Var], rest: List[Term]) extends Term {
      // We ensure that `rest` cannot contain constants and variables.
      // Once the code is better tested, we can remove these assertions.
      assert(!rest.exists(_.isInstanceOf[Term.Cst]))
      assert(!rest.exists(_.isInstanceOf[Term.Var]))
    }

    /**
      * Represents an intersection of terms (`∩`).
      *
      * We use a clever representation where we have a union of constants, variables, and then sub-terms.
      *
      * For example, the intersection: `x7 ∩ (^x2) ∩ c1 ∩ x4` is represented as: `Set(c1), Set(x4, x7), List(^x2)`.
      */
    case class Inter(posCsts: Set[Term.Cst], negCsts: Set[Term.Cst], posVars: Set[Term.Var], negVars: Set[Term.Var], rest: List[Term]) extends Term {
      // We ensure that `rest` cannot contain constants and variables.
      // Once the code is better tested, we can remove these assertions.
      assert(!rest.exists(_.isInstanceOf[Term.Cst]))
      assert(!rest.exists(_.isInstanceOf[Term.Var]))
    }

    /**
      * Smart constructor for complement (`^`).
      *
      * Rewrites complements deeply
      */
    final def mkCompl(t: Term): Term = t match {
      case Univ => Empty
      case Cst(c) => Compl(Cst(c))
      case Elem(i) => Compl(Elem(i))
      case Var(x) => Compl(Var(x))
      case Compl(t) => t
      case Union(posCsts, negCsts, posVars, negVars, rest) =>
        mkInter(Inter(negCsts, posCsts, negVars, posVars, Nil) :: rest.map(mkCompl))
      case Inter(posCsts, negCsts, posVars, negVars, rest) =>
        mkUnion(Union(negCsts, posCsts, negVars, posVars, Nil) :: rest.map(mkCompl))
    }

    /**
      * Smart constructor for union (`∪`).
      */
    final def mkUnion(t1: Term, t2: Term): Term = (t1, t2) match {
      case (Empty, _) => t2
      case (_, Empty) => t1
      case (Univ, _) => Univ
      case (_, Univ) => Univ
      case _ => mkUnion(List(t1, t2))
    }

    /**
      * Constructor for minus (`-`).
      *
      * `t1 - t2 == t1 ∩ ^t2`
      */
    final def mkMinus(t1: Term, t2: Term): Term = mkInter(t1, mkCompl(t2))

    /**
      * Smart constructor for intersection (`∩`).
      */
    final def mkInter(t1: Term, t2: Term): Term = (t1, t2) match {
      case (Univ, _) => t2
      case (_, Univ) => t1
      case (Empty, _) => Empty
      case (_, Empty) => Empty
      case _ => mkInter(List(t1, t2))
    }

    /**
      * Smart constructor for union.
      *
      * A lot of heavy lifting occurs here. In particular, we must partition `ts` into (a) constants, (b) variables,
      * and (c) other sub-terms. Moreover, we look into those sub-terms and flatten any unions we find within.
      */
    final def mkUnion(ts: List[Term]): Term = {
      // Mutable data structures to hold constants, variables, and other sub-terms.
      val posCstTerms = mutable.Set.empty[Term.Cst]
      val negCstTerms = mutable.Set.empty[Term.Cst]
      val posVarTerms = mutable.Set.empty[Term.Var]
      val negVarTerms = mutable.Set.empty[Term.Var]
      val restTerms = mutable.ListBuffer.empty[Term]
      for (t <- ts) {
        t match {
          case Empty => // NOP - We do not have to include Empty in a union.
          case Univ => return Univ // If the union contains UNIV then whole union is UNIV.
          case x@Term.Cst(_) => posCstTerms += x
          case Term.Compl(x@Term.Cst(_)) => negCstTerms += x
          case x@Term.Var(_) => posVarTerms += x
          case Term.Compl(x@Term.Var(_)) => negVarTerms += x
          case Union(posCsts0, negCsts0, posVars0, negVars0, rest0) =>
            // We have found a nested union. We can immediately add _its_ constants and variables.
            posCstTerms ++= posCsts0
            negCstTerms ++= negCsts0
            posVarTerms ++= posVars0
            negVarTerms ++= negVars0
            for (t0 <- rest0) {
              // We then iterate through the nested sub-terms of the nested union.
              t0 match {
                case Empty => // NOP - We do not have to include Empty in a union.
                case Univ => return Univ // If the sub-union contains UNIV then the whole union is UNIV.
                case x@Term.Cst(_) => posCstTerms += x
                case Term.Compl(x@Term.Cst(_)) => negCstTerms += x
                case x@Term.Var(_) => posVarTerms += x
                case Term.Compl(x@Term.Var(_)) => negVarTerms += x
                case _ => restTerms += t0
              }
            }
          case _ => restTerms += t // We found some other sub-term.
        }
      }

      val posCstTermsSet = posCstTerms.toSet
      val negCstTermsSet = negCstTerms.toSet
      if (posCstTermsSet.intersect(negCstTermsSet).nonEmpty) return Univ
      val posVarTermsSet = posVarTerms.toSet
      val negVarTermsSet = negVarTerms.toSet
      if (posVarTermsSet.intersect(negVarTermsSet).nonEmpty) return Univ

      // We now have a set of constants, a set of variables, and a list of sub-terms.
      // We optimize for the case where each of these is empty EXCEPT for one element.
      (posCstTerms.toList, negCstTerms.toList, posVarTerms.toList, negVarTerms.toList, restTerms.toList) match {
        case (Nil, Nil, Nil, Nil, Nil) => Term.Empty // Everything is empty, so we return the neutral element, i.e. EMPTY.
        case (List(c), Nil, Nil, Nil, Nil) => c
        case (Nil, List(x), Nil, Nil, Nil) => Term.Compl(x)
        case (Nil, Nil, List(x), Nil, Nil) => x
        case (Nil, Nil, Nil, List(x), Nil) => Term.Compl(x)
        case (Nil, Nil, Nil, Nil, List(t)) => t
        case _ => Union(posCstTermsSet, negCstTermsSet, posVarTermsSet, negVarTermsSet, restTerms.toList)
      }
    }

    /**
      * Smart constructor for intersection (∩).
      */
    final def mkInter(ts: List[Term]): Term = {
      // Mutable data structures to hold constants, variables, and other sub-terms.
      val posCstTerms = mutable.Set.empty[Term.Cst]
      val negCstTerms = mutable.Set.empty[Term.Cst]
      val posVarTerms = mutable.Set.empty[Term.Var]
      val negVarTerms = mutable.Set.empty[Term.Var]
      val restTerms = mutable.ListBuffer.empty[Term]
      for (t <- ts) {
        t match {
          case Univ => // NOP - We do not have to include UNIV in a union.
          case Empty => return Empty // If the union contains EMPTY then whole union is EMPTY.
          case x@Term.Cst(_) => posCstTerms += x
          case Term.Compl(x@Term.Cst(_)) => negCstTerms += x
          case x@Term.Var(_) => posVarTerms += x
          case Term.Compl(x@Term.Var(_)) => negVarTerms += x
          case Inter(posCsts0, negCsts0, posVars0, negVars0, rest0) =>
            // We have found a nested union. We can immediately add _its_ constants and variables.
            posCstTerms ++= posCsts0
            negCstTerms ++= negCsts0
            posVarTerms ++= posVars0
            negVarTerms ++= negVars0
            for (t0 <- rest0) {
              // We then iterate through the nested sub-terms of the nested union.
              t0 match {
                case Univ => // NOP - We do not have to include UNIV in a union.
                case Empty => return Empty // If the sub-union contains EMPTY then the whole union is EMPTY.
                case x@Term.Cst(_) => posCstTerms += x
                case Term.Compl(x@Term.Cst(_)) => negCstTerms += x
                case x@Term.Var(_) => posVarTerms += x
                case Term.Compl(x@Term.Var(_)) => negVarTerms += x
                case _ => restTerms += t0
              }
            }
          case _ => restTerms += t // We found some other sub-term.
        }
      }

      val posCstTermsSet = posCstTerms.toSet
      val negCstTermsSet = negCstTerms.toSet
      if (posCstTermsSet.intersect(negCstTermsSet).nonEmpty) return Empty
      val posVarTermsSet = posVarTerms.toSet
      val negVarTermsSet = negVarTerms.toSet
      if (posVarTermsSet.intersect(negVarTermsSet).nonEmpty) return Empty

      // We now have a set of constants, a set of variables, and a list of sub-terms.
      // We optimize for the case where each of these is empty EXCEPT for one element.
      (posCstTerms.toList, negCstTerms.toList, posVarTerms.toList, negVarTerms.toList, restTerms.toList) match {
        case (Nil, Nil, Nil, Nil, Nil) => Term.Univ // Everything is empty, so we return the neutral element, i.e. EMPTY.
        case (List(c), Nil, Nil, Nil, Nil) => c
        case (Nil, List(x), Nil, Nil, Nil) => Term.Compl(x)
        case (Nil, Nil, List(x), Nil, Nil) => x
        case (Nil, Nil, Nil, List(x), Nil) => Term.Compl(x)
        case (Nil, Nil, Nil, Nil, List(t)) => t
        case _ => Inter(posCstTermsSet, negCstTermsSet, posVarTermsSet, negVarTermsSet, restTerms.toList)
      }
    }

    /**
      * Returns the Xor of `x` and `y`. Implemented by desugaring.
      */
    final def mkXor(x: Term, y: Term): Term = mkUnion(mkMinus(x, y), mkMinus(y, x))

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
      case Term.Univ => Term.Univ
      case Term.Empty => Term.Empty
      case Term.Cst(c) => Term.Cst(c)
      case Term.Elem(i) => Term.Elem(i)

      case Term.Var(x) => m.get(x) match {
        case None => Term.Var(x) // Case 1: The substitution has a binding for `x`. Return the bound term.
        case Some(t0) => t0 // Case 2: The substitution has no binding for `x`. Return the original term.
      }

      case Term.Compl(t0) => Term.mkCompl(apply(t0))

      case Term.Union(posCsts, negCsts, posVars, negVars, rest) =>
        // A union is a sequence of: constants, variables, and terms. We know that the constants are unchanged by
        // the substitution. We know that some variables may become constants, variables, or terms. Since we do not want
        // to duplicate functionality from [[Term.mkUnion]], we simply apply the substitution to the variables and terms,
        // and put everything in one list. We then let [[Term.mkUnion]] reclassify all the sub-terms.
        val ts = posCsts.toList ++ negCsts.map(Term.Compl(_)).toList ++ posVars.toList.map(apply) ++ negVars.map(Term.Compl(_)).toList.map(apply) ++ rest.map(apply)
        Term.mkUnion(ts)

      case Term.Inter(posCsts, negCsts, posVars, negVars, rest) =>
        // see union
        val ts = posCsts.toList ++ negCsts.map(Term.Compl(_)).toList ++ posVars.toList.map(apply) ++ negVars.map(Term.Compl(_)).toList.map(apply) ++ rest.map(apply)
        Term.mkInter(ts)
    }

    /**
      * Applies `this` substitution to the given equation `e`.
      *
      * Intuitively, applies `this` to the lhs and rhs of `e` and reconstructs the equation.
      *
      * Applying the substitution and reconstructing the equation may "flip" the lhs and rhs. For example:
      *
      * If `s = [x -> y]` and `e = true ~ x and y` then `s(e) = y ~ true` which has flipped lhs and rhs.
      */
    def apply(e: Equation): Equation = e match {
      case Equation(t1, t2, loc) => Equation.mk(apply(t1), apply(t2), loc)
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
      // We want to check that `s(t1) == s(t2)`. In other words that both sides are either true or both sides are false.
      // If we can find a situation where one side is true and the other side is false then the equation does not hold.
      // We can look for such a situation by checking whether `s(t1) xor s(t2)` is satisfiable. If it is then we have
      // found an equation that is not solved.
      val t1 = s(e.t1)
      val t2 = s(e.t2)
      val query = Term.mkXor(t1, t2)
      if (!emptyEquivalent(flexify(query))) {
        println(s"  Original  : ${e.t1} ~ ${e.t2}")
        println(s"  with Subst: $t1 ~ $t2")
        throw InternalCompilerException(s"Incorrectly solved equation", SourceLocation.Unknown)
      }
    }
  }

  /**
    * A common super-type for exceptions throw by the solver.
    */
  sealed trait FastSetUnificationException extends RuntimeException

  /**
    * Represents a Boolean unification failure between the two terms: `x` and `y`.
    */
  case class ConflictException(x: Term, y: Term, loc: SourceLocation) extends FastSetUnificationException

  /**
    * Represents a solution that is too complex.
    *
    * @param msg the specific error message.
    */
  case class TooComplexException(msg: String) extends FastSetUnificationException

  case class SetUnificationException(t: Term) extends RuntimeException {
    override def toString: String = t.toString
  }

}
