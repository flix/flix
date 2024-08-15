/*
 *  Copyright 2024 Magnus Madsen
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
///   - We move true and false to the rhs.
///   - We move a single variable to the lhs.
///   - See the definition of `Equation.mk` for details.
/// - We progress in the following order:
///   1. We propagate ground terms (true/false/constant) in a fixpoint.
///   2. We propagate variables (i.e. resolving constraints of the form x = y).
///   3. We perform trivial assignments where the left-hand variables does not occur in the RHS.
///   4. We eliminate (now) trivial and redundant constraints.
///   5. We do full-blown Boolean unification with SVE.
/// - We represent a conjunction with n >= 2 terms.
///   - We group the terms into three: a set of constants, a set of variables, and a list of sub-terms.
///   - We flatten conjunctions at least one level per call to `mkAnd`.
///
/// In the future, we could:
/// - Explore change of basis.
/// - Explore use slack variables (i.e. variables marked don't care, and use SAT).
/// - Explore in detail how constraints are generated.
/// - Explore whether to run Phase 1-3 after _ONE_ SVE computation.
///
object FastBoolUnification {

  /**
    * The threshold for when a solution is considered too complex.
    *
    * The threshold is specified as an upper bound on the permitted connectives in a substitution.
    *
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
    *   - The current (pending) unification equations to solve.
    *   - The current (partial) substitution which represents the solution.
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
    *   -     true ~ true
    *   -    false ~ false
    *   -        c ~ c        (same constant)
    *   -        x ~ x        (same variable)
    *
    *
    * An unsolvable (conflicted) equation is one of:
    *
    *   -      true ~ false   (and mirrored)
    *   -       c_i ~ c_j     (different constants)
    *   -        c ~ true     (and mirrored)
    *   -        c ~ false    (and mirrored)
    */
  private def checkAndSimplify(l: List[Equation]): List[Equation] = l match {
    case Nil => Nil
    case Equation(t1, t2, loc) :: es => (t1, t2) match {
      // Trivial equations: skip them.
      case (Term.True, Term.True) => checkAndSimplify(es)
      case (Term.False, Term.False) => checkAndSimplify(es)
      case (Term.Cst(c1), Term.Cst(c2)) if c1 == c2 => checkAndSimplify(es)
      case (Term.Var(x1), Term.Var(x2)) if x1 == x2 => checkAndSimplify(es)

      // Unsolvable (conflicted) equations: raise an exception.
      case (Term.True, Term.False) => throw ConflictException(t1, t2, loc)
      case (Term.False, Term.True) => throw ConflictException(t1, t2, loc)
      case (Term.Cst(c1), Term.Cst(c2)) if c1 != c2 => throw ConflictException(t1, t2, loc)
      // Note: A constraint with two different variables is of course solvable!
      case (Term.Cst(_), Term.True) => throw ConflictException(t1, t2, loc)
      case (Term.Cst(_), Term.False) => throw ConflictException(t1, t2, loc)
      case (Term.True, Term.Cst(_)) => throw ConflictException(t1, t2, loc)
      case (Term.False, Term.Cst(_)) => throw ConflictException(t1, t2, loc)

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
    *   - `x ~ true` becomes `[x -> true]`.
    *   - `x ~ c` becomes `[x -> c]`.
    *   - `x /\ y /\ ... = true` becomes `[x -> true, y -> true, ...]`.
    *
    * For example, if the equation system is:
    *
    * {{{
    *     c1794221043 ~ (x55062 ∧ x55050 ∧ x55046 ∧ x55060 ∧ x55066 ∧ x55040 ∧ x55075 ∧ x55042 ∧ x55058 ∧ x55078)
    *     x55078 ~ x112431
    *     c1794221043 ~ x55040
    *     x55042 ~ x112433
    *     x55044 ~ x112437
    *     x55046 ~ (x112435 ∧ x55044)
    *     x55048 ~ true
    *     x55050 ~ (x112439 ∧ x55048)
    *     x55052 ~ x112443
    *     x55055 ~ true
    *     x55058 ~ (x55052 ∧ x112441 ∧ x55055)
    *     x55060 ~ x112446
    *     x55062 ~ x112448
    *     x55066 ~ true
    *     x55075 ~ x112453
    * }}}
    *
    * then after constant propagation it is:
    *
    * {{{
    *     c1794221043 ~ (c1794221043 ∧ x55062 ∧ x55050 ∧ x55046 ∧ x55060 ∧ x55075 ∧ x55042 ∧ x55058 ∧ x55078)
    *     x55078 ~ x112431
    *     x55042 ~ x112433
    *     x55044 ~ x112437
    *     x55046 ~ (x112435 ∧ x55044)
    *     x112439 ~ x55050
    *     x55052 ~ x112443
    *     x55058 ~ (x55052 ∧ x112441)
    *     x55060 ~ x112446
    *     x55062 ~ x112448
    *     x55075 ~ x112453
    * }}}
    *
    * with the substitution:
    *
    * {{{
    *     x55048 -> true
    *     x55055 -> true
    *     x55066 -> true
    *     x55040 -> c1794221043
    * }}}
    *
    * Note that several equations were simplified.
    *
    * Note: We do not propagate false. This extension can be added, if needed.
    *
    * Note: We use `subst.extended` to check for conflicts. For example, if we already know that `s = [x -> c17]` and we
    * learn that `x -> true` then we will try to extend s with the new binding which will raise a [[ConflictException]].
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
          // Case 1: x ~ true
          case Equation(Term.Var(x), Term.True, loc) =>
            subst = subst.extended(x, Term.True, loc) // Note: the extended function will check that `x` is not already mapped to another constant.
            changed = true

          // Case 2: x ~ c
          case Equation(Term.Var(x), Term.Cst(c), loc) =>
            subst = subst.extended(x, Term.Cst(c), loc) // Note: the extended function will check that `x` is not already mapped to another constant.
            changed = true

          // Case 3: x /\ y /\ z /\... ~ true
          case Equation(Term.And(csts, vars, rest), Term.True, loc) if csts.isEmpty && rest.isEmpty =>
            for (Term.Var(x) <- vars) {
              subst = subst.extended(x, Term.True, loc) // Note: the extended function will check that `x` is not already mapped to another constant.
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
    *     x78914 ~ (c1498 ∧ c1500 ∧ c1501)
    *     x78914 ~ (x78926 ∧ x78923 ∧ x78917)
    *     x78917 ~ x127244
    *     x78921 ~ x127251
    *     x78923 ~ (x127249 ∧ x127247 ∧ x127248)
    *     x78926 ~ (x127254 ∧ x127252)
    * }}}
    *
    * then after variable propagation it is:
    *
    * {{{
    *     x78914 ~ (c1498 ∧ c1500 ∧ c1501)
    *     x78914 ~ (x78926 ∧ x78923 ∧ x127244)
    *     x78923 ~ (x127249 ∧ x127247 ∧ x127248)
    *     x78926 ~ (x127254 ∧ x127252)
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
    *    x78914 ~ (c1498 ∧ c1500 ∧ c1501)
    *    x78914 ~ (x78926 ∧ x78923 ∧ x127244)
    *    x78923 ~ (x127249 ∧ x127247 ∧ x127248)
    *    x78926 ~ (x127254 ∧ x127252)
    * }}}
    *
    * We compute the substitution:
    *
    * {{{
    *     x78926 -> (x127254 ∧ x127252)
    *     x78914 -> (c1498 ∧ c1500 ∧ c1501)
    *     x78923 -> (x127249 ∧ x127247 ∧ x127248)
    * }}}
    *
    * and we have the remaining equations:
    *
    * {{{
    *     (c1498 ∧ c1500 ∧ c1501) ~ (x127248 ∧ x127244 ∧ x127254 ∧ x127252 ∧ x127249 ∧ x127247)
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
    *   (c9 ∧ c0) ~ (c9 ∧ c0)
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
    *   (c1 ∧ c3) ~ (c1 ∧ c3)
    *   (c1 ∧ c3) ~ (c1 ∧ c3)
    * }}}
    *
    * We return the new equation system:
    *
    * {{{
    *   (c1 ∧ c3) ~ (c1 ∧ c3)
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
    // The boolean expression we want to show is false.
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
    * Computes the most-general unifier of the given term `t ~ false` where `fvs` is the list of free variables in `t`.
    *
    * Eliminates variables one-by-one from the given list `fvs`.
    *
    * Throws a [[BoolUnificationException]] if there is no solution.
    */
  private def successiveVariableElimination(t: Term, fvs: List[Int]): BoolSubstitution = fvs match {
    case Nil =>
      // Determine if f is unsatisfiable when all constants are made into (flexible) variables.
      if (!satisfiable(flexify(t)))
        BoolSubstitution.empty
      else
        throw BoolUnificationException()

    case x :: xs =>
      val t0 = BoolSubstitution.singleton(x, Term.False)(t)
      val t1 = BoolSubstitution.singleton(x, Term.True)(t)
      val se = successiveVariableElimination(propagateAnd(Term.mkAnd(t0, t1)), xs)

      val f1 = propagateAnd(Term.mkOr(se(t0), Term.mkAnd(Term.Var(x), Term.mkNot(se(t1)))))
      val st = BoolSubstitution.singleton(x, f1)
      st ++ se
  }

  /**
    * Returns `t` where every constant has been made into a variable.
    *
    * Note: We use the constant name as the variable. This assume that constants and variables are disjoint.
    */
  private def flexify(t: Term): Term = t match {
    case Term.True => Term.True
    case Term.False => Term.False
    case Term.Cst(c) => Term.Var(c) // We use the constant name as the variable name.
    case Term.Var(x) => Term.Var(x)
    case Term.Not(t) => Term.mkNot(flexify(t))
    case Term.And(csts, vars, rest) =>
      // Translate every constant into a variable:
      Term.mkAnd(csts.map(c => Term.Var(c.c)).toList ++ vars ++ rest.map(flexify))
    case Term.Or(ts) => Term.mkOr(ts.map(flexify))
  }

  /**
    * Returns `true` if the given term `t` is satisfiable, i.e. if there is an assignment to its free variables that
    * makes the whole term evaluate to true.
    *
    * Note that a constant can never be satisfied because we do not know if it is true or false.
    *
    * A smarter implementation would use a full-blown SAT solver, but since this function is rarely called and typically
    * called with a small term, we use a very naive implementation.
    */
  private def satisfiable(t: Term): Boolean = t match {
    case Term.True => true // A true term is already satisfied.
    case Term.Var(_) => true // A variable is trivially satisfiable.
    case Term.False => false // A false term can never be satisfied.
    case _ => evaluateAll(t, t.freeVars.toList, SortedSet.empty) // Evaluate t on all its valuations.
  }

  /**
    * Returns `true` if `t` can evaluate to `true` under the assumption that `fvs` are the free variables in `t`
    * (which can be assigned freely) and that `trueVars` are variables assigned the TRUE value.
    *
    * Evaluates the given term `t` under all valuations of the free variables `fvs` where `trueVars` are assumed to be true.
    *
    * For each variable `x`, the function recursively explores both when `x` is true and when `x` is false.
    */
  private def evaluateAll(t: Term, fvs: List[Int], trueVars: SortedSet[Int]): Boolean = fvs match {
    case Nil =>
      // All variables are bound. Compute the truth value.
      evaluate(t, trueVars)
    case x :: xs =>
      // Recurse on two cases: x = true and x = false.
      evaluateAll(t, xs, trueVars + x) || evaluateAll(t, xs, trueVars)
  }

  /**
    * Returns `true` if the given term `t` evaluates to true when all variables in `trueVars` are true and all other variables are false.
    */
  private def evaluate(t: Term, trueVars: SortedSet[Int]): Boolean = t match {
    case Term.True => true
    case Term.False => false
    case Term.Cst(_) => false
    case Term.Var(x) => trueVars.contains(x)
    case Term.Not(t) => !evaluate(t, trueVars)

    case Term.And(csts, vars, rest) =>
      if (csts.nonEmpty) {
        // Case 1: If there is a constant then the term may be unsatisfiable.
        false
      } else {
        // Case 2: We know that csts is empty.
        // We must pay attention to performance here.

        // We first check if there is a variable x that is false (i.e. an x not in trueVars).
        for (x <- vars.map(_.x)) {
          if (!trueVars.contains(x)) {
            return false
          }
        }

        // All vars were true. We evaluate each sub-term until we find one that is false.
        for (t0 <- rest) {
          if (!evaluate(t0, trueVars)) {
            return false
          }
        }

        // All variables and sub-terms were true, return true.
        true
      }

    case Term.Or(ts) =>
      for (t0 <- ts) {
        if (evaluate(t0, trueVars)) {
          return true
        }
      }
      // All sub-terms were false, return false.
      false

  }

  /**
    * Perform AND-propagation on the given `term`.
    *
    * We use AND-propagation to simplify terms during the Successive Variable Elimination (SVE) algorithm.
    *
    * For example, the term:
    *
    * {{{
    *   x1 /\ x2 /\ not (x7 /\ x9 /\ x1)
    * }}}
    *
    * is simplified to the term:
    *
    * {{{
    *   x1 /\ x2 /\ not (x7 /\ x9 /\ TRUE)
    * }}}
    *
    * The idea is that since x1 (and x2) must hold for the entire conjunction to be TRUE they can be removed from the sub-term.
    */
  private def propagateAnd(t: Term): Term = {
    // Simplifies the given term `t` assuming that `trueCsts` and `trueVars` are all TRUE.
    def visit(t: Term, trueCsts: SortedSet[Int], trueVars: SortedSet[Int]): Term = t match {
      case Term.True => Term.True
      case Term.False => Term.False
      case Term.Cst(c) => if (trueCsts.contains(c)) Term.True else Term.Cst(c) // `c` holds so we can replace it by TRUE.
      case Term.Var(x) => if (trueVars.contains(x)) Term.True else Term.Var(x) // `x` holds so we can replace it by TRUE.
      case Term.Not(t0) => Term.mkNot(visit(t0, trueCsts, trueVars))
      case Term.And(csts0, vars0, rest0) =>
        // Compute the constants and variables that _must_ hold for the whole conjunction to hold.
        val termCsts = csts0.map(_.c)
        val termVars = vars0.map(_.x)

        // Extend trueCsts and trueVars.
        val currentCsts = trueCsts ++ termCsts
        val currentVars = trueVars ++ termVars

        // Recurse on the sub-terms with the updated maps.
        val rest = rest0.collect {
          case t: Term.Not => visit(t, currentCsts, currentVars)
          case t: Term.And => visit(t, currentCsts, currentVars)
          case t: Term.Or => visit(t, currentCsts, currentVars)
          case _: Term.Cst =>
            // Cannot happen because the invariant of [[Term.mkAnd]] ensures there are no constants in `rest`.
            throw InternalCompilerException("Unexpected constant", SourceLocation.Unknown)
          case _: Term.Var =>
            // Cannot happen because the invariant of [[Term.mkAnd]] ensures there are no variables in `rest`.
            throw InternalCompilerException("Unexpected variable", SourceLocation.Unknown)
        }

        // Compute new constant and variable sets by removing constants and variables that hold.
        val csts = termCsts -- trueCsts
        val vars = termVars -- trueVars

        // Recompose the conjunction. We use the smart constructor because some sets may have become empty.
        Term.mkAnd(csts.toList.map(Term.Cst) ++ vars.toList.map(Term.Var) ++ rest)

      case Term.Or(ts) => Term.mkOr(ts.map(visit(_, trueCsts, trueVars)))
    }

    visit(t, SortedSet.empty, SortedSet.empty)
  }

  /**
    * Companion object for [[Equation]].
    */
  object Equation {
    /**
      * Returns a unification equation  `t1 ~ t2` between the terms `t1` and `t2`.
      *
      * The smart constructor performs normalization:
      *   - We move true and false to the rhs.
      *   - We move a single variable to the lhs.
      *   - We reorder constant/variables so that the smaller constant/variable is on the lhs.
      *
      * Examples:
      *   -     true ~ x7 ==> x7 ~ true
      *   -       c3 ~ c2 ==> c2 ~ c3
      *   -       x7 ~ x5 ==> x5 ~ x7
      *   - x3 /\ x7 ~ x4 ==> x4 ~ x3 /\ x7
      */
    def mk(t1: Term, t2: Term, loc: SourceLocation): Equation = (t1, t2) match {
      case (Term.Cst(c1), Term.Cst(c2)) => if (c1 <= c2) Equation(t1, t2, loc) else Equation(t2, t1, loc)
      case (Term.Var(x1), Term.Var(x2)) => if (x1 <= x2) Equation(t1, t2, loc) else Equation(t2, t1, loc)
      case (Term.True, _) => Equation(t2, Term.True, loc)
      case (Term.False, _) => Equation(t2, Term.False, loc)
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
      * Syntactic sugar for [[Term.mkAnd]].
      */
    final def &(that: Term): Term = Term.mkAnd(this, that)

    /**
      * Syntactic sugar for [[Equation.mk]]
      */
    final def ~(that: Term)(implicit loc: SourceLocation): Equation = Equation.mk(this, that, loc)

    /**
      * Returns all variables that occur in `this` term.
      */
    final def freeVars: SortedSet[Int] = this match {
      case Term.True => SortedSet.empty
      case Term.False => SortedSet.empty
      case Term.Cst(_) => SortedSet.empty
      case Term.Var(x) => SortedSet(x)
      case Term.Not(t) => t.freeVars

      case Term.And(_, vars, rest) => SortedSet.empty[Int] ++ vars.map(_.x) ++ rest.flatMap(_.freeVars)

      case Term.Or(ts) => ts.foldLeft(SortedSet.empty[Int])(_ ++ _.freeVars)
    }

    /**
      * Returns the number of connectives in `this` term.
      *
      * For example, `size(x) = 0`, `size(x /\ y) = 1`, and `size(x /\ not y) = 2`.
      */
    final def size: Int = this match {
      case Term.True => 0
      case Term.False => 0
      case Term.Cst(_) => 0
      case Term.Var(_) => 0
      case Term.Not(t) => t.size + 1
      case Term.And(csts, vars, rest) =>
        // We need a connective for each constant, variable, and term minus one.
        // We then add the size of all the sub-terms in `rest`.
        ((csts.size + vars.size + rest.size) - 1) + rest.map(_.size).sum
      case Term.Or(ts) => ts.map(_.size).sum + (ts.length - 1)
    }

    /**
      * Returns a human-readable representation of `this` term.
      */
    override def toString: String = this match {
      case Term.True => formatter.red("true")
      case Term.False => formatter.red("false")
      case Term.Cst(c) => formatter.blue(s"c$c")
      case Term.Var(x) => s"x$x"
      case Term.Not(f) => f match {
        case Term.Var(x) => s"¬x$x"
        case _ => s"¬($f)"
      }
      case Term.And(csts, vars, rest) => s"(${(csts.toList ++ vars.toList ++ rest).mkString(" ∧ ")})"
      case Term.Or(ts) => s"(${ts.mkString(" ∨ ")})"
    }

  }

  object Term {

    /**
      * The TRUE symbol.
      */
    case object True extends Term

    /**
      * The FALSE symbol.
      */
    case object False extends Term

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

    /**
      * Represents the negation of the term `t`.
      */
    case class Not(t: Term) extends Term

    /**
      * Represents a conjunction of terms.
      *
      * We use a clever representation where we have a conjunction of constants, variables, and then sub-terms.
      *
      * For example, the conjunction: `x7 /\ (not x2) /\ c1 /\ x4` is represented as: `Set(c1), Set(x4, x7), List((not x2))`.
      *
      * This representation is key to efficiency because the equations we solve are heavy on conjunctions.
      */
    case class And(csts: Set[Term.Cst], vars: Set[Term.Var], rest: List[Term]) extends Term {
      // We ensure that `rest` cannot contain constants and variables.
      // Once the code is better tested, we can remove these assertions.
      assert(!rest.exists(_.isInstanceOf[Term.Cst]))
      assert(!rest.exists(_.isInstanceOf[Term.Var]))
    }

    /**
      * A disjunction of the terms `ts`.
      *
      * Note: We do not use currently use any clever representation of disjunctions (because there has been no need).
      */
    case class Or(ts: List[Term]) extends Term {
      assert(ts.length >= 2)
    }

    /**
      * Smart constructor for negation.
      */
    final def mkNot(t: Term): Term = t match {
      case True => False
      case False => True
      case Not(t0) => t0
      case _ => Not(t)
    }

    /**
      * Smart constructor for conjunction.
      */
    final def mkAnd(t1: Term, t2: Term): Term = (t1, t2) match {
      case (False, _) => False
      case (_, False) => False
      case (True, _) => t2
      case (_, True) => t1
      case _ => mkAnd(List(t1, t2))
    }

    /**
      * Smart constructor for disjunction.
      */
    final def mkOr(t1: Term, t2: Term): Term = (t1, t2) match {
      case (True, _) => True
      case (_, True) => True
      case (False, _) => t2
      case (_, False) => t1
      case _ => mkOr(List(t1, t2))
    }

    /**
      * Smart constructor for conjunction.
      *
      * A lot of heavy lifting occurs here. In particular, we must partition `ts` into (a) constants, (b) variables,
      * and (c) other sub-terms. Moreover, we look into those sub-terms and flatten any conjunctions we find within.
      */
    final def mkAnd(ts: List[Term]): Term = {
      // Mutable data structures to hold constants, variables, and other sub-terms.
      val cstTerms = mutable.Set.empty[Term.Cst]
      val varTerms = mutable.Set.empty[Term.Var]
      val restTerms = mutable.ListBuffer.empty[Term]
      for (t <- ts) {
        t match {
          case True => // NOP - We do not have to include True in a conjunction.
          case False => return False // If the conjunction contains FALSE then whole conjunct is FALSE.
          case x@Term.Cst(_) => cstTerms += x
          case x@Term.Var(_) => varTerms += x
          case And(csts0, vars0, rest0) =>
            // We have found a nested conjunction. We can immediately add _its_ constants and variables.
            cstTerms ++= csts0
            varTerms ++= vars0
            for (t0 <- rest0) {
              // We then iterate through the nested sub-terms of the nested conjunction.
              t0 match {
                case True => // NOP - We do not have to include True in a conjunction.
                case False => return False // If the sub-conjunction contains FALSE then the whole conjunct is FALSE.
                case x@Term.Cst(_) => cstTerms += x
                case x@Term.Var(_) => varTerms += x
                case _ => restTerms += t0
              }
            }
          case _ => restTerms += t // We found some other sub-term.
        }
      }

      // We now have a set of constants, a set of variables, and a list of sub-terms.
      // We optimize for the case where each of these is empty EXCEPT for one element.
      (cstTerms.toList, varTerms.toList, restTerms.toList) match {
        case (Nil, Nil, Nil) => Term.True // Everything is empty, so we return the neutral element, i.e. TRUE.
        case (List(c), Nil, Nil) => c // A single constant.
        case (Nil, List(x), Nil) => x // A single variable.
        case (Nil, Nil, List(t)) => t // A single non-constant and non-variable sub-term.
        case _ => And(cstTerms.toSet, varTerms.toSet, restTerms.toList)
      }
    }

    /**
      * Smart constructor for disjunction.
      */
    final def mkOr(ts: List[Term]): Term = {
      // We refer to [[mkAnd]] since the structure is similar.

      val varTerms = mutable.Set.empty[Term]
      val nonVarTerms = mutable.ListBuffer.empty[Term]
      for (t <- ts) {
        t match {
          case True => return True
          case False => // nop
          case x@Term.Var(_) => varTerms += x
          case Or(ts0) =>
            for (t0 <- ts0) {
              t0 match {
                case True => return True
                case False => // nop
                case x@Term.Var(_) => varTerms += x
                case _ => nonVarTerms += t0
              }
            }
          case _ => nonVarTerms += t
        }
      }

      varTerms.toList ++ nonVarTerms.toList match {
        case Nil => False
        case x :: Nil => x
        case xs => Or(xs)
      }
    }

    /**
      * Returns the Xor of `x` and `y`. Implemented by desugaring.
      */
    final def mkXor(x: Term, y: Term): Term = mkOr(mkAnd(x, mkNot(y)), mkAnd(mkNot(x), y))

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
      case Term.True => Term.True
      case Term.False => Term.False
      case Term.Cst(c) => Term.Cst(c)

      case Term.Var(x) => m.get(x) match {
        case None => Term.Var(x) // Case 1: The substitution has a binding for `x`. Return the bound term.
        case Some(t0) => t0 // Case 2: The substitution has no binding for `x`. Return the original term.
      }

      case Term.Not(t0) => Term.mkNot(apply(t0))

      case Term.And(csts, vars, rest) =>
        // A conjunction is a sequence of: constants, variables, and terms. We know that the constants are unchanged by
        // the substitution. We know that some variables may become constants, variables, or terms. Since we do not want
        // to duplicate functionality from [[Term.mkAnd]], we simply apply the substitution to the variables and terms,
        // and put everything in one list. We then let [[Term.mkAnd]] reclassify all the sub-terms.
        val ts = csts.toList ++ vars.toList.map(apply) ++ rest.map(apply)
        Term.mkAnd(ts)

      case Term.Or(ts) => Term.mkOr(ts.map(apply))
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
      if (satisfiable(query)) {
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
