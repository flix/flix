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
import scala.collection.mutable.ListBuffer

///
/// Fast Type Inference with Systems of Boolean Unification Equations
///
/// Work smarter, not harder -- Proverb
///
/// A Fast Boolean Unification Solver based on the following ideas:
///
/// - We work on all the equations as one whole system.
/// - We progress in the following order:
///   1. We propagate ground terms in a fixpoint.
///   2. We propagate variables in a fixpoint.
///   3. We perform trivial assignments where the left-hand variables does not occur in the RHS.
///   4. We do full-blown Boolean unification with SVE.
/// - We represent a conjunction with n >= 2 terms.
///   - We group the terms into three: a set of constants, a set of variables, and a list of the rest.
///   - We flatten conjunctions at least one level per call to `mkAnd`.
///   - We apply the same idea for negation and disjunction.
/// - We normalize the representation of an equation t1 ~ t2.
///   - We try to move single variables to the lhs.
///   - We try to move ground terms to the RHS.
///
///
/// Future Ideas:
/// - Explore change of basis.
///
object FastBoolUnification {

  /**
    * The threshold for when a solution is considered too complex.
    *
    * If a solution is too complex an [[TooComplexException]] exception is thrown.
    */
  private val Threshold: Int = 1_000

  /**
    * Enable debugging (prints information during Boolean unification).
    */
  private val Debugging: Boolean = true

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
    *
    * If multiple equations are unsolvable the implementation makes no guarantee about which one is returned.
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
    * @param l The list of Boolean unification equations to solve.
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
      * The current substitution. Initially empty, but then grows during computation.
      */
    private var currentSubst: BoolSubstitution = BoolSubstitution.empty

    /**
      * Attempts to solve the equation system that this solver was instantiated with.
      *
      * Returns `Result.Ok(s)` with a most-general substitution which solves the equations system.
      *
      * Returns `Result.Err((ex, l, s))` which an exception `ex`, a list of pending (unsolved) equations, and a partial substitution `s`.
      */
    def solve(): Result[BoolSubstitution, (FastBoolUnificationException, List[Equation], BoolSubstitution)] = {
      try {
        phase0Init()
        phase1UnitPropagation()
        phase2VarPropagation()
        phase3VarAssignment()
        phase4SVE()
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
      debugln("--- Input")
      debugln("-".repeat(80))
      printEquations()
      debugln()
    }

    private def phase1UnitPropagation(): Unit = {
      debugln("-".repeat(80))
      debugln("--- Phase 1: Unit Propagation")
      debugln("    (resolves all equations of the form: x = c where x is a var and c is const)")
      debugln("-".repeat(80))
      val s = propagateUnit(currentEqns, currentSubst)
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
      val s = propagateVars(currentEqns, currentSubst)
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
      val s = varAssignment(currentEqns, currentSubst)
      updateState(s)
      printEquations()
      printSubstitution()
      debugln()
    }

    private def phase4SVE(): Unit = {
      debugln("-".repeat(80))
      debugln("--- Phase 4: Boolean Unification")
      debugln("    (resolves all remaining equations using SVE.)")
      debugln("-".repeat(80))
      val restSubst = boolUnifyAll(currentEqns, Set.empty)
      currentEqns = Nil
      currentSubst = restSubst @@ currentSubst // TODO: Verify
      printSubstitution()
      debugln()
    }

    /**
      * If debugging is enabled, checks that the current substitution is a valid solution to the original equations `l`.
      *
      * Throws an exception from inside [[verify]] if the solution is invalid.
      */
    private def verifySolution(): Unit = {
      if (Debugging) {
        verify(currentSubst, l)
      }
    }

    /**
      * Checks that current substitution has not grown too large according to the defined threshold.
      */
    private def verifySolutionSize(): Unit = {
      val size = currentSubst.size
      if (size > Threshold) {
        throw TooComplexException(size)
      }
    }

    private def updateState(p: (List[Equation], BoolSubstitution)): Unit = {
      val (nextEqns, nextSubst) = p
      currentEqns = simplify(nextEqns)
      currentSubst = nextSubst
    }

    private def printEquations(): Unit = {
      debugln(s"Equations (${currentEqns.size}):")
      debugln(format(currentEqns))
    }

    private def printSubstitution(): Unit = {
      debugln(s"Substitution (${currentSubst.bindings}):")
      debugln(currentSubst.toString)
    }

    private def debugln(): Unit = debugln("")

    private def debugln(s: String): Unit = {
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
    * -     true ~ true
    * -    false ~ false
    * -        c ~ c        (same constants)
    * -        x ~ x        (same variables)
    *
    *
    * An unsolvable (conflicted) equation is one of:
    *
    * -      true ~ false   (and mirrored)
    * -       c_i ~ c_j     (different constants)
    * -        c ~ true     (and mirrored)
    * -        c ~ false    (and mirrored)
    */
  private def simplify(l: List[Equation]): List[Equation] = l match {
    case Nil => Nil
    case Equation(t1, t2) :: es => (t1, t2) match {
      // Trivial equations: skip them.
      case (Term.True, Term.True) => simplify(es)
      case (Term.False, Term.False) => simplify(es)
      case (Term.Cst(c1), Term.Cst(c2)) if c1 == c2 => simplify(es)
      case (Term.Var(x1), Term.Var(x2)) if x1 == x2 => simplify(es)

      // Unsolvable (conflicted) equations: raise an exception.
      case (Term.True, Term.False) => throw ConflictException(t1, t2)
      case (Term.False, Term.True) => throw ConflictException(t1, t2)
      case (Term.Cst(c1), Term.Cst(c2)) if c1 != c2 => throw ConflictException(t1, t2)
      // Note: two different variables are of course solvable!
      case (Term.Cst(_), Term.True) => throw ConflictException(t1, t2)
      case (Term.Cst(_), Term.False) => throw ConflictException(t1, t2)
      case (Term.True, Term.Cst(_)) => throw ConflictException(t1, t2)
      case (Term.False, Term.Cst(_)) => throw ConflictException(t1, t2)

      // Non-trivial and non-conflicted equation: keep it.
      case _ => Equation(t1, t2) :: simplify(es)
    }
  }


  // Saturates all unit clauses.
  private def propagateUnit(l: List[Equation], s: BoolSubstitution): (List[Equation], BoolSubstitution) = {
    var currentEqns = l
    var currentSubst = s

    var changed = true
    while (changed) {
      changed = false

      val remainder = ListBuffer.empty[Equation]
      for (e <- currentEqns) {
        e match {
          // Case 1: x =?= true
          case Equation(Term.Var(x), Term.True) =>
            currentSubst = currentSubst.extended(x, Term.True)
            changed = true

          // Case 2: x =?= c
          case Equation(Term.Var(x), Term.Cst(c)) =>
            currentSubst = currentSubst.extended(x, Term.Cst(c))
            changed = true

          // Case 3: x /\ y /\ z /\... = true
          case Equation(Term.And(csts, vars, rest), Term.True) if csts.isEmpty && rest.isEmpty =>
            for (Term.Var(x) <- vars) {
              currentSubst = currentSubst.extended(x, Term.True)
              changed = true
            }

          case _ =>
            remainder += e
        }
      }
      // Invariant: We apply the current substitution to all remaining equations.
      currentEqns = currentSubst(remainder.toList)
    }

    // Fixpoint complete. We return the remaining equations and the current substitution.
    // We do not have to apply the current substitution because that was already done in the last iteration.
    (currentEqns, currentSubst)
  }


  private def propagateVars(l: List[Equation], subst0: BoolSubstitution): (List[Equation], BoolSubstitution) = {
    // TODO: Fixpoint or disjoint sets needed?

    // TODO: Could start from empty subst and then use ++ later.

    var currentEqns = l
    var currentSubst = subst0
    var rest = ListBuffer.empty[Equation]

    while (currentEqns.nonEmpty) {
      val eqn = currentEqns.head
      currentEqns = currentEqns.tail
      eqn match {
        case Equation(Term.Var(x), Term.Var(y)) =>
          val singleton = BoolSubstitution.singleton(x, Term.Var(y))

          currentEqns = singleton(currentEqns)
          currentSubst = singleton @@ currentSubst
        case _ => rest += eqn
      }
    }

    (currentSubst(rest.toList), currentSubst)
  }

  // Deals with x92747 ~ (x135862 ∧ x135864)
  // where LHS is var and is free on RHS
  private def varAssignment(l: List[Equation], subst0: BoolSubstitution): (List[Equation], BoolSubstitution) = {
    var currentSubst = subst0
    var currentEqns = l
    var rest: List[Equation] = Nil

    while (currentEqns != Nil) {
      val eqn = currentEqns.head
      currentEqns = currentEqns.tail

      eqn match {
        case Equation(Term.Var(x), rhs) if !rhs.freeVars.contains(x) =>
          // Update the remaining equations with the new binding.
          // This is required for correctness.
          // We use a singleton subst. to avoid idempotence issues.
          val singleton = BoolSubstitution.singleton(x, rhs)

          // Update the current substitution.
          currentSubst = singleton @@ currentSubst

          // Update the remaining eqns and the remainder.
          currentEqns = singleton(currentEqns)
          rest = singleton(rest)

        case _ => rest = eqn :: rest
      }
    }

    (rest.reverse, currentSubst)
  }


  private def boolUnifyAll(l: List[Equation], renv: Set[Int]): BoolSubstitution = l match {
    case Nil => BoolSubstitution.empty
    case Equation(t1, t2) :: xs =>
      val subst = boolUnifyOne(t1, t2, renv)
      val subst1 = boolUnifyAll(subst(xs), renv)
      subst @@ subst1 // TODO: order?
  }

  private def boolUnifyOne(t1: Term, t2: Term, renv: Set[Int]): BoolSubstitution = {
    // The boolean expression we want to show is false.
    val query = Term.mkXor(t1, t2)

    // Compute the variables in the query.
    val typeVars = query.freeVars.toList

    // Compute the flexible variables.
    val flexibleTypeVars = typeVars.filterNot(renv.contains)

    // Determine the order in which to eliminate the variables.
    val freeVars = flexibleTypeVars

    // Eliminate all variables.
    val subst = successiveVariableElimination(query, freeVars)

    subst
  }

  private def successiveVariableElimination(t: Term, flexvs: List[Int]): BoolSubstitution = flexvs match {
    case Nil =>
      // Determine if f is unsatisfiable when all (rigid) variables are made flexible.
      if (!satisfiable(t))
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
        // We must ensure that all variables are true and that all sub-terms evaluate to true.
        val allVarsTrue = vars.forall(v => trueVars.contains(v.x))
        val allRestTrue = rest.foldLeft(true) { case (bacc, t0) => bacc && evaluate(t0, trueVars) }
        allVarsTrue && allRestTrue
      }

    case Term.Or(ts) => ts.foldLeft(false) { case (bacc, term) => bacc || evaluate(term, trueVars) }
  }


  private def propagateAnd(t: Term): Term = {
    def visit(t: Term, trueCsts: SortedSet[Int], trueVars: SortedSet[Int]): Term = t match {
      case Term.True => Term.True
      case Term.False => Term.False
      case Term.Cst(c) => if (trueCsts.contains(c)) Term.True else Term.Cst(c)
      case Term.Var(x) => if (trueVars.contains(x)) Term.True else Term.Var(x)
      case Term.Not(t0) => Term.mkNot(visit(t0, trueCsts, trueVars))
      case Term.And(csts0, vars0, rest0) =>
        // Compute the constants and variables that _must_ hold for the whole conjunction to hold.
        val termCsts = csts0.map(_.c)
        val termVars = vars0.map(_.x)
        val currentCsts = trueCsts ++ termCsts
        val currentVars = trueVars ++ termVars

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

        // Compute the constants and variables that do not already hold.
        val csts = termCsts -- trueCsts
        val vars = termVars -- trueVars

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
      * - We move true and false to the rhs.
      * - We move a variable to the lhs (unconditionally).
      * - We move a constant to the lhs (except if the lhs is a variable).
      * - We reorder constant/variables equations so that the smaller constant/variable is on the lhs.
      *
      * Examples:
      * -     true ~ x7 ==> x7 ~ true
      * -       c3 ~ c2 ==> c2 ~ c3
      * -       x7 ~ x5 ==> x5 ~ x7
      * - x3 /\ x7 ~ x4 ==> x4 ~ x3 /\ x7
      * - x1 /\ x2 ~ c5 ==> c5 ~ x1 /\ x2
      */
    def mk(t1: Term, t2: Term): Equation = (t1, t2) match {
      case (Term.True, _) => Equation(t2, Term.True)
      case (Term.False, _) => Equation(t2, Term.False)
      case (Term.Cst(c1), Term.Cst(c2)) if c1 > c2 => Equation(t2, t1)
      case (Term.Var(x1), Term.Var(x2)) if x1 > x2 => Equation(t2, t1)
      case (_, _: Term.Var) => Equation(t2, t1)
      case (_, _: Term.Cst) => Equation(t2, t1)
      case _ => Equation(t1, t2)
    }
  }

  /**
    * Represents a unification equation `t1 ~ t2` between the terms `t1` and `t2`.
    *
    * WARNING: Equations should be normalized. Use the smart constructor [[Equation.mk]] to create a new equation.
    */
  case class Equation(t1: Term, t2: Term) {
    def size: Int = t1.size + t2.size
  }

  /**
    * A common super-type for Boolean terms.
    */
  sealed trait Term {

    /**
      * Syntactic sugar for [[Term.mkAnd]].
      */
    final def &(that: Term): Term = Term.mkAnd(this, that)

    /**
      * Syntactic sugar for [[Equation.mk]]
      */
    final def ~(that: Term): Equation = Equation.mk(this, that)

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
      * Returns the size of `this` term.
      *
      * The size is the number of constants, variables, and connectives in the term.
      *
      * For example, `size(x /\ y) = 1` and `size(x /\ not y) = 2`.
      */
    final def size: Int = this match {
      case Term.True => 0
      case Term.False => 0
      case Term.Cst(_) => 1
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

    case object True extends Term

    case object False extends Term

    case class Cst(c: Int) extends Term

    case class Var(x: Int) extends Term

    case class Not(t: Term) extends Term

    case class And(csts: Set[Term.Cst], vars: Set[Term.Var], rest: List[Term]) extends Term { // TODO: SortedSets?
      assert(!rest.exists(_.isInstanceOf[Term.Cst]))
      assert(!rest.exists(_.isInstanceOf[Term.Var]))
    }

    case class Or(ts: List[Term]) extends Term {
      assert(ts.length >= 2)
    }

    final def mkNot(t0: Term): Term = t0 match {
      case True => False
      case False => True
      case Not(t) => t
      case _ => Not(t0)
    }

    final def mkAnd(t1: Term, t2: Term): Term = (t1, t2) match {
      case (False, _) => False
      case (_, False) => False
      case (True, _) => t2
      case (_, True) => t1
      case _ => mkAnd(List(t1, t2))
    }

    final def mkOr(t1: Term, t2: Term): Term = (t1, t2) match {
      case (True, _) => True
      case (_, True) => True
      case (False, _) => t2
      case (_, False) => t1
      case _ => mkOr(List(t1, t2))
    }

    final def mkAnd(ts: List[Term]): Term = {
      // TODO: Group cst and vars.
      val cstTerms = mutable.Set.empty[Term.Cst]
      val varTerms = mutable.Set.empty[Term.Var]
      val restTerms = mutable.ListBuffer.empty[Term]
      for (t <- ts) {
        t match {
          case True => // nop
          case False => return False
          case x@Term.Cst(_) => cstTerms += x
          case x@Term.Var(_) => varTerms += x
          case And(csts0, vars0, rest0) =>
            cstTerms ++= csts0
            varTerms ++= vars0
            for (t0 <- rest0) {
              t0 match {
                case True => // nop
                case False => return False
                case x@Term.Cst(_) => cstTerms += x
                case x@Term.Var(_) => varTerms += x
                case _ => restTerms += t0
              }
            }
          case _ => restTerms += t
        }
      }

      (cstTerms.toList, varTerms.toList, restTerms.toList) match { // TODO: Smarter?
        case (Nil, Nil, Nil) => Term.True
        case (List(t), Nil, Nil) => t
        case (Nil, List(t), Nil) => t
        case (Nil, Nil, List(t)) => t
        case _ => And(cstTerms.toSet, varTerms.toSet, restTerms.toList)
      }
    }

    final def mkOr(ts: List[Term]): Term = {
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
    def singleton(x: Int, t: Term): BoolSubstitution = empty.extended(x, t)
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
      case Equation(t1, t2) => Equation.mk(apply(t1), apply(t2))
    }

    /**
      * Applies `this` substitution to the given list of equations `l`.
      */
    def apply(l: List[Equation]): List[Equation] = l.map(apply)

    /**
      * Returns the number of bindings in `this` substitution.
      */
    def bindings: Int = m.size

    /**
      * Returns the size of `this` substitution.
      *
      * The size of a substitution is the sum of the sizes of all terms in its co-domain.
      */
    def size: Int = m.values.map(_.size).sum

    /**
      * Extends `this` substitution with a new binding from the variable `x` to the term `t`.
      *
      * If the variable `x` already occurs in `this` substitution then it must be bound to the same term as `t`.
      */
    def extended(x: Int, t: Term): BoolSubstitution = m.get(x) match {
      case None => BoolSubstitution(m + (x -> t))
      case Some(t1) =>
        // Note: If t == t1 we can just return the same substitution.
        if (t == t1)
          this
        else
          throw ConflictException(t, t1)
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

    def @@(that: BoolSubstitution): BoolSubstitution = { // TODO: Verify that every single use correct.
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
    for (Equation(t1, t2) <- l) {
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
  private def verify(s: BoolSubstitution, l: List[Equation]): Unit = {
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
  case class ConflictException(x: Term, y: Term) extends FastBoolUnificationException // TODO: Add source location.

  /**
    * Represents a solution that is too complex (i.e. too large according to the threshold).
    */
  case class TooComplexException(size: Int) extends FastBoolUnificationException

  /////////////////////////////////////////////////////////////////////////////
  /// Testing                                                               ///
  /////////////////////////////////////////////////////////////////////////////

  import Term._

  private def FixpointInterpreter_evalTerm(): List[Equation] = List(
    Cst(442) ~ (Var(69984) & (Var(69992) & (Var(70006) & (Var(70010) & ((Var(70016) & Var(70018)) & ((Var(70025) & (Var(70028) & Var(70032))) & ((Var(70040) & (Var(70043) & (Var(70046) & Var(70052)))) & ((Var(70061) & (Var(70064) & (Var(70067) & (Var(70070) & Var(70078))))) & ((Var(70088) & (Var(70091) & (Var(70094) & (Var(70097) & (Var(70100) & Var(70110)))))) & (Var(70126) & Var(70135))))))))))),
    Var(69982) ~ Var(121740),
    Var(69984) ~ Var(69982),
    Var(69992) ~ Var(121742),
    Var(69999) ~ Var(121748),
    Var(70001) ~ (Var(121746) & Var(69999)),
    Var(70004) ~ Var(121750),
    Var(70006) ~ (Var(121744) & (Var(70001) & Var(70004))),
    Var(70010) ~ Var(121752),
    Var(70016) ~ Var(121754),
    Var(70018) ~ Var(121756),
    Var(70025) ~ Var(121758),
    Var(70028) ~ Var(121760),
    Var(70030) ~ Var(121764),
    Var(70032) ~ (Var(121762) & Var(70030)),
    Var(70040) ~ Var(121766),
    Var(70043) ~ Var(121768),
    Var(70046) ~ Var(121770),
    Var(70048) ~ Var(121776),
    Var(70050) ~ (Var(121774) & Var(70048)),
    Var(70052) ~ (Var(121772) & Var(70050)),
    Var(70061) ~ Var(121778),
    Var(70064) ~ Var(121780),
    Var(70067) ~ Var(121782),
    Var(70070) ~ Var(121784),
    Var(70072) ~ Var(121792),
    Var(70074) ~ (Var(121790) & Var(70072)),
    Var(70076) ~ (Var(121788) & Var(70074)),
    Var(70078) ~ (Var(121786) & Var(70076)),
    Var(70088) ~ Var(121794),
    Var(70091) ~ Var(121796),
    Var(70094) ~ Var(121798),
    Var(70097) ~ Var(121800),
    Var(70100) ~ Var(121802),
    Var(70102) ~ Var(121812),
    Var(70104) ~ (Var(121810) & Var(70102)),
    Var(70106) ~ (Var(121808) & Var(70104)),
    Var(70108) ~ (Var(121806) & Var(70106)),
    Var(70110) ~ (Var(121804) & Var(70108)),
    Var(70118) ~ True,
    Var(70123) ~ True,
    Var(70126) ~ (Var(70118) & Var(70123)),
    Var(70132) ~ True,
    Var(70135) ~ Var(70132)
  )

  private def Array_copyOfRange(): List[Equation] = List(
    (Cst(22553) & Cst(22551)) ~ (Var(90584) & (Var(90592) & (Var(90600) & (Var(90608) & (Var(90616) & (Var(90625) & (Var(90633) & (Var(90641) & (Var(90649) & (Var(90657) & (Var(90666) & (Var(90675) & (Var(90684) & Var(90688)))))))))))))),
    Var(90580) ~ Var(134536),
    Var(90582) ~ (Var(134534) & Var(90580)),
    Var(90584) ~ (Var(134532) & Var(90582)),
    Var(90588) ~ Var(134542),
    Var(90590) ~ (Var(134540) & Var(90588)),
    Var(90592) ~ (Var(134538) & Var(90590)),
    Var(90596) ~ Var(134548),
    Var(90598) ~ (Var(134546) & Var(90596)),
    Var(90600) ~ (Var(134544) & Var(90598)),
    Var(90604) ~ Var(134554),
    Var(90606) ~ (Var(134552) & Var(90604)),
    Var(90608) ~ (Var(134550) & Var(90606)),
    Var(90612) ~ Var(134560),
    Var(90614) ~ (Var(134558) & Var(90612)),
    Var(90616) ~ (Var(134556) & Var(90614)),
    Var(90621) ~ Var(134566),
    Var(90623) ~ (Var(134564) & Var(90621)),
    Var(90625) ~ (Var(134562) & Var(90623)),
    Var(90629) ~ Var(134572),
    Var(90631) ~ (Var(134570) & Var(90629)),
    Var(90633) ~ (Var(134568) & Var(90631)),
    Var(90637) ~ Var(134578),
    Var(90639) ~ (Var(134576) & Var(90637)),
    Var(90641) ~ (Var(134574) & Var(90639)),
    Var(90645) ~ Var(134584),
    Var(90647) ~ (Var(134582) & Var(90645)),
    Var(90649) ~ (Var(134580) & Var(90647)),
    Var(90653) ~ Var(134590),
    Var(90655) ~ (Var(134588) & Var(90653)),
    Var(90657) ~ (Var(134586) & Var(90655)),
    Var(90662) ~ Var(134596),
    Var(90664) ~ (Var(134594) & Var(90662)),
    Var(90666) ~ (Var(134592) & Var(90664)),
    Var(90671) ~ Var(134602),
    Var(90673) ~ (Var(134600) & Var(90671)),
    Var(90675) ~ (Var(134598) & Var(90673)),
    Var(90680) ~ Var(134608),
    Var(90682) ~ (Var(134606) & Var(90680)),
    Var(90684) ~ (Var(134604) & Var(90682)),
    Var(90688) ~ True
  )

  private def FixpointAstDatalog_toString299997(): List[Equation] = List(
    True ~ (Var(100987) & (Var(101022) & Var(101116))),
    Var(100987) ~ (Var(100990) & (Var(100997) & (Var(101007) & Var(101019)))),
    Var(100990) ~ Var(108420),
    Var(100994) ~ True,
    Var(100996) ~ (Var(108422) & Var(108423)),
    Var(100997) ~ Var(101006),
    Var(101001) ~ Var(108430),
    Var(101004) ~ Var(108432),
    Var(101006) ~ (((Var(108427) & Var(108426)) & Var(108428)) & Var(101004)),
    Var(101007) ~ Var(101016),
    Var(101011) ~ Var(108439),
    Var(101014) ~ Var(108441),
    Var(101016) ~ (((Var(108436) & Var(108435)) & Var(108437)) & Var(101014)),
    Var(101019) ~ Var(108443),
    Var(101022) ~ (Var(101025) & (Var(101109) & Var(101112))),
    Var(101025) ~ Var(108444),
    Var(101029) ~ True,
    Var(101036) ~ Var(108463),
    Var(101039) ~ True,
    Var(101041) ~ Var(108461),
    Var(101043) ~ (Var(108458) & Var(101041)),
    Var(101046) ~ True,
    Var(101048) ~ Var(108467),
    Var(101050) ~ (Var(108455) & (Var(101043) & Var(101048))),
    Var(101054) ~ True,
    Var(101059) ~ True,
    Var(101063) ~ (Var(108469) & (Var(101054) & Var(101059))),
    Var(101065) ~ Var(108453),
    Var(101073) ~ Var(108484),
    Var(101076) ~ True,
    Var(101078) ~ Var(108482),
    Var(101080) ~ (Var(108479) & Var(101078)),
    Var(101083) ~ True,
    Var(101085) ~ Var(108488),
    Var(101087) ~ (Var(108476) & (Var(101080) & Var(101085))),
    Var(101091) ~ True,
    Var(101096) ~ True,
    Var(101101) ~ True,
    Var(101105) ~ (Var(108490) & ((Var(101091) & Var(101096)) & Var(101101))),
    Var(101107) ~ Var(108474),
    Var(101109) ~ Var(108447),
    Var(101112) ~ Var(108494),
    Var(101116) ~ (Var(101119) & (Var(101125) & (Var(101131) & Var(101134)))),
    Var(101119) ~ Var(108495),
    Var(101123) ~ True,
    Var(101125) ~ (Var(108496) & Var(101123)),
    Var(101129) ~ True,
    Var(101131) ~ (Var(108498) & Var(101129)),
    Var(101134) ~ Var(108500)
  )

  private def Nec_zipWithA(): List[Equation] = List(
    Cst(24685) ~ Var(68027),
    Var(67938) ~ True,
    Var(67940) ~ (Var(120514) & Var(67938)),
    Var(67943) ~ True,
    Var(67945) ~ (Var(120512) & (Var(67940) & Var(67943))),
    Var(67950) ~ True,
    Var(67952) ~ Var(120531),
    Var(67954) ~ (Var(120529) & Var(67952)),
    Var(67956) ~ (Var(120524) & Var(67954)),
    Var(67958) ~ (Var(120521) & Var(67956)),
    Var(67960) ~ (Var(120518) & Var(67958)),
    Var(67962) ~ (Var(120510) & Var(67945)),
    Var(67971) ~ True,
    Var(67973) ~ Var(120542),
    Var(67975) ~ (Var(120540) & Var(67973)),
    Var(67977) ~ (Var(120535) & Var(67975)),
    Var(67979) ~ (Var(120533) & Var(67977)),
    Var(67988) ~ True,
    Var(67990) ~ Var(120553),
    Var(67992) ~ (Var(120551) & Var(67990)),
    Var(67994) ~ (Var(120546) & Var(67992)),
    Var(67996) ~ (Var(120544) & Var(67994)),
    Var(68004) ~ True,
    Var(68006) ~ Var(120564),
    Var(68008) ~ (Var(120562) & Var(68006)),
    Var(68010) ~ (Var(120557) & Var(68008)),
    Var(68012) ~ (Var(120555) & Var(68010)),
    Var(68015) ~ True,
    Var(68017) ~ (Var(120570) & Var(68015)),
    Var(68020) ~ True,
    Var(68022) ~ (Var(120568) & (Var(68017) & Var(68020))),
    Var(68025) ~ Var(120574),
    Var(68027) ~ (Var(120566) & Var(68022))
  )

  private def ConcurrentChannel_selectHelper(): List[Equation] = List(
    Cst(1794221043) ~ (Var(85999) & (Var(86002) & (Var(86045) & (Var(86052) & ((Var(86063) & (Var(86069) & (Var(86072) & (Var(86075) & Var(86078))))) & Var(86094)))))),
    Var(85997) ~ Cst(1794221043),
    Var(85999) ~ Var(131629),
    Var(86002) ~ Cst(1794221043),
    Var(86008) ~ Var(131642),
    Var(86020) ~ Var(131646),
    Var(86022) ~ Var(86020),
    Var(86024) ~ Var(131640),
    Var(86026) ~ (Var(131637) & Var(86024)),
    Var(86029) ~ Var(131650),
    Var(86033) ~ Var(131648),
    Var(86035) ~ (Var(131634) & (Var(86026) & Var(86033))),
    Var(86038) ~ Var(131654),
    Var(86041) ~ True,
    Var(86043) ~ Var(131652),
    Var(86045) ~ (Var(131631) & (Var(86035) & Var(86043))),
    Var(86052) ~ True,
    Var(86057) ~ Cst(1794221043),
    Var(86059) ~ Var(131668),
    Var(86061) ~ (Var(131666) & Var(86059)),
    Var(86063) ~ (Var(131664) & Var(86061)),
    Var(86067) ~ Cst(1794221043),
    Var(86069) ~ Var(131670),
    Var(86072) ~ Cst(1794221043),
    Var(86075) ~ Cst(1794221043),
    Var(86078) ~ Cst(1794221043),
    Var(86084) ~ True,
    Var(86087) ~ Var(131680),
    Var(86090) ~ Cst(1794221043),
    Var(86092) ~ Var(131678),
    Var(86094) ~ (Var(131672) & (Var(86084) & Var(86092)))
  )

  private def Array_transpose(): List[Equation] = List(
    ((Cst(21536) & Cst(21537)) & Cst(21534)) ~ (Var(65522) & (Var(65525) & (Var(65527) & (Var(65533) & ((Var(65536) & Var(65539)) & (Var(65555) & Var(65577))))))),
    Var(65522) ~ True,
    Var(65525) ~ True,
    Var(65527) ~ Var(119096),
    Var(65531) ~ Var(119101),
    Var(65533) ~ Var(65531),
    Var(65536) ~ True,
    Var(65539) ~ Var(119105),
    Var(65546) ~ Var(119115),
    Var(65548) ~ Var(65546),
    Var(65551) ~ Var(119117),
    Var(65553) ~ ((Var(119111) & Var(119109)) & (Var(65548) & Var(65551))),
    Var(65555) ~ (Var(119107) & Var(119106)),
    Var(65562) ~ Var(119133),
    Var(65564) ~ Var(119131),
    Var(65566) ~ (Var(119128) & Var(65564)),
    Var(65569) ~ Var(119137),
    Var(65571) ~ Var(119135),
    Var(65573) ~ (Var(119125) & (Var(65566) & Var(65571))),
    Var(65575) ~ (Var(119122) & Var(119121)),
    Var(65577) ~ (Var(119119) & Var(119118))
  )


  //MutDeque.sameElements
  private def MutDeque_sameElements(): List[Equation] = List(
    (Var(876) & Var(877)) ~ Var(90798),
    Var(90798) ~ (Var(90801) & Var(90804) & Var(90807) & Var(90820) & Var(90823) & Var(90832) & Var(90841) & Var(90844)),
    Var(90801) ~ Var(134687),
    Var(90804) ~ Var(134689),
    Var(90807) ~ True,
    Var(90820) ~ Var(134695),
    Var(90823) ~ Var(134697),
    Var(90826) ~ Var(134703),
    Var(90828) ~ Var(134705),
    Var(90830) ~ Var(134707),
    Var(90832) ~ (Var(134701) & Var(134699) & Var(90826) & Var(90828) & Var(90830)),
    Var(90835) ~ Var(134712),
    Var(90837) ~ Var(134714),
    Var(90839) ~ Var(134716),
    Var(90841) ~ (Var(134710) & Var(134708) & Var(90835) & Var(90837) & Var(90839)),
    Var(90844) ~ (Var(134718) & Var(134719))
  )

  private def FixpointAstDatalog_predSymsOf29898(): List[Equation] = List(
    True ~ (Var(97799) & Var(97816) & Var(97819) & Var(97847) & Var(97859)),
    Var(97787) ~ Var(107197),
    Var(97790) ~ True,
    Var(97792) ~ Var(107195),
    Var(97794) ~ (Var(107192) & Var(97792)),
    Var(97797) ~ True,
    Var(97799) ~ (Var(107189) & Var(97794)),
    Var(97804) ~ Var(107211),
    Var(97807) ~ True,
    Var(97809) ~ Var(107209),
    Var(97811) ~ (Var(107206) & Var(97809)),
    Var(97814) ~ True,
    Var(97816) ~ (Var(107203) & Var(97811)),
    Var(97819) ~ True,
    Var(97827) ~ True,
    Var(97830) ~ True,
    Var(97832) ~ (Var(107224) & Var(97827)),
    Var(97835) ~ Var(107232),
    Var(97838) ~ True,
    Var(97840) ~ Var(107230),
    Var(97842) ~ (Var(107221) & Var(97832) & Var(97840)),
    Var(97845) ~ True,
    Var(97847) ~ (Var(107218) & Var(97842)),
    Var(97854) ~ True,
    Var(97857) ~ True,
    Var(97859) ~ (Var(97854) & Var(97857))
  )

  private def Iterator_toArray(): List[Equation] = List(
    ((Cst(1500) & Cst(1501)) & Cst(1498)) ~ Var(78914),
    Var(78914) ~ (Var(78917) & (Var(78923) & Var(78926))),
    Var(78917) ~ Var(127244),
    Var(78921) ~ Var(127251),
    Var(78923) ~ ((Var(127248) & Var(127247)) & Var(127249)),
    Var(78926) ~ (Var(127254) & Var(127252))
  )

  private def Files_append(): List[Equation] = List(
    Cst(1794221043) ~ ((Var(55040) & (Var(55042) & (Var(55046) & (Var(55050) & (Var(55058) & (Var(55060) & (Var(55062) & (Var(55066) & Var(55075))))))))) & Var(55078)),
    Var(55078) ~ Var(112431),
    Var(55040) ~ Cst(1794221043),
    Var(55042) ~ Var(112433),
    Var(55044) ~ Var(112437),
    Var(55046) ~ (Var(112435) & Var(55044)),
    Var(55048) ~ True,
    Var(55050) ~ (Var(112439) & Var(55048)),
    Var(55052) ~ Var(112443),
    Var(55055) ~ True,
    Var(55058) ~ (Var(112441) & (Var(55052) & Var(55055))),
    Var(55060) ~ Var(112446),
    Var(55062) ~ Var(112448),
    Var(55066) ~ True,
    Var(55075) ~ Var(112453)
  )

  private def Iterator_next(): List[Equation] = List(
    (Cst(1435) & Cst(1436)) ~ Var(55261),
    Var(55251) ~ Var(112576),
    Var(55257) ~ Var(112582),
    Var(55261) ~ Var(112585)
  )

  private def Boxable_lift1(): List[Equation] = List(
    Var(56474) ~ True,
    Var(56476) ~ (Var(113308) & Var(56474)),
    Var(56478) ~ Var(56476)
  )


  def main(args: Array[String]): Unit = {
    //solveAll(FixpointInterpreter_evalTerm()).get
    //solveAll(Array_copyOfRange()).get
    //solveAll(FixpointAstDatalog_toString299997()).get
    //solveAll(Nec_zipWithA()).get
    //solveAll(ConcurrentChannel_selectHelper()).get
    //solveAll(Array_transpose()).get
    //solveAll(MutDeque_sameElements()).get
    //solveAll(FixpointAstDatalog_predSymsOf29898()).get
    solveAll(Iterator_toArray()).get
    //solveAll(Files_append()).get
    //solveAll(Iterator_next()).get
    //solveAll(Boxable_lift1()).get
  }

}
