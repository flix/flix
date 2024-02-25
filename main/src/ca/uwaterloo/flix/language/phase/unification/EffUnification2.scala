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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Rigidity, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.collection.Bimap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

///
/// Type Inference with Boolean Unification Done Right
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
/// TODO: Explore change of basis.
///
object EffUnification2 {

  /**
    * Returns the most general unifier of the all the pairwise unification problems in `l`.
    *
    * Note: A type in `l` must not contain any associated effects.
    */
  def unifyAll(l: List[(Type, Type)], renv0: RigidityEnv)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    // If the list is empty we can return immediately.
    if (l.isEmpty) {
      return Result.Ok(Substitution.empty)
    }

    val allVars = mutable.Set.empty[Type.Var]
    for ((t1, t2) <- l) {
      allVars ++= t1.typeVars
      allVars ++= t2.typeVars
    }
    val forward = allVars.foldLeft(Map.empty[Type.Var, Int]) {
      case (macc, tvar) => macc + (tvar -> tvar.sym.id)
    }
    val backward = allVars.foldLeft(Map.empty[Int, Type.Var]) {
      case (macc, tvar) => macc + (tvar.sym.id -> tvar)
    }
    implicit val bimap: Bimap[Type.Var, Int] = Bimap(forward, backward)

    val equations: List[Equation] = mkEquations(l, renv0)

    solveAll(equations) match {
      case Result.Ok(subst) =>
        Result.Ok(subst.toSubst)

      case Result.Err(ex) =>
        val tpe1 = toType(ex.x, SourceLocation.Unknown)
        val tpe2 = toType(ex.y, SourceLocation.Unknown)
        Result.Err(UnificationError.MismatchedEffects(tpe1, tpe2))
    }
  }


  /**
    * Returns the list of pairwise unification problems `l` as a list of equations.
    */
  private def mkEquations(l: List[(Type, Type)], renv0: RigidityEnv)(implicit bimap: Bimap[Type.Var, Int]): List[Equation] =
    l.map {
      case (tpe1, tpe2) => Equation.mk(fromType(tpe1, bimap, renv0), fromType(tpe2, bimap, renv0))
    }

  private class Solver(l: List[Equation]) {

    private var currentEqns = l
    private var currentSubst: BoolSubstitution = BoolSubstitution.empty

    private def printEquations(): Unit = {
      println(s"Equations (${currentEqns.size}):")
      println(format(currentEqns))
    }

    private def printSubstitution(): Unit = {
      println(s"Substitution (${currentSubst.m.size}):")
      println(formatSubst(currentSubst))
    }

    private def phase0(): Unit = {
      println("-".repeat(80))
      println("--- Input")
      println("-".repeat(80))
      printEquations()
      println()
    }

    private def phase1(): Unit = {
      println("-".repeat(80))
      println("--- Phase 1: Unit Propagation")
      println("    (resolves all equations of the form: x = c where x is a var and c is const)")
      println("-".repeat(80))
      val (nextEqns, nextSubst) = propagateUnit(currentEqns, currentSubst)
      currentEqns = checkAndSimplify(nextEqns)
      currentSubst = nextSubst
      printEquations()
      printSubstitution()
      println()
    }

    private def phase2(): Unit = {
      println("-".repeat(80))
      println("--- Phase 2: Variable Propagation")
      println("    (resolves all equations of the form: x = y where x and y are vars)")
      println("-".repeat(80))
      val (nextEqns1, nextSubst1) = propagateVars(currentEqns, currentSubst)
      currentEqns = checkAndSimplify(nextEqns1)
      currentSubst = nextSubst1
      printEquations()
      printSubstitution()
      println()
    }

    def solve()(implicit flix: Flix): Result[BoolSubstitution, ConflictException] = {
      try {
        phase0()
        phase1()
        phase2()

        println("-".repeat(80))
        println("--- Phase 3: Variable Assignment")
        println("    (resolves all equations of the form: x = t where x is free in t)")
        println("-".repeat(80))
        val (nextEqns2, nextSubst2) = varAssignment(currentEqns, currentSubst)
        currentEqns = checkAndSimplify(nextEqns2)
        currentSubst = nextSubst2
        printEquations()
        printSubstitution()
        println()

        println("-- Result of Occurrence Analysis and Propagation -- ")
        val occur = occurrenceInfo(currentEqns) // TODO: Introduce type, but also check in Subst.
        println(occur)
        println()

        println("-".repeat(80))
        println("--- Phase 4: Boolean Unification")
        println("    (resolves all remaining equations using SVE.)")
        println("-".repeat(80))
        val restSubst = boolUnifyAll(currentEqns, Set.empty)
        currentEqns = Nil
        currentSubst = currentSubst @@ restSubst
        printSubstitution()
        println()
        Result.Ok(currentSubst)
      } catch {
        case ex: ConflictException => Result.Err(ex)
      }
    }

  }

  private def solveAll(l: List[Equation])(implicit flix: Flix): Result[BoolSubstitution, ConflictException] = {
    // TODO: Introduce small solver class.
    new Solver(l).solve()
  }

  private def toType(t0: Term, loc: SourceLocation)(implicit m: Bimap[Type.Var, Int]): Type = t0 match {
    case Term.True => Type.Pure
    case Term.False => Type.Univ
    case Term.Cst(c) => m.getBackward(c).get
    case Term.Var(x) => m.getBackward(x).get
    case Term.Not(t) => Type.mkComplement(toType(t, loc), loc)
    case Term.And(csts, vars, rest) =>
      val ts = csts.toList.map(toType(_, loc)) ++ vars.toList.map(toType(_, loc)) ++ rest.map(toType(_, loc))
      Type.mkIntersection(ts, loc)
    case Term.Or(ts) => Type.mkUnion(ts.map(toType(_, loc)), loc)
  }

  private def fromType(t: Type, env: Bimap[Type.Var, Int], renv: RigidityEnv): Term = Type.eraseTopAliases(t) match {
    case t: Type.Var => env.getForward(t) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$t'.", t.loc)
      case Some(x) => renv.get(t.sym) match {
        case Rigidity.Flexible => Term.Var(x)
        case Rigidity.Rigid => Term.Cst(x)
      }
    }
    case Type.Cst(TypeConstructor.Effect(sym), _) => throw InternalCompilerException("Not yet", t.loc) // TODO
    case Type.Pure => Term.True
    case Type.Univ => Term.False
    case Type.Apply(Type.Cst(TypeConstructor.Complement, _), tpe1, _) => Term.mkNot(fromType(tpe1, env, renv))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), tpe1, _), tpe2, _) => Term.mkAnd(fromType(tpe1, env, renv), fromType(tpe2, env, renv))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), tpe1, _), tpe2, _) => Term.mkOr(fromType(tpe1, env, renv), fromType(tpe2, env, renv))
    case Type.Cst(TypeConstructor.Error(_), _) => Term.True
    case _ => throw InternalCompilerException(s"Unexpected type: '$t'.", t.loc)
  }

  private case class ConflictException(x: Term, y: Term) extends RuntimeException

  /**
    * Checks for conflicts and removes any trivial equations.
    *
    * A conflict is an unsolvable equation such as:
    *
    * - true = false
    * - false = true
    * - true = r17 where r17 is rigid
    *
    * A trivial equation is one of:
    *
    * -  true = true
    * - false = false
    * -   r17 = r17
    */
  private def checkAndSimplify(l: List[Equation]): List[Equation] = l match {
    case Nil => Nil
    case e :: es => e match {
      case Equation(Term.True, Term.True) => checkAndSimplify(es)
      case Equation(Term.False, Term.False) => checkAndSimplify(es)
      case Equation(Term.True, Term.False) => throw ConflictException(Term.True, Term.False)
      case Equation(Term.False, Term.True) => throw ConflictException(Term.False, Term.True)
      // TODO: Rigid
      case _ => e :: checkAndSimplify(es)
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

    var currentSubst = subst0
    var rest = ListBuffer.empty[Equation]

    for (eqn <- l) {
      eqn match {
        case Equation(Term.Var(x), Term.Var(y)) =>
          currentSubst = currentSubst.extended(x, Term.Var(y))
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

          currentSubst = currentSubst @@ singleton

          // Update the remaining eqns and the remainder.
          currentEqns = singleton(currentEqns)
          rest = singleton(rest)

        case _ => rest = eqn :: rest
      }
    }

    (rest.reverse, currentSubst)
  }

  private object Equation {
    // Normalize: Move vars left and true/false/constants right.
    def mk(t1: Term, t2: Term): Equation = (t1, t2) match {
      case (_, _: Term.Var) => Equation(t2, t1)
      case (Term.True, _) => Equation(t2, Term.True)
      case (Term.False, _) => Equation(t2, Term.False)
      case _ => Equation(t1, t2)
    }
  }

  private case class Equation(t1: Term, t2: Term) {
    def size: Int = t1.size + t2.size
  }

  // TODO: Actually count occurrences.. Note that freeVars uses a set.
  private def occurrenceInfo(l: List[EffUnification2.Equation]): Map[Int, Int] = {
    val m = mutable.Map.empty[Int, Int]
    for (Equation(t1, t2) <- l) {
      val fvs = t1.freeVars ++ t2.freeVars
      for (x <- fvs) {
        val newCount = m.getOrElse(x, 0) + 1
        m += (x -> newCount)
      }
    }
    m.toMap
  }

  private def boolUnifyAll(l: List[Equation], renv: Set[Int])(implicit flix: Flix): BoolSubstitution = l match {
    case Nil => BoolSubstitution.empty
    case Equation(t1, t2) :: xs =>
      val subst = boolUnifyOne(t1, t2, renv)
      val subst1 = boolUnifyAll(subst(xs), renv)
      subst @@ subst1 // TODO: order?
  }

  private def boolUnifyOne(t1: Term, t2: Term, renv: Set[Int])(implicit flix: Flix): BoolSubstitution = {
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

    //    if (!subst.isEmpty) {
    //      val s = subst.toString
    //      val len = s.length
    //      if (len > 50) {
    //        println(s.substring(0, Math.min(len, 300)))
    //        println()
    //      }
    //    }

    subst
  }

  private def successiveVariableElimination(t: Term, flexvs: List[Int])(implicit flix: Flix): BoolSubstitution = flexvs match {
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

  private def satisfiable(t: Term): Boolean = t match {
    case Term.True => true
    case Term.Var(_) => true
    case Term.False => false
    case _ => evaluateAll(t, t.freeVars.toList, List.empty)
  }

  private def evaluateAll(f: Term, l: List[Int], env: List[Int]): Boolean = l match {
    case Nil =>
      // All variables are bound. Compute the truth value.
      evaluate(f, env)
    case x :: xs =>
      // Recurse on two cases: x = false and x = true.
      evaluateAll(f, xs, env) || evaluateAll(f, xs, x :: env)
  }

  private def evaluate(t: Term, trueVars: List[Int]): Boolean = t match {
    case Term.True => true
    case Term.False => false
    case Term.Cst(_) => false
    case Term.Var(x) => trueVars.contains(x)
    case Term.Not(t) => !evaluate(t, trueVars)
    case Term.Or(ts) => ts.foldLeft(false) { case (bacc, term) => bacc || evaluate(term, trueVars) }
    case Term.And(csts, vars, rest) =>
      if (csts.nonEmpty) {
        false
      } else {
        vars.forall(v => trueVars.contains(v)) && rest.foldLeft(true) { case (bacc, term) => bacc && evaluate(term, trueVars) }
      }

  }


  private def propagateAnd(t0: Term): Term = {
    def visit(t0: Term, trueCsts: SortedSet[Int], trueVars: SortedSet[Int]): Term = t0 match {
      case Term.True => Term.True
      case Term.False => Term.False
      case Term.Cst(c) => if (trueCsts.contains(c)) Term.True else Term.Cst(c)
      case Term.Var(x) => if (trueVars.contains(x)) Term.True else Term.Var(x)
      case Term.Not(t) => Term.mkNot(visit(t, trueCsts, trueVars))
      case Term.And(csts0, vars0, rest0) =>
        val termCsts = csts0.map(_.c)
        val termVars = vars0.map(_.x)
        val currentCsts = trueCsts ++ termCsts
        val currentVars = trueVars ++ termVars

        val rest = rest0.collect {
          case t: Term.Cst => ???
          case t: Term.Var => ???
          case t: Term.Not => visit(t, currentCsts, currentVars)
          case t: Term.And => visit(t, currentCsts, currentVars)
          case t: Term.Or => visit(t, currentCsts, currentVars)
        }

        // Compute the constants and variables that do not already hold.
        val csts = termCsts -- trueCsts
        val vars = termVars -- trueVars

        Term.mkAnd(csts.toList.map(Term.Cst) ++ vars.toList.map(Term.Var) ++ rest)

      case Term.Or(ts) => Term.mkOr(ts.map(visit(_, trueCsts, trueVars)))
    }


    visit(t0, SortedSet.empty, SortedSet.empty)
  }

  private def minimize(t0: Term): Term = {
    val fvs = t0.freeVars
    fromFormula(BoolFormulaTable.minimizeFormula(toFormula(t0)))(fvs)
  }

  private def toFormula(t0: Term): BoolFormula = t0 match {
    case Term.True => BoolFormula.True
    case Term.False => BoolFormula.False
    case Term.Cst(c) => BoolFormula.Var(c)
    case Term.Var(x) => BoolFormula.Var(x)
    case Term.Not(t) => BoolFormula.Not(toFormula(t))
    case Term.And(csts, vars, rest) => ??? /// TODO
    case Term.Or(ts) => ts.foldRight(BoolFormula.False: BoolFormula) {
      case (t, acc) => BoolFormula.Or(toFormula(t), acc)
    }
  }

  private def fromFormula(f0: BoolFormula)(implicit vars: SortedSet[Int]): Term = f0 match {
    case BoolFormula.True => Term.True
    case BoolFormula.False => Term.False
    case BoolFormula.Var(x) => if (vars.contains(x)) Term.Var(x) else Term.Cst(x)
    case BoolFormula.Not(f) => Term.mkNot(fromFormula(f))
    case BoolFormula.And(f1, f2) => Term.mkAnd(fromFormula(f1), fromFormula(f2))
    case BoolFormula.Or(f1, f2) => Term.mkOr(fromFormula(f1), fromFormula(f2))
  }

  private sealed trait Term {


    def &(that: Term): Term = Term.mkAnd(this, that)

    def ~(that: Term): Equation = Equation(this, that)

    /**
      * Returns  `true` if `this` term is a variable.
      */
    final def isVar: Boolean = this match {
      case Term.Var(_) => true
      case _ => false
    }

    final def freeVars: SortedSet[Int] = this match {
      case Term.True => SortedSet.empty
      case Term.False => SortedSet.empty
      case Term.Cst(_) => SortedSet.empty
      case Term.Var(x) => SortedSet(x)
      case Term.Not(t) => t.freeVars
      case Term.And(_, vars, rest) => rest.foldLeft(SortedSet.empty[Int])(_ ++ _.freeVars) ++ vars.map(_.x)
      case Term.Or(ts) => ts.foldLeft(SortedSet.empty[Int])(_ ++ _.freeVars)
    }

    final def size: Int = this match {
      case Term.True => 0
      case Term.False => 0
      case Term.Cst(_) => 0
      case Term.Var(_) => 1
      case Term.Not(t) => t.size + 1
      case Term.And(csts, vars, rest) => (csts.size + vars.size + rest.map(_.size).sum) - 1
      case Term.Or(ts) => ts.map(_.size).sum + (ts.length - 1)
    }

    override def toString: String = this match {
      case Term.True => "true"
      case Term.False => "false"
      case Term.Cst(c) => s"c$c"
      case Term.Var(x) => s"x$x"
      case Term.Not(f) => f match {
        case Term.Var(x) => s"¬x$x"
        case _ => s"¬($f)"
      }
      case Term.And(csts, vars, rest) => s"(${(csts.toList ++ vars.toList ++ rest).mkString(" ∧ ")})" // TODO: Better?
      case Term.Or(ts) => s"(${ts.mkString(" ∨ ")})"
    }

  }

  private object Term {

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

  // TODO: Rename to BoolSubst
  private object BoolSubstitution {
    val empty: BoolSubstitution = BoolSubstitution(Map.empty)

    def singleton(x: Int, t: Term): BoolSubstitution = empty.extended(x, t)
  }

  /**
    * Represents a substitution from Boolean variables (represented as integers) to Boolean terms.
    *
    * A substitution is a partial map from variables to terms. Every substitution induces a total function
    * `s: Term -> Term` that replaces every occurrence in the input term, which occurs in the domain of the
    * substitution, with its corresponding term from the co-domain.
    *
    * Note: constants and variables are a separate syntactic category. A substitution will never replace any constants.
    */
  private case class BoolSubstitution(m: Map[Int, Term]) {

    // TODO: DOC
    def apply(t: Term): Term = t match {
      case Term.True => Term.True
      case Term.False => Term.False
      case Term.Cst(c) => Term.Cst(c)
      case Term.Var(x) => m.get(x) match {
        case None => Term.Var(x)
        case Some(t0) => t0
      }
      case Term.Not(t) => Term.mkNot(this.apply(t))
      case Term.And(csts, vars, rest) =>
        val ts = csts.toList ++ vars.toList ++ rest
        Term.mkAnd(ts.map(this.apply)) // TODO: More efficient
      case Term.Or(ts) => Term.mkOr(ts.map(this.apply))
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



    def extended(x: Int, t: Term): BoolSubstitution = BoolSubstitution(m + (x -> t))

    def ++(that: BoolSubstitution): BoolSubstitution = {
      assert(this.m.keySet.intersect(that.m.keySet).isEmpty)

      if (this.m.isEmpty) {
        that
      } else if (that.m.isEmpty) {
        this
      } else {
        BoolSubstitution(
          this.m ++ that.m.filter(kv => !this.m.contains(kv._1))
        )
      }
    }

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

    def toSubst(implicit bimap: Bimap[Type.Var, Int]): Substitution = {
      Substitution(m.foldLeft(Map.empty[Symbol.KindedTypeVarSym, Type]) {
        case (macc, (k, v)) => macc + (bimap.getBackward(k).get.sym -> toType(v, SourceLocation.Unknown))
      })
    }
  }

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

  private def formatSubst(s: BoolSubstitution, indent: Int = 4): String = {
    val sb = new StringBuilder()
    // We sort the bindings by (size, name).
    for ((x, t) <- s.m.toList.sortBy(kv => (kv._2.size, kv._1))) {
      sb.append(" ".repeat(indent))
      sb.append(s"x$x")
      sb.append(" -> ")
      sb.append(t)
      sb.append("\n")
    }
    sb.toString()
  }

  /////////////////////////////////////////////////////////////////////////////
  /// Debugging                                                             ///
  /////////////////////////////////////////////////////////////////////////////

  import Term._

  private def example01(): List[Equation] = List(
    True -> True,
    Var(92719) -> True,
    Var(92722) -> True,
    Var(92722) -> True,
    Var(92725) -> True,
    Var(92728) -> True,
    Var(92730) -> mkAnd(List(Var(92719), Var(92722), Var(92725), Var(92728))),
    Var(92735) -> True,
    Var(92737) -> Var(92735),
    Var(92739) -> mkAnd(List(Var(135864), Var(92737))),
    Var(92743) -> True,
    Var(92745) -> Var(92743),
    Var(92747) -> mkAnd(List(Var(135862), Var(92739), Var(92745))),
    Var(92751) -> True,
    Var(92753) -> Var(92751),
    Var(92755) -> mkAnd(List(Var(135860), Var(92747), Var(92753))),
    Var(92759) -> True,
    Var(92761) -> Var(92759),
    Var(92763) -> mkAnd(List(Var(135858), Var(92755), Var(92761))),
    Var(92765) -> mkAnd(List(Var(135855), Var(92763)))
  ).map({ case (x, y) => Equation(x, y) })

  //   Fixpoint.Ast.Datalog.toString$29997
  private def example02(): List[Equation] = List(
    True ~ (Var(100987) & Var(101022) & Var(101116)),
    Var(100987) ~ (Var(100990) & Var(100997) & Var(101007) & Var(101019)),
    Var(100990) ~ Var(108420),
    Var(100994) ~ True,
    Var(100996) ~ (Var(108422) & Var(108423)),
    Var(100997) ~ Var(101006),
    Var(101001) ~ Var(108430),
    Var(101004) ~ Var(108432),
    Var(101006) ~ (Var(108427) & Var(108426) & Var(108428) & Var(101004)),
    Var(101007) ~ Var(101016),
    Var(101011) ~ Var(108439),
    Var(101014) ~ Var(108441),
    Var(101016) ~ (Var(108436) & Var(108435) & Var(108437) & Var(101014)),
    Var(101019) ~ Var(108443),
    Var(101022) ~ (Var(101025) & Var(101109) & Var(101112)),
    Var(101025) ~ Var(108444),
    Var(101029) ~ True,
    Var(101036) ~ Var(108463),
    Var(101039) ~ True,
    Var(101041) ~ Var(108461),
    Var(101043) ~ (Var(108458) & Var(101041)),
    Var(101046) ~ True,
    Var(101048) ~ Var(108467),
    Var(101050) ~ (Var(108455) & Var(101043) & Var(101048)),
    Var(101054) ~ True,
    Var(101059) ~ True,
    Var(101063) ~ (Var(108469) & Var(101054) & Var(101059)),
    Var(101065) ~ Var(108453),
    Var(101073) ~ Var(108484),
    Var(101076) ~ True,
    Var(101078) ~ Var(108482),
    Var(101080) ~ (Var(108479) & Var(101078)),
    Var(101083) ~ True,
    Var(101085) ~ Var(108488),
    Var(101087) ~ (Var(108476) & Var(101080) & Var(101085)),
    Var(101091) ~ True,
    Var(101096) ~ True,
    Var(101101) ~ True,
    Var(101105) ~ (Var(108490) & Var(101091) & Var(101096) & Var(101101)),
    Var(101107) ~ Var(108474),
    Var(101109) ~ Var(108447),
    Var(101112) ~ Var(108494),
    Var(101116) ~ (Var(101119) & Var(101125) & Var(101131) & Var(101134)),
    Var(101119) ~ Var(108495),
    Var(101123) ~ True,
    Var(101125) ~ (Var(108496) & Var(101123)),
    Var(101129) ~ True,
    Var(101131) ~ (Var(108498) & Var(101129)),
    Var(101134) ~ Var(108500)
  )

  // Concurrent.Channel.selectHelper
  // TODO: Note: io -> Var0
  private def example03(): List[Equation] = List(
    Var(0) ~ (Var(85999) & Var(86002) & Var(86045) & Var(86052) & Var(86063) & Var(86069) & Var(86072) & Var(86075) & Var(86078) & Var(86094)),
    Var(85997) ~ Var(0),
    Var(85999) ~ Var(131629),
    Var(86002) ~ Var(0),
    Var(86008) ~ Var(131642),
    Var(86020) ~ Var(131646),
    Var(86022) ~ Var(86020),
    Var(86024) ~ Var(131640),
    Var(86026) ~ (Var(131637) & Var(86024)),
    Var(86029) ~ Var(131650),
    Var(86033) ~ Var(131648),
    Var(86035) ~ (Var(131634) & Var(86026) & Var(86033)),
    Var(86038) ~ Var(131654),
    Var(86041) ~ True,
    Var(86043) ~ Var(131652),
    Var(86045) ~ (Var(131631) & Var(86035) & Var(86043)),
    Var(86052) ~ True,
    Var(86057) ~ Var(0),
    Var(86059) ~ Var(131668),
    Var(86061) ~ (Var(131666) & Var(86059)),
    Var(86063) ~ (Var(131664) & Var(86061)),
    Var(86067) ~ Var(0),
    Var(86069) ~ Var(131670),
    Var(86072) ~ Var(0),
    Var(86075) ~ Var(0),
    Var(86078) ~ Var(0),
    Var(86084) ~ True,
    Var(86087) ~ Var(131680),
    Var(86090) ~ Var(0),
    Var(86092) ~ Var(131678),
    Var(86094) ~ (Var(131672) & Var(86084) & Var(86092))
  )

  //MutDeque.sameElements
  private def example04(): List[Equation] = List(
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

  // Fixpoint.Ast.Datalog.predSymsOf$29898
  private def example05(): List[Equation] = List(
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

  //     DelayMap.maximumValueBy -- note assoc effect has been replaced.
  private def example06(): List[Equation] = List(
    Var(12755) ~ (Var(52648) & Var(52653) & Var(52670) & Var(52679) & Var(52683)),
    Var(52638) ~ Var(110940),
    Var(52640) ~ (Var(110938) & Var(52638)),
    Var(52643) ~ Var(52640),
    Var(52645) ~ Var(110935),
    Var(52648) ~ True,
    Var(52653) ~ True,
    Var(52656) ~ Var(110958),
    Var(52659) ~ (Var(110956) & Var(52656)),
    Var(52666) ~ True,
    Var(52668) ~ (Var(0) & Var(52666)),
    Var(52670) ~ Var(52668),
    Var(52679) ~ Var(110969),
    Var(52683) ~ Var(110971)
  )

  //Array.init -- refactored Aef. -- hits BU!
  private def example07(): List[Equation] = List(
    (Var(22316) & Var(22315)) ~ (Var(55489) & Var(55491) & Var(55493) & Var(55496) & Var(55511)),
    Var(55489) ~ True,
    Var(55491) ~ Var(112706),
    Var(55493) ~ Var(112709),
    Var(55496) ~ Var(112710),
    Var(55499) ~ True,
    Var(55502) ~ Var(112716),
    Var(55504) ~ (Var(112714) & Var(55502)),
    Var(55507) ~ Var(0),
    Var(55509) ~ (Var(112718) & Var(55507)),
    Var(55511) ~ Var(112721)
  )

  // Array.dropRight -- dropped aef
  private def example08(): List[Equation] = List(
    (Var(21890) & Var(21888)) ~ (Var(56456) & Var(56459) & Var(56461) & Var(56464) & Var(56467) & Var(56470)),
    Var(56456) ~ True,
    Var(56459) ~ True,
    Var(56461) ~ Var(113299),
    Var(56464) ~ True,
    Var(56467) ~ Var(0),
    Var(56470) ~ (Var(113305) & Var(113303))
  )

  // Iterator.toArray
  private def example09(): List[Equation] = List(
    (((Cst(1500)) & (Cst(1501))) & (Cst(1498))) ~ (Var(78914)),
    (Var(78914)) ~ ((Var(78917)) & ((Var(78923)) & (Var(78926)))),
    (Var(78917)) ~ (Var(127244)),
    (Var(78921)) ~ (Var(127251)),
    (Var(78923)) ~ (((Var(127248)) & (Var(127247))) & (Var(127249))),
    (Var(78926)) ~ ((Var(127254)) & (Var(127252)))
  )

  // Files.append
  private def example10(): List[Equation] = List(
    (Cst(1794221043)) ~ (((Var(55040)) & ((Var(55042)) & ((Var(55046)) & ((Var(55050)) & ((Var(55058)) & ((Var(55060)) & ((Var(55062)) & ((Var(55066)) & (Var(55075)))))))))) & (Var(55078))),
    (Var(55078)) ~ (Var(112431)),
    (Var(55040)) ~ (Cst(1794221043)),
    (Var(55042)) ~ (Var(112433)),
    (Var(55044)) ~ (Var(112437)),
    (Var(55046)) ~ ((Var(112435)) & (Var(55044))),
    (Var(55048)) ~ (True),
    (Var(55050)) ~ ((Var(112439)) & (Var(55048))),
    (Var(55052)) ~ (Var(112443)),
    (Var(55055)) ~ (True),
    (Var(55058)) ~ ((Var(112441)) & ((Var(55052)) & (Var(55055)))),
    (Var(55060)) ~ (Var(112446)),
    (Var(55062)) ~ (Var(112448)),
    (Var(55066)) ~ (True),
    (Var(55075)) ~ (Var(112453))
  )

  def main(args: Array[String]): Unit = {
    implicit val flix: Flix = new Flix()
    //solveAll(example01())
    //solveAll(example02())
    //solveAll(example03())
    //solveAll(example04())
    //solveAll(example05())
    //solveAll(example06())
    //solveAll(example07())
    //solveAll(example08())
    solveAll(example09()) // hard
    //solveAll(example10()) // very hard
  }


}
