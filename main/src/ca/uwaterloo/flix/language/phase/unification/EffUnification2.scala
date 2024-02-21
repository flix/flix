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
import ca.uwaterloo.flix.language.ast.{RigidityEnv, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.collection.Bimap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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

    var currentEqns: List[Equation] = l.map {
      case (tpe1, tpe2) => Equation(fromType(tpe1), fromType(tpe2))
    }

    solveAll(currentEqns, renv0) match {
      case Result.Ok(subst) =>
        Result.Ok(subst.toSubst)

      case Result.Err(ex) =>
        val tpe1 = toType(ex.x, SourceLocation.Unknown)
        val tpe2 = toType(ex.y, SourceLocation.Unknown)
        Result.Err(UnificationError.MismatchedEffects(tpe1, tpe2))
    }
  }

  private class Solver(l: List[Equation]) {

    private var currentEqns = l
    private var currentSubst: LocalSubstitution = LocalSubstitution.empty

    private def printEquations(): Unit = {
      println(s"Equations (${currentEqns.size}):")
      println(format(currentEqns))
    }

    private def printSubstitution(): Unit = {
      println(s"Substitution (${currentSubst.m.size}):")
      println(formatSubst(currentSubst))
    }

    def solve()(implicit flix: Flix): Result[LocalSubstitution, ConflictException] = {
      println("-".repeat(80))
      println("--- Input")
      println("-".repeat(80))
      printEquations()
      println()

      try {
        println("-".repeat(80))
        println("--- Phase 1: Unit Propagation ---")
        println("    (resolves all equations of the form: x = c where x is a var and c is const.)")
        println("-".repeat(80))
        val (nextEqns, nextSubst) = unitPropagate(currentEqns, currentSubst)
        currentEqns = checkAndSimplify(nextEqns)
        currentSubst = nextSubst
        printEquations()
        printSubstitution()
        println()
        if (currentEqns.isEmpty) {
          return Result.Ok(currentSubst)
        }
        println()

        println("-".repeat(80))
        println("--- Phase 2: Variable Propagation ---")
        println("    (resolves all equations of the form: x = y where x and y are vars.)")
        println("-".repeat(80))
        val (nextEqns1, nextSubst1) = varPropagate(currentEqns, currentSubst)
        currentEqns = nextEqns1
        currentSubst = nextSubst1
        printEquations()
        printSubstitution()
        println()

        println("-- Result of Occurrence Analysis and Propagation -- ")
        val occur = occurrenceInfo(currentEqns) // TODO: Introduce type, but also check in Subst.
        println(occur)
        println()

        println("-".repeat(80))
        println("--- Phase 3: Trivial Assignment ---")
        println("    (resolves all equations of the form: x = t where x is free in t.)")
        println("-".repeat(80))
        val (nextEqns2, nextSubst2) = trivialAssignment(currentEqns, currentSubst)
        currentEqns = nextEqns2
        currentSubst = nextSubst2
        printEquations()
        printSubstitution()
        println()

        println("-".repeat(80))
        println("--- Phase 4: Boolean Unification ---")
        println("    (resolves all remaining equations using SVE.)")
        println("-".repeat(80))
        val restSubst = boolUnifyAll(currentEqns, Set.empty)
        val resultSubst = currentSubst @@ restSubst
        printSubstitution()
        println()
        Result.Ok(resultSubst)
      } catch {
        case ex: ConflictException => Result.Err(ex)
      }
    }

  }

  private def solveAll(l: List[Equation], renv0: RigidityEnv)(implicit flix: Flix): Result[LocalSubstitution, ConflictException] = {
    // TODO: Introduce small solver class.
    new Solver(l).solve()
  }

  private def toType(t0: Term, loc: SourceLocation)(implicit m: Bimap[Type.Var, Int]): Type = t0 match {
    case Term.True => Type.True
    case Term.False => Type.Univ
    case Term.Var(x) => m.getBackward(x).get
    case Term.Not(t) => Type.mkComplement(toType(t, loc), loc)
    case Term.And(ts) => Type.mkIntersection(ts.map(toType(_, loc)), loc)
    case Term.Or(ts) => Type.mkUnion(ts.map(toType(_, loc)), loc)
  }

  private def fromType(t: Type): Term = ???

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
  private def unitPropagate(eqns: List[Equation], subst0: LocalSubstitution): (List[Equation], LocalSubstitution) = {
    var currentSubst = subst0

    val (initialGround, initialNonGround) = eqns.partition(isGround)
    var currentGround = initialGround
    var currentNonGround = initialNonGround
    while (currentGround.nonEmpty) {
      currentSubst = extendSubstWithGround(currentGround, currentSubst) // TODO: rigidity
      val updatedNonGround = currentSubst(currentNonGround)
      val (nextGround, nextNonGround) = updatedNonGround.partition(isGround)
      currentGround = nextGround
      currentNonGround = nextNonGround
    }

    (currentNonGround, currentSubst)
  }

  // x = true, x = false, or mirrored. + x flexible
  private def isGround(eq: Equation): Boolean = eq match {
    case Equation(Term.Var(x), Term.True) => true
    case Equation(Term.True, Term.Var(x)) => true
    // TODO: Rest
    case _ => false
  }

  private def extendSubstWithGround(eqns: List[Equation], subst: LocalSubstitution): LocalSubstitution = eqns match {
    case Nil => subst
    case x :: xs => extendSubstWithGround(xs, extendSubstWithSingleGround(x, subst))
  }

  private def extendSubstWithSingleGround(eq: Equation, subst: LocalSubstitution): LocalSubstitution = eq match {
    case Equation(Term.Var(x), t0) => subst.m.get(x) match {
      case None => subst.extended(x, t0)
      case Some(t1) => if (t0 == t1) subst else throw ConflictException(t0, t1)
    }
    case Equation(t0, Term.Var(x)) => subst.m.get(x) match {
      case None => subst.extended(x, t0)
      case Some(t1) => if (t0 == t1) subst else throw ConflictException(t0, t1)
    }
    case _ => throw InternalCompilerException(s"Unexpected equation: '$eq'.", SourceLocation.Unknown)
  }


  private def varPropagate(l: List[Equation], subst0: LocalSubstitution): (List[Equation], LocalSubstitution) = {
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
  private def trivialAssignment(l: List[Equation], subst0: LocalSubstitution): (List[Equation], LocalSubstitution) = {
    var currentSubst = subst0
    var rest = ListBuffer.empty[Equation]

    for (eqn <- l) {
      eqn match {
        case Equation(Term.Var(x), rhs) if !rhs.freeVars.contains(x) =>
          val updatedRhs = currentSubst(rhs)
          currentSubst = currentSubst.extended(x, updatedRhs)
        case _ => rest += eqn
      }
    }

    (currentSubst(rest.toList), currentSubst)
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

  private def boolUnifyAll(l: List[Equation], renv: Set[Int])(implicit flix: Flix): LocalSubstitution = l match {
    case Nil => LocalSubstitution.empty
    case Equation(t1, t2) :: xs =>
      val subst = boolUnifyOne(t1, t2, renv)
      val subst1 = boolUnifyAll(subst(xs), renv)
      subst @@ subst1 // TODO: order?
  }

  private def boolUnifyOne(t1: Term, t2: Term, renv: Set[Int])(implicit flix: Flix): LocalSubstitution = {
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

  private def successiveVariableElimination(t: Term, flexvs: List[Int])(implicit flix: Flix): LocalSubstitution = flexvs match {
    case Nil =>
      // Determine if f is unsatisfiable when all (rigid) variables are made flexible.
      if (!satisfiable(t))
        LocalSubstitution.empty
      else
        throw BoolUnificationException()

    case x :: xs =>
      val t0 = LocalSubstitution.singleton(x, Term.False)(t)
      val t1 = LocalSubstitution.singleton(x, Term.True)(t)
      val se = successiveVariableElimination(Term.mkAnd(t0, t1), xs)

      val f1 = Term.mkOr(se(t0), Term.mkAnd(Term.Var(x), Term.mkNot(se(t1))))
      val st = LocalSubstitution.singleton(x, f1)
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
    case Term.Var(x) => trueVars.contains(x)
    case Term.Not(t) => !evaluate(t, trueVars)
    case Term.Or(ts) => ts.foldLeft(false) { case (bacc, term) => bacc || evaluate(term, trueVars) }
    case Term.And(ts) => ts.foldLeft(true) { case (bacc, term) => bacc && evaluate(term, trueVars) }
  }

  private sealed trait Term {

    def ++(that: Term): Term = Term.mkAnd(this, that)

    final def freeVars: SortedSet[Int] = this match {
      case Term.True => SortedSet.empty
      case Term.False => SortedSet.empty
      case Term.Var(x) => SortedSet(x)
      case Term.Not(t) => t.freeVars
      case Term.And(ts) => ts.foldLeft(SortedSet.empty[Int])(_ ++ _.freeVars)
      case Term.Or(ts) => ts.foldLeft(SortedSet.empty[Int])(_ ++ _.freeVars)
    }

    final def size: Int = this match {
      case Term.True => 0
      case Term.False => 0
      case Term.Var(_) => 1
      case Term.Not(t) => t.size + 1
      case Term.And(ts) => ts.map(_.size).sum + (ts.length - 1)
      case Term.Or(ts) => ts.map(_.size).sum + (ts.length - 1)
    }

    override def toString: String = this match {
      case Term.True => "true"
      case Term.False => "false"
      case Term.Var(x) => s"x$x"
      case Term.Not(f) => f match {
        case Term.Var(x) => s"¬x$x"
        case _ => s"¬($f)"
      }
      case Term.And(ts) => s"(${ts.mkString(" ∧ ")})"
      case Term.Or(ts) => s"(${ts.mkString(" ∨ ")})"
    }

  }

  private object Term {

    case object True extends Term

    case object False extends Term

    case class Var(x: Int) extends Term

    case class Not(t: Term) extends Term

    case class And(ts: List[Term]) extends Term {
      assert(ts.length >= 2)
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
      val varTerms = mutable.Set.empty[Term]
      val nonVarTerms = mutable.ListBuffer.empty[Term]
      for (t <- ts) {
        t match {
          case True => // nop
          case False => return False
          case x@Term.Var(_) => varTerms += x
          case And(ts0) =>
            for (t0 <- ts0) {
              t0 match {
                case True => // nop
                case False => return False
                case x@Term.Var(_) => varTerms += x
                case _ => nonVarTerms += t
              }
            }
          case _ => nonVarTerms += t
        }
      }

      varTerms.toList ++ nonVarTerms.toList match {
        case Nil => True
        case x :: Nil => x
        case xs => And(xs)
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
                case _ => nonVarTerms += t
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
  private object LocalSubstitution {
    val empty: LocalSubstitution = LocalSubstitution(Map.empty)

    def singleton(x: Int, t: Term): LocalSubstitution = empty.extended(x, t)
  }

  private case class LocalSubstitution(m: Map[Int, Term]) {
    def apply(t: Term): Term = t match {
      case Term.True => Term.True
      case Term.False => Term.False
      case Term.Var(x) => m.get(x) match {
        case None => Term.Var(x)
        case Some(t0) => t0
      }
      case Term.Not(t) => Term.mkNot(this.apply(t))
      case Term.And(ts) => Term.mkAnd(ts.map(this.apply))
      case Term.Or(ts) => Term.mkOr(ts.map(this.apply))
    }

    def apply(eq: Equation): Equation = eq match {
      case Equation(t1, t2) => Equation(apply(t1), apply(t2))
    }

    def apply(l: List[Equation]): List[Equation] = l.map(apply)

    def extended(x: Int, t: Term): LocalSubstitution = LocalSubstitution(m + (x -> t))

    def ++(that: LocalSubstitution): LocalSubstitution = {
      assert(this.m.keySet.intersect(that.m.keySet).isEmpty)

      if (this.m.isEmpty) {
        that
      } else if (that.m.isEmpty) {
        this
      } else {
        LocalSubstitution(
          this.m ++ that.m.filter(kv => !this.m.contains(kv._1))
        )
      }
    }

    def @@(that: LocalSubstitution): LocalSubstitution = {
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

      LocalSubstitution(result.toMap)
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

  private def formatSubst(s: LocalSubstitution, indent: Int = 4): String = {
    val sb = new StringBuilder()
    // We sort the bindings by (size, name).
    for ((x, t) <- s.m.toList.sortBy(kv => (kv._2.size, kv._1))) {
      sb.append(" ".repeat(indent))
      sb.append(x)
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

  //  True ~ True
  //  e92719 ~ True
  //  e92722 ~ True
  //  e92725 ~ True
  //  e92728 ~ True
  //  e92730 ~ e92719 + e92722 + e92725 + e92728
  //  e92735 ~ True
  //  e92737 ~ e92735
  //  e92739 ~ e135864 + e92737
  //  e92743 ~ True
  //  e92745 ~ e92743
  //  e92747 ~ e135862 + e92739 + e92745
  //  e92751 ~ True
  //  e92753 ~ e92751
  //  e92755 ~ e135860 + e92747 + e92753
  //  e92759 ~ True
  //  e92761 ~ e92759
  //  e92763 ~ e135858 + e92755 + e92761
  //  e92765 ~ e135855 + e92763
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

  private def example02(): List[Equation] = List(
    True -> (Var(100987) ++ Var(101022) ++ Var(101116)),
    Var(100987) -> (Var(100990) ++ Var(100997) ++ Var(101007) ++ Var(101019)),
    Var(100990) -> Var(108420),
    Var(100994) -> True,
    Var(100996) -> (Var(108422) ++ Var(108423)),
    Var(100997) -> Var(101006),
    Var(101001) -> Var(108430),
    Var(101004) -> Var(108432),
    Var(101006) -> (Var(108427) ++ Var(108426) ++ Var(108428) ++ Var(101004)),
    Var(101007) -> Var(101016),
    Var(101011) -> Var(108439),
    Var(101014) -> Var(108441),
    Var(101016) -> (Var(108436) ++ Var(108435) ++ Var(108437) ++ Var(101014)),
    Var(101019) -> Var(108443),
    Var(101022) -> (Var(101025) ++ Var(101109) ++ Var(101112)),
    Var(101025) -> Var(108444),
    Var(101029) -> True,
    Var(101036) -> Var(108463),
    Var(101039) -> True,
    Var(101041) -> Var(108461),
    Var(101043) -> (Var(108458) ++ Var(101041)),
    Var(101046) -> True,
    Var(101048) -> Var(108467),
    Var(101050) -> (Var(108455) ++ Var(101043) ++ Var(101048)),
    Var(101054) -> True,
    Var(101059) -> True,
    Var(101063) -> (Var(108469) ++ Var(101054) ++ Var(101059)),
    Var(101065) -> Var(108453),
    Var(101073) -> Var(108484),
    Var(101076) -> True,
    Var(101078) -> Var(108482),
    Var(101080) -> (Var(108479) ++ Var(101078)),
    Var(101083) -> True,
    Var(101085) -> Var(108488),
    Var(101087) -> (Var(108476) ++ Var(101080) ++ Var(101085)),
    Var(101091) -> True,
    Var(101096) -> True,
    Var(101101) -> True,
    Var(101105) -> (Var(108490) ++ Var(101091) ++ Var(101096) ++ Var(101101)),
    Var(101107) -> Var(108474),
    Var(101109) -> Var(108447),
    Var(101112) -> Var(108494),
    Var(101116) -> (Var(101119) ++ Var(101125) ++ Var(101131) ++ Var(101134)),
    Var(101119) -> Var(108495),
    Var(101123) -> True,
    Var(101125) -> (Var(108496) ++ Var(101123)),
    Var(101129) -> True,
    Var(101131) -> (Var(108498) ++ Var(101129)),
    Var(101134) -> Var(108500)
  ).map({ case (x, y) => Equation(x, y) })

  def main(args: Array[String]): Unit = {
    implicit val flix: Flix = new Flix()
    //solveAll(example01(), RigidityEnv.empty)
    solveAll(example02(), RigidityEnv.empty)
  }


}
