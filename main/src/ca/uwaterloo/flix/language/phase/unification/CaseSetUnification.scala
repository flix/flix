/*
 *  Copyright 2023 Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.phase.unification.SetFormula.*
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.collection.Bimap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

import scala.collection.immutable.SortedSet

object CaseSetUnification {

  /**
    * Returns the most general unifier of the two given set formulas `tpe1` and `tpe2`.
    */
  def unify(tpe1: Type, tpe2: Type, renv0: RigidityEnv, cases: SortedSet[Symbol.RestrictableCaseSym], enumSym: Symbol.RestrictableEnumSym)(implicit scope: Scope, flix: Flix): Result[Substitution, UnificationError] = {
    ///
    /// Perform aggressive matching to optimize for common cases.
    ///
    if (tpe1 eq tpe2) {
      return Ok(Substitution.empty)
    }

    ///
    /// Get rid of of trivial variable cases.
    ///
    (tpe1, tpe2) match {
      case (t1@Type.Var(x, _), t2) if renv0.isFlexible(x) && !t2.typeVars.contains(t1) =>
        return Ok(Substitution.singleton(x, t2))

      case (t1, t2@Type.Var(x, _)) if renv0.isFlexible(x) && !t1.typeVars.contains(t2) =>
        return Ok(Substitution.singleton(x, t1))

      case _ => // nop
    }

    ///
    /// Run the expensive boolean unification algorithm.
    ///
    val (env, univ) = mkEnv(List(tpe1, tpe2), cases)
    val input1 = fromCaseType(tpe1, env, univ)
    val input2 = fromCaseType(tpe2, env, univ)
    val renv = liftRigidityEnv(renv0, env)

    booleanUnification(input1, input2, renv, univ, enumSym, env).map {
      case subst => subst.toTypeSubstitution(enumSym, env)
    }
  }

  /**
    * Returns the most general unifier of the two given set formulas `tpe1` and `tpe2`.
    */
  private def booleanUnification(tpe1: SetFormula, tpe2: SetFormula, renv: Set[Int], univ: Set[Int], sym: Symbol.RestrictableEnumSym, env: Bimap[VarOrCase, Int])(implicit flix: Flix): Result[CaseSetSubstitution, UnificationError] = {
    // The boolean expression we want to show is 0.
    val query = minimize(mkEq(tpe1, tpe2)(univ))(univ)

    // Compute the variables in the query.
    val typeVars = query.freeVars.toList

    // Compute the flexible variables.
    val flexibleTypeVars = typeVars.filterNot(renv.contains)

    // Determine the order in which to eliminate the variables.
    //    val freeVars = computeVariableOrder(flexibleTypeVars)
    val freeVars = flexibleTypeVars

    // Eliminate all variables.
    try {
      val subst = successiveVariableElimination(query, freeVars)(univ, flix)

      //    if (!subst.isEmpty) {
      //      val s = subst.toString
      //      val len = s.length
      //      if (len > 50) {
      //        println(s.substring(0, Math.min(len, 300)))
      //        println()
      //      }
      //    }

      Ok(subst)
    } catch {
      case SetUnificationException =>
        val t1 = toCaseType(tpe1, sym, env, SourceLocation.Unknown)
        val t2 = toCaseType(tpe2, sym, env, SourceLocation.Unknown)
        Err(UnificationError.MismatchedCaseSets(t1, t2))
    }
  }

  /**
    * Performs successive variable elimination on the given set expression `f`.
    */
  private def successiveVariableElimination(f: SetFormula, fvs: List[Int])(implicit univ: Set[Int], flix: Flix): CaseSetSubstitution = fvs match {
    case Nil =>
      // Determine if f is necessarily empty when all (rigid) variables and constants are made flexible.
      if (isEmpty(dnf(f)))
        CaseSetSubstitution.empty
      else
        throw SetUnificationException

    case x :: xs =>
      val t0 = CaseSetSubstitution.singleton(x, Empty)(f)
      val t1 = CaseSetSubstitution.singleton(x, mkUni())(f)
      val se = successiveVariableElimination(mkAnd(t0, t1), xs)

      val f1 = mkOr(se(t0), mkAnd(Var(x), mkNot(se(t1))))
      val f2 = minimize(f1)
      val st = CaseSetSubstitution.singleton(x, f2)
      st ++ se
  }

  /**
    * An exception thrown to indicate that boolean unification failed.
    */
  private case object SetUnificationException extends RuntimeException

  /**
    * To unify two set formulas p and q it suffices to unify t = (p ∧ ¬q) ∨ (¬p ∧ q) and check t = 0.
    */
  private def mkEq(p: SetFormula, q: SetFormula)(implicit univ: Set[Int]): SetFormula =
    mkOr(mkAnd(p, mkNot(q)), mkAnd(mkNot(p), q))

  /**
    * An atom is a constant or a variable.
    */
  private sealed trait Atom

  private object Atom {
    case class Var(sym: Int) extends Atom

    case class Case(sym: Int) extends Atom
  }

  /**
    * A literal is a negated or un-negated atom.
    */
  private sealed trait Literal

  private object Literal {
    case class Positive(atom: Atom) extends Literal

    case class Negative(atom: Atom) extends Literal
  }

  /**
    * A DNF intersection is a set of literals.
    */
  private type Intersection = Set[Literal]

  /**
    * A DNF formula is either:
    *   - a union of intersections of literals.
    *   - the universal set
    */
  private sealed trait Dnf

  private object Dnf {
    case class Union(inters: Set[Intersection]) extends Dnf // TODO should be a list to avoid extra comparisons?

    /**
      * The bottom value is an empty union.
      */
    val Empty: Union = Union(Set())

    /**
      * The top value is an empty intersection.
      */
    val All: Union = Union(Set(Set()))
  }

  /**
    * An NNF formula is a formula where all negations are on atoms.
    */
  private sealed trait Nnf

  private object Nnf {
    case class Union(tpe1: Nnf, tpe2: Nnf) extends Nnf

    case class Intersection(tpe1: Nnf, tpe2: Nnf) extends Nnf

    case class Singleton(tpe: Literal) extends Nnf

    case object Empty extends Nnf

    case object All extends Nnf
  }

  /**
    * Converts the given type to DNF
    */
  private def dnf(t: SetFormula)(implicit univ: Set[Int]): Dnf = {
    val n = nnf(t)
    val d = nnfToDnf(n)
    d
  }

  /**
    * Converts the given type to NNF.
    */
  private def nnf(t: SetFormula)(implicit univ: Set[Int]): Nnf = t match {
    case Cst(syms) =>
      val lits: Set[Nnf] = syms.map(sym => Nnf.Singleton(Literal.Positive(Atom.Case(sym))))
      lits.reduceLeftOption(Nnf.Union.apply).getOrElse(Nnf.Empty)
    case Var(sym) => Nnf.Singleton(Literal.Positive(Atom.Var(sym)))
    case Not(tpe) => nnfNot(tpe)
    case Or(tpe1, tpe2) => Nnf.Union(nnf(tpe1), nnf(tpe2))
    case And(tpe1, tpe2) => Nnf.Intersection(nnf(tpe1), nnf(tpe2))
  }

  /**
    * Converts the complement of the given type to NNF.
    */
  private def nnfNot(t: SetFormula)(implicit univ: Set[Int]): Nnf = t match {
    case Cst(syms) =>
      val lits: Set[Nnf] = syms.map(sym => Nnf.Singleton(Literal.Negative(Atom.Case(sym))))
      lits.reduceLeftOption(Nnf.Intersection.apply).getOrElse(Nnf.All)
    case Var(sym) => Nnf.Singleton(Literal.Negative(Atom.Var(sym)))
    case Not(tpe) => nnf(tpe)
    case Or(tpe1, tpe2) => Nnf.Intersection(
      nnf(mkNot(tpe1)),
      nnf(mkNot(tpe2))
    )
    case And(tpe1, tpe2) => Nnf.Union(
      nnf(mkNot(tpe1)),
      nnf(mkNot(tpe2))
    )
  }

  /**
    * Converts the given type from NNF to DNF.
    */
  private def nnfToDnf(t: Nnf): Dnf = t match {
    case Nnf.Union(tpe1, tpe2) => union(nnfToDnf(tpe1), nnfToDnf(tpe2))
    case Nnf.Intersection(tpe1, tpe2) => intersect(nnfToDnf(tpe1), nnfToDnf(tpe2))
    case Nnf.Singleton(tpe) => Dnf.Union(Set(Set(tpe)))
    case Nnf.Empty => Dnf.Empty
    case Nnf.All => Dnf.All
  }

  /**
    * Calculates the intersection of two DNF sets.
    */
  private def intersect(t1: Dnf, t2: Dnf): Dnf = (t1, t2) match {
    case (Dnf.Union(inters1), Dnf.Union(inters2)) =>
      val inters = for {
        inter1 <- inters1
        inter2 <- inters2
      } yield inter1 ++ inter2
      Dnf.Union(inters)
  }

  /**
    * Calculates the union of two DNF sets.
    */
  private def union(t1: Dnf, t2: Dnf): Dnf = (t1, t2) match {
    case (Dnf.Union(inters1), Dnf.Union(inters2)) => Dnf.Union(inters1 ++ inters2)
  }

  /**
    * Returns true if the given DNF set represents an empty set.
    */
  private def isEmpty(t1: Dnf)(implicit univ: Set[Int]): Boolean = t1 match {
    case Dnf.Union(inters) => inters.forall(isEmptyIntersection)
  }

  /*complement *
    * Returns true if `t1` represents an empty intersection of effects.
    */
  private def isEmptyIntersection(t1: Intersection)(implicit univ: Set[Int]): Boolean = {
    val pos = t1.collect {
      case Literal.Positive(atom) => atom
    }
    val neg = t1.collect {
      case Literal.Negative(atom) => atom
    }

    // an intersection is empty if any of the following is true:

    // 1. It contains two different positive constants
    val diffConst = pos.collect {
      case Atom.Case(sym) => sym
    }.size > 1

    // 2. It contains an atom in both the positive and negative sets
    val negation = (pos & neg).nonEmpty

    // 3. It contains the whole universe in the negative set.
    val negUniv = univ.forall {
      case sym => neg.contains(Atom.Case(sym))
    }

    diffConst || negation || negUniv
  }

}
