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
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.collection.Bimap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

import scala.annotation.tailrec

object CaseSetUnification2 {

  /**
    * Returns the most general unifier of the two given set formulas `tpe1` and `tpe2`.
    */
  def unify(tpe1: Type, tpe2: Type, renv: RigidityEnv, cases: List[Symbol.RestrictableCaseSym], enumSym: Symbol.RestrictableEnumSym)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    ///
    /// Perform aggressive matching to optimize for common cases.
    ///
    if (tpe1 eq tpe2) {
      return Ok(Substitution.empty)
    }

    // TODO RESTR-VARS this doesn't help lol
    //    (tpe1, tpe2) match {
    //      case (Type.Var(x, _), Type.Var(y, _)) =>
    //        if (renv.isFlexible(x)) {
    //          return Ok(Substitution.singleton(x, tpe2)) // 135 hits
    //        }
    //        if (renv.isFlexible(y)) {
    //          return Ok(Substitution.singleton(y, tpe1)) // 0 hits
    //        }
    //        if (x == y) {
    //          return Ok(Substitution.empty) // 0 hits
    //        }
    //
    //      case (Type.Cst(TypeConstructor.CaseAll(_), _), Type.Cst(TypeConstructor.CaseAll(_), _)) =>
    //        return Ok(Substitution.empty) // 0 hits
    //
    //      case (Type.Var(x, _), t2@Type.Cst(tc, _)) if renv.isFlexible(x) => tc match {
    //        case TypeConstructor.CaseAll(_) =>
    //          return Ok(Substitution.singleton(x, t2)) // 0 hits
    //        case TypeConstructor.CaseEmpty(sym) =>
    //          return Ok(Substitution.singleton(x, t2)) // 0 hits
    //        case _ => // nop
    //      }
    //
    //      case (Type.Cst(TypeConstructor.CaseEmpty(_), _), Type.Cst(TypeConstructor.CaseEmpty(_), _)) =>
    //        return Ok(Substitution.empty) //  0 hits
    //
    //      case _ => // nop
    //    }

    ///
    /// Run the expensive boolean unification algorithm.
    ///

    val (env, univ) = SetFormula.mkEnv(List(tpe1, tpe2), cases)
    val input1 = SetFormula.fromCaseType(tpe1, env, univ)
    val input2 = SetFormula.fromCaseType(tpe2, env, univ)

    booleanUnification(input1, input2, Set.empty, univ, enumSym, env).map {
      case subst => subst.toTypeSubstitution(enumSym, env)
    }
  }

  /**
    * Returns the most general unifier of the two given set formulas `tpe1` and `tpe2`.
    */
  private def booleanUnification(tpe1: SetFormula, tpe2: SetFormula, renv: Set[Int], univ: Set[Int], sym: Symbol.RestrictableEnumSym, env: Bimap[SetFormula.VarOrCase, Int])(implicit flix: Flix): Result[BoolSubstitution2, UnificationError] = {
    // The boolean expression we want to show is 0.
    val query = mkEq(tpe1, tpe2, univ)

    // Compute the variables in the query.
    val typeVars = query.freeVars.toList

    // Compute the flexible variables.
    val flexibleTypeVars = typeVars.filterNot(renv.contains)

    // Determine the order in which to eliminate the variables.
    //    val freeVars = computeVariableOrder(flexibleTypeVars)
    val freeVars = flexibleTypeVars

    // Eliminate all variables.
    try {
      val subst = successiveVariableElimination(query, freeVars, univ)

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
        val t1 = SetFormula.toCaseType(tpe1, sym, env, SourceLocation.Unknown)
        val t2 = SetFormula.toCaseType(tpe2, sym, env, SourceLocation.Unknown)
        Err(UnificationError.MismatchedBools(t1, t2)) // TODO make setty
    }
  }

  /**
    * A heuristic used to determine the order in which to eliminate variable.
    *
    * Semantically the order of variables is immaterial. Changing the order may
    * yield different unifiers, but they are all equivalent. However, changing
    * the can lead to significant speed-ups / slow-downs.
    *
    * We make the following observation:
    *
    * We want to have synthetic variables (i.e. fresh variables introduced during
    * type inference) expressed in terms of real variables (i.e. variables that
    * actually occur in the source code). We can ensure this by eliminating the
    * synthetic variables first.
    */
  private def computeVariableOrder(l: List[Type.Var]): List[Type.Var] = {
    val realVars = l.filter(_.sym.isReal)
    val synthVars = l.filterNot(_.sym.isReal)
    synthVars ::: realVars
  }

  /**
    * Performs successive variable elimination on the given set expression `f`.
    */
  private def successiveVariableElimination(f: SetFormula, fvs: List[Int], univ: Set[Int])(implicit flix: Flix): BoolSubstitution2 = fvs match {
    case Nil =>
      // Determine if f is necessarily empty when all (rigid) variables and constants are made flexible.
      if (eval(f, univ).isEmpty)
        BoolSubstitution2.empty
      else
        throw SetUnificationException

    case x :: xs =>
      val t0 = BoolSubstitution2.singleton(x, SetFormula.Empty)(f)
      val t1 = BoolSubstitution2.singleton(x, SetFormula.Cst(univ))(f)
      val se = successiveVariableElimination(mkIntersection(t0, t1, univ), xs, univ)

      //      val f1 = TypeMinimization.minimizeType(mkUnion(se(t0), mkIntersection(SetFormula.Var(x), mkComplement(se(t1), univ), univ), univ))
      val f1 = mkUnion(se(t0), mkIntersection(SetFormula.Var(x), mkComplement(se(t1), univ), univ), univ)
      val f2 = SetFormulaAlg.simplifyByExhaustiveEvaluation(f1)(univ)
      val st = BoolSubstitution2.singleton(x, f2)
      st ++ se
  }

  /**
    * An exception thrown to indicate that boolean unification failed.
    */
  private case object SetUnificationException extends RuntimeException

  /**
    * To unify two set formulas p and q it suffices to unify t = (p ∧ ¬q) ∨ (¬p ∧ q) and check t = 0.
    */
  private def mkEq(p: SetFormula, q: SetFormula, univ: Set[Int]): SetFormula = mkUnion(mkIntersection(p, mkComplement(q, univ), univ), mkIntersection(mkComplement(p, univ), q, univ), univ)

  /**
    * Returns the negation of the set formula `tpe0`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  def mkComplement(tpe0: SetFormula, univ: Set[Int]): SetFormula = tpe0 match {
    case SetFormula.Cst(s) =>
      SetFormula.Cst(univ -- s)

    case SetFormula.Not(x) =>
      x

    // ¬(¬x ∨ y) => x ∧ ¬y
    case SetFormula.Or(SetFormula.Not(x), y) =>
      mkIntersection(x, mkComplement(y, univ), univ)

    // ¬(x ∨ ¬y) => ¬x ∧ y
    case SetFormula.Or(x, SetFormula.Not(y)) =>
      mkIntersection(mkComplement(x, univ), y, univ)

    case _ => SetFormula.Not(tpe0)
  }

  /**
    * Returns the conjunction of the two set formulas `tpe1` and `tpe2`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  @tailrec
  def mkIntersection(tpe1: SetFormula, tpe2: SetFormula, univ: Set[Int]): SetFormula = (tpe1, tpe2) match {
    case (SetFormula.Cst(x1), SetFormula.Cst(x2)) =>
      SetFormula.Cst(x1 & x2)

    // ¬x ∧ (x ∨ y) => ¬x ∧ y
    case (SetFormula.Not(x1), SetFormula.Or(x2, y)) if x1 == x2 =>
      mkIntersection(mkComplement(x1, univ), y, univ)

    // x ∧ ¬x => F
    case (x1, SetFormula.Not(x2)) if x1 == x2 =>
      SetFormula.Empty

    // ¬x ∧ x => F
    case (SetFormula.Not(x1), x2) if x1 == x2 =>
      SetFormula.Empty

    // x ∧ (x ∧ y) => (x ∧ y)
    case (x1, SetFormula.And(x2, y)) if x1 == x2 =>
      mkIntersection(x1, y, univ)

    // x ∧ (y ∧ x) => (x ∧ y)
    case (x1, SetFormula.And(y, x2)) if x1 == x2 =>
      mkIntersection(x1, y, univ)

    // (x ∧ y) ∧ x) => (x ∧ y)
    case (SetFormula.And(x1, y), x2) if x1 == x2 =>
      mkIntersection(x1, y, univ)

    // (x ∧ y) ∧ y) => (x ∧ y)
    case (SetFormula.And(x, y1), y2) if y1 == y2 =>
      mkIntersection(x, y1, univ)

    // x ∧ (x ∨ y) => x
    case (x1, SetFormula.Or(x2, _)) if x1 == x2 =>
      x1

    // (x ∨ y) ∧ x => x
    case (SetFormula.Or(x1, _), x2) if x1 == x2 =>
      x1

    // x ∧ (y ∧ ¬x) => F
    case (x1, SetFormula.And(_, SetFormula.Not(x2))) if x1 == x2 =>
      SetFormula.Empty

    // (¬x ∧ y) ∧ x => F
    case (SetFormula.And(SetFormula.Not(x1), _), x2) if x1 == x2 =>
      SetFormula.Empty

    // x ∧ ¬(x ∨ y) => F
    case (x1, SetFormula.Not(SetFormula.Or(x2, _))) if x1 == x2 =>
      SetFormula.Empty

    // ¬(x ∨ y) ∧ x => F
    case (SetFormula.Not(SetFormula.Or(x1, _)), x2) if x1 == x2 =>
      SetFormula.Empty

    // x ∧ (¬x ∧ y) => F
    case (x1, SetFormula.And(SetFormula.Not(x2), _)) if x1 == x2 =>
      SetFormula.Empty

    // (¬x ∧ y) ∧ x => F
    case (SetFormula.And(SetFormula.Not(x1), _), x2) if x1 == x2 =>
      SetFormula.Empty

    // x ∧ x => x
    case _ if tpe1 == tpe2 => tpe1

    case _ =>
      //      val s = s"And($eff1, $eff2)"
      //      val len = s.length
      //      if (true) {
      //        println(s.substring(0, Math.min(len, 300)))
      //      }

      SetFormula.And(tpe1, tpe2)
  }

  /**
    * Returns the disjunction of the two set formulas `tpe1` and `tpe2`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  @tailrec
  def mkUnion(tpe1: SetFormula, tpe2: SetFormula, univ: Set[Int]): SetFormula = (tpe1, tpe2) match {
    case (SetFormula.Cst(s1), SetFormula.Cst(s2)) =>
      SetFormula.Cst(s1 ++ s2)

    // x ∨ (y ∨ x) => x ∨ y
    case (x1, SetFormula.Or(y, x2)) if x1 == x2 =>
      mkUnion(x1, y, univ)

    // (x ∨ y) ∨ x => x ∨ y
    case (SetFormula.Or(x1, y), x2) if x1 == x2 =>
      mkUnion(x1, y, univ)

    // ¬x ∨ x => T
    case (SetFormula.Not(x), y) if x == y =>
      SetFormula.Cst(univ)

    // x ∨ ¬x => T
    case (x, SetFormula.Not(y)) if x == y =>
      SetFormula.Cst(univ)

    // (¬x ∨ y) ∨ x) => T
    case (SetFormula.Or(SetFormula.Not(x), _), y) if x == y =>
      SetFormula.Cst(univ)

    // x ∨ (¬x ∨ y) => T
    case (x, SetFormula.Or(SetFormula.Not(y), _)) if x == y =>
      SetFormula.Cst(univ)

    // x ∨ (y ∧ x) => x
    case (x1, SetFormula.And(_, x2)) if x1 == x2 => x1

    // (y ∧ x) ∨ x => x
    case (SetFormula.And(_, x1), x2) if x1 == x2 => x1

    // x ∨ x => x
    case _ if tpe1 == tpe2 =>
      tpe1

    case _ =>

      //              val s = s"Or($eff1, $eff2)"
      //              val len = s.length
      //              if (len > 30) {
      //                println(s.substring(0, Math.min(len, 300)))
      //              }

      SetFormula.Or(tpe1, tpe2)
  }

  // MATT docs
  def eval(f: SetFormula, univ: Set[Int]): Set[Int] = f match {
    case SetFormula.Cst(s) => s
    case SetFormula.Not(f) => univ -- eval(f, univ)
    case SetFormula.And(f1, f2) => eval(f1, univ) & eval(f2, univ)
    case SetFormula.Or(f1, f2) => eval(f1, univ) ++ eval(f2, univ)
    case SetFormula.Var(x) => throw InternalCompilerException("unexpected var", SourceLocation.Unknown)
  }

  //  /**
  //    * An atom is a constant or a variable.
  //    */
  //  private sealed trait Atom
  //
  //  private object Atom {
  //    case class Var(sym: Symbol.KindedTypeVarSym) extends Atom
  //
  //    case class Case(sym: Symbol.RestrictableCaseSym) extends Atom
  //  }
  //
  //  /**
  //    * A literal is a negated or un-negated atom.
  //    */
  //  private sealed trait Literal
  //
  //  private object Literal {
  //    case class Positive(atom: Atom) extends Literal
  //
  //    case class Negative(atom: Atom) extends Literal
  //  }
  //
  //  /**
  //    * A DNF intersection is a set of literals.
  //    */
  //  private type Intersection = Set[Literal]
  //
  //  /**
  //    * A DNF formula is either:
  //    * - a union of intersections of literals.
  //    * - the universal set
  //    */
  //  private sealed trait Dnf
  //
  //  private object Dnf {
  //    case class Union(inters: Set[Intersection]) extends Dnf // TODO should be a list to avoid extra comparisons?
  //
  //    /**
  //      * The bottom value is an empty union.
  //      */
  //    val Empty = Union(Set())
  //
  //    /**
  //      * The top value is an empty intersection.
  //      */
  //    val All = Union(Set(Set()))
  //  }
  //
  //  /**
  //    * An NNF formula is a formula where all negations are on atoms.
  //    */
  //  private sealed trait Nnf
  //
  //  private object Nnf {
  //    case class Union(tpe1: Nnf, tpe2: Nnf) extends Nnf
  //
  //    case class Intersection(tpe1: Nnf, tpe2: Nnf) extends Nnf
  //
  //    case class Singleton(tpe: Literal) extends Nnf
  //
  //    case object Empty extends Nnf
  //
  //    case object All extends Nnf
  //  }
  //
  //  private def simplify(t: Type)(implicit univ: Universe): Type = {
  //    fromDnf(dnf(t))
  //  }
  //
  //  private def fromDnf(t: Dnf)(implicit univ: Universe): SetFormula = t match {
  //    case Dnf.Union(inters) => inters.filterNot(isEmptyIntersection).map(fromIntersection).reduceOption(mkUnion(_, _, univ.enumSym)).getOrElse(mkEmpty(univ.enumSym))
  //  }
  //
  //  private def fromAtom(a: Atom)(implicit univ: Universe): SetFormula = a match {
  //    case Atom.Var(sym) => Type.Var(sym, SourceLocation.Unknown)
  //    case Atom.Case(sym) => Type.Cst(TypeConstructor.CaseConstant(sym), SourceLocation.Unknown)
  //  }
  //
  //  private def fromLiteral(l: Literal)(implicit univ: Universe): SetFormula = l match {
  //    case Literal.Positive(atom) => fromAtom(atom)
  //    case Literal.Negative(atom) => mkComplement(fromAtom(atom), univ.enumSym)
  //  }
  //
  //  private def fromIntersection(i: Intersection)(implicit univ: Universe): SetFormula = {
  //    i.map(fromLiteral).reduceOption(mkIntersection(_, _, univ.enumSym)).getOrElse(mkAll(univ.enumSym))
  //  }
  //
  //  /**
  //    * Converts the given type to DNF
  //    */
  //  private def dnf(t: SetFormula, univ: Set[Int]): Dnf = {
  //    val n = nnf(t, univ)
  //    val d = nnfToDnf(n)
  //    d
  //  }
  //
  //  /**
  //    * Converts the given type to NNF.
  //    */
  //  private def nnf(t: SetFormula)(implicit univ: Universe): Nnf = t match {
  //    case CONSTANT(sym) => Nnf.Singleton(Literal.Positive(Atom.Case(sym)))
  //    case VAR(sym) => Nnf.Singleton(Literal.Positive(Atom.Var(sym)))
  //    case COMPLEMENT(tpe) => nnfNot(tpe)
  //    case UNION(tpe1, tpe2) => Nnf.Union(nnf(tpe1), nnf(tpe2))
  //    case INTERSECTION(tpe1, tpe2) => Nnf.Intersection(nnf(tpe1), nnf(tpe2))
  //    case EMPTY() => Nnf.Empty
  //    case ALL() => Nnf.All
  //    case _ => throw InternalCompilerException(s"unexpected type: $t", t.loc)
  //  }
  //
  //  /**
  //    * Converts the complement of the given type to NNF.
  //    */
  //  private def nnfNot(t: Type)(implicit univ: Universe): Nnf = t match {
  //    case CONSTANT(sym) => Nnf.Singleton(Literal.Negative(Atom.Case(sym)))
  //    case VAR(sym) => Nnf.Singleton(Literal.Negative(Atom.Var(sym)))
  //    case COMPLEMENT(tpe) => nnf(tpe)
  //    case UNION(tpe1, tpe2) => Nnf.Intersection(
  //      nnf(mkComplement(tpe1, univ.enumSym)),
  //      nnf(mkComplement(tpe2, univ.enumSym))
  //    )
  //    case INTERSECTION(tpe1, tpe2) => Nnf.Union(
  //      nnf(mkComplement(tpe1, univ.enumSym)),
  //      nnf(mkComplement(tpe2, univ.enumSym))
  //    )
  //    case EMPTY() => Nnf.All // MATT ???
  //    case ALL() => Nnf.Empty // MATT ???
  //    case _ => throw InternalCompilerException(s"unexpected type: $t", t.loc)
  //  }
  //
  //  /**
  //    * Converts the given type from NNF to DNF.
  //    */
  //  private def nnfToDnf(t: Nnf): Dnf = t match {
  //    case Nnf.Union(tpe1, tpe2) => union(nnfToDnf(tpe1), nnfToDnf(tpe2))
  //    case Nnf.Intersection(tpe1, tpe2) => intersect(nnfToDnf(tpe1), nnfToDnf(tpe2))
  //    case Nnf.Singleton(tpe) => Dnf.Union(Set(Set(tpe)))
  //    case Nnf.Empty => Dnf.Empty
  //    case Nnf.All => Dnf.All
  //  }
  //
  //  /**
  //    * Calculates the intersection of two DNF sets.
  //    */
  //  private def intersect(t1: Dnf, t2: Dnf): Dnf = (t1, t2) match {
  //    case (Dnf.Union(inters1), Dnf.Union(inters2)) =>
  //      val inters = for {
  //        inter1 <- inters1
  //        inter2 <- inters2
  //      } yield inter1 ++ inter2
  //      Dnf.Union(inters)
  //  }
  //
  //  /**
  //    * Calculates the union of two DNF sets.
  //    */
  //  private def union(t1: Dnf, t2: Dnf): Dnf = (t1, t2) match {
  //    case (Dnf.Union(inters1), Dnf.Union(inters2)) => Dnf.Union(inters1 ++ inters2)
  //  }
  //
  //  /**
  //    * Returns true if the given DNF set represents an empty set.
  //    */
  //  private def isEmpty(t1: Dnf)(implicit univ: Universe): Boolean = t1 match {
  //    case Dnf.Union(inters) => inters.forall(isEmptyIntersection)
  //  }
  //
  //  /**
  //    * Returns true if `t1` represents an empty intersection of effects.
  //    */
  //  private def isEmptyIntersection(t1: Intersection)(implicit univ: Universe): Boolean = {
  //    val pos = t1.collect {
  //      case Literal.Positive(atom) => atom
  //    }
  //    val neg = t1.collect {
  //      case Literal.Negative(atom) => atom
  //    }
  //
  //    // an intersection is empty if any of the following is true:
  //
  //    // 1. It contains two different positive constants
  //    val diffConst = pos.collect {
  //      case Atom.Case(sym) => sym
  //    }.size > 1
  //
  //    // 2. It contains an atom in both the positive and negative sets
  //    val negation = (pos & neg).nonEmpty
  //
  //    // 3. It contains all the negative constants
  //    val allNegConsts = univ.cases.forall {
  //      case c => neg.exists {
  //        case Atom.Case(sym) => c == sym
  //        case Atom.Var(_) => false
  //      }
  //    }
  //
  //    //    if (diffConst || negation || allNegConsts) {
  //    //      println(s" IS EMPTY: $t1")
  //    //    } else {
  //    //      println(s"NOT EMPTY: $t1")
  //    //    }
  //
  //    diffConst || negation || allNegConsts
  //  }
  //
  //  case class Universe(cases: List[Symbol.RestrictableCaseSym], enumSym: Symbol.RestrictableEnumSym)
}
