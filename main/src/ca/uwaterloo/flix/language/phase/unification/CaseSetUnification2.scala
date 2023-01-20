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

    ///
    /// Get rid of of trivial variable cases.
    ///
    (tpe1, tpe2) match {
      case (t1@Type.Var(x, _), t2) if renv.isFlexible(x) && !t2.typeVars.contains(t1) =>
        return Ok(Substitution.singleton(x, t2))

      case (t1, t2@Type.Var(x, _))  if renv.isFlexible(x) && !t1.typeVars.contains(t2) =>
        return Ok(Substitution.singleton(x, t1))

      case _ => // nop
    }

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
      val t0 = BoolSubstitution2.singleton(x, SetFormula.Empty)(f, univ)
      val t1 = BoolSubstitution2.singleton(x, SetFormula.Cst(univ))(f, univ)
      val se = successiveVariableElimination(mkIntersection(t0, t1, univ), xs, univ)

      val f1 = mkUnion(se(t0, univ), mkIntersection(SetFormula.Var(x), mkComplement(se(t1, univ), univ), univ), univ)
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
    case (SetFormula.Cst(x1), x2) if x1 == univ =>
      x2

    case (x1, SetFormula.Cst(x2)) if x2 == univ =>
      x1

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
    case (SetFormula.Cst(x1), x2) if x1 == univ =>
      SetFormula.Cst(x1)

    case (x1, SetFormula.Cst(x2)) if x2 == univ =>
      SetFormula.Cst(x2)

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

  /**
    * Evaluates the set formula. Assumes there are no variables in the formula.
    */
  def eval(f: SetFormula, univ: Set[Int]): Set[Int] = f match {
    case SetFormula.Cst(s) => s
    case SetFormula.Not(f) => univ -- eval(f, univ)
    case SetFormula.And(f1, f2) => eval(f1, univ) & eval(f2, univ)
    case SetFormula.Or(f1, f2) => eval(f1, univ) ++ eval(f2, univ)
    case SetFormula.Var(x) => throw InternalCompilerException("unexpected var", SourceLocation.Unknown)
  }
}
