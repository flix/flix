/*
 *  Copyright 2020 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.Type.eraseAliases
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

object BoolUnification2 {
  sealed trait Formula {
    /**
      * Returns the set of type variables in this formula.
      */
    def typeVars: SortedSet[Symbol.KindedTypeVarSym] = this match {
      case Formula.True => SortedSet.empty
      case Formula.False => SortedSet.empty
      case Formula.Not(f) => f.typeVars
      case Formula.And(f1, f2) => f1.typeVars ++ f2.typeVars
      case Formula.Or(f1, f2) => f1.typeVars ++ f2.typeVars
      case Formula.Var(sym) => SortedSet(sym)
    }
  }

  object Formula {
    case object True extends Formula
    case object False extends Formula
    case class Not(f: Formula) extends Formula
    case class And(f1: Formula, f2: Formula) extends Formula
    case class Or(f1: Formula, f2: Formula) extends Formula
    case class Var(sym: Symbol.KindedTypeVarSym) extends Formula
  }

  /**
    * Returns the most general unifier of the two given Boolean formulas `tpe1` and `tpe2`.
    */
  def unify(tpe1: Type, tpe2: Type, renv: RigidityEnv)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    ///
    /// Perform aggressive matching to optimize for common cases.
    ///
    if (tpe1 eq tpe2) {
      return Ok(Substitution.empty)
    }

    tpe1 match {
      case x: Type.Var if renv.isFlexible(x.sym) =>
        if (tpe2 eq Type.True)
          return Ok(Substitution.singleton(x.sym, Type.True))
        if (tpe2 eq Type.False)
          return Ok(Substitution.singleton(x.sym, Type.False))

      case _ => // nop
    }

    tpe2 match {
      case y: Type.Var if renv.isFlexible(y.sym) =>
        if (tpe1 eq Type.True)
          return Ok(Substitution.singleton(y.sym, Type.True))
        if (tpe1 eq Type.False)
          return Ok(Substitution.singleton(y.sym, Type.False))

      case _ => // nop
    }

    ///
    /// Run the expensive boolean unification algorithm.
    ///
    booleanUnification(eraseAliases(tpe1), eraseAliases(tpe2), renv)
  }

  /**
    * Returns the most general unifier of the two given Boolean formulas `tpe1` and `tpe2`.
    */
  private def booleanUnification(tpe1: Formula, tpe2: Formula, renv: RigidityEnv)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    // The boolean expression we want to show is 0.
    val query = mkEq(tpe1, tpe2)

    // Compute the variables in the query.
    val typeVars = query.typeVars.toList

    // Compute the flexible variables.
    val flexibleTypeVars = typeVars.filter(renv.isFlexible)

    // Determine the order in which to eliminate the variables.
    val freeVars = computeVariableOrder(flexibleTypeVars)

    // Eliminate all variables.
    try {
      val subst = successiveVariableElimination(query, freeVars)

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
      case BooleanUnificationException => Err(UnificationError.MismatchedBools(tpe1, tpe2))
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
  private def computeVariableOrder(l: List[Symbol.KindedTypeVarSym]): List[Symbol.KindedTypeVarSym] = {
    val realVars = l.filter(_.isReal)
    val synthVars = l.filterNot(_.isReal)
    synthVars ::: realVars
  }

  /**
    * Performs success variable elimination on the given boolean expression `f`.
    *
    * `flexvs` is the list of remaining flexible variables in the expression.
    */
  private def successiveVariableElimination(f: Formula, flexvs: List[Symbol.KindedTypeVarSym])(implicit flix: Flix): Substitution = flexvs match {
    case Nil =>
      // Determine if f is unsatisfiable when all (rigid) variables are made flexible.
      if (!satisfiable(f))
        Substitution.empty
      else
        throw BooleanUnificationException

    case x :: xs =>
      val t0 = Substitution.singleton(x, Type.False)(f)
      val t1 = Substitution.singleton(x, Type.True)(f)
      val se = successiveVariableElimination(mkAnd(t0, t1), xs)

      val f1 = TypeMinimization.minimizeType(mkOr(se(t0), mkAnd(x, mkNot(se(t1)))))
      val st = Substitution.singleton(x.sym, f1)
      st ++ se
  }

  /**
    * An exception thrown to indicate that boolean unification failed.
    */
  private case object BooleanUnificationException extends RuntimeException

  /**
    * Returns `true` if the given boolean formula `f` is satisfiable
    * when ALL variables in the formula are flexible.
    */
  private def satisfiable(f: Type)(implicit flix: Flix): Boolean = f match {
    case Type.True => true
    case Type.False => false
    case _ =>
      val q = mkEq(f, Type.True)
      try {
        successiveVariableElimination(q, q.typeVars.toList)
        true
      } catch {
        case BooleanUnificationException => false
      }
  }


  /**
    * To unify two Boolean formulas p and q it suffices to unify t = (p ∧ ¬q) ∨ (¬p ∧ q) and check t = 0.
    */
  private def mkEq(p: Formula, q: Formula): Formula = mkOr(mkAnd(p, mkNot(q)), mkAnd(mkNot(p), q))

  /**
    * Returns the negation of the Boolean formula `tpe0`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  def mkNot(f0: Formula): Formula = f0 match {
    case Formula.True =>
      Formula.False

    case Formula.False =>
      Formula.True

    case Formula.Not(x) =>
      x

    // ¬(¬x ∨ y) => x ∧ ¬y
    case Formula.Or(Formula.Not(x), y) =>
      mkAnd(x, mkNot(y))

    // ¬(x ∨ ¬y) => ¬x ∧ y
    case Formula.Or(x, Formula.Not(y)) =>
      mkAnd(mkNot(x), y)

    case _ => Formula.Not(f0)
  }

  /**
    * Returns the conjunction of the two Boolean formulas `tpe1` and `tpe2`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  @tailrec
  def mkAnd(tpe1: Formula, tpe2: Formula): Formula = (tpe1, tpe2) match {
    // T ∧ x => x
    case (Formula.True, _) =>
      tpe2

    // x ∧ T => x
    case (_, Formula.True) =>
      tpe1

    // F ∧ x => F
    case (Formula.False, _) =>
      Formula.False

    // x ∧ F => F
    case (_, Formula.False) =>
      Formula.False

    // ¬x ∧ (x ∨ y) => ¬x ∧ y
    case (Formula.Not(x1), Formula.Or(x2, y)) if x1 == x2 =>
      mkAnd(mkNot(x1), y)

    // x ∧ ¬x => F
    case (x1, Formula.Not(x2)) if x1 == x2 =>
      Formula.False

    // ¬x ∧ x => F
    case (Formula.Not(x1), x2) if x1 == x2 =>
      Formula.False

    // x ∧ (x ∧ y) => (x ∧ y)
    case (x1, Formula.And(x2, y)) if x1 == x2 =>
      mkAnd(x1, y)

    // x ∧ (y ∧ x) => (x ∧ y)
    case (x1, Formula.And(y, x2)) if x1 == x2 =>
      mkAnd(x1, y)

    // (x ∧ y) ∧ x) => (x ∧ y)
    case (Formula.And(x1, y), x2) if x1 == x2 =>
      mkAnd(x1, y)

    // (x ∧ y) ∧ y) => (x ∧ y)
    case (Formula.And(x, y1), y2) if y1 == y2 =>
      mkAnd(x, y1)

    // x ∧ (x ∨ y) => x
    case (x1, Formula.Or(x2, _)) if x1 == x2 =>
      x1

    // (x ∨ y) ∧ x => x
    case (Formula.Or(x1, _), x2) if x1 == x2 =>
      x1

    // x ∧ (y ∧ ¬x) => F
    case (x1, Formula.And(_, Formula.Not(x2))) if x1 == x2 =>
      Formula.False

    // (¬x ∧ y) ∧ x => F
    case (Formula.And(Formula.Not(x1), _), x2) if x1 == x2 =>
      Formula.False

    // x ∧ ¬(x ∨ y) => F
    case (x1, Formula.Not(Formula.Or(x2, _))) if x1 == x2 =>
      Formula.False

    // ¬(x ∨ y) ∧ x => F
    case (Formula.Not(Formula.Or(x1, _)), x2) if x1 == x2 =>
      Formula.False

    // x ∧ (¬x ∧ y) => F
    case (x1, Formula.And(Formula.Not(x2), _)) if x1 == x2 =>
      Formula.False

    // (¬x ∧ y) ∧ x => F
    case (Formula.And(Formula.Not(x1), _), x2) if x1 == x2 =>
      Formula.False

    // x ∧ x => x
    case _ if tpe1 == tpe2 => tpe1

    case _ =>
      //      val s = s"And($eff1, $eff2)"
      //      val len = s.length
      //      if (true) {
      //        println(s.substring(0, Math.min(len, 300)))
      //      }

      Formula.And(tpe1, tpe2)
  }

  /**
    * Returns the disjunction of the two Boolean formulas `tpe1` and `tpe2`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  @tailrec
  def mkOr(tpe1: Formula, tpe2: Formula): Formula = (tpe1, tpe2) match {
    // T ∨ x => T
    case (Formula.True, _) =>
      Formula.True

    // F ∨ y => y
    case (Formula.False, _) =>
      tpe2

    // x ∨ T => T
    case (_, Formula.True) =>
      Formula.True

    // x ∨ F => x
    case (_, Formula.False) =>
      tpe1

    // x ∨ (y ∨ x) => x ∨ y
    case (x1, Formula.Or(y, x2)) if x1 == x2 =>
      mkOr(x1, y)

    // (x ∨ y) ∨ x => x ∨ y
    case (Formula.Or(x1, y), x2) if x1 == x2 =>
      mkOr(x1, y)

    // ¬x ∨ x => T
    case (Formula.Not(x), y) if x == y =>
      Formula.True

    // x ∨ ¬x => T
    case (x, Formula.Not(y)) if x == y =>
      Formula.True

    // (¬x ∨ y) ∨ x) => T
    case (Formula.Or(Formula.Not(x), _), y) if x == y =>
      Formula.True

    // x ∨ (¬x ∨ y) => T
    case (x, Formula.Or(Formula.Not(y), _)) if x == y =>
      Formula.True

    // x ∨ (y ∧ x) => x
    case (x1, Formula.And(_, x2)) if x1 == x2 => x1

    // (y ∧ x) ∨ x => x
    case (Formula.And(_, x1), x2) if x1 == x2 => x1

    // x ∨ x => x
    case _ if tpe1 == tpe2 =>
      tpe1

    case _ =>

      //              val s = s"Or($eff1, $eff2)"
      //              val len = s.length
      //              if (len > 30) {
      //                println(s.substring(0, Math.min(len, 300)))
      //              }

      Formula.Or(tpe1, tpe2)
  }
}
