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
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.phase.unification.BoolAlgebra.{VarOrEff, fromBoolType, fromEffType, toType}
import ca.uwaterloo.flix.language.phase.unification.BoolAlgebraTable.minimizeFormula
import ca.uwaterloo.flix.util.Result.Ok
import ca.uwaterloo.flix.util.collection.Bimap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object BoolUnification3 {

  /**
    * Returns the most general unifier of the two given Boolean formulas `tpe1` and `tpe2`.
    */
  def unify(tpe10: Type, tpe20: Type, renv0: RigidityEnv)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    ///
    /// Perform aggressive matching to optimize for common cases.
    ///
    if (tpe10 eq tpe20) {
      return Ok(Substitution.empty)
    }

    tpe10 match {
      case x: Type.Var if renv0.isFlexible(x.sym) =>
        if (tpe20 eq Type.True)
          return Ok(Substitution.singleton(x.sym, Type.True))
        if (tpe20 eq Type.False)
          return Ok(Substitution.singleton(x.sym, Type.False))

      case _ => // nop
    }

    tpe20 match {
      case y: Type.Var if renv0.isFlexible(y.sym) =>
        if (tpe10 eq Type.True)
          return Ok(Substitution.singleton(y.sym, Type.True))
        if (tpe10 eq Type.False)
          return Ok(Substitution.singleton(y.sym, Type.False))

      case _ => // nop
    }

    // translate the types into formulas
    // Erase aliases to get a processable type
    val tpe1 = Type.eraseAliases(tpe10)
    val tpe2 = Type.eraseAliases(tpe20)

    // Compute the variables in `tpe`.
    val tvars = (tpe1.typeVars ++ tpe2.typeVars).toList.map(tvar => BoolAlgebra.VarOrEff.Var(tvar.sym))
    val effs = (tpe1.effects ++ tpe2.effects).toList.map(BoolAlgebra.VarOrEff.Eff)

    // Construct a bi-directional map from type variables to indices.
    // The idea is that the first variable becomes x0, the next x1, and so forth.
    val m = (tvars ++ effs).zipWithIndex.foldLeft(Bimap.empty[BoolAlgebra.VarOrEff, BoolAlgebraTable.Variable]) {
      case (macc, (sym, x)) => macc + (sym -> x)
    }

    // Convert the type `tpe` to a Boolean formula.
    val f1 = tpe1.kind match {
      case Kind.Bool => fromBoolType(tpe1, m)
      case Kind.Effect => fromEffType(tpe1, m)
      case _ => throw InternalCompilerException(s"Unexpected non-Bool/non-Effect kind: '${tpe1.kind}'.")
    }

    val f2 = tpe2.kind match {
      case Kind.Bool => fromBoolType(tpe2, m)
      case Kind.Effect => fromEffType(tpe2, m)
      case _ => throw InternalCompilerException(s"Unexpected non-Bool/non-Effect kind: '${tpe2.kind}'.")
    }

    ///
    /// Run the expensive boolean unification algorithm.
    ///
    val renv = renv0.s.toList.flatMap(tvar => m.getForward(VarOrEff.Var(tvar))).toSet
    booleanUnification(f1, f2, renv) match {
      case Success(subst0) =>
        val map = subst0.m.toList.map {
          case (key0, value0) =>
            val key = m.getBackward(key0) match {
              case Some(VarOrEff.Var(sym)) => sym
              case _ => throw InternalCompilerException(s"unexpected missing var $key0")
            }
            val value = toType(value0, m, tpe1.kind, tpe1.loc)
            (key: Symbol.TypeVarSym, value)
        }.toMap
        Ok(Substitution(map))
      case Failure(BooleanUnificationException) => Result.Err(UnificationError.MismatchedBools(tpe1, tpe2))
      case Failure(error) => throw error
    }
  }

  /**
    * Returns the most general unifier of the two given Boolean formulas `tpe1` and `tpe2`.
    */
  private def booleanUnification(tpe1: BoolAlgebra, tpe2: BoolAlgebra, renv: Set[Int])(implicit flix: Flix): Try[BoolAlgebraSubstitution] = {
    // The boolean expression we want to show is 0.
    val query = mkEq(tpe1, tpe2)

    // Compute the variables in the query.
    val typeVars = query.freeVars.toList

    // Compute the flexible variables.
    val flexibleTypeVars = typeVars.filterNot(renv.contains)

    // Determine the order in which to eliminate the variables.
    val freeVars = computeVariableOrder(flexibleTypeVars)

    // Eliminate all variables.
    Try {
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
  private def computeVariableOrder(l: List[Int]): List[Int] = {
    //    val realVars = l.filter(_.isReal
    //    val synthVars = l.filterNeg(_.isReal)
    //    synthVars ::: realVars
    l
  }

  /**
    * Performs success variable elimination on the given boolean expression `f`.
    *
    * `flexvs` is the list of remaining flexible variables in the expression.
    */
  private def successiveVariableElimination(f: BoolAlgebra, flexvs: List[Int])(implicit flix: Flix): BoolAlgebraSubstitution = flexvs match {
    case Nil =>
      // Determine if f is unsatisfiable when all (rigid) variables are made flexible.
      if (!satisfiable(f))
        BoolAlgebraSubstitution.empty
      else
        throw BooleanUnificationException

    case x :: xs =>
      val t0 = BoolAlgebraSubstitution.singleton(x, BoolAlgebra.Bot)(f)
      val t1 = BoolAlgebraSubstitution.singleton(x, BoolAlgebra.Top)(f)
      val se = successiveVariableElimination(mkJoin(t0, t1), xs)

      val f1 = minimizeFormula(mkMeet(se(t0), mkJoin(BoolAlgebra.Var(x), mkNeg(se(t1)))))
      val st = BoolAlgebraSubstitution.singleton(x, f1)
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
  private def satisfiable(f: BoolAlgebra)(implicit flix: Flix): Boolean = f match {
    case BoolAlgebra.Top => true
    case BoolAlgebra.Bot => false
    case _ =>
      val q = mkEq(f, BoolAlgebra.Top)
      try {
        successiveVariableElimination(q, q.freeVars.toList)
        true
      } catch {
        case BooleanUnificationException => false
      }
  }


  /**
    * To unify two Boolean formulas p and q it suffices to unify t = (p ∧ ¬q) ∨ (¬p ∧ q) and check t = 0.
    */
  private def mkEq(p: BoolAlgebra, q: BoolAlgebra): BoolAlgebra = mkMeet(mkJoin(p, mkNeg(q)), mkJoin(mkNeg(p), q))

  /**
    * Returns the negation of the Boolean formula `tpe0`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  def mkNeg(f0: BoolAlgebra): BoolAlgebra = f0 match {
    case BoolAlgebra.Top =>
      BoolAlgebra.Bot

    case BoolAlgebra.Bot =>
      BoolAlgebra.Top

    case BoolAlgebra.Neg(x) =>
      x

    // ¬(¬x ∨ y) => x ∧ ¬y
    case BoolAlgebra.Meet(BoolAlgebra.Neg(x), y) =>
      mkJoin(x, mkNeg(y))

    // ¬(x ∨ ¬y) => ¬x ∧ y
    case BoolAlgebra.Meet(x, BoolAlgebra.Neg(y)) =>
      mkJoin(mkNeg(x), y)

    case _ => BoolAlgebra.Neg(f0)
  }

  /**
    * Returns the conjunction of the two Boolean formulas `tpe1` and `tpe2`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  @tailrec
  def mkJoin(tpe1: BoolAlgebra, tpe2: BoolAlgebra): BoolAlgebra = (tpe1, tpe2) match {
    // T ∧ x => x
    case (BoolAlgebra.Top, _) =>
      tpe2

    // x ∧ T => x
    case (_, BoolAlgebra.Top) =>
      tpe1

    // F ∧ x => F
    case (BoolAlgebra.Bot, _) =>
      BoolAlgebra.Bot

    // x ∧ F => F
    case (_, BoolAlgebra.Bot) =>
      BoolAlgebra.Bot

    // ¬x ∧ (x ∨ y) => ¬x ∧ y
    case (BoolAlgebra.Neg(x1), BoolAlgebra.Meet(x2, y)) if x1 == x2 =>
      mkJoin(mkNeg(x1), y)

    // x ∧ ¬x => F
    case (x1, BoolAlgebra.Neg(x2)) if x1 == x2 =>
      BoolAlgebra.Bot

    // ¬x ∧ x => F
    case (BoolAlgebra.Neg(x1), x2) if x1 == x2 =>
      BoolAlgebra.Bot

    // x ∧ (x ∧ y) => (x ∧ y)
    case (x1, BoolAlgebra.Join(x2, y)) if x1 == x2 =>
      mkJoin(x1, y)

    // x ∧ (y ∧ x) => (x ∧ y)
    case (x1, BoolAlgebra.Join(y, x2)) if x1 == x2 =>
      mkJoin(x1, y)

    // (x ∧ y) ∧ x) => (x ∧ y)
    case (BoolAlgebra.Join(x1, y), x2) if x1 == x2 =>
      mkJoin(x1, y)

    // (x ∧ y) ∧ y) => (x ∧ y)
    case (BoolAlgebra.Join(x, y1), y2) if y1 == y2 =>
      mkJoin(x, y1)

    // x ∧ (x ∨ y) => x
    case (x1, BoolAlgebra.Meet(x2, _)) if x1 == x2 =>
      x1

    // (x ∨ y) ∧ x => x
    case (BoolAlgebra.Meet(x1, _), x2) if x1 == x2 =>
      x1

    // x ∧ (y ∧ ¬x) => F
    case (x1, BoolAlgebra.Join(_, BoolAlgebra.Neg(x2))) if x1 == x2 =>
      BoolAlgebra.Bot

    // (¬x ∧ y) ∧ x => F
    case (BoolAlgebra.Join(BoolAlgebra.Neg(x1), _), x2) if x1 == x2 =>
      BoolAlgebra.Bot

    // x ∧ ¬(x ∨ y) => F
    case (x1, BoolAlgebra.Neg(BoolAlgebra.Meet(x2, _))) if x1 == x2 =>
      BoolAlgebra.Bot

    // ¬(x ∨ y) ∧ x => F
    case (BoolAlgebra.Neg(BoolAlgebra.Meet(x1, _)), x2) if x1 == x2 =>
      BoolAlgebra.Bot

    // x ∧ (¬x ∧ y) => F
    case (x1, BoolAlgebra.Join(BoolAlgebra.Neg(x2), _)) if x1 == x2 =>
      BoolAlgebra.Bot

    // (¬x ∧ y) ∧ x => F
    case (BoolAlgebra.Join(BoolAlgebra.Neg(x1), _), x2) if x1 == x2 =>
      BoolAlgebra.Bot

    // x ∧ x => x
    case _ if tpe1 == tpe2 => tpe1

    case _ =>
      //      val s = s"Join($eff1, $eff2)"
      //      val len = s.length
      //      if (true) {
      //        println(s.substring(0, Math.min(len, 300)))
      //      }

      BoolAlgebra.Join(tpe1, tpe2)
  }

  /**
    * Returns the disjunction of the two Boolean formulas `tpe1` and `tpe2`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  @tailrec
  def mkMeet(tpe1: BoolAlgebra, tpe2: BoolAlgebra): BoolAlgebra = (tpe1, tpe2) match {
    // T ∨ x => T
    case (BoolAlgebra.Top, _) =>
      BoolAlgebra.Top

    // F ∨ y => y
    case (BoolAlgebra.Bot, _) =>
      tpe2

    // x ∨ T => T
    case (_, BoolAlgebra.Top) =>
      BoolAlgebra.Top

    // x ∨ F => x
    case (_, BoolAlgebra.Bot) =>
      tpe1

    // x ∨ (y ∨ x) => x ∨ y
    case (x1, BoolAlgebra.Meet(y, x2)) if x1 == x2 =>
      mkMeet(x1, y)

    // (x ∨ y) ∨ x => x ∨ y
    case (BoolAlgebra.Meet(x1, y), x2) if x1 == x2 =>
      mkMeet(x1, y)

    // ¬x ∨ x => T
    case (BoolAlgebra.Neg(x), y) if x == y =>
      BoolAlgebra.Top

    // x ∨ ¬x => T
    case (x, BoolAlgebra.Neg(y)) if x == y =>
      BoolAlgebra.Top

    // (¬x ∨ y) ∨ x) => T
    case (BoolAlgebra.Meet(BoolAlgebra.Neg(x), _), y) if x == y =>
      BoolAlgebra.Top

    // x ∨ (¬x ∨ y) => T
    case (x, BoolAlgebra.Meet(BoolAlgebra.Neg(y), _)) if x == y =>
      BoolAlgebra.Top

    // x ∨ (y ∧ x) => x
    case (x1, BoolAlgebra.Join(_, x2)) if x1 == x2 => x1

    // (y ∧ x) ∨ x => x
    case (BoolAlgebra.Join(_, x1), x2) if x1 == x2 => x1

    // x ∨ x => x
    case _ if tpe1 == tpe2 =>
      tpe1

    case _ =>

      //              val s = s"Meet($eff1, $eff2)"
      //              val len = s.length
      //              if (len > 30) {
      //                println(s.substring(0, Math.min(len, 300)))
      //              }

      BoolAlgebra.Meet(tpe1, tpe2)
  }
}
