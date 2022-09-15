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
import ca.uwaterloo.flix.language.phase.unification.ExplicitFormula.{VarOrEff, fromBoolType, toType}
import ca.uwaterloo.flix.language.phase.unification.BoolAlgebraTable.minimizeFormula
import ca.uwaterloo.flix.util.Result.Ok
import ca.uwaterloo.flix.util.collection.Bimap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

import scala.util.{Failure, Success, Try}

object BoolUnification2 {

  def unify[F](f1: F, f2: F, renv: Set[Int])(implicit flix: Flix, alg: BoolFormula[F]): Option[BoolAlgebraSubstitution[F]] = {
    booleanUnification(f1, f2, renv).toOption
  }

  /**
    * Returns the most general unifier of the two given Boolean formulas `tpe1` and `tpe2`.
    */
  private def booleanUnification[F](tpe1: F, tpe2: F, renv: Set[Int])(implicit flix: Flix, alg: BoolFormula[F]): Try[BoolAlgebraSubstitution[F]] = {
    // The boolean expression we want to show is 0.
    val query = mkEq(tpe1, tpe2)

    // Compute the variables in the query.
    val typeVars = alg.freeVars(query).toList

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
    l.reverse // MATT idk just reversing to see what happens
  }

  /**
    * Performs success variable elimination on the given boolean expression `f`.
    *
    * `flexvs` is the list of remaining flexible variables in the expression.
    */
  private def successiveVariableElimination[F](f: F, flexvs: List[Int])(implicit flix: Flix, alg: BoolFormula[F]): BoolAlgebraSubstitution[F] = flexvs match {
    case Nil =>
      // Determine if f is unsatisfiable when all (rigid) variables are made flexible.
      if (!satisfiable(f))
        BoolAlgebraSubstitution.empty
      else
        throw BooleanUnificationException

    case x :: xs =>
      val t0 = BoolAlgebraSubstitution.singleton(x, alg.mkFalse)(f)
      val t1 = BoolAlgebraSubstitution.singleton(x, alg.mkTrue)(f)
      val se = successiveVariableElimination(alg.mkAnd(t0, t1), xs)

      val f1 = alg.minimize(alg.mkOr(se(t0), alg.mkAnd(alg.mkVar(x), alg.mkNot(se(t1)))))
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
  private def satisfiable[F](f: F)(implicit flix: Flix, alg: BoolFormula[F]): Boolean = {
    if (f == alg.mkTrue) {
      true
    } else if (f == alg.mkFalse) {
      false
    } else {
      val q = mkEq(f, alg.mkTrue)
      try {
        successiveVariableElimination(q, alg.freeVars(q).toList)
        true
      } catch {
        case BooleanUnificationException => false
      }
    }
  }


  /**
    * To unify two Boolean formulas p and q it suffices to unify t = (p ∧ ¬q) ∨ (¬p ∧ q) and check t = 0.
    */
  private def mkEq[F](p: F, q: F)(implicit alg: BoolFormula[F]): F = alg.mkOr(alg.mkAnd(p, alg.mkNot(q)), alg.mkAnd(alg.mkNot(p), q))
}
