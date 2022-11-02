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
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Ok, ToErr, ToOk}

import scala.util.Try

object BoolUnification {

  /**
    * Returns the most general unifier of the two given Boolean formulas `tpe1` and `tpe2`.
    */
  def unify(tpe1: Type, tpe2: Type, renv0: RigidityEnv)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    ///
    /// Perform aggressive matching to optimize for common cases.
    ///
    if (tpe1 eq tpe2) {
      return Ok(Substitution.empty)
    }

    tpe1 match {
      case x: Type.Var if renv0.isFlexible(x.sym) =>
        if (tpe2 eq Type.True)
          return Ok(Substitution.singleton(x.sym, Type.True))
        if (tpe2 eq Type.False)
          return Ok(Substitution.singleton(x.sym, Type.False))

      case _ => // nop
    }

    tpe2 match {
      case y: Type.Var if renv0.isFlexible(y.sym) =>
        if (tpe1 eq Type.True)
          return Ok(Substitution.singleton(y.sym, Type.True))
        if (tpe1 eq Type.False)
          return Ok(Substitution.singleton(y.sym, Type.False))

      case _ => // nop
    }

    // translate the types into formulas
    implicit val alg: BoolAlg[BoolFormula] = BoolFormula.AsBoolAlgTrait

    val env = alg.getEnv(List(tpe1, tpe2))
    val f1 = alg.fromType(tpe1, env)
    val f2 = alg.fromType(tpe2, env)

    val renv = alg.liftRigidityEnv(renv0, env)

    booleanUnification(f1, f2, renv) match {
      case None => UnificationError.MismatchedBools(tpe1, tpe2).toErr
      case Some(subst) => subst.toTypeSubstitution(env).toOk
    }
  }

  /**
    * Returns the most general unifier of the two given Boolean formulas `tpe1` and `tpe2`.
    */
  private def booleanUnification[F](tpe1: F, tpe2: F, renv: Set[Int])(implicit flix: Flix, alg: BoolAlg[F]): Option[BoolSubstitution[F]] = {
    // The boolean expression we want to show is 0.
    val query = alg.mkXor(tpe1, tpe2)

    // Compute the variables in the query.
    val typeVars = alg.freeVars(query).toList

    // Compute the flexible variables.
    val flexibleTypeVars = typeVars.filterNot(renv.contains)

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

      Some(subst)
    } catch {
      case ex: BooleanUnificationException => None
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
    l.reverse // TODO have to reverse the order for regions to work
  }

  /**
    * Performs success variable elimination on the given boolean expression `f`.
    *
    * `flexvs` is the list of remaining flexible variables in the expression.
    */
  private def successiveVariableElimination[F](f: F, flexvs: List[Int])(implicit flix: Flix, alg: BoolAlg[F]): BoolSubstitution[F] = flexvs match {
    case Nil =>
      // Determine if f is unsatisfiable when all (rigid) variables are made flexible.
      if (!satisfiable(f))
        BoolSubstitution.empty
      else
        throw BooleanUnificationException()

    case x :: xs =>
      val t0 = BoolSubstitution.singleton(x, alg.mkFalse)(f)
      val t1 = BoolSubstitution.singleton(x, alg.mkTrue)(f)
      val se = successiveVariableElimination(alg.mkAnd(t0, t1), xs)

      val f1 = alg.minimize(alg.mkOr(se(t0), alg.mkAnd(alg.mkVar(x), alg.mkNot(se(t1)))))
      val st = BoolSubstitution.singleton(x, f1)
      st ++ se
  }

  /**
    * An exception thrown to indicate that boolean unification failed.
    */
  private case class BooleanUnificationException() extends RuntimeException

  /**
    * Returns `true` if the given boolean formula `f` is satisfiable
    * when ALL variables in the formula are flexible.
    */
  private def satisfiable[F](f: F)(implicit flix: Flix, alg: BoolAlg[F]): Boolean = {
    if (alg.isTrue(f)) {
      true
    } else if (alg.isFalse(f)) {
      false
    } else {
      alg.satisfiable(f) match {
        case None => naiveSatisfiable(f)
        case Some(sat) => sat
      }
    }
  }

  /**
    * Naively computes if `f` is satisfiable using the SVE algorithm.
    */
  private def naiveSatisfiable[F](f: F)(implicit flix: Flix, alg: BoolAlg[F]): Boolean = {
    val q = alg.mkXor(f, alg.mkTrue)
    try {
      successiveVariableElimination(q, alg.freeVars(q).toList)
      true
    } catch {
      case ex: BooleanUnificationException => false
    }
  }
}
