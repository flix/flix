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
package ca.uwaterloo.flix.language.phase.unification.shared

import ca.uwaterloo.flix.api.Flix

object SveAlgorithm {

  /**
   * Returns the most general unifier of the two given Boolean formulas `tpe1` and `tpe2`.
   */
  def unify[F](tpe1: F, tpe2: F, renv: Set[Int])(implicit flix: Flix, alg: BoolAlg[F]): Option[BoolSubstitution[F]] = {
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
      Some(successiveVariableElimination(query, freeVars))
    } catch {
      case _: BoolUnificationException => None
    }
  }

  /**
   * Determine the variable order.
   */
  private def computeVariableOrder(l: List[Int]): List[Int] = l

  /**
   * Performs success variable elimination on the given boolean expression `f`.
   *
   * `flexvs` is the list of remaining flexible variables in the expression.
   */
  private def successiveVariableElimination[F](f: F, flexvs: List[Int])(implicit alg: BoolAlg[F], flix: Flix): BoolSubstitution[F] = flexvs match {
    case Nil =>
      // Determine if f is unsatisfiable when all (rigid) variables are made flexible.
      if (!alg.isSatisfiable(f))
        BoolSubstitution.empty
      else
        throw BoolUnificationException()

    case x :: xs =>
      val t0 = BoolSubstitution.singleton(x, alg.mkBot)(alg)(f)
      val t1 = BoolSubstitution.singleton(x, alg.mkTop)(alg)(f)
      val se = successiveVariableElimination(alg.mkAnd(t0, t1), xs)

      val f1 = alg.mkOr(se(t0), alg.mkAnd(alg.mkVar(x), alg.mkNot(se(t1))))
      val st = BoolSubstitution.singleton(x, f1)
      st ++ se
  }

}
