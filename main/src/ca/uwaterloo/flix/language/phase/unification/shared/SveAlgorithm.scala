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

object SveAlgorithm {

  /**
    * Returns the most general unifier of the two given Boolean formulas `f1` and `f2`.
    */
  def unify[F](f1: F, f2: F, renv: Set[Int])(implicit alg: BoolAlg[F]): Option[BoolSubstitution[F]] = {
    // The boolean expression we want to show is 0.
    val query = alg.mkXor(f1, f2)

    // Compute the variables in the query.
    val typeVars = alg.freeVars(query).toList

    // Compute the flexible variables.
    val freeVars = typeVars.filterNot(renv.contains)

    // Eliminate all variables.
    try {
      Some(successiveVariableElimination(query, freeVars))
    } catch {
      case _: BoolUnificationException => None
    }
  }

  /**
    * Performs success variable elimination on the given list of Boolean equations `l`.
    *
    * @throws `BoolUnificationException` if SVE fails.
    */
  def sveAll[F](l: List[(F, F)])(implicit alg: BoolAlg[F]): BoolSubstitution[F] = {
    var subst = BoolSubstitution.empty[F]
    var rest = l
    while (rest.nonEmpty) {
      val (f1, f2) = rest.head
      val q = alg.mkXor(f1, f2)
      val s1 = sveOne(q)
      rest = rest.tail.map(p => (s1(p._1), s1(p._2)))
      subst = s1 @@ subst
    }
    subst
  }

  /**
    * Performs success variable elimination on the given Boolean expression `f`.
    *
    * Eliminates all free variables in `f`.
    *
    * Uses the SVE cache, if enabled.
    *
    * @throws `BoolUnificationException` if SVE fails.
    */
  private def sveOne[F](f: F)(implicit alg: BoolAlg[F]): BoolSubstitution[F] = {
    alg.lookupOrComputeSVE(f, _ => {
      val fvs = alg.freeVars(f).toList
      successiveVariableElimination(f, fvs)
    })
  }

  /**
    * Performs success variable elimination on the given Boolean expression `f`.
    *
    * `flexvs` is the list of remaining flexible variables in the expression.
    *
    * @throws `BoolUnificationException` if SVE fails.
    */
  private def successiveVariableElimination[F](f: F, flexvs: List[Int])(implicit alg: BoolAlg[F]): BoolSubstitution[F] = flexvs match {
    case Nil =>
      // Determine if f is unsatisfiable when all (rigid) variables are made flexible.
      if (alg.isEquivBot(f))
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
