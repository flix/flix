/*
 *  Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.unification.shared

import scala.collection.immutable.SortedSet

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
    * Performs successive variable elimination on the given list of Boolean equations `l` at the
    * granularity given by `mode`:
    *
    *   - [[SveMode.PerEquation]]: solves the equations one at a time, applying the substitution
    *     of each equation to the remaining equations.
    *   - [[SveMode.PerComponent]]: solves each connected component (equations that share a
    *     flexible variable) as a single equation.
    *   - [[SveMode.PerSystem]]: solves the whole system as a single equation.
    *
    * The equations `f1 ~ g1, ..., fn ~ gn` of a group hold iff the single equation
    * `(f1 ⊕ g1) ∨ ... ∨ (fn ⊕ gn) = ⊥` holds, which is solved by one run of SVE.
    *
    * @throws `BoolUnificationException` if SVE fails.
    */
  def sveAll[F](l: List[(F, F)], mode: SveMode)(implicit alg: BoolAlg[F]): BoolSubstitution[F] = mode match {
    case SveMode.PerEquation => sveAllPerEquation(l)
    case SveMode.PerComponent => sveAllPerComponent(l)
    case SveMode.PerSystem => sveAllPerSystem(l)
  }

  /**
    * Solves the equations `l` one at a time, applying the substitution of each equation to the
    * remaining equations.
    *
    * @throws `BoolUnificationException` if SVE fails.
    */
  private def sveAllPerEquation[F](l: List[(F, F)])(implicit alg: BoolAlg[F]): BoolSubstitution[F] = {
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
    * Solves each connected component of the equations `l` as a single equation.
    *
    * @throws `BoolUnificationException` if SVE fails.
    */
  private def sveAllPerComponent[F](l: List[(F, F)])(implicit alg: BoolAlg[F]): BoolSubstitution[F] = {
    val queries = l.map { case (f1, f2) => alg.mkXor(f1, f2) }

    // The components have pairwise disjoint flexible variables, so their substitutions have
    // disjoint domains and ranges and can be merged without composition.
    connectedComponents(queries).foldLeft(BoolSubstitution.empty[F]) {
      case (acc, qs) => acc ++ sveGroup(qs)
    }
  }

  /**
    * Solves the whole system of equations `l` as a single equation.
    *
    * WARNING: This strategy does not terminate on every program. Building the single query
    * `q1 ∨ q2 ∨ ... ∨ qn` multiplies the number of terms of independent equations. The standard
    * library and the `Xperf` inputs compile, but on at least one program in the test suite the
    * construction of the query alone did not finish within 15 minutes and had to be killed.
    *
    * @throws `BoolUnificationException` if SVE fails.
    */
  private def sveAllPerSystem[F](l: List[(F, F)])(implicit alg: BoolAlg[F]): BoolSubstitution[F] = {
    if (l.isEmpty) {
      return BoolSubstitution.empty[F]
    }
    sveGroup(l.map { case (f1, f2) => alg.mkXor(f1, f2) })
  }

  /**
    * Solves the non-empty group of queries `qs` as the single query `q1 ∨ q2 ∨ ... ∨ qn`.
    *
    * @throws `BoolUnificationException` if SVE fails.
    */
  private def sveGroup[F](qs: List[F])(implicit alg: BoolAlg[F]): BoolSubstitution[F] = {
    sveOne(qs.reduce[F]((q1, q2) => alg.mkOr(q1, q2)))
  }

  /**
    * Partitions the given queries `qs` into connected components, where two queries are
    * connected if they share a flexible variable.
    *
    * Returns each component as its queries, in their original order. A query without flexible
    * variables forms a component of its own.
    */
  private def connectedComponents[F](qs: List[F])(implicit alg: BoolAlg[F]): List[List[F]] = {
    // The components found so far. Each query is paired with its index to restore the order.
    var components: List[(List[(F, Int)], SortedSet[Int])] = Nil
    for ((q, i) <- qs.zipWithIndex) {
      val vars = alg.freeVars(q)
      // Merge `q` with every component that shares a variable with it.
      val (connected, rest) = components.partition { case (_, vs) => vars.exists(vs.contains) }
      val merged = connected.foldLeft((List((q, i)), vars)) {
        case ((accQs, accVars), (cQs, cVars)) => (cQs ::: accQs, accVars ++ cVars)
      }
      components = merged :: rest
    }
    components.map { case (cQs, _) => cQs.sortBy(_._2).map(_._1) }
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
