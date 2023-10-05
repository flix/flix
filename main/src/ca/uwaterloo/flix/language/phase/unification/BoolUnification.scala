/*
 *  Copyright 2023 Magnus Madsen
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

/**
 * An implementation of Boolean Unification is for the `Bool` kind.
 */
object BoolUnification {

  /**
   * Returns the most general unifier of the two given Boolean formulas `tpe1` and `tpe2`.
   */
  def unify(tpe1: Type, tpe2: Type, renv0: RigidityEnv)(implicit flix: Flix): Result[(Substitution, List[Ast.BroadEqualityConstraint]), UnificationError] = {

    // Set the variable levels to the minimum of all flexible variables involved.
    val tvars = tpe1.typeVars ++ tpe2.typeVars
    val levelOpt = tvars.filter(tv => renv0.isFlexible(tv.sym)).map(_.sym.level).minOption
    levelOpt match {
      case Some(level) => tvars.foreach(_.sym.level = level)
      case None => ()
    }

    // Give up early if either type contains an associated type.
    if (Type.hasAssocType(tpe1) || Type.hasAssocType(tpe2)) {
      return Ok((Substitution.empty, List(Ast.BroadEqualityConstraint(tpe1, tpe2))))
    }

    implicit val alg: BoolAlg[BoolFormula] = new SimpleBoolFormulaAlgClassic

    val result = lookupOrSolve(tpe1, tpe2, renv0)
    result.map(subst => (subst, Nil))
  }


  /**
   * Lookup the unifier of `tpe1` and `tpe2` or solve them.
   */
  private def lookupOrSolve[F](tpe1: Type, tpe2: Type, renv0: RigidityEnv)
                              (implicit flix: Flix, alg: BoolAlg[F]): Result[Substitution, UnificationError] = {
    //
    // Translate the types into formulas.
    //
    val env = alg.getEnv(List(tpe1, tpe2))
    val f1 = alg.fromType(tpe1, env)
    val f2 = alg.fromType(tpe2, env)

    val renv = alg.liftRigidityEnv(renv0, env)

    //
    // Run the expensive Boolean unification algorithm.
    //
    booleanUnification(f1, f2, renv) match {
      case None => UnificationError.MismatchedBools(tpe1, tpe2).toErr
      case Some(subst) => subst.toTypeSubstitution(env).toOk
    }
  }

  /**
   * Returns the most general unifier of the two given Boolean formulas `tpe1` and `tpe2`.
   */
  private def booleanUnification[F](tpe1: F, tpe2: F, renv: Set[Int])
                                   (implicit flix: Flix, alg: BoolAlg[F]): Option[BoolSubstitution[F]] = {
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
  private def successiveVariableElimination[F](f: F, flexvs: List[Int])(implicit flix: Flix, alg: BoolAlg[F]): BoolSubstitution[F] = flexvs match {
    case Nil =>
      // Determine if f is unsatisfiable when all (rigid) variables are made flexible.
      if (!alg.satisfiable(f))
        BoolSubstitution.empty
      else
        throw BoolUnificationException()

    case x :: xs =>
      val t0 = BoolSubstitution.singleton(x, alg.mkFalse)(f)
      val t1 = BoolSubstitution.singleton(x, alg.mkTrue)(f)
      val se = successiveVariableElimination(alg.mkAnd(t0, t1), xs)

      val f1 = alg.minimize(alg.mkOr(se(t0), alg.mkAnd(alg.mkVar(x), alg.mkNot(se(t1)))))
      val st = BoolSubstitution.singleton(x, f1)
      st ++ se
  }

}
