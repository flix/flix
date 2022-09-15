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
    implicit val alg: BoolAlgTrait[ExplicitFormula] = ExplicitFormula.AsBoolAlgTrait

    val env = alg.getEnv(List(tpe1, tpe2))
    val f1 = alg.fromType(tpe1, env)
    val f2 = alg.fromType(tpe2, env)

    val renv = alg.liftRigidityEnv(renv0, env)

    BoolUnification2.unify(f1, f2, renv) match {
      case Some(subst) =>
        subst.toTypeSubstitution(env).toOk
      case None => UnificationError.MismatchedBools(tpe1, tpe2).toErr
    }
  }
}
