/*
 * Copyright 2024 Matthew Lutze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.util.{Result, Validation}
import ca.uwaterloo.flix.util.collection.ListMap

import scala.runtime.AbstractFunction3

/** A proxy for implementations of unification as we transition to the new solver. */
object Unification {

  /** Unifies the given variable `x` with the given non-variable type `tpe`. */
  def unifyVar(x: Type.Var, tpe: Type, renv: RigidityEnv)(implicit scope: Scope, flix: Flix): Result[(Substitution, List[Ast.BroadEqualityConstraint]), UnificationError] = {
    OldStarUnification.unifyVar(x, tpe, renv)
  }

  /** Unifies the two given types `tpe1` and `tpe2`. */
  def unifyTypes(tpe1: Type, tpe2: Type, renv: RigidityEnv)(implicit scope: Scope, flix: Flix): Result[(Substitution, List[Ast.BroadEqualityConstraint]), UnificationError] = {
    OldStarUnification.unifyTypes(tpe1, tpe2, renv)
  }

  /** Fully unifies the given types, returning None if there are unresolvable constraints. */
  def fullyUnifyTypes(tpe1: Type, tpe2: Type, renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit scope: Scope, flix: Flix): Option[Substitution] = {
    OldStarUnification.unifyTypes(tpe1, tpe2, renv) match {
      case Result.Ok((subst, constrs)) => EqualityEnvironment.entailAll(Nil, constrs, renv, eqEnv).toHardResult.toOption.map {
        case entailSubst => entailSubst @@ subst
      }
      case Result.Err(_) => None
    }
  }

  /** Unifies the given types, but ignores any unresolved constraints from associated types. */
  def unifyTypesIgnoreLeftoverAssocs(tpe1: Type, tpe2: Type, renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit scope: Scope, flix: Flix): Option[Substitution] = {
    OldStarUnification.unifyTypes(tpe1, tpe2, renv).toOption.map {
      case (subst, _) => subst
    }
  }

  /** Returns true iff `tpe1` unifies with `tpe2`, without introducing equality constraints. */
  def unifiesWith(tpe1: Type, tpe2: Type, renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit scope: Scope, flix: Flix): Boolean = {
    OldStarUnification.unifiesWith(tpe1, tpe2, renv, eqEnv)
  }

}
