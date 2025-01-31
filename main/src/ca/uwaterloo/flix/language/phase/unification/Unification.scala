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
import ca.uwaterloo.flix.language.ast.shared.{AssocTypeDef, Scope}
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint.Provenance
import ca.uwaterloo.flix.language.phase.typer.{ConstraintSolver2, Progress, TypeConstraint}
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.collection.ListMap

/**
  * A proxy for implementations of unification as we transition to the new solver.
  */
object Unification {

  /**
    * Fully unifies the given types, returning None if there are unresolvable constraints.
    */
  def fullyUnifyTypes(tpe1: Type, tpe2: Type, renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, AssocTypeDef])(implicit scope: Scope, flix: Flix): Option[Substitution] = {
    implicit val r: RigidityEnv = renv
    implicit val e: ListMap[Symbol.AssocTypeSym, AssocTypeDef] = eqEnv
    ConstraintSolver2.solveAllTypes(List(TypeConstraint.Equality(tpe1, tpe2, Provenance.Match(tpe1, tpe2, SourceLocation.Unknown)))) match {
      case (Nil, subst) => Some(subst)
      case (_ :: _, _) => None
    }
  }
}
