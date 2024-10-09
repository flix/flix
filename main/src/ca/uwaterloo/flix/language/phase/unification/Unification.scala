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
import ca.uwaterloo.flix.language.phase.typer.ConstraintSolver2
import ca.uwaterloo.flix.language.phase.typer.ConstraintSolver2.TypeConstraint
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.collection.ListMap

/**
  * A proxy for implementations of unification as we transition to the new solver.
  */
object Unification {

  /**
    * Unifies the given variable `x` with the given non-variable type `tpe`.
    */
  def unifyVar(x: Type.Var, tpe: Type, renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit scope: Scope, flix: Flix): Result[(Substitution, List[Ast.BroadEqualityConstraint], Boolean), UnificationError] = {
    implicit val t: ConstraintSolver2.Progress = ConstraintSolver2.Progress()
    implicit val r: RigidityEnv = renv
    val (leftovers, subst) = ConstraintSolver2.makeSubstitution(ConstraintSolver2.TypeConstraint.Equality(x, tpe))
    Result.Ok((subst.root, leftovers.map(ConstraintSolver2.unsafeTypeConstraintToBroadEqualityConstraint), t.query()))
  }

  /**
    * Unifies the two given types `tpe1` and `tpe2`.
    */
  def unifyTypes(tpe1: Type, tpe2: Type, renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit scope: Scope, flix: Flix): Result[(Substitution, List[Ast.BroadEqualityConstraint], Boolean), UnificationError] = {
    implicit val r: RigidityEnv = renv
    implicit val e: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef] = eqEnv
    val (leftovers, subst) = ConstraintSolver2.goAllTypes(List(ConstraintSolver2.TypeConstraint.Equality(tpe1, tpe2)))
    Result.Ok((subst, leftovers.map(ConstraintSolver2.unsafeTypeConstraintToBroadEqualityConstraint), true)) // MATT hack: assuming progress
  }

  /**
    * Fully unifies the given types, returning None if there are unresolvable constraints.
    */
  def fullyUnifyTypes(tpe1: Type, tpe2: Type, renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit scope: Scope, flix: Flix): Option[Substitution] = {
    implicit val r: RigidityEnv = renv
    implicit val e: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef] = eqEnv
    ConstraintSolver2.goAllTypes(List(ConstraintSolver2.TypeConstraint.Equality(tpe1, tpe2))) match {
      case (Nil, subst) => Some(subst)
      case (_ :: _, _) => None
    }
  }

  /**
    * Unifies the given types, but ignores any unresolved constraints from associated types.
    */
  def unifyTypesIgnoreLeftoverAssocs(tpe1: Type, tpe2: Type, renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit scope: Scope, flix: Flix): Option[Substitution] = {
    implicit val r: RigidityEnv = renv
    implicit val e: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef] = eqEnv
    ConstraintSolver2.goAllTypes(List(ConstraintSolver2.TypeConstraint.Equality(tpe1, tpe2))) match {
      case (cs, subst) =>
        if (cs.forall(isAssocConstraint)) {
          Some(subst)
        } else {
          None
        }
    }
  }

  def isAssocConstraint(constr: ConstraintSolver2.TypeConstraint): Boolean = constr match {
    case TypeConstraint.Equality(_: Type.AssocType, _) => true
    case TypeConstraint.Equality(_, _: Type.AssocType) => true
    case _ => false
  }

  /**
    * Returns true iff `tpe1` unifies with `tpe2`, without introducing equality constraints.
    */
  def unifiesWith(tpe1: Type, tpe2: Type, renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit scope: Scope, flix: Flix): Boolean = {
    fullyUnifyTypes(tpe1, tpe2, renv, eqEnv).isDefined
  }

}
