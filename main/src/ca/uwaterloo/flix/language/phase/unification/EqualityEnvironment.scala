/*
 *  Copyright 2025 Ry Wiese
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
import ca.uwaterloo.flix.language.ast.{RigidityEnv, Type}
import ca.uwaterloo.flix.language.ast.shared.{EqualityConstraint, Scope, SymOrNot}
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint.Provenance
import ca.uwaterloo.flix.language.phase.typer.{ConstraintSolver2, ConstraintSolverInterface, SubstitutionTree, TypeConstraint}
import ca.uwaterloo.flix.util.Result

object EqualityEnvironment {

  /**
    * Returns `Ok` iff equality constraints `econstrs0` entail type constraint `econstr0`.
    * That is, `econstr0` is true if all of `tconstrs0` are true.
    *
    * If `econstr0` is not entailed, returns `Err(errs)` where `errs` are the unsolved constraints.
    */
  def entail(econstrs0: List[EqualityConstraint], econstr0: EqualityConstraint, traitEnv: TraitEnv, eqEnv: EqualityEnv)(implicit scope: Scope, flix: Flix): Result[Unit, List[TypeConstraint]] = {
    val econstr = econstr0 match {
      case EqualityConstraint(symOrNot, tpe1, tpe2, loc) =>
        symOrNot match {
          case SymOrNot.Found(symUse) =>
            val assoc = Type.AssocType(symUse, tpe1, tpe2.kind, loc)
            TypeConstraint.Equality(assoc, tpe2, Provenance.Match(assoc, tpe2, loc))
          case SymOrNot.NotFound =>
            return Result.Ok(())
        }
    }

    val eenv = ConstraintSolverInterface.expandEqualityEnv(eqEnv, econstrs0)

    ConstraintSolver2.solveAll(List(econstr), SubstitutionTree.empty)(scope, RigidityEnv.empty, traitEnv, eenv, flix) match {
      case (Nil, _) => Result.Ok(())
      case (errs@(_ :: _), _) => Result.Err(errs)
    }
  }

}
