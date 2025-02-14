/*
 * Copyright 2020 Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.shared.SymUse.TraitSymUse
import ca.uwaterloo.flix.language.ast.shared.{AssocTypeDef, Instance, Scope, TraitConstraint}
import ca.uwaterloo.flix.language.ast.{RigidityEnv, Symbol, Type}
import ca.uwaterloo.flix.language.phase.typer.{ConstraintSolver2, ConstraintSolverInterface, SubstitutionTree, TypeConstraint}
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{Result, Validation}

import scala.annotation.tailrec

object TraitEnvironment {

  /**
    * Returns success iff type constraints `tconstrs0` entail type constraint `tconstr`, under trait environment `instances`.
    * That is, `tconstr` is true if all of `tconstrs0` are true.
    */
  def entail(tconstrs0: List[TraitConstraint], tconstr0: TraitConstraint, traitEnv: TraitEnv, eqEnv: EqualityEnv)(implicit scope: Scope, flix: Flix): Result[Unit, List[TypeConstraint]] = {
    val tconstr = tconstr0 match {
      case TraitConstraint(symUse, arg, loc) => TypeConstraint.Trait(symUse.sym, arg, loc)
    }

    val tenv = ConstraintSolverInterface.expandTraitEnv(traitEnv, tconstrs0)

    ConstraintSolver2.solveAll(List(tconstr), SubstitutionTree.empty)(scope, RigidityEnv.empty, tenv, eqEnv, flix) match {
      case (Nil, _) => Result.Ok(())
      case (errs@(_ :: _), _) => Result.Err(errs)
    }
  }

  /**
    * Returns true iff type constraint `tconstr1` entails tconstr2 under trait environment `traitEnv`.
    */
  def entails(tconstr1: TraitConstraint, tconstr2: TraitConstraint, traitEnv: TraitEnv): Boolean = {
    val superTraits = bySuper(tconstr1, traitEnv)
    superTraits.contains(tconstr2)
  }

  /**
    * Returns true iff the given type constraint holds under the given trait environment.
    */
  def holds(tconstr: TraitConstraint, traitEnv: TraitEnv, eqEnv: EqualityEnv)(implicit scope: Scope, flix: Flix): Boolean = {
    byInst(tconstr, traitEnv, eqEnv).toResult match {
      case Result.Ok(_) => true
      case Result.Err(_) => false
    }
  }

  /**
    * Returns the list of constraints that hold if the given constraint `tconstr` holds, using the constraints on available instances.
    */
  private def byInst(tconstr: TraitConstraint, traitEnv: TraitEnv, eqEnv: EqualityEnv)(implicit scope: Scope, flix: Flix): Validation[List[TraitConstraint], UnificationError] = tconstr match {
    case TraitConstraint(head, arg, _) =>

      val matchingInstanceOpt = traitEnv.getInstance(head.sym, arg)

      val renv = RigidityEnv.ofRigidVars(arg.typeVars.map(_.sym))

      def tryInst(inst: Instance): Validation[List[TraitConstraint], UnificationError] = {
        val substOpt = Unification.fullyUnifyTypes(inst.tpe, arg, renv, eqEnv)
        substOpt match {
          case Some(subst) => Validation.Success(inst.tconstrs.map(subst.apply))
          // if there are leftover constraints, then we can't be sure that this is the right instance
          case None => Validation.Failure(UnificationError.MismatchedTypes(inst.tpe, arg))
        }
      }

      val tconstrGroups = matchingInstanceOpt.map(tryInst).map(_.toResult).collect {
        case Result.Ok(tconstrs) => tconstrs
      }

      tconstrGroups match {
        case None => Validation.Failure(UnificationError.NoMatchingInstance(tconstr))
        case Some(tconstrs) =>
          // apply the base tconstr location to the new tconstrs
          Validation.Success(tconstrs.map(_.copy(loc = tconstr.loc)))
      }
  }

  /**
    * Returns the list of constraints that hold if the given constraint `tconstr` holds, using the super traits of the constraint.
    *
    * E.g. if we have 3 traits: `A`, `B`, `C` where
    *   - `A` extends `B`
    *   - `B` extends `C`
    *     Then for the constraint `t : A`, we return:
    *   - `t : A` (given)
    *   - `t : B` (because `B` is a super trait of `A`)
    *   - `t : C` (because `C` is a super trait of `B`, and transitively a super trait of `A`)
    *
    */
  private def bySuper(tconstr: TraitConstraint, traitEnv: TraitEnv): List[TraitConstraint] = {

    // Get the traits that are directly super traits of the trait in `tconstr`
    val directSupers = traitEnv.getSuperTraitsOpt(tconstr.symUse.sym).getOrElse(Nil)

    // Walk the super trait tree.
    // There may be duplicates, but this will terminate since super traits must be acyclic.
    tconstr :: directSupers.flatMap {
      // recurse on the super traits of each direct super trait
      superTrait => bySuper(TraitConstraint(TraitSymUse(superTrait, tconstr.loc), tconstr.arg, tconstr.loc), traitEnv)
    }
  }
}
