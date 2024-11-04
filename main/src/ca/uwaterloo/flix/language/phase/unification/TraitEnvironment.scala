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
import ca.uwaterloo.flix.language.ast.shared.{AssocTypeDef, Instance, Scope, TraitConstraint}
import ca.uwaterloo.flix.language.ast.{RigidityEnv, Symbol, Type}
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{Result, Validation}

import scala.annotation.tailrec

object TraitEnvironment {

  /**
    * Returns success iff type constraints `tconstrs0` entail type constraint `tconstr`, under trait environment `instances`.
    * That is, `tconstr` is true if all of `tconstrs0` are true.
    */
  // MATT THIH says that toncstrs0 should always be in HNF so checking for byInst is a waste.
  def entail(tconstrs0: List[TraitConstraint], tconstr: TraitConstraint, traitEnv: TraitEnv, eqEnv: ListMap[Symbol.AssocTypeSym, AssocTypeDef])(implicit scope: Scope, flix: Flix): Validation[Unit, UnificationError] = {

    val superTraits = tconstrs0.flatMap(bySuper(_, traitEnv))

    // Case 1: tconstrs0 entail tconstr if tconstr is a super trait of any member of tconstrs0
    if (superTraits.contains(tconstr)) {
      Validation.success(())
    } else {
      // Case 2: there is an instance matching tconstr and all of the instance's constraints are entailed by tconstrs0
      Validation.flatMapN(byInst(tconstr, traitEnv, eqEnv)) {
        case tconstrs => Validation.sequenceX(tconstrs.map(entail(tconstrs0, _, traitEnv, eqEnv)))
      }
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
  def holds(tconstr: TraitConstraint, traitEnv: TraitEnv, eqEnv: ListMap[Symbol.AssocTypeSym, AssocTypeDef])(implicit scope: Scope, flix: Flix): Boolean = {
    byInst(tconstr, traitEnv, eqEnv).toHardResult match {
      case Result.Ok(_) => true
      case Result.Err(_) => false
    }
  }

  /**
    * Removes the type constraints which are entailed by the others in the list.
    */
  private def simplify(tconstrs0: List[TraitConstraint], traitEnv: TraitEnv, eqEnv: ListMap[Symbol.AssocTypeSym, AssocTypeDef])(implicit scope: Scope, flix: Flix): List[TraitConstraint] = {

    @tailrec
    def loop(tconstrs0: List[TraitConstraint], acc: List[TraitConstraint]): List[TraitConstraint] = tconstrs0 match {
      // Case 0: no tconstrs left to process, we're done
      case Nil => acc
      case head :: tail => entail(acc ++ tail, head, traitEnv, eqEnv).toHardResult match {
        // Case 1: `head` is entailed by the other type constraints, skip it
        case Result.Ok(_) => loop(tail, acc)
        // Case 2: `head` is not entailed, add it to the list
        case Result.Err(_) => loop(tail, head :: acc)
      }
    }

    loop(tconstrs0, Nil)
  }

  /**
    * Normalizes a list of type constraints, converting to head-normal form and removing semantic duplicates.
    */
  def reduce(tconstrs0: List[TraitConstraint], traitEnv: TraitEnv, eqEnv: ListMap[Symbol.AssocTypeSym, AssocTypeDef])(implicit scope: Scope, flix: Flix): Validation[List[TraitConstraint], UnificationError] = {
    val tconstrs1 = tconstrs0.map {
      case TraitConstraint(head, tpe, loc) => TraitConstraint(head, Type.eraseAliases(tpe), loc)
    }
    val normalization = Validation.sequence(tconstrs1.map(toHeadNormalForm(_, traitEnv, eqEnv)))
    Validation.mapN(normalization)(tconstrs => simplify(tconstrs.flatten, traitEnv, eqEnv))
  }

  /**
    * Converts the type constraint to head-normal form, i.e. `a[X1, Xn]`, where `a` is a variable and `n >= 0`.
    */
  private def toHeadNormalForm(tconstr: TraitConstraint, traitEnv: TraitEnv, eqEnv: ListMap[Symbol.AssocTypeSym, AssocTypeDef])(implicit scope: Scope, flix: Flix): Validation[List[TraitConstraint], UnificationError] = {
    if (isHeadNormalForm(tconstr.arg)) {
      Validation.success(List(tconstr))
    } else {
      byInst(tconstr, traitEnv, eqEnv)
    }
  }

  /**
    * Returns the list of constraints that hold if the given constraint `tconstr` holds, using the constraints on available instances.
    */
  private def byInst(tconstr: TraitConstraint, traitEnv: TraitEnv, eqEnv: ListMap[Symbol.AssocTypeSym, AssocTypeDef])(implicit scope: Scope, flix: Flix): Validation[List[TraitConstraint], UnificationError] = tconstr match {
    case TraitConstraint(head, arg, _) =>
      val matchingInstances = traitEnv.getInstancesOpt(head.sym).getOrElse(Nil)

      val renv = RigidityEnv.ofRigidVars(arg.typeVars.map(_.sym))

      def tryInst(inst: Instance): Validation[List[TraitConstraint], UnificationError] = {
        val substOpt = Unification.fullyUnifyTypes(inst.tpe, arg, renv, eqEnv)
        substOpt match {
          case Some(subst) => Validation.success(inst.tconstrs.map(subst.apply))
          // if there are leftover constraints, then we can't be sure that this is the right instance
          case None => Validation.toHardFailure(UnificationError.MismatchedTypes(inst.tpe, arg))
        }
      }

      val tconstrGroups = matchingInstances.map(tryInst).map(_.toHardResult).collect {
        case Result.Ok(tconstrs) => tconstrs
      }

      tconstrGroups match {
        case Nil => Validation.toHardFailure(UnificationError.NoMatchingInstance(tconstr))
        case tconstrs :: Nil =>
          // apply the base tconstr location to the new tconstrs
          Validation.success(tconstrs.map(_.copy(loc = tconstr.loc)))
        case _ :: _ :: _ =>
          // Multiple matching instances: This will be caught in the Instances phase.
          // We return Nil here because there is no canonical set of constraints,
          // so we stop adding constraints and let the later phase take care of it.
          Validation.success(Nil)
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
    val directSupers = traitEnv.getSuperTraitsOpt(tconstr.head.sym).getOrElse(Nil)

    // Walk the super trait tree.
    // There may be duplicates, but this will terminate since super traits must be acyclic.
    tconstr :: directSupers.flatMap {
      // recurse on the super traits of each direct super trait
      superTrait => bySuper(TraitConstraint(TraitConstraint.Head(superTrait, tconstr.loc), tconstr.arg, tconstr.loc), traitEnv)
    }
  }

  /**
    * Returns true iff this type is in head-normal form.
    */
  private def isHeadNormalForm(tpe: Type): Boolean = {
    tpe.typeConstructor.isEmpty
  }
}
