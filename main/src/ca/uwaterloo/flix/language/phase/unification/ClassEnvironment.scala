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
import ca.uwaterloo.flix.language.ast.Ast.ClassContext
import ca.uwaterloo.flix.language.ast.{Ast, RigidityEnv, Scheme, Symbol, Type}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation.{ToSuccess}
import ca.uwaterloo.flix.util.collection.ListMap

import scala.annotation.tailrec

object ClassEnvironment {


  /**
    * Returns success iff type constraints `tconstrs0` entail type constraint `tconstr`, under class environment `instances`.
    * That is, `tconstr` is true if all of `tconstrs0` are true.
    */
  // MATT THIH says that toncstrs0 should always be in HNF so checking for byInst is a waste.
  def entail(tconstrs0: List[Ast.TypeConstraint], tconstr: Ast.TypeConstraint, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[Unit, UnificationError] = {

    val superClasses = tconstrs0.flatMap(bySuper(_, classEnv))

    // Case 1: tconstrs0 entail tconstr if tconstr is a super class of any member of tconstrs0
    if (superClasses.contains(tconstr)) {
      ().toSuccess
    } else {
      // Case 2: there is an instance matching tconstr and all of the instance's constraints are entailed by tconstrs0
      Validation.flatMapN(byInst(tconstr, classEnv)) {
        case tconstrs => Validation.sequenceX(tconstrs.map(entail(tconstrs0, _, classEnv)))
      }
    }
  }

  /**
    * Returns true iff type constraint `tconstr1` entails tconstr2 under class environment `classEnv`.
    */
  def entails(tconstr1: Ast.TypeConstraint, tconstr2: Ast.TypeConstraint, classEnv: Map[Symbol.ClassSym, Ast.ClassContext]): Boolean = {
    val superClasses = bySuper(tconstr1, classEnv)
    superClasses.contains(tconstr2)
  }

  /**
    * Returns true iff the given type constraint holds under the given class environment.
    */
  def holds(tconstr: Ast.TypeConstraint, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Boolean = {
    byInst(tconstr, classEnv) match {
      case Validation.Success(_) => true
      case _failure => false
    }
  }

  /**
    * Removes the type constraints which are entailed by the others in the list.
    */
  private def simplify(tconstrs0: List[Ast.TypeConstraint], classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): List[Ast.TypeConstraint] = {

    @tailrec
    def loop(tconstrs0: List[Ast.TypeConstraint], acc: List[Ast.TypeConstraint]): List[Ast.TypeConstraint] = tconstrs0 match {
      // Case 0: no tconstrs left to process, we're done
      case Nil => acc
      case head :: tail => entail(acc ++ tail, head, classEnv) match {
        // Case 1: `head` is entailed by the other type constraints, skip it
        case Validation.Success(_) => loop(tail, acc)
        // Case 2: `head` is not entailed, add it to the list
        case _failure => loop(tail, head :: acc)
      }
    }

    loop(tconstrs0, Nil)
  }

  /**
    * Normalizes a list of type constraints, converting to head-normal form and removing semantic duplicates.
    */
  def reduce(tconstrs0: List[Ast.TypeConstraint], classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[List[Ast.TypeConstraint], UnificationError] = {
    val tconstrs1 = tconstrs0.map {
      case Ast.TypeConstraint(head, tpe, loc) => Ast.TypeConstraint(head, Type.eraseAliases(tpe), loc)
    }
    for {
      tconstrs <- Validation.sequence(tconstrs1.map(toHeadNormalForm(_, classEnv)))
    } yield simplify(tconstrs.flatten, classEnv)
  }

  /**
    * Converts the type constraint to head-normal form, i.e. `a[X1, Xn]`, where `a` is a variable and `n >= 0`.
    */
  private def toHeadNormalForm(tconstr: Ast.TypeConstraint, classEnv: Map[Symbol.ClassSym, ClassContext])(implicit flix: Flix): Validation[List[Ast.TypeConstraint], UnificationError] = {
    if (isHeadNormalForm(tconstr.arg)) {
      List(tconstr).toSuccess
    } else {
      byInst(tconstr, classEnv)
    }
  }

  /**
    * Returns the list of constraints that hold if the given constraint `tconstr` holds, using the constraints on available instances.
    */
  private def byInst(tconstr: Ast.TypeConstraint, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[List[Ast.TypeConstraint], UnificationError] = {
    val matchingInstances = classEnv.get(tconstr.head.sym).map(_.instances).getOrElse(Nil)
    val tconstrSc = Scheme.generalize(Nil, Nil, tconstr.arg, RigidityEnv.empty) // TODO ASSOC-TYPES Nil right?

    def tryInst(inst: Ast.Instance): Validation[List[Ast.TypeConstraint], UnificationError] = {
      val instSc = Scheme.generalize(Nil, Nil, inst.tpe, RigidityEnv.empty) // TODO ASSOC-TYPES Nil right?

      // NB: This is different from the THIH implementation.
      // We also check `leq` instead of just `unifies` in order to support complex types in instances.
      for {
        subst <- Scheme.checkLessThanEqual(instSc, tconstrSc, Map.empty, ListMap.empty) // TODO ASSOC-TYPES ListMap.empty right?
      } yield inst.tconstrs.map(subst(_))
    }

    val tconstrGroups = matchingInstances.map(tryInst).collect {
      case Validation.Success(tconstrs) => tconstrs
    }

    tconstrGroups match {
      case Nil => Validation.toHardFailure(UnificationError.NoMatchingInstance(tconstr))
      case tconstrs :: Nil =>
        // apply the base tconstr location to the new tconstrs
        tconstrs.map(_.copy(loc = tconstr.loc)).toSuccess
      case _ :: _ :: _ =>
        // Multiple matching instances: This will be caught in the Instances phase.
        // We return Nil here because there is no canonical set of constraints,
        // so we stop adding constraints and let the later phase take care of it.
        Nil.toSuccess
    }
  }

  /**
    * Returns the list of constraints that hold if the given constraint `tconstr` holds, using the super classes of the constraint.
    *
    * E.g. if we have 3 classes: `A`, `B`, `C` where
    * - `A` extends `B`
    * - `B` extends `C`
    * Then for the constraint `t : A`, we return:
    * - `t : A` (given)
    * - `t : B` (because `B` is a super class of `A`)
    * - `t : C` (because `C` is a super class of `B`, and transitively a super class of `A`)
    *
    */
  private def bySuper(tconstr: Ast.TypeConstraint, classEnv: Map[Symbol.ClassSym, Ast.ClassContext]): List[Ast.TypeConstraint] = {

    // Get the classes that are directly superclasses of the class in `tconstr`
    val directSupers = classEnv.get(tconstr.head.sym).map(_.superClasses).getOrElse(Nil)

    // Walk the super class tree.
    // There may be duplicates, but this will terminate since super classes must be acyclic.
    tconstr :: directSupers.flatMap {
      // recurse on the superclasses of each direct superclass
      superClass => bySuper(Ast.TypeConstraint(Ast.TypeConstraint.Head(superClass, tconstr.loc), tconstr.arg, tconstr.loc), classEnv)
    }
  }

  /**
    * Returns true iff this type is in head-normal form.
    */
  private def isHeadNormalForm(tpe: Type): Boolean = {
    tpe.typeConstructor.isEmpty
  }
}
