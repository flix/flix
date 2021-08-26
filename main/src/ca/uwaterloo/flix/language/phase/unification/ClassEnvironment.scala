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
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess}
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

import scala.annotation.tailrec

object ClassEnvironment {


  /**
    * Returns success iff type constraints `tconstrs0` entail type constraint `tconstr`, under class environment `instances`.
    * That is, `tconstr` is true if all of `tconstrs0` are true.
    */
  // MATT THIH says that toncstrs0 should always be in HNF so checking for byInst is a waste.
  def entail(tconstrs0: List[Ast.TypeConstraint], tconstr: Ast.TypeConstraint, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[Unit, UnificationError] = {

    val superClasses = tconstrs0.flatMap(bySuper(_, classEnv))

    // Case 1: tconstrs0 entail tconstr if tconstr is a super class of any member or tconstrs0
    if (superClasses.contains(tconstr)) {
      ().toSuccess
    } else {
      // Case 2: there is an instance matching tconstr and all of the instance's constraints are entailed by tconstrs0
      for {
        tconstrs <- byInst(tconstr, classEnv)
        _ <- Validation.sequence(tconstrs.map(entail(tconstrs0, _, classEnv)))
      } yield ()
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
        case Validation.Failure(_) => loop(tail, head :: acc)
      }
    }

    loop(tconstrs0, Nil)
  }

  /**
    * Normalizes a list of type constraints, converting to head-normal form and removing semantic duplicates.
    */
  def reduce(tconstrs0: List[Ast.TypeConstraint], classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[List[Ast.TypeConstraint], UnificationError] = {
    for {
      tconstrs <- Validation.sequence(tconstrs0.map(toHeadNormalForm(_, classEnv)))
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
    val matchingInstances = classEnv.get(tconstr.sym).map(_.instances).getOrElse(Nil)

    def tryInst(inst: Ast.Instance): Validation[List[Ast.TypeConstraint], UnificationError] = {
      for {
        subst <- Unification.unifyTypes(tconstr.arg, inst.tpe).toValidation
      } yield inst.tconstrs.map(subst(_))
    }

    val tconstrGroups = matchingInstances.map(tryInst).collect {
      case Validation.Success(tconstrs) => tconstrs
    }

    tconstrGroups match {
      case Nil => UnificationError.NoMatchingInstance(tconstr).toFailure
      case tconstrs :: Nil =>
        // apply the base tconstr location to the new tconstrs
        tconstrs.map(_.copy(loc = tconstr.loc)).toSuccess
      case _ :: _ :: _ =>
        // Multiple matching instances: this will be caught in a later phase but for now we return Nil
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
    val directSupers = classEnv.get(tconstr.sym).map(_.superClasses).getOrElse(Nil)

    // Walk the super class tree.
    // There may be duplicates, but this will terminate since super classes must be acyclic.
    tconstr :: directSupers.flatMap {
      // recurse on the superclasses of each direct superclass
      superClass => bySuper(Ast.TypeConstraint(superClass, tconstr.arg, tconstr.loc), classEnv)
    }
  }

  /**
    * Returns true iff this type is in head-normal form.
    */
  private def isHeadNormalForm(tpe: Type): Boolean = {
    tpe.typeConstructor.isEmpty
  }
}
