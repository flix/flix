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
import ca.uwaterloo.flix.language.ast.{Ast, Symbol, Type}
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess}
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

import scala.annotation.tailrec

object ClassEnvironment {


  /**
    * Returns `true` iff type constraints `tconstrs0` entail type constraint `tconstr`, under class environment `instances`.
    * That is, `tconstr` is true if all of `tconstrs0` are true.
    */
  // MATT THIH says that toncstrs0 should always be in HNF so checking for byInst is a waste.
  def entail(tconstrs0: List[Ast.TypeConstraint], tconstr: Ast.TypeConstraint, instances: Map[Symbol.ClassSym, List[Ast.Instance]])(implicit flix: Flix): Validation[Unit, UnificationError] = {

    val superClasses = tconstrs0.flatMap(bySuper)

    // Case 1: tconstrs0 entail tconstr if tconstr is a superclass of any member or tconstrs0
    if (superClasses.contains(tconstr)) {
      ().toSuccess
    } else {
      // Case 2: there is an instance matching tconstr and all of the instance's constraints are entailed by tconstrs0
      for {
        tconstrs <- byInst(tconstr, instances)
        _ <- Validation.sequence(tconstrs.map(entail(tconstrs0, _, instances)))
      } yield ()
    }
  }

  /**
    * Removes the type constraints which are entailed by the others in the list.
    */
  private def simplify(tconstrs0: List[Ast.TypeConstraint], instances: Map[Symbol.ClassSym, List[Ast.Instance]])(implicit flix: Flix): List[Ast.TypeConstraint] = {

    @tailrec
    def loop(tconstrs0: List[Ast.TypeConstraint], acc: List[Ast.TypeConstraint]): List[Ast.TypeConstraint] = tconstrs0 match {
      case Nil => acc
      case head :: tail if entail(acc ++ tail, head, instances).isInstanceOf[Validation.Success[_, _]] => loop(tail, acc)
      case head :: tail => loop(tail, head :: acc)
    }

    loop(tconstrs0, Nil)
  }

  /**
    * Normalizes a list of type constraints, converting to head-normal form and removing semantic duplicates.
    */
  private def reduce(tconstrs0: List[Ast.TypeConstraint], instances: Map[Symbol.ClassSym, List[Ast.Instance]])(implicit flix: Flix): Validation[List[Ast.TypeConstraint], UnificationError] = {
    for {
      tconstrs <- Validation.sequence(tconstrs0.map(toHeadNormalForm(_, instances)))
    } yield simplify(tconstrs.flatten, instances)
  }

  /**
    * Converts the type constraint to head-normal form, i.e. `a[X1, Xn]`, where `a` is a variable and `n >= 0`.
    */
  private def toHeadNormalForm(tconstr: Ast.TypeConstraint, instances: Map[Symbol.ClassSym, List[Ast.Instance]])(implicit flix: Flix): Validation[List[Ast.TypeConstraint], UnificationError] = {
    if (isHeadNormalForm(tconstr.arg)) {
      List(tconstr).toSuccess
    } else {
      byInst(tconstr, instances)
    }
  }

  /**
    * Returns the list of constraints that hold if the given constraint `tconstr` holds, using the constraints on available instances.
    */
  private def byInst(tconstr: Ast.TypeConstraint, instances: Map[Symbol.ClassSym, List[Ast.Instance]])(implicit flix: Flix): Validation[List[Ast.TypeConstraint], UnificationError] = {
    val matchingInstances = instances.getOrElse(tconstr.sym, Nil)

    def tryInst(inst: Ast.Instance): Validation[List[Ast.TypeConstraint], UnificationError] = {
      for {
        subst <- Unification.unifyTypes(tconstr.arg, inst.tpe).toValidation
      } yield inst.tconstrs.map(subst(_))
    }

    val tconstrGroups = matchingInstances.map(tryInst).collect {
      case Validation.Success(tconstrs) => tconstrs
    }

    tconstrGroups match {
      case Nil => UnificationError.NoMatchingInstance(tconstr.sym, tconstr.arg).toFailure
      case tconstrs :: Nil => tconstrs.toSuccess
      case _ :: _ :: _ => throw InternalCompilerException("Multiple matching instances")
    }
  }

  /**
    * Returns the list of constraints that hold if the given constraint `tconstr` hold, using the superclasses of the constraint.
    * (Currently only returns the given constraint since superclasses are not yet implemented.)
    */
  private def bySuper(tconstr: Ast.TypeConstraint)(implicit flix: Flix): List[Ast.TypeConstraint] = {
    List(tconstr)
  }

  /**
    * Returns true iff this type is in head-normal form.
    */
  private def isHeadNormalForm(tpe: Type): Boolean = {
    tpe.typeConstructor.isEmpty
  }
}
