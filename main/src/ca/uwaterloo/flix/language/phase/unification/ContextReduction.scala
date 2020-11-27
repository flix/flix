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
import ca.uwaterloo.flix.language.ast.{ResolvedAst, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}
import ca.uwaterloo.flix.util.Result.{ToErr, ToOk}
import ca.uwaterloo.flix.util.collection.MultiMap

import scala.annotation.tailrec

object ContextReduction {

  /**
    * Returns `true` iff type constraints `tconstrs0` entail type constraint `tconstr`, under class environment `instances`.
    * That is, `tconstr` is true if all of `tconstrs0` are true.
    */
  // MATT THIH says that toncstrs0 should always be in HNF so checking for byInst is a waste.
  def entail(tconstrs0: List[TypedAst.TypeConstraint], tconstr: TypedAst.TypeConstraint, instances: MultiMap[Symbol.ClassSym, ResolvedAst.Instance])(implicit flix: Flix): Boolean = {
    // tconstrs0 entail tconstr if tconstr is a superclass of any member or tconstrs0
    // or if there is an instance matching tconstr and all of the instance's constraints are entailed by tconstrs0
    tconstrs0.exists(bySuper(_, instances).contains(tconstr)) || {
      byInst(tconstr, instances) match {
        case Result.Ok(tconstrs) => tconstrs.forall(entail(tconstrs0, _, instances))
        case Result.Err(_) => false
      }
    }
  }

  /**
    * Removes the type constraints which are entailed by the others in the list.
    */
  private def simplify(tconstrs0: List[TypedAst.TypeConstraint], instances: MultiMap[Symbol.ClassSym, ResolvedAst.Instance])(implicit flix: Flix): List[TypedAst.TypeConstraint] = {

    @tailrec
    def loop(tconstrs0: List[TypedAst.TypeConstraint], acc: List[TypedAst.TypeConstraint]): List[TypedAst.TypeConstraint] = tconstrs0 match {
      case Nil => acc
      case head :: tail if entail(acc ++ tail, head, instances) => loop(tail, acc)
      case head :: tail => loop(tail, head :: acc)
    }

    loop(tconstrs0, Nil)
  }

  /**
    * Normalizes a list of type constraints, converting to head-normal form and removing semantic duplicates.
    */
  private def reduce(tconstrs0: List[TypedAst.TypeConstraint], instances: MultiMap[Symbol.ClassSym, ResolvedAst.Instance])(implicit flix: Flix): Result[List[TypedAst.TypeConstraint], UnificationError] = {
    for {
      tconstrs <- Result.sequence(tconstrs0.map(toHeadNormalForm(_, instances)))
    } yield simplify(tconstrs.flatten, instances)
  }

  /**
    * Converts the type constraint to head-normal form, i.e. `a[X1, Xn]`, where `a` is a variable and `n >= 0`.
    */
  private def toHeadNormalForm(tconstr: TypedAst.TypeConstraint, instances: MultiMap[Symbol.ClassSym, ResolvedAst.Instance])(implicit flix: Flix): Result[List[TypedAst.TypeConstraint], UnificationError] = {
    if (isHeadNormalForm(tconstr.arg)) {
      List(tconstr).toOk
    } else {
      byInst(tconstr, instances)
    }
  }

  /**
    * Returns the list of constraints that hold if the given constraint `tconstr` holds, using the constraints on available instances.
    */
  private def byInst(tconstr: TypedAst.TypeConstraint, instances0: MultiMap[Symbol.ClassSym, ResolvedAst.Instance])(implicit flix: Flix): Result[List[TypedAst.TypeConstraint], UnificationError] = {
    val matchingInstances = instances0(tconstr.sym).toList

    def tryInst(inst: ResolvedAst.Instance): Result[List[TypedAst.TypeConstraint], UnificationError] = {
      for {
        subst <- Unification.unifyTypes(tconstr.arg, inst.tpe)
      } yield inst.tconstrs.map(subst(_))
    }

    val tconstrGroups = matchingInstances.map(tryInst).collect {
      case Result.Ok(tconstrs) => tconstrs
    }

    tconstrGroups match {
      case Nil => UnificationError.NoMatchingInstance(tconstr.sym, tconstr.arg).toErr
      case tconstrs :: Nil => tconstrs.toOk
      case _ :: _ :: _ => throw InternalCompilerException("Multiple matching instances")
    }
  }

  /**
    * Returns the list of constraints that hold if the given constraint `tconstr` hold, using the superclasses of the constraint.
    * (Currently only returns the given constraint since superclasses are not yet implemented.)
    */
  private def bySuper(tconstr: TypedAst.TypeConstraint, _instances0: MultiMap[Symbol.ClassSym, ResolvedAst.Instance])(implicit flix: Flix): List[TypedAst.TypeConstraint] = {
    List(tconstr)
  }

  /**
    * Returns true iff this type is in head-normal form.
    */
  private def isHeadNormalForm(tpe: Type): Boolean = {
    tpe.typeConstructor.isEmpty
  }
}
