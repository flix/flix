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
import ca.uwaterloo.flix.util.Result.{ToErr, ToOk}
import ca.uwaterloo.flix.util.collection.MultiMap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

import scala.annotation.tailrec

// MATT maybe change name to ClassUnification or something like that
object ContextReduction {

  /**
    * Returns `true` iff type constraints `tconstrs0` entail type constraint `tconstr`, under class environment `instances`.
    * That is, `tconstr` is true if all of `tconstrs0` are true.
    */
  // MATT THIH says that toncstrs0 should always be in HNF so checking for byInst is a waste.
  def entail(instances: MultiMap[Symbol.ClassSym, ResolvedAst.Instance], tconstrs0: List[TypedAst.TypeConstraint], tconstr: TypedAst.TypeConstraint)(implicit flix: Flix): Boolean = {
    // tconstrs0 entail tconstr if tconstr is a superclass of any member or tconstrs0
    // or if there is an instance matching tconstr and all of the instance's constraints are entailed by tconstrs0
    tconstrs0.exists(bySuper(instances, _).contains(tconstr)) || {
      byInst(instances, tconstr) match {
        case Result.Ok(tconstrs) => tconstrs.forall(entail(instances, tconstrs0, _))
        case Result.Err(_) => false
      }
    }
  }

  /**
    * Removes the type constraints which are entailed by the others in the list.
    */
  private def simplify(instances: MultiMap[Symbol.ClassSym, ResolvedAst.Instance], tconstrs0: List[TypedAst.TypeConstraint])(implicit flix: Flix): List[TypedAst.TypeConstraint] = {

    @tailrec
    def loop(acc: List[TypedAst.TypeConstraint], tconstrs0: List[TypedAst.TypeConstraint]): List[TypedAst.TypeConstraint] = tconstrs0 match {
      case Nil => acc
      case head :: tail if entail(instances, acc ++ tail, head) => loop(acc, tail)
      case head :: tail => loop(head :: acc, tail)
    }

    loop(Nil, tconstrs0)
  }

  /**
    * Normalizes a list of type constraints, converting to head-normal form and removing semantic duplicates.
    */
  private def reduce(instances: MultiMap[Symbol.ClassSym, ResolvedAst.Instance], tconstrs0: List[TypedAst.TypeConstraint])(implicit flix: Flix): Result[List[TypedAst.TypeConstraint], UnificationError] = {
    for {
      tconstrs <- Result.sequence(tconstrs0.map(toHeadNormalForm(instances, _)))
    } yield simplify(instances, tconstrs.flatten)
  }

  /**
    * Converts the type constraint to head-normal form, i.e. `a[X1, Xn]`, where `a` is a variable and `n >= 0`.
    */
  private def toHeadNormalForm(instances: MultiMap[Symbol.ClassSym, ResolvedAst.Instance], tconstr: TypedAst.TypeConstraint)(implicit flix: Flix): Result[List[TypedAst.TypeConstraint], UnificationError] = {
    if (isHeadNormalForm(tconstr.arg)) {
      List(tconstr).toOk
    } else {
      byInst(instances, tconstr)
    }
  }

  /**
    * Returns the list of constraints that hold if the given constraint `tconstr` holds, using the constraints on available instances.
    */
  private def byInst(instances0: MultiMap[Symbol.ClassSym, ResolvedAst.Instance], tconstr: TypedAst.TypeConstraint)(implicit flix: Flix): Result[List[TypedAst.TypeConstraint], UnificationError] = {
    val matchingInstances = instances0(tconstr.sym).toList

    def tryInst(inst: ResolvedAst.Instance): Result[List[TypedAst.TypeConstraint], UnificationError] = {
      for {
        subst <- Unification.unifyTypes(tconstr.arg, inst.tpe)
      } yield inst.tconstrs.map(subst(_))
    }

    matchingInstances.map(tryInst).find(_.isOk) match {
      case Some(Result.Ok(tconstrs)) => tconstrs.toOk
      case None => UnificationError.NoMatchingInstance(tconstr.sym, tconstr.arg).toErr
    }
  }

  /**
    * Returns the list of constraints that hold if the given constraint `tconstr` hold, using the superclasses of the constraint.
    * (Currently only returns the given constraint since superclasses are not yet implemented.)
    */
  private def bySuper(_instances0: MultiMap[Symbol.ClassSym, ResolvedAst.Instance], tconstr: TypedAst.TypeConstraint)(implicit flix: Flix): List[TypedAst.TypeConstraint] = {
    List(tconstr)
  }

  /**
    * Returns true iff this type is in head-normal form.
    */
  private def isHeadNormalForm(tpe: Type): Boolean = {
    tpe.typeConstructor.isEmpty
  }

  /**
    * Splits a list of type constraints among those which must be dealt with under the scope of free variables (`retained`)
    * and those which are dealt with in the surrounding scope (`deferred`).
    */
  def split(instances: MultiMap[Symbol.ClassSym, ResolvedAst.Instance], fixedVars: List[Type.Var], quantifiedVars: List[Type.Var], tconstrs: List[TypedAst.TypeConstraint])(implicit flix: Flix): Result[(List[TypedAst.TypeConstraint], List[TypedAst.TypeConstraint]), UnificationError] = {
    for {
      tconstrs1 <- reduce(instances, tconstrs)
      (deferred, retained) = tconstrs1.partition(_.arg.typeVars.forall(fixedVars.contains))
      // MATT defaulted predicates here (?)
    } yield (deferred, retained)
  }
}
