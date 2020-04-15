/*
 * Copyright 2016 Magnus Madsen
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

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Type.VarMode
import ca.uwaterloo.flix.language.phase.Unification
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}
import ca.uwaterloo.flix.util.tc.Show.ShowableSyntax

object Scheme {

  /**
    * A common super-type to control instantiation.
    */
  sealed trait InstantiateMode

  object InstantiateMode {

    // TODO: DOC

    /**
      * Instantiated variables are flexible. Free variables are unchanged.
      */
    case object Flexible extends InstantiateMode

    /**
      * Instantiated variables are rigid. Free variables are made rigid (regardless of their prior mode).
      */
    case object Rigid extends InstantiateMode

    /**
      * Instantiated variables are flexible. Free variables are made rigid (regardless of their prior mode).
      */
    case object Mixed extends InstantiateMode

  }

  /**
    * Instantiates the given type scheme `sc` by replacing all quantified variables with fresh type variables.
    */
  def instantiate(sc: Scheme, mode: InstantiateMode)(implicit flix: Flix): Type = {
    // Compute the base type.
    val baseType = sc.base

    //
    // Compute the fresh variables taking the instantiation mode into account.
    //
    val freshVars = baseType.typeVars.foldLeft(Map.empty[Int, Type.Var]) {
      case (macc, tvar) =>
        // Determine the rigidity of the fresh type variable.
        val m = mode match {
          case InstantiateMode.Flexible => VarMode.Flexible
          case InstantiateMode.Rigid => VarMode.Rigid
          case InstantiateMode.Mixed => VarMode.Flexible
        }
        macc + (tvar.id -> Type.freshTypeVar(tvar.kind, m))
    }

    /**
      * Replaces every variable occurrence in the given type using the map `freeVars`.
      */
    def visitType(t0: Type): Type = t0 match {
      case Type.Var(x, k, m) => freshVars.get(x) match {
        case None =>
          // Determine the rigidity of the free type variable.
          val m1 = mode match {
            case InstantiateMode.Flexible => m
            case InstantiateMode.Rigid => VarMode.Rigid
            case InstantiateMode.Mixed => VarMode.Rigid
          }
          Type.Var(x, k, m1)
        case Some(tvar) => tvar
      }
      case Type.Cst(tc) => Type.Cst(tc)
      case Type.Arrow(l, eff) => Type.Arrow(l, visitType(eff))
      case Type.RecordEmpty => Type.RecordEmpty
      case Type.RecordExtend(label, value, rest) => Type.RecordExtend(label, visitType(value), visitType(rest))
      case Type.SchemaEmpty => Type.SchemaEmpty
      case Type.SchemaExtend(sym, t, rest) => Type.SchemaExtend(sym, visitType(t), visitType(rest))
      case Type.Zero => Type.Zero
      case Type.Succ(n, t) => Type.Succ(n, t)
      case Type.Apply(tpe1, tpe2) => Type.Apply(visitType(tpe1), visitType(tpe2))
      case Type.Lambda(tvar, tpe) => throw InternalCompilerException(s"Unexpected type: '$t0'.")
    }

    visitType(baseType)
  }

  /**
    * Generalizes the given type `tpe0` w.r.t. the given type environment `subst0`.
    */
  def generalize(tpe0: Type, subst0: Unification.Substitution): Scheme = {
    // Compute all the free type variables in `tpe0`.
    val freeVars = tpe0.typeVars

    // Compute all the bound type variables in the type environment `subst0`.
    val boundVars = subst0.m // TODO: compute the free variables in the context.

    // Compute the variables that may be quantified.
    val quantifiers = freeVars -- boundVars.keySet

    Scheme(quantifiers.toList, tpe0)
  }

  /**
    * Returns `true` if the given scheme `sc1` is smaller or equal to the given scheme `sc2`.
    */
  def lessThanEqual(sc1: Scheme, sc2: Scheme)(implicit flix: Flix): Boolean = {
    // TODO: Instantiated variables are flexible, already present variables should become rigid.
    val tpe1 = instantiate(sc1, InstantiateMode.Mixed) // TODO: Want: fresh ones are flexible, old ones are rigid.
    val tpe2 = instantiate(sc2, InstantiateMode.Rigid) // TODO: Everything is rigid (both fresh and old).
    Unification.unifyTypes(tpe1, tpe2) match {
      case Result.Ok(_) => true
      case Result.Err(_) =>
        println(s"failed $tpe1 : $tpe2")

        true // TODO
    }
  }

}

/**
  * Representation of polytypes.
  */
case class Scheme(quantifiers: List[Type.Var], base: Type) {

  /**
    * Returns a human readable representation of the polytype.
    */
  override def toString: String = {
    if (quantifiers.isEmpty)
      base.show
    else
      s"∀(${quantifiers.map(tvar => tvar.getText.getOrElse(tvar.id)).mkString(", ")}). ${base.show}"
  }

}
