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
import ca.uwaterloo.flix.language.phase.Unification
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}
import ca.uwaterloo.flix.util.tc.Show.ShowableSyntax

object Scheme {

  /**
    * Instantiates the given type scheme `sc` by replacing all quantified variables with fresh type variables.
    */
  def instantiate(sc: Scheme)(implicit flix: Flix): Type = refreshTypeVars(sc.quantifiers, sc.base)

  /**
    * Generalizes the given type `tpe0` w.r.t. the given type environment `subst0`.
    */
  def generalize(tpe0: Type, subst0: Unification.Substitution): Scheme = {
    // Compute all the free type variables in `tpe0`.
    val freeVars = tpe0.typeVars

    // Compute all the bound type variables in the type environment `subst0`.
    val boundVars = subst0.m

    // Compute the variables that may be quantified.
    val quantifiers = freeVars -- boundVars.keySet // TODO: Is this correct?

    Scheme(quantifiers.toList, tpe0)
  }

  /**
    * Returns `true` if the given scheme `sc1` is smaller or equal to the given scheme `sc2`.
    */
  def lessThanEqual(sc1: Scheme, sc2: Scheme)(implicit flix: Flix): Boolean = {
    if (sc1.quantifiers.isEmpty && sc2.quantifiers.isEmpty) {
      sc1.base == sc2.base
    } else {
      // TODO: Is this correct?
      val tpe1 = instantiate(sc1)
      val tpe2 = instantiate(sc2)
      Unification.unifyTypes(tpe2, tpe1, matchRight = false) match {
        case Result.Ok(subst) =>
         true
        case Result.Err(_) =>
          false
      }
    }
  }

  /**
    * Replaces every free occurrence of a type variable in `typeVars`
    * with a fresh type variable in the given type `tpe`.
    */
  private def refreshTypeVars(typeVars: List[Type.Var], tpe: Type)(implicit flix: Flix): Type = {
    val freshVars = typeVars.foldLeft(Map.empty[Int, Type.Var]) {
      case (macc, tvar) => macc + (tvar.id -> Type.freshTypeVar(tvar.kind))
    }

    /**
      * Replaces every variable occurrence in the given type using the map `freeVars`.
      */
    def visitType(t0: Type): Type = t0 match {
      case Type.Var(x, k) => freshVars.getOrElse(x, t0)
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

    visitType(tpe)
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
