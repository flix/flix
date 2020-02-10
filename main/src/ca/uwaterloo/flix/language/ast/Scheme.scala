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
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.tc.Show.ShowableSyntax

object Scheme {

  /**
    * Instantiates the given type scheme `sc` by replacing all quantified variables with fresh type variables.
    */
  def instantiate(sc: Scheme)(implicit flix: Flix): Type = refreshTypeVars(sc.quantifiers, sc.base)

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
  override def toString: String = s"∀(${quantifiers.mkString(", ")}). ${base.show}"

}
