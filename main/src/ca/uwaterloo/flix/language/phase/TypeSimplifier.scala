/*
 * Copyright 2025 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{Kind, Type}
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint
import ca.uwaterloo.flix.language.phase.unification.EffUnification3
import ca.uwaterloo.flix.util.InternalCompilerException

/** This type simplification is purely focused on user readability. */
object TypeSimplifier {

  /**
    * Simplifies types and effects - intended for user readability.
    *
    * Returns a type that is equivalent to `tpe` but is easier to read.
    *
    * This function should never crash and works for any type, no matter how ill-kinded.
    */
  def simplify(tpe: Type): Type =
    visitType(tpe)

  /** Simplifies all effects in `tpe0`. */
  private def visitType(tpe0: Type): Type = tpe0 match {
    case t if t.kind == Kind.Eff =>
      // This might throw [[InternalCompilerException]] since it reuses unification infrastructure
      // but since the error is so imprecise we cannot handle it here.
      EffUnification3.simplify(t)
    case t: Type.Var => t
    case t: Type.Cst => t
    case t@Type.Apply(tpe1, tpe2, loc) =>
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      t.renew(t1, t2, loc)
    case Type.Alias(symUse, args, tpe, loc) =>
      val as = args.map(visitType)
      val t = visitType(tpe)
      Type.Alias(symUse, as, t, loc)
    case Type.AssocType(symUse, arg, kind, loc) =>
      val a = visitType(arg)
      Type.AssocType(symUse, a, kind, loc)
    case Type.JvmToType(tpe, loc) =>
      val t = visitType(tpe)
      Type.JvmToType(t, loc)
    case Type.JvmToEff(tpe, loc) =>
      val t = visitType(tpe)
      Type.JvmToEff(t, loc)
    case Type.UnresolvedJvmType(member, loc) =>
      val m = member.map(visitType)
      Type.UnresolvedJvmType(m, loc)
  }

}
