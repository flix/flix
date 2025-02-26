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
    * This function will never crash and works for any type, no matter how ill-kinded.
    */
  def simplify(tpe: Type): Type = {
    try {
      simplifyInternal(tpe)
    } catch {
      case _: InternalCompilerException => tpe
    }
  }

  /** Simplifies types inside `tc` with [[simplify]]. */
  def simplifyConstraint(tc: TypeConstraint): TypeConstraint = tc match {
    case TypeConstraint.Equality(tpe1, tpe2, prov) =>
      TypeConstraint.Equality(simplify(tpe1), simplify(tpe2), prov)
    case TypeConstraint.Trait(sym, tpe, loc) =>
      TypeConstraint.Trait(sym, simplify(tpe), loc)
    case TypeConstraint.Purification(sym, tpe1, tpe2, prov, nested) =>
      TypeConstraint.Purification(sym, simplify(tpe1), simplify(tpe2), prov, nested.map(simplifyConstraint))
    case TypeConstraint.Conflicted(tpe1, tpe2, prov) =>
      TypeConstraint.Conflicted(simplify(tpe1), simplify(tpe2), prov)
  }

  /**
    * Simplifies all effects in `tpe0`.
    *
    * Might throw [[InternalCompilerException]] for non-well-kinded effects
    * because of [[EffUnification3.simplify]].
    */
  private def simplifyInternal(tpe0: Type): Type = tpe0 match {
    case t if t.kind == Kind.Eff => EffUnification3.simplify(t)
    case t: Type.Var => t
    case t: Type.Cst => t
    case t@Type.Apply(tpe1, tpe2, loc) =>
      val t1 = simplifyInternal(tpe1)
      val t2 = simplifyInternal(tpe2)
      t.renew(t1, t2, loc)
    case Type.Alias(symUse, args, tpe, loc) =>
      val as = args.map(simplifyInternal)
      val t = simplifyInternal(tpe)
      Type.Alias(symUse, as, t, loc)
    case Type.AssocType(symUse, arg, kind, loc) =>
      val a = simplifyInternal(arg)
      Type.AssocType(symUse, a, kind, loc)
    case Type.JvmToType(tpe, loc) =>
      val t = simplifyInternal(tpe)
      Type.JvmToType(t, loc)
    case Type.JvmToEff(tpe, loc) =>
      val t = simplifyInternal(tpe)
      Type.JvmToEff(t, loc)
    case Type.UnresolvedJvmType(member, loc) =>
      val m = member.map(simplifyInternal)
      Type.UnresolvedJvmType(m, loc)
  }

}
