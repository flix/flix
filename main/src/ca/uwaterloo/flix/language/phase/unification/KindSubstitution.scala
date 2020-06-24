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
import ca.uwaterloo.flix.language.ast.{Kind, Type}
import ca.uwaterloo.flix.util.InternalCompilerException

// MATT docs
case class KindSubstitution(m: Map[Kind.Var, Kind]) {

  // MATT docs
  // MATT empty check for efficiency
  def makeTypeSubst(t: Type)(implicit flix: Flix): Substitution = t match {
    case tvar@Type.Var(_, kvar: Kind.Var, _) if m contains kvar =>
      Substitution.singleton(tvar, Type.freshTypeVar(m(kvar))) // MATT maybe check for repeats
    case Type.Apply(tpe1, tpe2) => makeTypeSubst(tpe1) ++ makeTypeSubst(tpe2)
    case Type.Arrow(_, eff) => makeTypeSubst(eff)
    case Type.Cst(_) => Substitution.empty
    case Type.Lambda(_, _) => throw InternalCompilerException(s"Unexpected type: $t")
  }
}

object KindSubstitution {
  // MATT check for equality
  def singleton(kind1: Kind.Var, kind2: Kind): KindSubstitution = KindSubstitution(Map(kind1 -> kind2))

  // MATT change to singleton val
  def empty: KindSubstitution = KindSubstitution(Map.empty)
}
