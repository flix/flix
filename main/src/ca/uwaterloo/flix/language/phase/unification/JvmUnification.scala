/*
 * Copyright 2024 Matthew Lutze
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

import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.ast.{RigidityEnv, Type}

object JvmUnification {

  /** Unifies the given JVM types. */
  def unify(tpe1: Type, tpe2: Type, renv: RigidityEnv)(implicit scope: Scope): Option[Substitution] = (tpe1, tpe2) match {
    case (Type.Var(sym, _), t2) if renv.isFlexible(sym) => Some(Substitution.singleton(sym, t2))
    case (t1, Type.Var(sym, _)) if renv.isFlexible(sym) => Some(Substitution.singleton(sym, t1))
    case _ => None
  }
}
