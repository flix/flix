/*
 * Copyright 2023 Matthew Lutze
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

import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.{SourceLocation, Type}
import ca.uwaterloo.flix.util.InternalCompilerException

object EqualityEnvironment {
  /**
    * Converts the given EqualityConstraint into a BroadEqualityConstraint.
    */
  def narrow(econstr: BroadEqualityConstraint): EqualityConstraint = econstr match {
    case BroadEqualityConstraint(Type.AssocType(cst, tpe1, _, _), tpe2) =>
      EqualityConstraint(cst, tpe1, tpe2, SourceLocation.Unknown)
    case _ => throw InternalCompilerException("unexpected broad equality constraint", SourceLocation.Unknown)
  }

  /**
    * Converts the given Equality
    */
  def broaden(econstr: EqualityConstraint): BroadEqualityConstraint = econstr match {
    case EqualityConstraint(cst, tpe1, tpe2, loc) =>
      BroadEqualityConstraint(Type.AssocType(cst, tpe1, tpe2.kind, loc), tpe2)
  }
}
