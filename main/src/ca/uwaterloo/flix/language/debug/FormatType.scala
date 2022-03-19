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


package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.{Kind, Rigidity, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException

object FormatType {

  /**
    * Formats the given type.
    * The type is assumed to be well-kinded, though not necessarily a proper type (e.g. it may be partially applied).
    */
  def formatWellKindedType(tpe: Type)(implicit audience: Audience): String = {

    FormatSimpleType.formatWellKindedType(tpe) // MATT hacking for testing

  }
}
