/*
 * Copyright 2020 Matthew Lutze, Magnus Madsen
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

import ca.uwaterloo.flix.language.ast.Scheme

object FormatScheme {

  /**
    * Construct a string representation of the type scheme,  e.g.
    * `∀(a, b).a -> Int -> b with Show[a], Eq[b]`
    */
  def formatScheme(sc: Scheme)(implicit audience: Audience): String = {
    val mainPart = formatSchemeWithoutConstraints(sc)

    val tconstrPart =
      if (sc.constraints.isEmpty)
        ""
      else
        " with " + sc.constraints.map(FormatTypeConstraint.formatTypeConstraint).mkString(", ")

    mainPart + tconstrPart
  }

  /**
    * Construct a string representation of the type scheme, excluding type constraints, e.g.,
    * `∀(a, b).a -> Int -> b`
    */
  def formatSchemeWithoutConstraints(sc: Scheme)(implicit audience: Audience): String = {
    val quantifiersPart =
      if (sc.quantifiers.isEmpty)
        ""
      else
        "∀(" + sc.quantifiers.map(FormatType.formatType).mkString(", ") + "). "

    val typePart = FormatType.formatType(sc.base)

    quantifiersPart + typePart
  }
}
