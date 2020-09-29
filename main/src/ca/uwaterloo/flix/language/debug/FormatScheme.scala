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

import ca.uwaterloo.flix.language.ast.{Scheme, TypedAst}

object FormatScheme {

  /**
    * Construct a string representation of the type scheme,  e.g.
    * `∀(a, b).a -> Int -> b with Show[a], Eq[b]`
    */
  def formatScheme(sc: Scheme)(implicit audience: Audience): String = {
    val quantifiersPart =
      if (sc.quantifiers.isEmpty)
        ""
      else
        "∀(" + sc.quantifiers.map(tvar => tvar.getText.getOrElse(tvar.id)).mkString(", ") + ")."

    val typePart = FormatType.formatType(sc.base)

    val tconstrPart =
      if (sc.constraints.isEmpty)
        ""
      else
        "with " + sc.constraints.map(tconstr => s"${tconstr.sym.name}[${FormatType.formatType(tconstr.arg)}]").mkString(", ")

    quantifiersPart + typePart + tconstrPart
  }
}
