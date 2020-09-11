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
    * Construct a string representation of the type scheme.
    */
  def formatScheme(sc: Scheme)(implicit audience: Audience): String = {
    if (sc.quantifiers.isEmpty)
      FormatType.formatType(sc.base)
    else
      s"∀(${sc.quantifiers.map(tvar => tvar.getText.getOrElse(tvar.id)).mkString(", ")}). ${FormatType.formatType(sc.base)}" + s" with ${sc.constraints}" // MATT better formatting
  }
}
