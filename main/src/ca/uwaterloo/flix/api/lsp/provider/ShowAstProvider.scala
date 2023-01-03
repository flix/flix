/*
 * Copyright 2022 Jonathan Lindegaard Starup
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
package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._

object ShowAstProvider {

  private val IrFileExtension = "flixir"

  /**
    * Returns a JSON object with
    *
    * - `title` (a string like `Namer.flix.ir`)
    *
    * - `text` (a string with the ir representation).
    */
  def showAst(phase: String)(implicit index: Index, root: Option[Root], flix: Flix): JObject = {

    // The optional root should be reused if present and the phase is in the
    // code-gen part of the compiler.

    val text = "def main(): String = \"Work in progress!\"\n"

    ("title" -> s"$phase.$IrFileExtension") ~ ("text" -> text)
  }

}
