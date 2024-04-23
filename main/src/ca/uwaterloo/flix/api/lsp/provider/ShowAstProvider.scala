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
import ca.uwaterloo.flix.api.Flix.IrFileExtension
import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.dbg.AstPrinter
import ca.uwaterloo.flix.language.phase.Typer
import ca.uwaterloo.flix.util.Similarity
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._

object ShowAstProvider {

  /**
    * Returns a JSON object with
    *
    * - `title` (a string like `Namer.flix.ir`)
    * - `text` (a string with the ir representation).
    */
  def showAst(phase: String)(implicit index: Index, root: Option[Root], flix: Flix): JObject = {
    val names = AstPrinter.phaseNames().m1.keys.map(s => (s, s)).toMap
    val closestName = Similarity.closestMatch(phase, names)
    val oldOpts = flix.options
    flix.setOptions(oldOpts.copy(xprintphase = Set(closestName)))
    flix.compile()
    flix.setOptions(oldOpts)
    astObject(closestName, s"Check in build/asts/${closestName}.flixir")
  }

  private def astObject(phase: String, text: String): JObject = {
    ("title" -> s"$phase.$IrFileExtension") ~ ("text" -> text)
  }
}
