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
import org.json4s.JsonDSL.*

import java.nio.file.Path

object ShowAstProvider {

  /**
    * Returns a JSON object with
    *
    *   - `title` (a string like `Namer.flix.ir`)
    *   - `text` (a string with the ir representation).
    */
  def showAst()(implicit flix: Flix): JObject = {
    val oldOpts = flix.options
    flix.setOptions(oldOpts.copy(xprintphases = true))
    flix.compile()
    flix.setOptions(oldOpts)
    pathObject(AstPrinter.astFolderPath)
  }

  private def pathObject(path: Path): JObject = {
    ("path" -> path.toAbsolutePath.toString)
  }
}
