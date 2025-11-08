/*
 * Copyright 2025 gwydd
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
import ca.uwaterloo.flix.api.lsp.{Position, PrettyPrinter, Range, TextEdit}
import org.eclipse.lsp4j.FormattingOptions

import java.nio.file.Path


object FormattingProvider {

  def formatDocument(uri: String, options: FormattingOptions)(implicit flix: Flix): List[TextEdit] = {
    val _ = options // TODO: Implement logic to use formatting options.

    val oldOpts = flix.options
    flix.setOptions(oldOpts.copy(incremental = true))
    flix.compile()
    val parsedAst = flix.getCachedParserCst

    val formattedDocument = PrettyPrinter.printPretty(parsedAst, Path.of(uri))
    val fullRange = Range(Position(1, 1), Position(Int.MaxValue, 1))
    val edit = TextEdit(fullRange, formattedDocument)

    flix.setOptions(oldOpts)
    List(edit)
  }

}
