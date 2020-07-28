/*
 * Copyright 2020 Magnus Madsen
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
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.util.vt.TerminalContext
import ca.uwaterloo.flix.util.vt.TerminalContext.NoTerminal
import org.json4s.JsonDSL._
import org.json4s._

/**
  * Companion object of [[PublishDiagnosticsParams]].
  */
object PublishDiagnosticsParams {
  def from(errors: LazyList[CompilationError]): List[PublishDiagnosticsParams] = {
    implicit val ctx: TerminalContext = NoTerminal

    // Group the error messages by source.
    val errorsBySource = errors.toList.groupBy(_.loc.source)

    // Translate each compilation error to a diagnostic.
    errorsBySource.foldLeft(Nil: List[PublishDiagnosticsParams]) {
      case (acc, (source, compilationErrors)) =>
        val diagnostics = compilationErrors.map(Diagnostic.from)
        PublishDiagnosticsParams(source.name, diagnostics) :: acc
    }
  }
}

/**
  * Represent a `PublishDiagnosticsParams` in LSP.
  *
  * @param uri         The URI for which diagnostic information is reported.
  * @param diagnostics An array of diagnostic information items.
  */
case class PublishDiagnosticsParams(uri: String, diagnostics: List[Diagnostic]) {
  def toJSON: JValue = ("uri" -> uri) ~ ("diagnostics" -> diagnostics.map(_.toJSON))
}
