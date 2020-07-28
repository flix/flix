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
import org.json4s.JsonDSL._
import org.json4s._

/**
  * Companion object for [[Diagnostic]].
  */
object Diagnostic {
  def from(compilationError: CompilationError): Diagnostic = {
    val range = Range.from(compilationError.loc)
    val code = compilationError.kind
    val message = compilationError.summary
    Diagnostic(range, Some(DiagnosticSeverity.Error), Some(code), None, message, Nil)
  }
}

/**
  * Represents a `Diagnostic` in LSP.
  *
  * @param range    The range at which the message applies.
  * @param severity The diagnostic's severity. Can be omitted. If omitted it is up to the client to interpret diagnostics as error, warning, info or hint.
  * @param code     The diagnostic's code, which might appear in the user interface.
  * @param source   A human-readable string describing the source of this diagnostic, e.g. 'typescript' or 'super lint'.
  * @param message  The diagnostic's message.
  * @param tags     Additional metadata about the diagnostic.
  */
case class Diagnostic(range: Range, severity: Option[DiagnosticSeverity], code: Option[String], source: Option[String], message: String, tags: List[DiagnosticTag]) {
  def toJSON: JObject =
    ("range" -> range.toJSON) ~
      ("severity" -> severity.map(_.toInt)) ~
      ("code" -> code) ~
      ("source" -> source) ~
      ("message" -> message) ~
      ("tags" -> tags.map(_.toInt))
}
