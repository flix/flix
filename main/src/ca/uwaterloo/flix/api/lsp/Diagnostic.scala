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

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.errors.CodeHint
import ca.uwaterloo.flix.util.Formatter
import org.json4s.JsonDSL._
import org.json4s._

/** Companion object for [[Diagnostic]]. */
object Diagnostic {
  def from(compilationMessage: CompilationMessage, explain: Boolean, formatter: Formatter): Diagnostic = {
    val range = Range.from(compilationMessage.loc)
    val severity = Some(DiagnosticSeverity.Error)
    val code = compilationMessage.kind
    val summary = compilationMessage.summary
    val explanationHeading =
      s"""
         |${formatter.underline("Explanation:")}
         |""".stripMargin
    val explanation = compilationMessage.explain(formatter) match {
      case Some(expl) if explain => explanationHeading + expl
      case _ => ""
    }
    val fullMessage = compilationMessage.messageWithLoc(formatter) + explanation
    Diagnostic(range, severity, Some(code), None, summary, fullMessage, Nil)
  }

  def from(codeHint: CodeHint, formatter: Formatter): Diagnostic = {
    val range = Range.from(codeHint.loc)
    val severity = Some(DiagnosticSeverity.from(codeHint.severity))
    val summary = codeHint.summary
    Diagnostic(range, severity, None, None, summary, summary, Nil)
  }
}

/**
  * Represents a `Diagnostic` in LSP.
  *
  * @param range       The range at which the message applies.
  * @param severity    The diagnostic's severity. Can be omitted. If omitted it is up to the client to interpret diagnostics as error, warning, info or hint.
  * @param code        The diagnostic's code, which might appear in the user interface.
  * @param source      A human-readable string describing the source of this diagnostic, e.g. 'typescript' or 'super lint'.
  * @param message     The diagnostic's message.
  * @param fullMessage The full error message (non-standard).
  * @param tags        Additional metadata about the diagnostic.
  */
case class Diagnostic(range: Range, severity: Option[DiagnosticSeverity], code: Option[String], source: Option[String], message: String, fullMessage: String, tags: List[DiagnosticTag]) {
  def toJSON: JValue =
    ("range" -> range.toJSON) ~
      ("severity" -> severity.map(_.toInt)) ~
      ("code" -> code) ~
      ("source" -> source) ~
      ("message" -> message) ~
      ("fullMessage" -> fullMessage) ~
      ("tags" -> tags.map(_.toInt))
}
