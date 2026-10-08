/*
 * Copyright 2020 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.errors.CodeHint
import ca.uwaterloo.flix.util.Formatter.AnsiTerminalFormatter
import org.json4s.JsonDSL.*
import org.json4s.*
import org.eclipse.lsp4j

import scala.jdk.CollectionConverters.*

/**
  * Companion object for [[Diagnostic]].
  */
object Diagnostic {
  def from(m: CompilationMessage, root: Option[TypedAst.Root]): Diagnostic = {
    val range = Range.from(m.loc)
    val severity = Some(DiagnosticSeverity.Error)
    val code = m.kind.toString
    val summary = m.summary
    val fullMessage = m.messageWithLoc(AnsiTerminalFormatter)(root)
    val relatedInformation = m.locs.map(l => DiagnosticRelatedInformation(Location.from(l), m.summary))
    Diagnostic(range, severity, Some(code), None, summary, fullMessage, Nil, relatedInformation)
  }

  def from(codeHint: CodeHint): Diagnostic = {
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
case class Diagnostic(range: Range, severity: Option[DiagnosticSeverity], code: Option[String], source: Option[String], message: String, fullMessage: String, tags: List[DiagnosticTag], relatedInformation: List[DiagnosticRelatedInformation] = Nil) {
  def toJSON: JValue =
    ("range" -> range.toJSON) ~
      ("severity" -> severity.map(_.toInt)) ~
      ("code" -> code) ~
      ("source" -> source) ~
      ("message" -> message) ~
      ("fullMessage" -> fullMessage) ~
      ("tags" -> tags.map(_.toInt)) ~
      ("relatedInformation" -> relatedInformation.map(_.toJSON))

  def toLsp4j: lsp4j.Diagnostic = {
    val diagnostic = new lsp4j.Diagnostic()
    diagnostic.setRange(range.toLsp4j)
    diagnostic.setSeverity(severity.map(_.toLsp4j).orNull)
    diagnostic.setCode(code.orNull)
    diagnostic.setSource(source.orNull)
    diagnostic.setMessage(message)
    diagnostic.setTags(tags.map(_.toLsp4j).asJava)
    diagnostic.setRelatedInformation(relatedInformation.map(_.toLsp4j).asJava)
    diagnostic
  }
}
