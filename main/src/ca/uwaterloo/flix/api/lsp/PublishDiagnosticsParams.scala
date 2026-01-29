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
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.errors.CodeHint
import ca.uwaterloo.flix.util.Formatter
import org.json4s.JsonDSL.*
import org.json4s.*
import org.eclipse.lsp4j

import scala.jdk.CollectionConverters.*

/**
  * Companion object of [[PublishDiagnosticsParams]].
  */
object PublishDiagnosticsParams {
  def fromMessages(errors: List[CompilationMessage], root: Option[TypedAst.Root]): List[PublishDiagnosticsParams] = {
    // Group the error messages by source.
    val errorsBySource = errors.groupBy(_.loc.source)

    // Translate each compilation message to a diagnostic.
    errorsBySource.foldLeft(Nil: List[PublishDiagnosticsParams]) {
      case (acc, (source, compilationMessages)) =>
        val diagnostics = compilationMessages.map(msg => Diagnostic.from(msg, root))
        PublishDiagnosticsParams(source.name, diagnostics) :: acc
    }
  }

  def fromCodeHints(hints: List[CodeHint]): List[PublishDiagnosticsParams] = {
    // Group the error messages by source.
    val errorsBySource = hints.groupBy(_.loc.source)

    // Translate each code hint to a diagnostic.
    errorsBySource.foldLeft(Nil: List[PublishDiagnosticsParams]) {
      case (acc, (source, codeHints)) =>
        val diagnostics = codeHints.map(codeHint => Diagnostic.from(codeHint))
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

  def toLsp4j: lsp4j.PublishDiagnosticsParams = {
    val params = new lsp4j.PublishDiagnosticsParams()
    params.setUri(uri)
    params.setDiagnostics(diagnostics.map(_.toLsp4j).asJava)
    params
  }
}
