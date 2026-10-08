/*
 * Copyright 2020 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp

import org.eclipse.lsp4j
import org.json4s.JsonDSL.*
import org.json4s.*

/**
  * Represents a `DocumentHighlight` in LSP.
  *
  * @param range The range this highlight applies to.
  * @param kind  The highlight kind, default is DocumentHighlightKind.Text.
  */
case class DocumentHighlight(range: Range, kind: DocumentHighlightKind) {
  def toJSON: JValue = ("range" -> range.toJSON) ~ ("kind" -> kind.toJSON)

  def toLsp4j: lsp4j.DocumentHighlight = {
    val documentHighlight = new lsp4j.DocumentHighlight()
    documentHighlight.setRange(range.toLsp4j)
    documentHighlight.setKind(kind.toLsp4j)
    documentHighlight
  }
}
