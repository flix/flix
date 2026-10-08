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
  * Represents a `MarkupContent` in LSP.
  *
  * @param kind  The type of the Markup.
  * @param value The content itself.
  */
case class MarkupContent(kind: MarkupKind, value: String) {
  def toJSON: JValue = ("kind" -> kind.toJSON) ~ ("value" -> value)

  def toLsp4j: lsp4j.MarkupContent = {
    val markupContent = new lsp4j.MarkupContent()
    markupContent.setKind(kind.toLsp4j)
    markupContent.setValue(value)
    markupContent
  }
}
