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
  * Represents a `TextEdit` in LSP.
  *
  * @param range   The range of the text document to be manipulated.
  *                To insert text into a document create a range where start === end.
  * @param newText The string to be inserted. For delete operations use an empty string.
  */
case class TextEdit(range: Range, newText: String) {
  def toJSON: JValue = ("range" -> range.toJSON) ~ ("newText" -> newText)

  def toLsp4j: lsp4j.TextEdit = new lsp4j.TextEdit(range.toLsp4j, newText)
}
