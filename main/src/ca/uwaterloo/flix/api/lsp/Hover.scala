/*
 * Copyright 2025 Chenhao Gao
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp

import org.eclipse.lsp4j
import org.json4s.JObject
import org.json4s.JsonDSL.*

case class Hover(contents: MarkupContent, range: Range) {
  def toJSON: JObject = {
    val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
    ("status" -> ResponseStatus.Success) ~ ("result" -> result)
  }

  def toLsp4j: lsp4j.Hover = {
    val hover = new lsp4j.Hover()
    hover.setContents(contents.toLsp4j)
    hover.setRange(range.toLsp4j)
    hover
  }

}
