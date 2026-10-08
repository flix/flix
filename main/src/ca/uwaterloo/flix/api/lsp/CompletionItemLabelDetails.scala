/*
 * Copyright 2024 Chenhao Gao
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp

import org.json4s.JsonDSL.*
import org.json4s.*

/**
  * Represents a `CompletionItemLabelDetails` in LSP.
  */
case class CompletionItemLabelDetails(detail: Option[String], description: Option[String]) {
  def toJSON: JValue = ("detail" -> detail) ~ ("description" -> description)
}
