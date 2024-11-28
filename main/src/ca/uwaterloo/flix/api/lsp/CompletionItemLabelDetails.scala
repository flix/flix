package ca.uwaterloo.flix.api.lsp

import org.json4s.JsonDSL.*
import org.json4s.*

/**
  * Represents a `CompletionItemLabelDetails` in LSP.
  */
case class CompletionItemLabelDetails(detail: Option[String], description: Option[String]) {
  def toJSON: JValue = ("detail" -> detail) ~ ("description" -> description)
}
