/*
 * Copyright 2020 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp

import org.eclipse.lsp4j
import org.json4s.JString
import org.json4s.JsonAST.JValue

/**
  * Represents a `MarkupKind` in LSP.
  */
sealed trait MarkupKind {
  def toJSON: JValue = this match {
    case MarkupKind.PlainText => JString("plaintext")
    case MarkupKind.Markdown => JString("markdown")
  }

  def toLsp4j: String = this match {
    case MarkupKind.PlainText => lsp4j.MarkupKind.PLAINTEXT
    case MarkupKind.Markdown => lsp4j.MarkupKind.MARKDOWN
  }
}

object MarkupKind {

  case object PlainText extends MarkupKind

  case object Markdown extends MarkupKind

}
