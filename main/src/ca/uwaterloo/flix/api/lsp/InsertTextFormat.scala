/*
 * Copyright 2021 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp

import org.eclipse.lsp4j

/**
  * Represents an `InsertTextFormat` in LSP.
  */
sealed trait InsertTextFormat {
  def toInt: Int = this match {
    case InsertTextFormat.PlainText => 1
    case InsertTextFormat.Snippet => 2
  }

  def toLsp4j: lsp4j.InsertTextFormat = this match {
    case InsertTextFormat.PlainText => lsp4j.InsertTextFormat.PlainText
    case InsertTextFormat.Snippet => lsp4j.InsertTextFormat.Snippet
  }
}

object InsertTextFormat {

  /**
    * The primary text to be inserted is treated as a plain string.
    */
  case object PlainText extends InsertTextFormat

  /**
    *
    * The primary text to be inserted is treated as a snippet.
    *
    * A snippet can define tab stops and placeholders with `$1`, `$2`
    * and `${3:foo}`. `$0` defines the final tab stop, it defaults to
    * the end of the snippet. Placeholders with equal identifiers are
    * linked, that is typing in one will update others too.
    */
  case object Snippet extends InsertTextFormat

}
