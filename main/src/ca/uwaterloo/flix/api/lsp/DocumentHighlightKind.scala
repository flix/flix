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
  */
sealed trait DocumentHighlightKind {
  def toJSON: JValue = this match {
    case DocumentHighlightKind.Text => 1
    case DocumentHighlightKind.Read => 2
    case DocumentHighlightKind.Write => 3
  }

  def toLsp4j: lsp4j.DocumentHighlightKind = this match {
    case DocumentHighlightKind.Text => lsp4j.DocumentHighlightKind.Text
    case DocumentHighlightKind.Read => lsp4j.DocumentHighlightKind.Read
    case DocumentHighlightKind.Write => lsp4j.DocumentHighlightKind.Write
  }
}

object DocumentHighlightKind {

  /**
    * A textual occurrence.
    */
  case object Text extends DocumentHighlightKind

  /**
    * Read-access of a symbol, like reading a variable.
    */
  case object Read extends DocumentHighlightKind

  /**
    * Write-access of a symbol, like writing to a variable.
    */
  case object Write extends DocumentHighlightKind

}
