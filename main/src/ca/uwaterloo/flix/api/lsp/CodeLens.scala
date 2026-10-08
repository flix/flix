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
  * Represents a `CodeLens` in LSP.
  *
  * @param range   The range in which this code lens is valid. Should only span a single line.
  * @param command The command this code lens represents.
  */
case class CodeLens(range: Range, command: Option[Command]) {
  def toJSON: JValue = ("range" -> range.toJSON) ~ ("command" -> command.map(_.toJSON))

  def toLsp4j: lsp4j.CodeLens = {
    val codeLens = new lsp4j.CodeLens()
    codeLens.setRange(range.toLsp4j)
    command.foreach(cmd => codeLens.setCommand(cmd.toLsp4j))
    codeLens
  }
}
