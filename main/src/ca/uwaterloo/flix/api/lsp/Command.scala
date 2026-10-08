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

import scala.jdk.CollectionConverters.*

/**
  * Represents a `Command` in LSP.
  *
  * @param title     Title of the command, like `save`.
  * @param command   The identifier of the actual command handler.
  * @param arguments Arguments that the command handler should be invoked with.
  *
  */
case class Command(title: String, command: String, arguments: List[JValue]) {
  def toJSON: JValue = ("title" -> title) ~ ("command" -> command) ~ ("arguments" -> arguments)

  def toLsp4j: lsp4j.Command = new lsp4j.Command(title, command, arguments.map(_.asInstanceOf[Object]).asJava)
}
