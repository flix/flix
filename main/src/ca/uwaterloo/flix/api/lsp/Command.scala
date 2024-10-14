/*
 * Copyright 2020 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.api.lsp

import org.json4s.*
import org.json4s.JsonDSL.*

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
}
