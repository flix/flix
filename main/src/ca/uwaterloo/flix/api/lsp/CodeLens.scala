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
  * Represents a `CodeLens` in LSP.
  *
  * @param range   The range in which this code lens is valid. Should only span a single line.
  * @param command The command this code lens represents.
  */
case class CodeLens(range: Range, command: Option[Command]) {
  def toJSON: JValue = ("range" -> range.toJSON) ~ ("command" -> command.map(_.toJSON))
}
