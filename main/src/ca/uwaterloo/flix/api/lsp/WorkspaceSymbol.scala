/*
 * Copyright 2025 Chenhao Gao
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

import org.json4s.JsonDSL.*
import org.json4s.*

/**
  * Represents a `WorkspaceSymbol` in LSP.
  * Provides information about programming constructs like variables, classes, interfaces etc.
  *
  * @param name           The name of this symbol.
  * @param kind           The kind of this symbol.
  * @param tags           Tags for this symbol.
  * @param containerName  The name of the symbol containing this symbol. This information is for
  *                       user interface purposes (e.g. to render a qualifier in the user interface
  *                       if necessary). It can't be used to re-infer a hierarchy for the document
  *                       symbols.
  * @param location       The location of this symbol. Whether a server is allowed to
  *                       return a location without a range depends on the client
  *                       capability `workspace.symbol.resolveSupport`.
  */
case class WorkspaceSymbol(name: String,
                           kind: SymbolKind,
                           tags: List[SymbolTag] = Nil,
                           containerName: Option[String],
                           location: Location,
                           ) {
  def toJSON: JValue =
    ("name" -> name) ~
      ("kind" -> JInt(kind.toInt)) ~
      ("tags" -> tags.map(_.toJSON)) ~
      ("containerName" -> containerName.orNull) ~
      ("location" -> location.toJSON)
}
