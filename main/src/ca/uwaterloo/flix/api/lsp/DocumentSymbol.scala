/*
 * Copyright 2021 Nicola Dardanis
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
  * Represents a `DocumentSymbol` in LSP.
  *
  * @param name           The name of this symbol. Will be displayed in the user interface.
  * @param detail         More detail for this symbol, e.g the signature of a function.
  * @param kind           The kind of this symbol.
  * @param range          The range enclosing this symbol not including leading/trailing whitespace
  *                       but everything else like comments. This information is typically used to
  *                       determine if the clients cursor is inside the symbol to reveal in the
  *                       symbol in the UI.
  * @param selectionRange The range that should be selected and revealed when this symbol is being
  *                       picked, e.g. the name of a function. Must be contained by the `range`.
  * @param tags           Tags for this symbol.
  * @param children       Children of this symbol, e.g. properties of a class.
  */
case class DocumentSymbol(name: String,
                          detail: Option[String],
                          kind: SymbolKind,
                          range: Range,
                          selectionRange: Range,
                          tags: List[SymbolTag] = List(),
                          children: List[DocumentSymbol]) {
  def toJSON: JValue =
    ("name" -> name) ~
      ("detail" -> detail) ~
      ("kind" -> JInt(kind.toInt)) ~
      ("range" -> range.toJSON) ~
      ("selectionRange" -> selectionRange.toJSON) ~
      ("tags" -> tags.map(_.toJSON)) ~
      ("children" -> children.map(_.toJSON))
}

