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
package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp.{DocumentSymbol, Index, Position, Range, SymbolKind, SymbolTag}
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.debug.Audience

object SymbolProvider {

  private implicit val audience: Audience = Audience.External

  def processDocumentSymbols(uri: String)(implicit index: Index, root: Root): List[DocumentSymbol] =
    List(DocumentSymbol("name",
      Some("description"),
      SymbolKind.Interface,
      Range(Position(1, 1), Position(2, 5)),
      Range(Position(1, 1), Position(2, 5)),
      List(SymbolTag.Deprecated),
      List(DocumentSymbol(
        "parent",
        Some("parentDescription"),
        SymbolKind.Interface,
        Range(Position(0, 4), Position(0, 10)),
        Range(Position(0, 4), Position(0, 10)),
        List(),
        List()
      ))
    ))
}
