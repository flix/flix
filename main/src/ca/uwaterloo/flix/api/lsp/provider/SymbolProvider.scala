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

import ca.uwaterloo.flix.api.lsp.{DocumentSymbol, Position, Range, SymbolKind}
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Root

object SymbolProvider {

  def processDocumentSymbols(uri: String)(implicit root: Root): List[DocumentSymbol] = root.enums.filter {
    case (_, enum) => enum.loc.source.name == uri }.map {
      case (_, enum) => enumToDocumentSymbol(enum)
    }.toList

  /**
    * Returns an Enum DocumentSymbol from an Enum node.
    * It navigates the AST and adds Cases of enum as children DocumentSymbols.
    */
  private def enumToDocumentSymbol(enum: TypedAst.Enum): DocumentSymbol = DocumentSymbol(
    enum.sym.name,
    Some(enum.doc.lines.mkString(" ")),
    SymbolKind.Enum,
    Range.from(enum.loc),
    Range.from(enum.loc),
    List(),
    enum.cases.map { case (_, value) => value }.map(mkCaseDocumentSymbol).toList
  )

  /**
    * Returns an EnumMember DocumentSymbol from a Case node.
    */
  private def mkCaseDocumentSymbol(c: TypedAst.Case): DocumentSymbol = DocumentSymbol(
    c.tag.name, None, SymbolKind.EnumMember, Range.from(c.loc), Range.from(c.loc), List(), List()
  )
}
