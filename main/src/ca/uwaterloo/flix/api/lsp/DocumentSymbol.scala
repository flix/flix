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

import ca.uwaterloo.flix.language.ast.TypedAst.Def
import ca.uwaterloo.flix.language.ast.TypedAst.Enum
import ca.uwaterloo.flix.language.ast.TypedAst.Case
import org.json4s.JsonDSL._
import org.json4s._

/**
  * Companion object of [[DocumentSymbol]].
  */
object DocumentSymbol {

  /**
    * Returns the document symbol of the given definition declaration `decl0`.
    */
  def from(decl0: Def): DocumentSymbol = {
    val name = decl0.sym.name
    val kind = SymbolKind.Function
    val range = Range.from(decl0.sym.loc)
    val selectionRange = Range(Position(decl0.exp.loc.beginLine, 0), Position(decl0.exp.loc.endLine, decl0.exp.loc.endCol))
    DocumentSymbol(name, kind, range, selectionRange, Nil)
  }

  /**
    * Returns the document symbol of the given enum declaration `decl0`.
    */
  def from(decl0: Enum): DocumentSymbol = {
    val enumName = decl0.sym.name
    val enumKind = SymbolKind.Enum
    val enumRange = Range.from(decl0.sym.loc)
    val enumSelectionRange = Range.from(decl0.loc)
    val enumChildren = decl0.cases.values.map {
      case Case(_, tag, _, _, loc) =>
        val tagName = tag.name
        val tagKind = SymbolKind.EnumMember
        val tagRange = Range.from(tag.loc)
        val tagSelectionRange = Range.from(loc)
        DocumentSymbol(tagName, tagKind, tagRange, tagSelectionRange, Nil)
    }
    DocumentSymbol(enumName, enumKind, enumRange, enumSelectionRange, enumChildren.toList)
  }

}

/**
  * Represents a `DocumentSymbol` in LSP.
  *
  * @param name           The name of this symbol. Will be displayed in the user interface and therefore must not be an
  *                       empty string or a string only consisting of white spaces.
  * @param kind           The kind of this symbol.
  * @param range          The range enclosing this symbol not including leading/trailing whitespace but everything else
  *                       like comments. This information is typically used to determine if the clients cursor is inside
  *                       the symbol to reveal in the symbol in the UI.
  * @param selectionRange The range that should be selected and revealed when this symbol is being picked, e.g the name
  *                       of a function. Must be contained by the `range`.
  * @param children       Children of this symbol, e.g. properties of a class.
  */
case class DocumentSymbol(name: String, kind: SymbolKind, range: Range, selectionRange: Range, children: List[DocumentSymbol]) {
  def toJSON: JValue =
    ("name" -> name) ~
      ("kind" -> kind.toInt) ~
      ("range" -> range.toJSON) ~
      ("selectionRange" -> selectionRange.toJSON) ~
      ("children" -> children.map(_.toJSON))
}
