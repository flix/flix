/*
 * Copyright 2021 Jacob Harris Cryer Kragh
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

import ca.uwaterloo.flix.api.lsp._
import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, Root}
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._

import scala.collection.immutable.SortedSet
import scala.collection.mutable.ArrayBuffer

object SemanticTokensProvider {

  /**
   * Processes a request for (full) semantic tokens.
   */
  def provideSemanticTokens(uri: String)(implicit index: Index, root: Root): JObject = {
    val entities = index.query(uri)
    val semanticTokens = entities.flatMap(getSemanticTokens)
    val encoding = encodeSemanticTokens(semanticTokens)
    val result = ("data" -> encoding)
    ("status" -> "success") ~ ("result" -> result)
  }

  /**
   * Returns the semantic tokens in the given Entity.
   */
  private def getSemanticTokens(entity: Entity): List[SemanticToken] = entity match {
    case Entity.Exp(e) => e match {
      case Expression.Int8(_, _)
           | Expression.Int16(_, _)
           | Expression.Int32(_, _)
           | Expression.Float32(_, _)
           | Expression.Float64(_, _)
           | Expression.BigInt(_, _) => List(SemanticToken(e.loc, SemanticTokenType.Number, List()))
      case Expression.Str(_, loc) => List(SemanticToken(loc, SemanticTokenType.Str, List()))
      case _ => List() // TODO: Handle other kinds of expressions
    }
    case _ => List() // TODO: Handle other kinds of entities
  }

  // Inspired by https://github.com/microsoft/vscode-languageserver-node/blob/f425af9de46a0187adb78ec8a46b9b2ce80c5412/server/src/sematicTokens.proposed.ts#L45
  private def encodeSemanticTokens(tokens: Iterable[SemanticToken]): List[Int] = {
    val encoding = new ArrayBuffer[Int](initialSize = 5 * tokens.size)

    var prevLine = 0
    var prevCol = 0

    implicit val tokenOrdering: Ordering[SemanticToken] = Ordering.by(_.loc)
    for (token <- SortedSet.empty.concat(tokens)) {
      var relLine = token.loc.beginLine - 1
      var relCol = token.loc.beginCol - 1

      if (encoding.nonEmpty) {
        relLine -= prevLine
        if (relLine == 0) {
          relCol -= prevCol
        }
      }

      encoding += relLine
      encoding += relCol
      encoding += token.loc.endCol - token.loc.beginCol
      encoding += token.tokenType.toInt
      encoding += encodeModifiers(token.tokenModifiers)

      prevLine = token.loc.beginLine - 1
      prevCol = token.loc.beginCol - 1
    }

    encoding.toList
  }

  /**
   * Encodes a list of modifiers as a bitset (as per the LSP spec).
   */
  def encodeModifiers(modifiers: List[SemanticTokenModifier]): Int =
    modifiers.foldLeft(0)((bitset, modifier) => bitset | (1 << modifier.toInt))
}
