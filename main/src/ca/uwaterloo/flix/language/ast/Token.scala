/*
 * Copyright 2023 Herluf Baggesen
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
package ca.uwaterloo.flix.language.ast

/**
  * A Token holding a kind, line and column.
  *
  * Token does not hold its lexeme directly to avoid duplication of common keywords like "def".
  * Instead it holds a pointer to its source along with start and end offsets.
  *
  * We take extra efforts to ensure that tokens are compact, i.e. have small memory footprint.
  *
  * We do so because [[Token]]s are very common objects.
  *
  * Specifically, we:
  *
  * - Use `Short`s instead of `Int`s to represent column offsets (i.e. `beginCol` and `endCol`).
  *
  * @param kind      The kind of token this instance represents.
  * @param src       A pointer to the source that this lexeme stems from.
  * @param start     The absolute character offset into `src` of the beginning of the lexeme.
  * @param end       The absolute character offset into `src` of the end of the lexeme.
  * @param beginLine The line that the lexeme __starts__ on.
  * @param beginCol  The column that the lexeme __starts__ on.
  * @param endLine   The line that the lexeme __ends__ on.
  * @param endCol    The column that the lexeme __ends__ on.
  */
case class Token(kind: TokenKind, src: Ast.Source, start: Int, end: Int, beginLine: Int, beginCol: Short, endLine: Int, endCol: Short) extends SyntaxTree.Child {
  /**
    * Computes the lexeme that the token refers to by slicing it from `src`.
    *
    * Note: We do *not* cache the text because it takes up significant memory.
    */
  def text: String = src.data.slice(start, end).mkString("")

  /**
    * Returns a string representation of this token. Should only be used for debugging.
    */
  override def toString: String = s"Token($kind, $text, $beginLine, $beginCol, $endLine, $endCol)"

  /**
    * Makes a [[SourcePosition]] pointing at the start of this token.
    * NB: Tokens are zero-indexed while SourcePositions are one-indexed
    */
  def mkSourcePosition: SourcePosition = SourcePosition(src, beginLine + 1, (beginCol + 1).toShort)

  /**
    * Makes a [[SourcePosition]] pointing at the _end_ of this token.
    * NB: Tokens are zero-indexed while SourcePositions are one-indexed
    */
  def mkSourcePositionEnd: SourcePosition = SourcePosition(src, endLine + 1, (endCol + 1).toShort)

  /**
    * Makes a [[SourceLocation]] spanning this token.
    * NB: Tokens are zero-indexed while SourceLocations are one-indexed
    */
  def mkSourceLocation(isReal: Boolean = true): SourceLocation = {
    SourceLocation(src, isReal, beginLine + 1, (beginCol + 1).toShort, endLine + 1, (endCol + 1).toShort)
  }
}

