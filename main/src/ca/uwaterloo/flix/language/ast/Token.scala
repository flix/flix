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

import ca.uwaterloo.flix.language.ast.shared.Source

/**
  * A Token holding a kind, a source, and a location.
  *
  * Token does not hold its lexeme directly to avoid duplication of common keywords like "def".
  * Instead it holds a pointer to its source along with start and end indices.
  *
  * We take extra efforts to ensure that tokens are compact, i.e. have small memory footprint.
  *
  * We do so because [[Token]]s are very common objects.
  *
  * @param kind       The kind of token this instance represents.
  * @param src        A pointer to the source that this lexeme stems from.
  * @param startIndex The absolute character index into `src` of the beginning of the lexeme. Must be zero-indexed.
  * @param endIndex   The absolute character index into `src` of the end (exclusive) of the lexeme. Must be zero-indexed.
  */
case class Token(kind: TokenKind, src: Source, startIndex: Int, endIndex: Int) extends SyntaxTree.Child {
  /**
    * Computes the lexeme that the token refers to by slicing it from `src`.
    *
    * Note: We do *not* cache the text because it takes up significant memory.
    */
  def text: String = src.data.slice(startIndex, endIndex).mkString("")

  /**
    * Makes a [[SourceLocation]] spanning this token.
    *
    * NB: Tokens are zero-indexed
    */
  def mkSourceLocation(isReal: Boolean = true): SourceLocation = SourceLocation(isReal, src, startIndex, endIndex)

  /**
    * Returns the start position (inclusive).
    *
    * Time Complexity: O(log lineCount)
    */
  def start: SourcePosition =
    src.lines.getPosition(startIndex)

  /**
    * Returns the end position (exclusive).
    *
    * Time Complexity: O(log lineCount)
    */
  def end: SourcePosition =
    src.lines.getPositionExclusive(endIndex)

  /**
    * Returns a one-indexed string representation of this token. Must only be used for debugging.
    */
  override def toString: String = {
    val start = this.start
    val end = this.end
    s"Token($kind, $text, ${start.lineOneIndexed}, ${start.colOneIndexed}, ${end.lineOneIndexed}, ${end.colOneIndexed})"
  }
}
