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

import org.parboiled2.ParserInput

/**
 * A Token holding a kind, line and column.
 *
 * Token does not hold its lexeme directly to avoid duplication of common keywords like "def".
 * Instead it holds a pointer to its source along with start and end offsets.
 *
 * @param kind     The kind of token this instance represents.
 * @param src      A pointer to the source that this lexeme stems from.
 * @param start    The absolute character offset into `src` of the beginning of the lexeme.
 * @param end      The absolute character offset into `src` of the end of the lexeme.
 * @param line     The line that the lexeme __starts__ on.
 * @param col      The column that the lexeme __starts__ on.
 * @param lineEnd  The line that the lexeme __ends__ on.
 * @param colEnd   The column that the lexeme __ends__ on.
 */
case class Token(kind: TokenKind, src: Array[Char], start: Int, end: Int, line: Int, col: Int, lineEnd: Int, colEnd: Int) {
  /**
   * Computes the lexeme that the token refers to by slicing it from `src`.
   * Note: This is explicitly lazy since we do not want to compute strings that are never used,
   * such as keywords for instance. Take care to only access text when necessary.
   */
  lazy val text: String = src.slice(start, end).mkString("")

  /**
   * Returns a string representation of this token. Should only be used for debugging.
   */
  override def toString: String = s"Token($kind, $text, $line, $col, $lineEnd, $colEnd)"

  /**
   * Makes a [[SourcePosition]] pointing at the start of this token.
   * NB: Tokens are zero-indexed while SourcePositions are one-indexed
   */
  def mkSourcePosition(src: Ast.Source, parserInput: Option[ParserInput]):SourcePosition = {
    SourcePosition(src, line + 1, col + 1, parserInput)
  }

  /**
   * Makes a [[SourcePosition]] pointing at the _end_ of this token.
   * NB: Tokens are zero-indexed while SourcePositions are one-indexed
   */
  def mkSourcePositionEnd(src: Ast.Source, parserInput: Option[ParserInput]): SourcePosition = {
    SourcePosition(src, lineEnd + 1, colEnd + 1, parserInput)
  }

  /**
   * Makes a [[SourceLocation]] spanning this token.
   * NB: Tokens are zero-indexed while SourceLocations are one-indexed
   */
  def mkSourceLocation(src: Ast.Source, parserInput: Option[ParserInput], locationKind: SourceKind = SourceKind.Real): SourceLocation = {
    SourceLocation(parserInput, src, locationKind, line + 1, col + 1, lineEnd + 1, colEnd + 1)
  }
}

