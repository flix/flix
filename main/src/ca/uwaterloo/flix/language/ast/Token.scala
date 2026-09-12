/*
 * Copyright 2023 Herluf Baggesen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
  * @param start      The source position that the lexeme __starts__ on.
  * @param end        The source position that the lexeme __ends__ on (exclusive).
  */
case class Token(kind: TokenKind, src: Source, startIndex: Int, endIndex: Int, start: SourcePosition, end: SourcePosition) extends SyntaxTree.Child {
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
  def mkSourceLocation(isReal: Boolean = true): SourceLocation = SourceLocation(isReal, src, start, end)

  /**
    * Returns a one-indexed string representation of this token. Must only be used for debugging.
    */
  override def toString: String = s"Token($kind, $text, ${start.lineOneIndexed}, ${start.colOneIndexed}, ${end.lineOneIndexed}, ${end.colOneIndexed})"
}
