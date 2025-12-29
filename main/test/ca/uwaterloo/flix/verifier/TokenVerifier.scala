/*
 * Copyright 2025 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.verifier

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.Source
import ca.uwaterloo.flix.language.ast.{SourceLocation, SourcePosition, Token, TokenKind}
import ca.uwaterloo.flix.util.{Formatter, InternalCompilerException, ParOps}

/**
  * Invariants:
  *   - I-Eof-A: The last token is [[TokenKind.Eof]] and no other token is.
  *   - I-Eof-B: No token before the last is [[TokenKind.Eof]].
  *   - I-Src: All tokens have a [[Source]] consistent with the source map.
  *   - I-Ranges: Offset ranges must end after they are started (except EOF).
  *   - I-Bounds: Offset ranges must be inside bounds.
  *   - I-Offset: Token offsets must be in order and not overlapping.
  *
  * Assumptions:
  *   - Offset ranges are end-exclusive.
  */
object TokenVerifier {

  /** Checks that tokens adhere to the invariants. */
  def verify(m: Map[Source, Array[Token]])(implicit flix: Flix): Unit = {
    ParOps.parMap(m) {
      case (src, tokens) => checkSource(src, tokens)
    }
  }

  /** Checks the token invariants of `tokens` of `src`. */
  private def checkSource(src: Source, tokens: Array[Token]): Unit = {
    val lastIndex = tokens.length - 1
    checkEndOfFileA(src, tokens)
    var prev: Token = null
    for ((token, i) <- tokens.iterator.zipWithIndex) {
      checkEndOfFileB(token, i == lastIndex)
      checkSrc(src, token)
      checkRange(token)
      checkBounds(src, token)
      if (prev != null) {
        checkOffsetOrder(prev, token)
      }
      prev = token
    }
  }

  /** Checks I-Eef-A. */
  private def checkEndOfFileA(src: Source, tokens: Array[Token]): Unit = {
    tokens.lastOption match {
      case None => emptyTokens(src)
      case Some(last) =>
        if (last.kind != TokenKind.Eof) missingEof(last)
    }
  }

  /** Checks I-Eef-B. */
  private def checkEndOfFileB(token: Token, isLast: Boolean): Unit =
    if (token.kind == TokenKind.Eof && !isLast) unexpectedEof(token)

  /** Checks I-Src. */
  private def checkSrc(src: Source, token: Token): Unit =
    if (token.src != src) wrongSource(src, token)

  /** Checks I-Ranges. */
  private def checkRange(token: Token): Unit = {
    if (token.kind != TokenKind.Eof) {
      if (token.startIndex >= token.endIndex) wrongOffsetRange(token)
    }
  }

  /** Checks I-Bounds. */
  private def checkBounds(src: Source, token: Token): Unit = {
    // Tokens end offsets are exclusive and offsets are zero-indexed.
    def outOfBounds(i: Int): Boolean = i < 0 || src.data.length < i
    if (outOfBounds(token.startIndex) || outOfBounds(token.endIndex)) outOfBoundOffset(token)
  }

  /** Checks I-Offset. */
  private def checkOffsetOrder(left: Token, right: Token): Unit = {
    // Tokens end offsets are exclusive.
    if (left.endIndex > right.startIndex) outOfOrderOffsets(left, right)
  }

  private def missingEof(found: Token): Nothing = {
    val loc = found.mkSourceLocation()
    val msg =
      s""">> Found non-eof last token: ${found.kind}.
         |
         |${Formatter.NoFormatter.code(loc, "here")}
         |
         |""".stripMargin
    throw InternalCompilerException(msg, loc)
  }

  private def unexpectedEof(found: Token): Nothing = {
    val loc = found.mkSourceLocation()
    val msg =
      s""">> Found ${TokenKind.Eof} with tokens following it.
         |
         |${Formatter.NoFormatter.code(loc, "here")}
         |
         |""".stripMargin
    throw InternalCompilerException(msg, loc)
  }

  private def wrongSource(src: Source, found: Token): Nothing = {
    val loc = found.mkSourceLocation()
    val msg =
      s""">> Found token with source ${found.src.name} in the tokens of ${src.name}.
         |
         |${Formatter.NoFormatter.code(loc, s"here (${found.kind})")}
         |
         |""".stripMargin
    throw InternalCompilerException(msg, loc)
  }

  private def wrongOffsetRange(found: Token): Nothing = {
    val loc = found.mkSourceLocation()
    val msg =
      s""">> Invalid offset range: ${found.startIndex} - ${found.endIndex}.
         |
         |${Formatter.NoFormatter.code(loc, s"here (${found.kind})")}
         |
         |""".stripMargin
    throw InternalCompilerException(msg, loc)
  }

  private def outOfBoundOffset(found: Token): Nothing = {
    val loc = found.mkSourceLocation()
    val msg =
      s""">> Token with out-of-bound offsets: ${found.startIndex} - ${found.endIndex}.
         |
         |${Formatter.NoFormatter.code(loc, s"here (${found.kind})")}
         |
         |""".stripMargin
    throw InternalCompilerException(msg, loc)
  }

  private def outOfOrderOffsets(left: Token, right: Token): Nothing = {
    val loc = left.mkSourceLocation()
    val msg =
      s""">> Overlapping tokens: ${left.startIndex} - ${right.endIndex} and ${right.startIndex} - ${right.endIndex}.
         |
         |${Formatter.NoFormatter.code(left.mkSourceLocation(), s"left token here (${left.kind})")}
         |
         |${Formatter.NoFormatter.code(right.mkSourceLocation(), s"right token here (${right.kind})")}
         |
         |""".stripMargin
    throw InternalCompilerException(msg, loc)
  }

  private def emptyTokens(src: Source): Nothing = {
    val loc = SourceLocation.zeroPoint(isReal = true, src, 0)
    val msg = s"Found empty token array for ${src.name}."
    throw InternalCompilerException(msg, loc)
  }

}
