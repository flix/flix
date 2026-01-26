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
import ca.uwaterloo.flix.language.errors.Highlighter.highlight
import ca.uwaterloo.flix.util.{Formatter, InternalCompilerException, ParOps}

/**
  * Invariants:
  *   - I-Eof-A: The last token is [[TokenKind.Eof]] and no other token is.
  *   - I-Eof-B: No token before the last is [[TokenKind.Eof]].
  *   - I-Src: All tokens have a [[Source]] consistent with the source map.
  *   - I-Ranges: Offset- and position ranges must end after they are started (except EOF).
  *   - I-Bounds: Offset ranges must be inside bounds.
  *   - I-Offset: Token offsets must be in order and not overlapping.
  *   - I-Pos: Positions must be in order and not overlapping.
  *   - I-Col-End: Exclusive column ends of ranges should never be the first column (except EOF).
  *
  * Assumptions:
  *   - Offset ranges are end-exclusive.
  *   - Position ranges are end-exclusive.
  *
  * Invariants not checked:
  *   - Position ranges must be valid line/col ranges.
  *   - Position ranges must correspond to offset ranges.
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
      checkColEnd(token)
      if (prev != null) {
        checkOffsetOrder(prev, token)
        checkPositionOrder(prev, token)
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
      if (!isStrictlyBefore(token.start, token.end)) wrongPositionRange(token)
    }
  }

  /** Returns true if `sp1` is strictly before `sp2`, ignoring [[SourcePosition.source]]. */
  private def isStrictlyBefore(sp1: SourcePosition, sp2: SourcePosition): Boolean = {
    if (sp1.lineOneIndexed == sp2.lineOneIndexed) {
      sp1.colOneIndexed < sp2.colOneIndexed
    } else sp1.lineOneIndexed < sp2.lineOneIndexed
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

  /** Checks I-Pos. */
  private def checkPositionOrder(left: Token, right: Token): Unit = {
    // Token end positions are exclusive.
    if (!isBefore(left.end, right.start)) outOfOrderPositions(left, right)
  }

  /** Checks I-Col-End. */
  private def checkColEnd(token: Token): Unit = {
    if (token.kind != TokenKind.Eof) {
      if (token.end.colOneIndexed == 1) unneccesaryEndCol(token)
    }
  }

  /** Returns true if `sp1` is before or the same as `sp2`, ignoring [[SourcePosition.source]]. */
  private def isBefore(sp1: SourcePosition, sp2: SourcePosition): Boolean = {
    if (sp1.lineOneIndexed == sp2.lineOneIndexed) {
      sp1.colOneIndexed <= sp2.colOneIndexed
    } else sp1.lineOneIndexed <= sp2.lineOneIndexed
  }

  private def missingEof(found: Token): Nothing = {
    val loc = found.mkSourceLocation()
    val msg =
      s""">> Found non-eof last token: ${found.kind}.
         |
         |${highlight(loc, "here", Formatter.NoFormatter)(None)}
         |
         |""".stripMargin
    throw InternalCompilerException(msg, loc)
  }

  private def unexpectedEof(found: Token): Nothing = {
    val loc = found.mkSourceLocation()
    val msg =
      s""">> Found ${TokenKind.Eof} with tokens following it.
         |
         |${highlight(loc, "here", Formatter.NoFormatter)(None)}
         |
         |""".stripMargin
    throw InternalCompilerException(msg, loc)
  }

  private def wrongSource(src: Source, found: Token): Nothing = {
    val loc = found.mkSourceLocation()
    val msg =
      s""">> Found token with source ${found.src.name} in the tokens of ${src.name}.
         |
         |${highlight(loc, s"here (${found.kind})", Formatter.NoFormatter)(None)}
         |
         |""".stripMargin
    throw InternalCompilerException(msg, loc)
  }

  private def wrongOffsetRange(found: Token): Nothing = {
    val loc = found.mkSourceLocation()
    val msg =
      s""">> Invalid offset range: ${found.startIndex} - ${found.endIndex}.
         |
         |${highlight(loc, s"here (${found.kind})", Formatter.NoFormatter)(None)}
         |
         |""".stripMargin
    throw InternalCompilerException(msg, loc)
  }

  private def wrongPositionRange(found: Token): Nothing = {
    val loc = found.mkSourceLocation()
    val positionRangeString = s"${found.start.lineOneIndexed}:${found.start.colOneIndexed} - ${found.end.lineOneIndexed}:${found.end.colOneIndexed}"
    val msg =
      s""">> Invalid position range: $positionRangeString.
         |
         |${highlight(loc, s"here (${found.kind})", Formatter.NoFormatter)(None)}
         |
         |""".stripMargin
    throw InternalCompilerException(msg, loc)
  }

  private def outOfBoundOffset(found: Token): Nothing = {
    val loc = found.mkSourceLocation()
    val msg =
      s""">> Token with out-of-bound offsets: ${found.startIndex} - ${found.endIndex}.
         |
         |${highlight(loc, s"here (${found.kind})", Formatter.NoFormatter)(None)}
         |
         |""".stripMargin
    throw InternalCompilerException(msg, loc)
  }

  private def outOfOrderPositions(left: Token, right: Token): Nothing = {
    val loc = left.mkSourceLocation()
    val leftPos = s"${left.start.lineOneIndexed}:${left.start.colOneIndexed} - ${left.end.lineOneIndexed}:${left.end.colOneIndexed}"
    val rightPos = s"${right.start.lineOneIndexed}:${right.start.colOneIndexed} - ${right.end.lineOneIndexed}:${right.end.colOneIndexed}"
    val msg =
      s""">> Overlapping tokens (position): $leftPos and $rightPos.
         |
         |${highlight(left.mkSourceLocation(), s"left token here (${left.kind})", Formatter.NoFormatter)(None)}
         |
         |${highlight(right.mkSourceLocation(), s"right token here (${right.kind})", Formatter.NoFormatter)(None)}
         |
         |""".stripMargin
    throw InternalCompilerException(msg, loc)
  }

  private def outOfOrderOffsets(left: Token, right: Token): Nothing = {
    val loc = left.mkSourceLocation()
    val msg =
      s""">> Overlapping tokens: ${left.startIndex} - ${right.endIndex} and ${right.startIndex} - ${right.endIndex}.
         |
         |${highlight(left.mkSourceLocation(), s"left token here (${left.kind})", Formatter.NoFormatter)(None)}
         |
         |${highlight(right.mkSourceLocation(), s"right token here (${right.kind})", Formatter.NoFormatter)(None)}
         |
         |""".stripMargin
    throw InternalCompilerException(msg, loc)
  }

  private def unneccesaryEndCol(found: Token): Nothing = {
    val loc = found.mkSourceLocation()
    val msg =
      s""">> End column is the first column of next line instead of last column of the existing line.
         |
         |${highlight(found.mkSourceLocation(), s"here (${found.kind})", Formatter.NoFormatter)(None)}
         |
         |""".stripMargin
    throw InternalCompilerException(msg, loc)
  }

  private def emptyTokens(src: Source): Nothing = {
    val loc = SourceLocation.zeroPoint(isReal = true, src, SourcePosition.FirstPosition)
    val msg = s"Found empty token array for ${src.name}."
    throw InternalCompilerException(msg, loc)
  }

}
