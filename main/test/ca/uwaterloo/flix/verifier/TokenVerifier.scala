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
  *   - I-Eof: The last token is [[TokenKind.Eof]] and no other token is.
  *   - I-Src: All tokens have a [[Source]] consistent with the source map.
  *   - I-Ranges: Offset- and position ranges must end after they are started.
  *   - I-Bounds: Offset ranges must be inside bounds.
  *   - I-Offset: Token offsets must be in order and not overlapping.
  *   - I-Pos: Positions must be in order and not overlapping.
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
      case (src, tokens) =>
        checkEndOfFile(src, tokens)
        checkSrc(src, tokens)
        checkRanges(tokens)
        checkBounds(src, tokens)
        checkOffsetOrder(tokens)
        checkPositionOrder(tokens)
    }
  }

  /** Checks I-Eef. */
  private def checkEndOfFile(src: Source, tokens: Array[Token]): Unit = {
    tokens.lastOption match {
      case None => emptyTokens(src)
      case Some(last) =>
        if (last.kind != TokenKind.Eof) missingEof(last)
        tokens
          .iterator
          .zipWithIndex
          .filter { case (token, i) => token.kind == TokenKind.Eof && i != tokens.length - 1 }
          .foreach { case (token, _) => unexpectedEof(token) }
    }
  }

  /** Checks I-Src. */
  private def checkSrc(src: Source, tokens: Array[Token]): Unit = {
    tokens
      .iterator
      .filter(_.src != src)
      .foreach(wrongSource(src, _))
  }

  /** Checks I-Ranges. */
  private def checkRanges(tokens: Array[Token]): Unit = {
    tokens
      .iterator
      .filter(token => token.start > token.end)
      .foreach(wrongOffsetRange)
    tokens
      .iterator
      .filter(token => {
        if (token.sp1.lineOneIndexed == token.sp2.lineOneIndexed) {
          token.sp1.colOneIndexed > token.sp2.colOneIndexed
        } else token.sp1.lineOneIndexed > token.sp2.lineOneIndexed
      })
      .foreach(wrongPositionRange)
  }

  /** Checks I-Bounds. */
  private def checkBounds(src: Source, tokens: Array[Token]): Unit = {
    // Tokens end offsets are exclusive and offsets are zero-indexed.
    def outOfBounds(i: Int): Boolean = i < 0 || src.data.length < i

    tokens
      .iterator
      .filter(token => outOfBounds(token.start) || outOfBounds(token.end))
      .foreach(outOfBoundOffset)
  }

  /** Checks I-Offset. */
  private def checkOffsetOrder(tokens: Array[Token]): Unit = {
    tokens.iterator
      .zip(tokens.iterator.drop(1))
      .filter { case (one, two) => one.end > two.start }
      .foreach { case (one, two) => outOfOrderOffsets(one, two) }
  }

  /** Checks I-Pos. */
  private def checkPositionOrder(tokens: Array[Token]): Unit = {
    tokens.iterator
      .zip(tokens.iterator.drop(1))
      .filter { case (one, two) =>
        if (one.sp2.lineOneIndexed == two.sp1.lineOneIndexed) {
          // Token end positions are exclusive.
          one.sp2.colOneIndexed > two.sp1.colOneIndexed
        } else one.sp2.lineOneIndexed > two.sp1.lineOneIndexed
      }
      .foreach { case (one, two) => outOfOrderPositions(one, two) }
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
      s""">> Invalid offset range: ${found.start} - ${found.end}.
         |
         |${Formatter.NoFormatter.code(loc, s"here (${found.kind})")}
         |
         |""".stripMargin
    throw InternalCompilerException(msg, loc)
  }

  private def wrongPositionRange(found: Token): Nothing = {
    val loc = found.mkSourceLocation()
    val positionRangeString = s"${found.sp1.lineOneIndexed}:${found.sp1.colOneIndexed} - ${found.sp2.lineOneIndexed}:${found.sp2.colOneIndexed}"
    val msg =
      s""">> Invalid position range: $positionRangeString.
         |
         |${Formatter.NoFormatter.code(loc, s"here (${found.kind})")}
         |
         |""".stripMargin
    throw InternalCompilerException(msg, loc)
  }

  private def outOfBoundOffset(found: Token): Nothing = {
    val loc = found.mkSourceLocation()
    val msg =
      s""">> Token with out-of-bound offsets: ${found.start} - ${found.end}.
         |
         |${Formatter.NoFormatter.code(loc, s"here (${found.kind})")}
         |
         |""".stripMargin
    throw InternalCompilerException(msg, loc)
  }

  private def outOfOrderPositions(left: Token, right: Token): Nothing = {
    val loc = left.mkSourceLocation()
    val leftPos = s"${left.sp1.lineOneIndexed}:${left.sp1.colOneIndexed} - ${left.sp2.lineOneIndexed}:${left.sp2.colOneIndexed}"
    val rightPos = s"${right.sp1.lineOneIndexed}:${right.sp1.colOneIndexed} - ${right.sp2.lineOneIndexed}:${right.sp2.colOneIndexed}"
    val msg =
      s""">> Overlapping tokens (position): $leftPos and $rightPos.
         |
         |${Formatter.NoFormatter.code(left.mkSourceLocation(), s"left token here (${left.kind})")}
         |
         |${Formatter.NoFormatter.code(right.mkSourceLocation(), s"right token here (${right.kind})")}
         |
         |""".stripMargin
    throw InternalCompilerException(msg, loc)
  }

  private def outOfOrderOffsets(left: Token, right: Token): Nothing = {
    val loc = left.mkSourceLocation()
    val msg =
      s""">> Overlapping tokens: ${left.start} - ${right.end} and ${right.start} - ${right.end}.
         |
         |${Formatter.NoFormatter.code(left.mkSourceLocation(), s"left token here (${left.kind})")}
         |
         |${Formatter.NoFormatter.code(right.mkSourceLocation(), s"right token here (${right.kind})")}
         |
         |""".stripMargin
    throw InternalCompilerException(msg, loc)
  }

  private def emptyTokens(src: Source): Nothing = {
    val loc = SourceLocation.zeroPoint(isReal = true, SourcePosition.firstPosition(src))
    val msg = s"Found empty token array for ${src.name}."
    throw InternalCompilerException(msg, loc)
  }

}
