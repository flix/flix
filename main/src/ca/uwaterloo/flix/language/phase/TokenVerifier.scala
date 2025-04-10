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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.Source
import ca.uwaterloo.flix.language.ast.{Token, TokenKind}
import ca.uwaterloo.flix.language.errors.TokenVerifierError
import ca.uwaterloo.flix.util.ParOps

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

  /** This limit avoids gigantic error lists. */
  private val errorLimitPerTypePerFile: Int = 2

  def run(m: Map[Source, Array[Token]])(implicit flix: Flix): (Map[Source, Array[Token]], List[TokenVerifierError]) = {
    if (flix.options.xverifylexer) {
      val errors = ParOps.parMap(m) {
        case (src, tokens) =>
          (checkEndOfFile(src, tokens).take(errorLimitPerTypePerFile)
            ++ checkSrc(src, tokens).take(errorLimitPerTypePerFile)
            ++ checkRanges(tokens).take(errorLimitPerTypePerFile)
            ++ checkBounds(src, tokens).take(errorLimitPerTypePerFile)
            ++ checkOffsetOrder(tokens).take(errorLimitPerTypePerFile)
            ++ checkPositionOrder(tokens).take(errorLimitPerTypePerFile)
            ).toList
      }.flatten.toList
      (m, errors)
    } else {
      (m, Nil)
    }
  }

  /** Checks I-Eef. */
  private def checkEndOfFile(src: Source, tokens: Array[Token]): Iterator[TokenVerifierError] = {
    tokens.lastOption match {
      case None => Iterator(TokenVerifierError.EmptyTokens(src))
      case Some(last) =>
        val errors1 = if (last.kind != TokenKind.Eof) Iterator(TokenVerifierError.MissingEof(last)) else Iterator.empty
        errors1 ++ tokens
          .iterator
          .zipWithIndex
          .filter { case (token, i) => token.kind == TokenKind.Eof && i != tokens.length - 1 }
          .map { case (token, _) => TokenVerifierError.UnexpectedEof(token) }
    }
  }

  /** Checks I-Src. */
  private def checkSrc(src: Source, tokens: Array[Token]): Iterator[TokenVerifierError] = {
    tokens
      .iterator
      .filter(_.src != src)
      .map(TokenVerifierError.WrongSource(src, _))
  }

  /** Checks I-Ranges. */
  private def checkRanges(tokens: Array[Token]): Iterator[TokenVerifierError] = {
    val errors1 = tokens
      .iterator
      .filter(token => token.start > token.end)
      .map(TokenVerifierError.WrongOffsetRange(_))
    val errors2 = tokens
      .iterator
      .filter(token => {
        if (token.sp1.lineOneIndexed == token.sp2.lineOneIndexed) {
          token.sp1.colOneIndexed > token.sp2.colOneIndexed
        } else token.sp1.lineOneIndexed > token.sp2.lineOneIndexed
      })
      .map(TokenVerifierError.WrongPositionRange(_))
    errors1 ++ errors2
  }

  /** Checks I-Bounds. */
  private def checkBounds(src: Source, tokens: Array[Token]): Iterator[TokenVerifierError] = {
    // Tokens end offsets are exclusive.
    def outOfBounds(i: Int): Boolean = i < 0 || src.data.length < i

    tokens
      .iterator
      .filter(token => outOfBounds(token.start) || outOfBounds(token.end))
      .map(TokenVerifierError.OutOfBoundOffset(_))
  }

  /** Checks I-Offset. */
  private def checkOffsetOrder(tokens: Array[Token]): Iterator[TokenVerifierError] = {
    tokens.iterator
      .zip(tokens.iterator.drop(1))
      .filter { case (one, two) => one.end > two.start }
      .map { case (one, two) => TokenVerifierError.OutOfOrderOffsets(one, two) }
  }

  /** Checks I-Pos. */
  private def checkPositionOrder(tokens: Array[Token]): Iterator[TokenVerifierError] = {
    tokens.iterator
      .zip(tokens.iterator.drop(1))
      .filter { case (one, two) =>
        if (one.sp2.lineOneIndexed == two.sp1.lineOneIndexed) {
          // Token end positions are exclusive.
          one.sp2.colOneIndexed > two.sp1.colOneIndexed
        } else one.sp2.lineOneIndexed > two.sp1.lineOneIndexed
      }
      .map { case (one, two) => TokenVerifierError.OutOfOrderPositions(one, two) }
  }

}
