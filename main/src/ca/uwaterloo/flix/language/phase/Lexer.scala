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
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{Ast, TokenErrorKind, ReadAst, SourceKind, SourceLocation, Token, TokenKind}
import ca.uwaterloo.flix.language.errors.LexerError
import ca.uwaterloo.flix.util.{ParOps, Validation}
import ca.uwaterloo.flix.util.Validation._

import scala.collection.mutable

object Lexer {

  def run(root: ReadAst.Root)(implicit flix: Flix): Validation[Map[Ast.Source, Array[Token]], CompilationMessage] = {
    if (!flix.options.xparser) {
      // New lexer and parser disabled. Return immediately.
      return Map.empty[Ast.Source, Array[Token]].toSuccess
    }

    flix.phase("Lexer") {
      // Lex each source file in parallel.
      val results = ParOps.parMap(root.sources) {
        case (src, _) => mapN(lex(src))({
          case tokens => src -> tokens
        })
      }

      // Construct a map from each source to its tokens.
      mapN(sequence(results))(_.toMap)
    }

  }

  private def lex(src: Ast.Source): Validation[Array[Token], CompilationMessage] = {
    implicit val s: State = new State(src)
    // TODO: LEXER
    Validation.SoftFailure(s.tokens.toArray, LazyList.from(s.tokens).collect {
      case Token(TokenKind.Err(e), t, l, c) => tokenErrToCompilationMessage(e, t, l, c)
    })
  }

  private def advance()(implicit s: State): Char = ???

  private def isAtEnd()(implicit s: State): Boolean = ???

  private def scanToken()(implicit s: State): Unit = ???

  private def addToken(token: Token)(implicit s: State): Unit = ???

  /**
   * Converts a `Token` of kind `TokenKind.Err` into a CompilationMessage.
   * NOTE: Why is this necessary?
   * We would like the lexer to capture as many errors as possible before terminating.
   * To do this, the lexer will produce error tokens instead of halting,
   * each holding a kind of the simple type `ErrKind`.
   * So we need this mapping to produce a `CompilationMessage`, which is a case class, if there were any errors.
   */
  private def tokenErrToCompilationMessage(e: TokenErrorKind, t: String, l: Int, c: Int)(implicit s: State): CompilationMessage = {
    val o = e match {
      case TokenErrorKind.UnexpectedChar | TokenErrorKind.DoubleDottedNumber => t.length
      case TokenErrorKind.BlockCommentTooDeep => 2
      case _ => 1
    }

    val loc = SourceLocation(None, s.src, SourceKind.Real, l, c, l, c + o)
    e match {
      case TokenErrorKind.UnexpectedChar => LexerError.UnexpectedChar(t, loc)
      case TokenErrorKind.DoubleDottedNumber => LexerError.DoubleDottedNumber(loc)
      case TokenErrorKind.UnterminatedString => LexerError.UnterminatedString(loc)
      case TokenErrorKind.UnterminatedChar => LexerError.UnterminatedChar(loc)
      case TokenErrorKind.UnterminatedInfixFunction => LexerError.UnterminatedInfixFunction(loc)
      case TokenErrorKind.UnterminatedBuiltIn => LexerError.UnterminatedBuiltIn(loc)
      case TokenErrorKind.UnterminatedBlockComment => LexerError.UnterminatedBlockComment(loc)
      case TokenErrorKind.BlockCommentTooDeep => LexerError.BlockCommentTooDeep(loc)
    }
  }

  private class State(val src: Ast.Source) {
    var start: Int = 0
    var current: Int = 0
    var line: Int = 0
    var tokens: mutable.ListBuffer[Token] = mutable.ListBuffer.empty
  }
}
