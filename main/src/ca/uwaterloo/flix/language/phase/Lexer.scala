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

    val hasErrors = s.tokens.exists(t => t.kind.isInstanceOf[TokenKind.Err])
    if (hasErrors) {
      // TODO: Fix type error in return type
      s.tokens.flatMap(tokenErrToCompilationMessage).toArray.toFailure
    } else {
      s.tokens.toArray.toSuccess
    }
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
  private def tokenErrToCompilationMessage(t: Token)(implicit s: State): Option[CompilationMessage] = {
    t.kind match {
      case TokenKind.Err(e) => e match {
        case TokenErrorKind.UnexpectedChar => Some(
          LexerError.UnexpectedChar(
            t.text,
            SourceLocation(None, s.src, SourceKind.Real, t.line, t.col, t.line, t.col + t.text.length)
          )
        )
        case TokenErrorKind.DoubleDottedNumber => Some(
          LexerError.DoubleDottedNumber(
            SourceLocation(None, s.src, SourceKind.Real, t.line, t.col, t.line, t.col + t.text.length)
          )
        )
        case TokenErrorKind.UnterminatedString => Some(
          LexerError.UnterminatedString(
            SourceLocation(None, s.src, SourceKind.Real, t.line, t.col, t.line, t.col + 1)
          )
        )
        case TokenErrorKind.UnterminatedChar => Some(
          LexerError.UnterminatedChar(
            SourceLocation(None, s.src, SourceKind.Real, t.line, t.col, t.line, t.col + 1)
          )
        )
        case TokenErrorKind.UnterminatedInfixFunction => Some(
          LexerError.UnterminatedInfixFunction(
            SourceLocation(None, s.src, SourceKind.Real, t.line, t.col, t.line, t.col + 1)
          )
        )
        case TokenErrorKind.UnterminatedBuiltIn => Some(
          LexerError.UnterminatedBuiltin(
            SourceLocation(None, s.src, SourceKind.Real, t.line, t.col, t.line, t.col + 1)
          )
        )
        case TokenErrorKind.UnterminatedBlockComment => Some(
          LexerError.UnterminatedBlockComment(
            SourceLocation(None, s.src, SourceKind.Real, t.line, t.col, t.line, t.col + 1)
          )
        )
        case TokenErrorKind.BlockCommentTooDeep => Some(
          LexerError.BlockCommentTooDeep(
            SourceLocation(None, s.src, SourceKind.Real, t.line, t.col, t.line, t.col + 2)
          )
        )
      }
      case _ => None
    }
  }

  private class State(val src: Ast.Source) {
    var start: Int = 0
    var current: Int = 0
    var line: Int = 0
    var tokens: mutable.ListBuffer[Token] = mutable.ListBuffer.empty
  }
}
