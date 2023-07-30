/*
 * Copyright 2023 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.{Ast, ReadAst, Token}
import ca.uwaterloo.flix.util.{ParOps, Validation}
import ca.uwaterloo.flix.util.Validation._

import scala.collection.mutable

object Lexer {

  def run(root: ReadAst.Root)(implicit flix: Flix): Validation[Map[Ast.Source, Array[Token]], CompilationMessage] = {
    // Lex each source file in parallel.
    val results = ParOps.parMap(root.sources) {
      case (src, _) => mapN(lex(src))({
        case tokens => src -> tokens
      })
    }

    // Construct a map from each source to its tokens.
    mapN(sequence(results))(_.toMap)
  }

  private def lex(s: Ast.Source): Validation[Array[Token], CompilationMessage] = {
    // TODO: LEXER
    //    implicit val s = new State()
    //    while (!isAtEnd()) {
    //      s.start = s.current
    //      scanToken()
    //    }
    //    s.tokens += Token(TokenKind.EofToken, "<eof>")

    Array.empty[Token].toSuccess
  }

  private def advance()(implicit s: State): Char = ???

  private def isAtEnd()(implicit s: State): Boolean = ???

  private def scanToken()(implicit s: State): Unit = ???

  private def addToken(token: Token)(implicit s: State): Unit = ???

  private class State {
    var start: Int = 0
    var current: Int = 0
    var line: Int = 0
    var tokens: mutable.ListBuffer[Token] = mutable.ListBuffer.empty
  }

}
