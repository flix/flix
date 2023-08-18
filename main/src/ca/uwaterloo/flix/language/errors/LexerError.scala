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
package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.Formatter

sealed trait LexerError extends CompilationMessage {
  val kind = "Lexer Error"
}

object LexerError {
  
  /**
   * An error raised when an unexpected character, such as €, is encountered
   * @param char the problematic character
   * @param loc the location of `char`
   */
  case class UnexpectedChar(char: Char, loc: SourceLocation) extends LexerError {
      override def summary: String = s"Unexpected character '$char'"

      override def message(formatter: Formatter): String = {
        import formatter._
        s"""${line(kind, source.name)}
           |>> Unexpected character '${red(char.toString)}'.
           |
           |${code(loc, "found here")}
           |
           |""".stripMargin
      }

      override def explain(formatter: Formatter): Option[String] = None
    }

//    case object UnterminatedString extends LexerErr
//
//    case object UnterminatedChar extends LexerErr
//
//    case object UnterminatedInfixFunction extends LexerErr
//
//    case object DoubleDottedNumber extends LexerErr
//
//    case object MalformedNumber extends LexerErr
//
//    case object BlockCommentTooDeep extends LexerErr
//
//    case object UnterminatedBlockComment extends LexerErr
//
//    case object UnterminatedBuiltIn extends LexerErr

}

