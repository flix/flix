/*
 * Copyright 2024 Herluf Baggesen
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
import ca.uwaterloo.flix.language.ast.TokenKind

sealed trait Parser2Error extends CompilationMessage {
  val kind = "Parse Error"
}

object Parser2Error {
  /**
   * An error raised when an unexpected token is encountered.
   *
   * @param loc      The source location where the erroneous token was found.
   * @param expected The kind of token that was expected.
   */
  case class UnexpectedToken(loc: SourceLocation, expected: TokenKind) extends Parser2Error {
    override def summary: String = s"Expected $expected"

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Expected $expected
         |
         |${code(loc, s"Expected $expected here")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }
}
