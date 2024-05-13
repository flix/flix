/*
 * Copyright 2024 Magnus Madsen, Herluf Baggesen
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
import ca.uwaterloo.flix.language.ast.Ast.SyntacticContext
import ca.uwaterloo.flix.language.ast.{SourceLocation, TokenKind}
import ca.uwaterloo.flix.util.Formatter

/**
  * An error raised to indicate an unexpected token was found.
  *
  * @param expected Names of the tokens that are expected at the location. See [[TokenKind.display]].
  * @param actual   Name of the token that was actually found. See [[TokenKind.display]]
  * @param ctx      The syntactic context.
  * @param loc      The source location.
  * @param hint     Optional hint with more details about the error
  */
case class UnexpectedToken(expected: Seq[String], actual: String, ctx: SyntacticContext, loc: SourceLocation, hint: Option[String] = None) extends CompilationMessage with Recoverable {
  val kind = s"Parse Error ($ctx)"

  /**
    * Joins items nicely with comma separation ending with an "or".
    * For instance prettyJoin(List("def", "enum", "trait")) gives "def, enum or trait".
    */
  private def prettyJoin[T](items: Seq[T]): String = items match {
    case i1 :: i2 :: Nil => s"$i1 or $i2"
    case i1 :: Nil => s"$i1"
    case i :: tail => s"$i, ${prettyJoin(tail)}"
  }

  def summary: String = s"Expected ${prettyJoin(expected)} before $actual"

  def message(formatter: Formatter): String = {
    import formatter._
    val hintStr = hint.map(s"\nHint: " + _).getOrElse("")
    val expectedStr = prettyJoin(expected.map(cyan))
    s"""${line(kind, source.name)}
       |>> Expected $expectedStr before ${red(actual)}
       |
       |${code(loc, s"Here")}$hintStr
       |""".stripMargin
  }
}

/**
  * An generic parse error. We should prefer [[UnexpectedToken]] when possible.
  *
  * @param message Error message to be displayed.
  * @param ctx     The syntactic context.
  * @param loc     The source location.
  * @param hint    Optional hint with more details about the error
  */
case class ParseError(message: String, ctx: SyntacticContext, loc: SourceLocation, hint: Option[String] = None) extends CompilationMessage with Recoverable {
  val kind = s"Parse Error ($ctx)"

  def summary: String = message

  def message(formatter: Formatter): String = {
    val hintStr = hint.map(s"\nHint: " + _).getOrElse("")
    s"""${formatter.line(kind, source.name)}
       |>> $message
       |
       |${formatter.code(loc, s"Here")}$hintStr
       |""".stripMargin
  }
}
