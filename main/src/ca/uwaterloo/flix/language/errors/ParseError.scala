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
import ca.uwaterloo.flix.util.{Formatter, InternalCompilerException}

/**
  * A common super-type for parser errors.
  */
sealed trait ParseError extends CompilationMessage {
  val kind = "Parse Error"
  val context: SyntacticContext = SyntacticContext.Unknown
}

object ParseError {

  /**
    * Joins items nicely with comma separation ending with an "or".
    * For instance prettyJoin(List("def", "enum", "trait")) gives "def, enum or trait".
    */
  private def prettyJoin[T](items: Seq[T]): String = items match {
    case i1 :: i2 :: Nil => s"$i1 or $i2"
    case i1 :: Nil => s"$i1"
    case i :: tail => s"$i, ${prettyJoin(tail)}"
  }

  /**
    * An error raised to indicate an unexpected token was found.
    *
    * @param expected Names of the tokens that are expected at the location. See [[TokenKind.display]].
    * @param actual   Name of the token that was actually found. See [[TokenKind.display]].
    * @param ctx      The syntactic context.
    * @param loc      The source location.
    * @param hint     Optional hint with more details about the error
    */
  case class UnexpectedToken(expected: Seq[String], actual: Option[String], ctx: SyntacticContext, loc: SourceLocation, hint: Option[String] = None) extends ParseError with Recoverable {
    override val kind = s"Parse Error ($ctx)"
    override val context: SyntacticContext = ctx

    def summary: String = {
      val expectedStr = s"Expected ${prettyJoin(expected)}"
      val actualStr = actual.map(a => s" before $a").getOrElse("")
      s"$expectedStr$actualStr."
    }

    def message(formatter: Formatter): String = {
      import formatter._
      val hintStr = hint.map(s"\nHint: " + _).getOrElse("")
      val expectedStr = s"Expected ${prettyJoin(expected.map(cyan))}"
      val actualStr = actual.map(a => s" before ${red(a)}").getOrElse("")
      s"""${line(kind, source.name)}
         |>> $expectedStr$actualStr.
         |
         |${code(loc, s"Here")}$hintStr
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that no items were found in a context where one or more is needed.
    *
    * @param expected Names of the items that are expected at least one of. See [[TokenKind.display]].
    * @param ctx      The syntactic context.
    * @param loc      The source location.
    * @param hint     Optional hint with more details about the error
    */
  case class NeedAtleastOne(expected: Seq[String], ctx: SyntacticContext, loc: SourceLocation, hint: Option[String] = None) extends ParseError with Recoverable {
    override val kind = s"Parse Error ($ctx)"
    override val context: SyntacticContext = ctx

    def summary: String = s"Expected at least one ${prettyJoin(expected)}."

    def message(formatter: Formatter): String = {
      import formatter._
      val hintStr = hint.map(s"\nHint: " + _).getOrElse("")
      s"""${line(kind, source.name)}
         |>> Expected at least one ${prettyJoin(expected.map(cyan))}.
         |
         |${code(loc, s"Here")}$hintStr
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a trailing separator is present.
    *
    * @param separator Name of the separator that is trailing. See [[TokenKind.display]].
    * @param ctx       The syntactic context.
    * @param loc       The source location.
    */
  case class TrailingSeparator(separator: String, ctx: SyntacticContext, loc: SourceLocation) extends ParseError with Recoverable {
    override val kind = s"Parse Error ($ctx)"
    override val context: SyntacticContext = ctx

    def summary: String = s"Trailing $separator."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Trailing ${red(separator)}.
         |
         |${code(loc, s"Here")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a scoped expression is missing a scope.
    *
    * @param name Name of the expreesion missing a scope. See [[TokenKind.display]].
    * @param ctx  The syntactic context.
    * @param loc  The source location.
    */
  case class MissingScope(name: String, ctx: SyntacticContext, loc: SourceLocation) extends ParseError with Recoverable {
    override val kind = s"Parse Error ($ctx)"
    override val context: SyntacticContext = ctx

    def summary: String = s"Expected scope on $name."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Expected ${red("scope")} on ${cyan(name)}.
         |
         |${code(loc, s"Here")}
         |Hint: Add a scope using `@ <scope>`
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that one or more doc-comments are misplaced.
    *
    * @param ctx  The syntactic context.
    * @param loc  The source location.
    */
  case class MisplacedDocComments(ctx: SyntacticContext, loc: SourceLocation) extends ParseError with Recoverable {
    override val kind = s"Parse Error ($ctx)"
    override val context: SyntacticContext = ctx

    def summary: String = s"Misplaced doc-comment(s)."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Misplaced doc-comment(s).
         |
         |${code(loc, s"Here")}
         |Hint: doc-comments must annotate declarations.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that one or more sequential comments are misplaced.
    *
    * @param ctx  The syntactic context.
    * @param loc  The source location.
    */
  case class MisplacedComments(ctx: SyntacticContext, loc: SourceLocation) extends ParseError with Recoverable {
    override val kind = s"Parse Error ($ctx)"
    override val context: SyntacticContext = ctx

    def summary: String = s"Misplaced comment(s)."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Misplaced comment(s).
         |
         |${code(loc, s"Here")}
         |Hint: Place comments on their own line.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that something was syntactically malformed.
    * This error is very generic, and should be avoided in favor of other ParserErrors if possible.
    *
    * @param name Name of the malformed item. See [[TokenKind.display]].
    * @param ctx  The syntactic context.
    * @param loc  The source location.
    * @param hint Optional hint with more details about the error.
    */
  case class Malformed(name: String, ctx: SyntacticContext, loc: SourceLocation, hint: Option[String] = None) extends ParseError with Recoverable {
    override val kind = s"Parse Error ($ctx)"
    override val context: SyntacticContext = ctx

    def summary: String = s"Malformed $name."

    def message(formatter: Formatter): String = {
      import formatter._
      val hintStr = hint.map(s"\nHint: " + _).getOrElse("")
      s"""${line(kind, source.name)}
         |>> Malformed ${red(name)}.
         |
         |${code(loc, s"Here")}$hintStr
         |""".stripMargin
    }
  }

  /**
    * An __internal__ error indicating that a Mark.Opened did not have a corresponding Mark.Closed when parsing ended.
    * This is used when opening new parser marks and should get overwritten with TreeKind or actual errors as parsing happens.
    * If a UnclosedParserMark is left when the syntax-tree is being built, that is an internal compiler error.
    * As such, any attempt to display the error also throws an internal compiler exception.
    */
  case class UnclosedParserMark(loc: SourceLocation) extends ParseError with Recoverable {
    def summary: String = throw InternalCompilerException(s"Unclosed parser mark at $loc.", loc)
    def message(formatter: Formatter): String = throw InternalCompilerException(s"Unclosed parser mark at $loc.", loc)
  }

  /**
    * A __legacy__ error used to support the previous parser.
    * TODO: Remove this with the previous parser.
    */
  case class Legacy(message: String, ctx: SyntacticContext, loc: SourceLocation) extends ParseError with Recoverable {
    override val kind = s"Parse Error ($ctx)"
    override val context: SyntacticContext = ctx

    def summary: String = message

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> $message
         |
         |${code(loc, s"Here")}
         |""".stripMargin
    }
  }
}

