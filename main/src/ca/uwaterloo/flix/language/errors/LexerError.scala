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
import ca.uwaterloo.flix.language.phase.Lexer.{InterpolatedStringMaxNestingLevel, BlockCommentMaxNestingLevel}

sealed trait LexerError extends CompilationMessage {
  val kind = "Lexer Error"
}

object LexerError {
  /**
   * An error raised when block-comments are nested too deep.
   *
   * @param loc The location of the opening "\*".
   */
  case class BlockCommentTooDeep(loc: SourceLocation) extends LexerError {
    override def summary: String = s"Block-comment nested too deep."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Block-comment nested too deep.
         |
         |${code(loc, "Block-comment starts here.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Ensure that block-comments are not nested more than $BlockCommentMaxNestingLevel levels deep."
    })
  }

  /**
   * An error raised when more than one decimal dot is found in a number.
   * For instance `123.456.78f32`.
   *
   * @param loc The location of the double dotted number literal.
   */
  case class DoubleDottedNumber(loc: SourceLocation) extends LexerError {
    override def summary: String = s"Number has two decimal dots."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Number has two decimal dots.
         |
         |${code(loc, "Number found here.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
   * An error raised when an unexpected character, such as €, is encountered.
   *
   * @param s   the problematic character.
   * @param loc the location of char.
   */
  case class UnexpectedChar(s: String, loc: SourceLocation) extends LexerError {
    override def summary: String = s"Unexpected character '$s'."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unexpected character '${red(s)}'.
         |
         |${code(loc, "Unexpected character.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
   * An error raised when an unterminated block comment is encountered.
   *
   * @param loc The location of the opening "/ *".
   */
  case class UnterminatedBlockComment(loc: SourceLocation) extends LexerError {
    override def summary: String = s"Unterminated block-comment."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unterminated block-comment.
         |
         |${code(loc, "Block-comment starts here.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
   * An error raised when an unterminated built-in function is encountered.
   *
   * @param loc The location of the opening "$".
   */
  case class UnterminatedBuiltIn(loc: SourceLocation) extends LexerError {
    override def summary: String = s"Unterminated built-in."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unterminated built-in.
         |
         |${code(loc, "Built-in starts here.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
   * An error raised when an unterminated char is encountered.
   *
   * @param loc The location of the opening `'`.
   */
  case class UnterminatedChar(loc: SourceLocation) extends LexerError {
    override def summary: String = s"Unterminated char."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unterminated char.
         |
         |${code(loc, "Char starts here")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
   * An error raised when an unterminated infix function is encountered.
   *
   * @param loc The location of the opening '&#96;'.
   */
  case class UnterminatedInfixFunction(loc: SourceLocation) extends LexerError {
    override def summary: String = s"Unterminated infix function."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unterminated infix function.
         |
         |${code(loc, "Infix function starts here.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
   * An error raised when an unterminated string is encountered.
   *
   * @param loc The location of the opening `"`.
   */
  case class UnterminatedString(loc: SourceLocation) extends LexerError {
    override def summary: String = s"Unterminated string."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unterminated string.
         |
         |${code(loc, "String starts here.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
   * An error raised when an unterminated string is encountered.
   *
   * @param loc The location of the opening `{`.
   */
  case class UnterminatedStringInterpolation(loc: SourceLocation) extends LexerError {
    override def summary: String = s"Unterminated string interpolation."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unterminated string interpolation.
         |
         |${code(loc, "Interpolation starts here.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
   * An error raised when block-comments are nested too deep.
   *
   * @param loc The location of the opening "${".
   */
  case class StringInterpolationTooDeep(loc: SourceLocation) extends LexerError {
    override def summary: String = s"String interpolation nested too deep."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> String interpolation  nested too deep.
         |
         |${code(loc, "Interpolation starts here.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Ensure that string interpolations are not nested more than $InterpolatedStringMaxNestingLevel levels deep."
    })
  }
}git 
