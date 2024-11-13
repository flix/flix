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
    * An error raised when block-comments are nested too deep.
    *
    * @param loc The location of the opening "\*".
    */
  case class BlockCommentTooDeep(loc: SourceLocation) extends LexerError {
    override def summary: String = s"Block-comment nested too deep."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Block-comment nested too deep.
         |
         |${code(loc, "This is nested too deep.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
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
      import formatter.*
      s""">> Number has two decimal dots.
         |
         |${code(loc, "Second decimal dot is here.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised when more than one `e` (used for scientific notation) is found in a number.
    *
    * @param loc The location of the double e number literal.
    */
  case class DoubleEInNumber(loc: SourceLocation) extends LexerError {
    override def summary: String = s"Number has two scientific notation indicators."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Number has two scientific notation indicators.
         |
         |${code(loc, "Second 'e' is here.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised when a number contains a sequence of underscores.
    *
    * @param loc The location of the number literal.
    */
  case class DoubleUnderscoreInNumber(loc: SourceLocation) extends LexerError {
    override def summary: String = s"Number has sequence of '_'"

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Number has sequence of '_'.
         |
         |${code(loc, "Ending here")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised when a period has whitespace before it.
    * This is problematic because we want to disallow tokens like: "Rectangle   .Shape".
    *
    * @param loc The location of the '.'.
    */
  case class FreeDot(loc: SourceLocation) extends LexerError {
    override def summary: String = s"'.' has leading whitespace."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> '.' has leading whitespace.
         |
         |${code(loc, "here")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised when a number ends on an underscore.
    *
    * @param loc The location of the number literal.
    */
  case class TrailingUnderscoreInNumber(loc: SourceLocation) extends LexerError {
    override def summary: String = s"Number ends on a '_'."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Number ends on a '_'.
         |
         |${code(loc, "Here")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised when the digits of a hex literal starts with an '_'
    *
    * @param loc The location of the number literal.
    */
  case class HexLiteralStartsOnUnderscore(loc: SourceLocation) extends LexerError {
    override def summary: String = s"Hex literal starts on a '_'."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Hex literal starts on a '_'.
         |
         |${code(loc, "Here")}
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
      import formatter.*
      s""">> String interpolation nested too deep.
         |
         |${code(loc, "This is nested too deep.")}
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
      import formatter.*
      s""">> Unexpected character '${red(s)}'.
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
      import formatter.*
      s""">> Missing '*/' in block-comment.
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
      import formatter.*
      s""">> Missing '$$' in built-in.
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
      import formatter.*
      s""">> Missing `'` in char.
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
      import formatter.*
      s""">> Missing '`' in infix function.
         |
         |${code(loc, "Infix function starts here.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised when an unterminated regex is encountered.
    *
    * @param loc The location of the opening `"`.
    */
  case class UnterminatedRegex(loc: SourceLocation) extends LexerError {
    override def summary: String = s"Unterminated regex."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing `"` in regex.
         |
         |${code(loc, "Regex starts here")}
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
      import formatter.*
      s""">> Missing '"' in string.
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
      import formatter.*
      s""">> Missing '}' in string interpolation.
         |
         |${code(loc, "Interpolation starts here.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }
}
