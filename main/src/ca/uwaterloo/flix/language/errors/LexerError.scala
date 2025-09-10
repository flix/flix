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

import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.Formatter

sealed trait LexerError extends CompilationMessage {
  val kind: CompilationMessageKind = CompilationMessageKind.LexerError
}

object LexerError {

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
    * An error raised when a hexadecimal number suffix is unrecognized.
    */
  case class IncorrectHexNumberSuffix(loc: SourceLocation) extends LexerError {
    override def summary: String = s"Incorrect hexadecimal number suffix."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Incorrect hexadecimal number suffix.
         |
         |${code(loc, "Here")}
         |
         |Hexadecimal number suffixes are i8, i16, i32, i64, and ii.
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised when a number suffix is unrecognized.
    */
  case class IncorrectNumberSuffix(loc: SourceLocation) extends LexerError {
    override def summary: String = s"Incorrect number suffix."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Incorrect number suffix.
         |
         |${code(loc, "Here")}
         |
         |Number suffixes are i8, i16, i32, i64, ii, f32, f64, and ff.
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised when an integer suffix is put on a decimal number.
    */
  case class IntegerSuffixOnFloat(loc: SourceLocation) extends LexerError {
    override def summary: String = s"A decimal number cannot have integer suffix."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> A decimal number cannot have integer suffix.
         |
         |${code(loc, "Here")}
         |
         |Float suffixes are f32, f64, and ff.
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised when a hexadecimal number is malformed.
    */
  case class MalformedHexNumber(found: String, loc: SourceLocation) extends LexerError {
    override def summary: String = s"Malformed hexadecimal number, found '$found'."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Malformed hexadecimal number, found '$found'.
         |
         |${code(loc, "Number was correct up to here")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised when a number is malformed.
    */
  case class MalformedNumber(found: String, loc: SourceLocation) extends LexerError {
    override def summary: String = s"Malformed number, found '$found'."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Malformed number, found '$found'.
         |
         |${code(loc, "Number was correct up to here")}
         |
         |""".stripMargin
    }
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
    * An error raised when a digit is expected in a number (e.g. `1.` or `1.2e`).
    */
  case class ExpectedDigit(loc: SourceLocation) extends LexerError {
    override def summary: String = s"A digit (0-9) is expected here."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> A digit (0-9) is expected here.
         |
         |${code(loc, "Here")}
         |""".stripMargin
    }
  }

  /**
    * An error raised when a hexadecimal number is unterminated (e.g. `0x` or `0xff_`).
    */
  case class UnterminatedHexNumber(loc: SourceLocation) extends LexerError {
    override def summary: String = s"Unterminated Hexadecimal number."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unterminated Hexadecimal number.
         |
         |${code(loc, "Here")}
         |
         |""".stripMargin
    }
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
