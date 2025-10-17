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

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.util.Formatter

sealed trait LexerError extends CompilationMessage {
  val kind: CompilationMessageKind = CompilationMessageKind.LexerError
}

object LexerError {

  /**
    * An error raised when a digit is expected in a number (e.g. `1.` or `1.2e`).
    *
    * @param loc The location of the unexpected char.
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
    * An error raised when a hexadecimal digit is expected in a number (e.g. `0x` or `0xFF_`).
    *
    * @param loc The location of the unexpected char.
    */
  case class ExpectedHexDigit(loc: SourceLocation) extends LexerError {
    override def summary: String = s"A hexadecimal digit (0-9, a-f, or A-F) is expected here."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> A hexadecimal digit (0-9, a-f, or A-F) is expected here.
         |
         |${code(loc, "Here")}
         |""".stripMargin
    }
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
         |${code(loc, "Here")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised when a hexadecimal number suffix is unrecognized.
    *
    * @param loc The location of the start of the suffix.
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
    *
    * @param loc The location of the start of the suffix.
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
    *
    * @param loc The location of the start of the suffix.
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
    *
    * @param loc The location of `found`.
    */
  case class MalformedHexNumber(found: Char, loc: SourceLocation) extends LexerError {
    override def summary: String = s"Malformed hexadecimal number, found '${showChar(found)}'."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Malformed hexadecimal number, found '${showChar(found)}'.
         |
         |${code(loc, "Number was correct up to here")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised when a number is malformed.
    *
    * @param loc the location of `found`.
    */
  case class MalformedNumber(found: Char, loc: SourceLocation) extends LexerError {
    override def summary: String = s"Malformed number, found '${showChar(found)}'."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Malformed number, found '${showChar(found)}'.
         |
         |${code(loc, "Number was correct up to here")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised when an unexpected character, such as €, is encountered.
    *
    * @param loc the location of `found`.
    */
  case class UnexpectedChar(found: Char, loc: SourceLocation) extends LexerError {
    override def summary: String = s"Unexpected character '${showChar(found)}'."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected character '${red(showChar(found))}'.
         |
         |${code(loc, "Unexpected character.")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised when an unterminated block comment is encountered.
    *
    * @param loc The location of the block comment.
    */
  case class UnterminatedBlockComment(loc: SourceLocation) extends LexerError {
    override def summary: String = s"Unterminated block-comment."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing '*/' in block-comment.
         |
         |${code(loc, "Here.")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised when an unterminated built-in function is encountered.
    *
    * @param loc The location of the built-in.
    */
  case class UnterminatedBuiltIn(loc: SourceLocation) extends LexerError {
    override def summary: String = s"Unterminated built-in."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing '%%' in built-in.
         |
         |${code(loc, "Here.")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised when an unterminated char is encountered.
    *
    * @param loc The location of the char.
    */
  case class UnterminatedChar(loc: SourceLocation) extends LexerError {
    override def summary: String = s"Unterminated char."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing `'` in char.
         |
         |${code(loc, "Here")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised when an unterminated infix function is encountered.
    *
    * @param loc The location of the infix function.
    */
  case class UnterminatedInfixFunction(loc: SourceLocation) extends LexerError {
    override def summary: String = s"Unterminated infix function."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing '`' in infix function.
         |
         |${code(loc, "Here.")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised when an unterminated regex is encountered.
    *
    * @param loc The location of the regex.
    */
  case class UnterminatedRegex(loc: SourceLocation) extends LexerError {
    override def summary: String = s"Unterminated regex."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing `"` in regex.
         |
         |${code(loc, "Here")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised when an unterminated string is encountered.
    *
    * @param loc The location of the string.
    */
  case class UnterminatedString(loc: SourceLocation) extends LexerError {
    override def summary: String = s"Unterminated string."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing '"' in string.
         |
         |${code(loc, "Here.")}
         |
         |""".stripMargin
    }
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
  }

  /** Returns an ASCII printable version of `c`. */
  private def showChar(c: Char): String = {
    c match {
      case '\r' => "\\r"
      case '\n' => "\\n"
      case '\t' => "\\t"
      case _ if 32 <= c.toInt && c.toInt <= 126 => c.toString
      case _ => s"\\u${c.toHexString.toUpperCase}"
    }
  }

}
