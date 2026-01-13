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
    def code: ErrorCode = ErrorCode.E3736

    override def summary: String = "Expected digit (0-9)."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Expected digit (0-9).
         |
         |${src(loc, "expected digit")}
         |""".stripMargin
    }
  }

  /**
    * An error raised when a hexadecimal digit is expected in a number (e.g. `0x` or `0xFF_`).
    *
    * @param loc The location of the unexpected char.
    */
  case class ExpectedHexDigit(loc: SourceLocation) extends LexerError {
    def code: ErrorCode = ErrorCode.E3849

    override def summary: String = "Expected hexadecimal digit (0-9, a-f, A-F)."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Expected hexadecimal digit (0-9, a-f, A-F).
         |
         |${src(loc, "expected hexadecimal digit")}
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
    def code: ErrorCode = ErrorCode.E3952

    override def summary: String = "Unexpected whitespace before '.'."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected whitespace before '.'.
         |
         |${src(loc, "unexpected whitespace")}
         |""".stripMargin
    }
  }

  /**
    * An error raised when a hexadecimal number suffix is unrecognized.
    *
    * @param loc The location of the start of the suffix.
    */
  case class IncorrectHexNumberSuffix(loc: SourceLocation) extends LexerError {
    def code: ErrorCode = ErrorCode.E4063

    override def summary: String = "Unexpected hexadecimal number suffix."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected hexadecimal number suffix.
         |
         |${src(loc, "unexpected suffix")}
         |
         |${underline("Explanation:")} Hexadecimal literals require a valid type suffix.
         |
         |  Type      Suffix    Example
         |  Int8      i8        let x: Int8 = 0xFFi8
         |  Int16     i16       let x: Int16 = 0xFFi16
         |  Int32     i32       let x: Int32 = 0xFFi32
         |  Int64     i64       let x: Int64 = 0xFFi64
         |  BigInt    ii        let x: BigInt = 0xFFii
         |""".stripMargin
    }
  }

  /**
    * An error raised when a number suffix is unrecognized.
    *
    * @param loc The location of the start of the suffix.
    */
  case class IncorrectNumberSuffix(loc: SourceLocation) extends LexerError {
    def code: ErrorCode = ErrorCode.E4176

    override def summary: String = s"Incorrect number suffix."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Incorrect number suffix.
         |
         |${src(loc, "Here")}
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
    def code: ErrorCode = ErrorCode.E4289

    override def summary: String = s"A decimal number cannot have integer suffix."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> A decimal number cannot have integer suffix.
         |
         |${src(loc, "Here")}
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
    def code: ErrorCode = ErrorCode.E4392

    override def summary: String = s"Malformed hexadecimal number, found '${showChar(found)}'."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Malformed hexadecimal number, found '${showChar(found)}'.
         |
         |${src(loc, "Number was correct up to here")}
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
    def code: ErrorCode = ErrorCode.E4405

    override def summary: String = s"Malformed number, found '${showChar(found)}'."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Malformed number, found '${showChar(found)}'.
         |
         |${src(loc, "Number was correct up to here")}
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
    def code: ErrorCode = ErrorCode.E4518

    override def summary: String = s"Unexpected character '${showChar(found)}'."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected character '${red(showChar(found))}'.
         |
         |${src(loc, "Unexpected character.")}
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
    def code: ErrorCode = ErrorCode.E4629

    override def summary: String = s"Unterminated block-comment."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing '*/' in block-comment.
         |
         |${src(loc, "Here.")}
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
    def code: ErrorCode = ErrorCode.E4736

    override def summary: String = s"Unterminated built-in."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing '%%' in built-in.
         |
         |${src(loc, "Here.")}
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
    def code: ErrorCode = ErrorCode.E4843

    override def summary: String = s"Unterminated char."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing `'` in char.
         |
         |${src(loc, "Here")}
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
    def code: ErrorCode = ErrorCode.E4956

    override def summary: String = s"Unterminated regex."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing `"` in regex.
         |
         |${src(loc, "Here")}
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
    def code: ErrorCode = ErrorCode.E5067

    override def summary: String = s"Unterminated string."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing '"' in string.
         |
         |${src(loc, "Here.")}
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
    def code: ErrorCode = ErrorCode.E5178

    override def summary: String = s"Unterminated string interpolation."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing '}' in string interpolation.
         |
         |${src(loc, "Interpolation starts here.")}
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
