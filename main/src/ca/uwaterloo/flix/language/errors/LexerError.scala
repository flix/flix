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

import ca.uwaterloo.flix.language.ast.{SourceLocation, TypedAst}
import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.util.Formatter

sealed trait LexerError extends CompilationMessage {
  val kind: CompilationMessageKind = CompilationMessageKind.LexerError
}

object LexerError {

  /**
    * An error raised when a digit is expected in a number (e.g. `1.` or `1.2e`).
    *
    * @param loc The location where a digit is expected.
    */
  case class ExpectedDigit(loc: SourceLocation) extends LexerError {
    def code: ErrorCode = ErrorCode.E3736

    def summary: String = "Expected digit (0-9)."

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
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
    * @param loc The location where a hexadecimal digit is expected.
    */
  case class ExpectedHexDigit(loc: SourceLocation) extends LexerError {
    def code: ErrorCode = ErrorCode.E3849

    def summary: String = "Expected hexadecimal digit (0-9, a-f, A-F)."

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
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

    def summary: String = "Unexpected whitespace before '.'."

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
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

    def summary: String = "Unexpected hexadecimal number suffix."

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
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

    def summary: String = "Unexpected number suffix."

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      s""">> Unexpected number suffix.
         |
         |${src(loc, "unexpected suffix")}
         |
         |${underline("Explanation:")} Numeric literals require a valid type suffix.
         |
         |  Type        Suffix    Example
         |  Int8        i8        let x: Int8 = 123i8
         |  Int16       i16       let x: Int16 = 123i16
         |  Int32       i32       let x: Int32 = 123i32
         |  Int64       i64       let x: Int64 = 123i64
         |  BigInt      ii        let x: BigInt = 123ii
         |  Float32     f32       let x: Float32 = 1.0f32
         |  Float64     f64       let x: Float64 = 1.0f64
         |  BigDecimal  ff        let x: BigDecimal = 1.0ff
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

    def summary: String = "Unexpected integer suffix on decimal number."

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      s""">> Unexpected integer suffix on decimal number.
         |
         |${src(loc, "integer suffix not allowed here")}
         |
         |${underline("Explanation:")} Decimal numbers require a float suffix.
         |
         |  Type        Suffix    Example
         |  Float32     f32       let x: Float32 = 1.0f32
         |  Float64     f64       let x: Float64 = 1.0f64
         |  BigDecimal  ff        let x: BigDecimal = 1.0ff
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

    def summary: String = s"Malformed hexadecimal number: unexpected '${showChar(found)}'."

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      s""">> Malformed hexadecimal number: unexpected '${red(showChar(found))}'.
         |
         |${src(loc, "unexpected character")}
         |""".stripMargin
    }
  }

  /**
    * An error raised when a number is malformed.
    *
    * @param loc The location of `found`.
    */
  case class MalformedNumber(found: Char, loc: SourceLocation) extends LexerError {
    def code: ErrorCode = ErrorCode.E4405

    def summary: String = s"Malformed number: unexpected '${showChar(found)}'."

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      s""">> Malformed number: unexpected '${red(showChar(found))}'.
         |
         |${src(loc, "unexpected character")}
         |""".stripMargin
    }
  }

  /**
    * An error raised when an unexpected character, such as €, is encountered.
    *
    * @param loc The location of `found`.
    */
  case class UnexpectedChar(found: Char, loc: SourceLocation) extends LexerError {
    def code: ErrorCode = ErrorCode.E4518

    def summary: String = s"Unexpected character '${showChar(found)}'."

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      s""">> Unexpected character '${red(showChar(found))}'.
         |
         |${src(loc, "unexpected character")}
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

    def summary: String = "Unterminated block comment."

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      s""">> Unterminated block comment.
         |
         |${src(loc, "missing closing '*/'")}
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

    def summary: String = "Unterminated built-in."

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      s""">> Unterminated built-in.
         |
         |${src(loc, "missing closing '%%'")}
         |""".stripMargin
    }
  }

  /**
    * An error raised when an unterminated character literal is encountered.
    *
    * @param loc The location of the character literal.
    */
  case class UnterminatedChar(loc: SourceLocation) extends LexerError {
    def code: ErrorCode = ErrorCode.E4843

    def summary: String = "Unterminated character literal."

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      s""">> Unterminated character literal.
         |
         |${src(loc, "missing closing `'`")}
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

    def summary: String = "Unterminated regex literal."

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      s""">> Unterminated regex literal.
         |
         |${src(loc, "missing closing `\"`")}
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

    def summary: String = "Unterminated string literal."

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      s""">> Unterminated string literal.
         |
         |${src(loc, "missing closing `\"`")}
         |""".stripMargin
    }
  }

  /**
    * An error raised when an unterminated string interpolation is encountered.
    *
    * @param loc The location of the opening `{`.
    */
  case class UnterminatedStringInterpolation(loc: SourceLocation) extends LexerError {
    def code: ErrorCode = ErrorCode.E5178

    def summary: String = "Unterminated string interpolation."

    def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      s""">> Unterminated string interpolation.
         |
         |${src(loc, "missing closing `}`")}
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
