package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.LexerError
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestLexer extends AnyFunSuite with TestUtils {

  test("LexerError.ExpectedDigit.01") {
    val input = "12..3"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.02") {
    val input = "123.."
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.03") {
    val input = "123..32f32"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.04") {
    val input = "12332..f32"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.05") {
    val input =
      s"""
         |def f(): Int = 1_
         """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.06") {
    val input =
      s"""
         |def f(): Int = 1_000_
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.07") {
    val input =
      s"""
         |def f(): Int8 = 1_i8
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.08") {
    val input =
      s"""
         |def f(): Int8 = 1_000_i8
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.09") {
    val input =
      s"""
         |def f(): Int16 = 1_i16
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.10") {
    val input =
      s"""
         |def f(): Int16 = 1_000_i16
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.11") {
    val input =
      s"""
         |def f(): Int32 = 1_i32
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.12") {
    val input =
      s"""
         |def f(): Int32 = 1_000_i32
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.13") {
    val input =
      s"""
         |def f(): Int64 = 1_i64
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.14") {
    val input =
      s"""
         |def f(): Int64 = 1_000_i64
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.15") {
    val input =
      s"""
         |def f(): BigInt = 1_ii
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.16") {
    val input =
      s"""
         |def f(): BigInt = 1_000_ii
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.17") {
    val input =
      s"""
         |def f(): Float = 1_.0
         """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.18") {
    val input =
      s"""
         |def f(): Float = 1_000_.0
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.19") {
    val input =
      s"""
         |def f(): Float = 1.0_
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.20") {
    val input =
      s"""
         |def f(): Float32 = 1_.0f32
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.21") {
    val input =
      s"""
         |def f(): Float32 = 1_000_.0f32
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.22") {
    val input =
      s"""
         |def f(): Float32 = 1.0_f32
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.23") {
    val input =
      s"""
         |def f(): Float64 = 1_.0f64
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.24") {
    val input =
      s"""
         |def f(): Float64 = 1_000_.0f64
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.25") {
    val input =
      s"""
         |def f(): Float64 = 1.0_f64
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.26") {
    val input =
      s"""
         |def f(): BigDecimal = 1_.0ff
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.27") {
    val input =
      s"""
         |def f(): BigDecimal = 1_000_.0ff
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.28") {
    val input =
      s"""
         |def f(): Float64 = 1.0_ff
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.IncorrectHexNumberSuffix.01") {
    val input =
      s"""
         |def f(): Int32 = 0xFFi32f
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.IncorrectHexNumberSuffix](result)
  }

  test("LexerError.IncorrectHexNumberSuffix.02") {
    val input =
      s"""
         |def f(): Int32 = 0xFFi322
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.IncorrectHexNumberSuffix](result)
  }

  test("LexerError.IncorrectHexNumberSuffix.03") {
    val input =
      s"""
         |def f(): Int32 = 0xFFi32FFi32
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.IncorrectHexNumberSuffix](result)
  }

  test("LexerError.IncorrectNumberSuffix.01") {
    val input = "1_2i3"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.IncorrectNumberSuffix](result)
  }

  test("LexerError.IncorrectNumberSuffix.02") {
    val input = "3.1_2e-23f223"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.IncorrectNumberSuffix](result)
  }

  test("LexerError.IntegerSuffixOnFloat.01") {
    val input = "1_000.00_01i32"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.IntegerSuffixOnFloat](result)
  }

  test("LexerError.IntegerSuffixOnFloat.02") {
    val input = "1e32i32"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.IntegerSuffixOnFloat](result)
  }

  test("LexerError.MalformedHexNumber.01") {
    val input = "0xabcdefg"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.MalformedHexNumber](result)
  }

  test("LexerError.MalformedHexNumber.02") {
    val input = "0xAx0"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.MalformedHexNumber](result)
  }

  test("LexerError.MalformedHexNumber.03") {
    val input = "0x3.14"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.MalformedHexNumber](result)
  }

  test("LexerError.MalformedNumber.01") {
    val input = "1.2.3"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.MalformedNumber](result)
  }

  test("LexerError.MalformedNumber.02") {
    val input = "1.2e1e2"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.MalformedNumber](result)
  }

  test("LexerError.MalformedNumber.03") {
    val input = "1e32A"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.MalformedNumber](result)
  }

  test("LexerError.MalformedNumber.04") {
    val input = "1x"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.MalformedNumber](result)
  }

  test("LexerError.UnexpectedChar.01") {
    val input = "€"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnexpectedChar](result)
  }

  test("LexerError.UnexpectedChar.02") {
    val input = "⟂"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnexpectedChar](result)
  }

  test("LexerError.UnexpectedChar.03") {
    // Unicode hex U+2189, just below valid math unicode char
    val input = "↉"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnexpectedChar](result)
  }

  test("LexerError.UnexpectedChar.04") {
    // Unicode hex U+2300, just above valid math unicode char
    val input = "⌀"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnexpectedChar](result)
  }

  test("LexerError.UnterminatedBlockComment.01") {
    val input =
      s"""
         |/* This is unterminated
         |def f(): Unit = ()
       """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBlockComment](result)
  }

  test("LexerError.UnterminatedBlockComment.02") {
    val input = "def f(): /* This is unterminated Unit = ()"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBlockComment](result)
  }

  test("LexerError.UnterminatedBlockComment.03") {
    val input =
      s"""
         |def f(): Unit = ()
         |/* This is unterminated""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBlockComment](result)
  }

  test("LexerError.UnterminatedBlockComment.04") {
    val input =
      s"""
         | /* /* This is unterminated and nested */
         |def f(): Unit = ()
       """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBlockComment](result)
  }

  test("LexerError.UnterminatedBuiltIn.01") {
    val input = "%%BUILT_IN; 42"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBuiltIn](result)
  }

  test("LexerError.UnterminatedBuiltIn.02") {
    val input = "%%BUILT_IN"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBuiltIn](result)
  }

  test("LexerError.UnterminatedBuiltIn.03") {
    val input = "%%BUILT_/*IN*/%%"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBuiltIn](result)
  }

  test("LexerError.UnterminatedBuiltIn.04") {
    val input = "%%BUILT_IN"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBuiltIn](result)
  }

  test("LexerError.UnterminatedChar.01") {
    val input = "'a"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedChar](result)
  }

  test("LexerError.UnterminatedChar.02") {
    val input = "'\uffff"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedChar](result)
  }

  test("LexerError.UnterminatedChar.03") {
    val input = "'/* This is a block-comment */a"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedChar](result)
  }

  test("LexerError.UnterminatedChar.04") {
    val input =
      """def foo(): Char = '"""
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedChar](result)
  }

  test("LexerError.ExpectedHexDigit.01") {
    val input =
      s"""
         |def f(): Int32 = 0x_1
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedHexDigit](result)
  }

  test("LexerError.ExpectedHexDigit.02") {
    val input =
      s"""
         |def f(): Int32 = 0xF__Fi32f
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedHexDigit](result)
  }

  test("LexerError.ExpectedHexDigit.03") {
    val input =
      s"""
         |def f(): Int8 = 0x_1i8
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedHexDigit](result)
  }

  test("LexerError.ExpectedHexDigit.04") {
    val input =
      s"""
         |def f(): Int8 = 0x1_i8
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedHexDigit](result)
  }

  test("LexerError.ExpectedHexDigit.05") {
    val input =
      s"""
         |def f(): Int16 = 0x_1i16
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedHexDigit](result)
  }

  test("LexerError.ExpectedHexDigit.06") {
    val input =
      s"""
         |def f(): Int16 = 0x1_i16
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedHexDigit](result)
  }

  test("LexerError.ExpectedHexDigit.07") {
    val input =
      s"""
         |def f(): Int32 = 0x_1i32
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedHexDigit](result)
  }

  test("LexerError.ExpectedHexDigit.08") {
    val input =
      s"""
         |def f(): Int32 = 0x1_i32
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedHexDigit](result)
  }

  test("LexerError.ExpectedHexDigit.09") {
    val input =
      s"""
         |def f(): Int64 = 0x_1i64
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedHexDigit](result)
  }

  test("LexerError.ExpectedHexDigit.10") {
    val input =
      s"""
         |def f(): Int64 = 0x1_i64
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedHexDigit](result)
  }

  test("LexerError.ExpectedHexDigit.11") {
    val input =
      s"""
         |def f(): BigInt = 0x_1ii
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedHexDigit](result)
  }

  test("LexerError.ExpectedHexDigit.12") {
    val input =
      s"""
         |def f(): BigInt = 0x1_ii
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedHexDigit](result)
  }

  test("LexerError.ExpectedHexDigit.13") {
    val input =
      s"""
         |def f(): Int32 = 0x
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedHexDigit](result)
  }

  test("LexerError.ExpectedHexDigit.14") {
    val input =
      s"""
         |def f(): Int32 = 0xF_
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedHexDigit](result)
  }

  test("LexerError.ExpectedHexDigit.15") {
    val input =
      s"""
         |def f(): Int32 = 0x1_
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedHexDigit](result)
  }

  test("LexerError.ExpectedHexDigit.16") {
    val input =
      s"""
         |def f(): Int32 = 0x1__2
           """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedHexDigit](result)
  }

  test("LexerError.UnterminatedRegex.01") {
    val input = """ regex" """
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedRegex](result)
  }

  test("LexerError.UnterminatedRegex.02") {
    val input = """ regex"\" """
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedRegex](result)
  }

  test("LexerError.UnterminatedRegex.03") {
    val input = """ regex" regex\" """
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedRegex](result)
  }

  test("LexerError.UnterminatedString.01") {
    val input = """ "This is unterminated """
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedString](result)
  }

  test("LexerError.UnterminatedString.02") {
    val input = """ "\ """
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedString](result)
  }

  test("LexerError.UnterminatedString.03") {
    val input = "\""
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedString](result)
  }

  test("LexerError.UnterminatedString.04") {
    val input = """ regex" regex"" """
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedString](result)
  }

  test("LexerError.UnterminatedString.05") {
    // Actually not related to the error but asserts that the error
    // is not a false positive
    val input = """ def f(): String = "This is terminated" """
    val result = check(input, Options.TestWithLibNix)
    expectSuccessOnCheck(result)
  }

  test("LexerError.UnterminatedString.06") {
    val input = """"${x}""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedString](result)
  }

  test("LexerError.UnterminatedString.07") {
    val input =
      s"""
         |def f(): String = "This is a
         |multi-line string"
         |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedString](result)
  }

  test("LexerError.UnterminatedString.08") {
    val input = """def foo(): String = """"
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedString](result)
  }

  test("LexerError.UnterminatedString.09") {
    val input =
      """def foo (): String = "\"""
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedString](result)
  }

  test("LexerError.UnterminatedString.10") {
    val input = """def foo (): Char = "\"""
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedString](result)
  }

  test("LexerError.UnterminatedStringInterpolation.01") {
    val input = """ "Hi ${name!" """
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedStringInterpolation](result)
  }

  test("LexerError.UnterminatedStringInterpolation.02") {
    val input = """ "${"Hi ${name!"}" """
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedStringInterpolation](result)
  }

  test("LexerError.UnterminatedStringInterpolation.03") {
    val input = """ "${"Hi ${name!}"" """
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedStringInterpolation](result)
  }

  test("LexerError.UnterminatedStringInterpolation.04") {
    // Note: The innermost interpolation is unterminated,
    // but the lexer should stop after bottoming out so this should still be a 'too deep' error.
    val input = """ "${"${"${"${"${"${"${"${"${"${"${"${"${"${"${${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${unterminated"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}}"}"}"}"}"}"}"}"}"}"}"}"}"}"}" """
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedStringInterpolation](result)
  }

  test("LexerError.UnterminatedStringInterpolation.05") {
    // Note: The innermost interpolation is unterminated,
    // but the lexer should stop after bottoming out so this should still be a 'too deep' error.
    val input = """ "${"${"${"${"${"${"${"${"${"${"${"${"${"${"${${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${unclosed and deep"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}}"}"}"}"}"}"}"}"}"}"}"}"}"}"}" """
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedStringInterpolation](result)
  }

}
