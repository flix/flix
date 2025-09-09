package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.LexerError
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestLexer extends AnyFunSuite with TestUtils {

  test("LexerError.MalformedNumber.01") {
    val input = "1.2.3"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.MalformedNumber](result)
  }

  test("LexerError.MalformedNumber.02") {
    val input = "1.2e1e2"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.MalformedNumber](result)
  }

  test("LexerError.MalformedNumber.03") {
    val input = "1e32A"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.MalformedNumber](result)
  }

  test("LexerError.MalformedNumber.04") {
    val input = "1x"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.MalformedNumber](result)
  }

  test("LexerError.IncorrectNumberSuffix.01") {
    val input = "1_2i3"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.IncorrectNumberSuffix](result)
  }

  test("LexerError.IncorrectNumberSuffix.02") {
    val input = "3.1_2e-23f223"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.IncorrectNumberSuffix](result)
  }

  test("LexerError.IntegerSuffixOnFloat.01") {
    val input = "1_000.00_01i32"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.IntegerSuffixOnFloat](result)
  }

  test("LexerError.IntegerSuffixOnFloat.02") {
    val input = "1e32i32"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.IntegerSuffixOnFloat](result)
  }

  test("LexerError.ExpectedDigit.01") {
    val input = "12..3"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.02") {
    val input = "123.."
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.03") {
    val input = "123..32f32"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.ExpectedDigit.04") {
    val input = "12332..f32"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.ExpectedDigit](result)
  }

  test("LexerError.StringInterpolationTooDeep.01") {
    val input = """ "${"${"${"${"${"${"${"${"${"${"${"${"${"${"${${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}}"}"}"}"}"}"}"}"}"}"}"}"}"}"}" """
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.StringInterpolationTooDeep](result)
  }

  test("LexerError.StringInterpolationTooDeep.02") {
    // Note: The innermost interpolation is unterminated,
    // but the lexer should stop after bottoming out so this should still be a 'too deep' error.
    val input = """ "${"${"${"${"${"${"${"${"${"${"${"${"${"${"${${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${unterminated"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}}"}"}"}"}"}"}"}"}"}"}"}"}"}"}" """
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.StringInterpolationTooDeep](result)
  }

  test("LexerError.StringInterpolationTooDeep.03") {
    // Note: The innermost interpolation is unterminated,
    // but the lexer should stop after bottoming out so this should still be a 'too deep' error.
    val input = """ "${"${"${"${"${"${"${"${"${"${"${"${"${"${"${${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${unclosed and deep"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}}"}"}"}"}"}"}"}"}"}"}"}"}"}"}" """
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.StringInterpolationTooDeep](result)
  }

  ignore("LexerError.UnexpectedChar.01") {
    val input = "€"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnexpectedChar](result)
  }

  test("LexerError.UnexpectedChar.02") {
    val input = "⟂"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnexpectedChar](result)
  }

  test("LexerError.UnexpectedChar.03") {
    // Unicode hex U+2189, just below valid math unicode char
    val input = "↉"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnexpectedChar](result)
  }

  test("LexerError.UnexpectedChar.04") {
    // Unicode hex U+2300, just above valid math unicode char
    val input = "⌀"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnexpectedChar](result)
  }

  test("LexerError.UnterminatedBlockComment.01") {
    val input =
      s"""
         |/* This is unterminated
         |def f(): Unit = ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBlockComment](result)
  }

  test("LexerError.UnterminatedBlockComment.02") {
    val input = "def f(): /* This is unterminated Unit = ()"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBlockComment](result)
  }

  test("LexerError.UnterminatedBlockComment.03") {
    val input =
      s"""
         |def f(): Unit = ()
         |/* This is unterminated""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBlockComment](result)
  }

  test("LexerError.UnterminatedBlockComment.04") {
    val input =
      s"""
         | /* /* This is unterminated and nested */
         |def f(): Unit = ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBlockComment](result)
  }

  test("LexerError.UnterminatedBuiltIn.01") {
    val input = "$BUILT_IN; 42"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBuiltIn](result)
  }

  test("LexerError.UnterminatedBuiltIn.02") {
    val input = "$BUILT_IN"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBuiltIn](result)
  }

  test("LexerError.UnterminatedBuiltIn.03") {
    val input = "$BUILT_/*IN*/$"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBuiltIn](result)
  }

  test("LexerError.UnterminatedBuiltIn.04") {
    val input = "$BUILT_IN"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBuiltIn](result)
  }

  test("LexerError.UnterminatedChar.01") {
    val input = "'a"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedChar](result)
  }

  test("LexerError.UnterminatedChar.02") {
    val input = "'\uffff"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedChar](result)
  }

  test("LexerError.UnterminatedChar.03") {
    val input = "'a/* This is a block-comment */'"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedChar](result)
  }

  test("LexerError.UnterminatedChar.04") {
    val input = "'/* This is a block-comment */a"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedChar](result)
  }

  test("LexerError.UnterminatedInfixFunction.01") {
    val input = "1 `add 2"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedInfixFunction](result)
  }

  test("LexerError.UnterminatedInfixFunction.02") {
    val input = "1 `add/*this is a block comment*/` 2"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedInfixFunction](result)
  }

  test("LexerError.UnterminatedInfixFunction.03") {
    val input = "1 `add 2"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedInfixFunction](result)
  }

  test("LexerError.UnterminatedRegex.01") {
    val input = """ regex" """
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedRegex](result)
  }

  test("LexerError.UnterminatedRegex.02") {
    val input = """ regex"\" """
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedRegex](result)
  }

  test("LexerError.UnterminatedRegex.03") {
    val input = """ regex" regex\" """
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedRegex](result)
  }

  test("LexerError.UnterminatedString.01") {
    val input = """ "This is unterminated """
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedString](result)
  }

  test("LexerError.UnterminatedString.02") {
    val input = """ "\ """
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedString](result)
  }

  test("LexerError.UnterminatedString.03") {
    val input = "\""
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedString](result)
  }

  test("LexerError.UnterminatedString.04") {
    val input = """ regex" regex"" """
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedString](result)
  }

  test("LexerError.UnterminatedString.05") {
    // Actually not related to the error but asserts that the error
    // is not a false positive
    val input = """ def f(): String = "This is terminated" """
    val result = compile(input, Options.TestWithLibNix)
    expectSuccess(result)
  }

  test("LexerError.UnterminatedString.06") {
    val input = """"${x}""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedString](result)
  }

  test("LexerError.MultilineString.01") {
    val input = s"""
         |def f(): String = "This is a
         |multi-line string"
         |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedString](result)
  }

  test("LexerError.UnterminatedStringInterpolation.01") {
    val input = """ "Hi ${name!" """
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedStringInterpolation](result)
  }

  test("LexerError.UnterminatedStringInterpolation.02") {
    val input = """ "${"Hi ${name!"}" """
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedStringInterpolation](result)
  }

  test("LexerError.UnterminatedStringInterpolation.03") {
    val input = """ "${"Hi ${name!}"" """
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedStringInterpolation](result)
  }

  test("LexerError.TrailingUnderscoreInNumber.01") {
    val input =
      s"""
         |def f(): Int = 1_
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.TrailingUnderscoreInNumber.02") {
    val input =
      s"""
         |def f(): Int = 1_000_
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.MalformedHexNumber.01") {
    val input =
      s"""
         |def f(): Int32 = 0x_1
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.MalformedHexNumber](result)
  }

  test("LexerError.MalformedHexNumber.02") {
    val input =
      s"""
         |def f(): Int32 = 0xF__Fi32f
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.MalformedHexNumber](result)
  }

  test("LexerError.MalformedHexNumber.03") {
    val input =
      s"""
         |def f(): Int8 = 0x_1i8
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.MalformedHexNumber](result)
  }

  test("LexerError.MalformedHexNumber.04") {
    val input =
      s"""
         |def f(): Int8 = 0x1_i8
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.MalformedHexNumber](result)
  }

  test("LexerError.MalformedHexNumber.05") {
    val input =
      s"""
         |def f(): Int16 = 0x_1i16
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.MalformedHexNumber](result)
  }

  test("LexerError.MalformedHexNumber.06") {
    val input =
      s"""
         |def f(): Int16 = 0x1_i16
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.MalformedHexNumber](result)
  }

  test("LexerError.MalformedHexNumber.07") {
    val input =
      s"""
         |def f(): Int32 = 0x_1i32
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.MalformedHexNumber](result)
  }

  test("LexerError.MalformedHexNumber.08") {
    val input =
      s"""
         |def f(): Int32 = 0x1_i32
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.MalformedHexNumber](result)
  }

  test("LexerError.MalformedHexNumber.09") {
    val input =
      s"""
         |def f(): Int64 = 0x_1i64
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.MalformedHexNumber](result)
  }

  test("LexerError.MalformedHexNumber.10") {
    val input =
      s"""
         |def f(): Int64 = 0x1_i64
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.MalformedHexNumber](result)
  }

  test("LexerError.MalformedHexNumber.11") {
    val input =
      s"""
         |def f(): BigInt = 0x_1ii
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.MalformedHexNumber](result)
  }

  test("LexerError.MalformedHexNumber.12") {
    val input =
      s"""
         |def f(): BigInt = 0x1_ii
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.MalformedHexNumber](result)
  }

  test("LexerError.IncorrectHexNumberSuffix.01") {
    val input =
    s"""
        |def f(): Int32 = 0xFFi32f
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.IncorrectHexNumberSuffix](result)
  }

  test("LexerError.IncorrectHexNumberSuffix.02") {
    val input =
    s"""
        |def f(): Int32 = 0xFFi322
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.IncorrectHexNumberSuffix](result)
  }

  test("LexerError.IncorrectHexNumberSuffix.03") {
    val input =
      s"""
         |def f(): Int32 = 0xFFi32FFi32
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.IncorrectHexNumberSuffix](result)
  }

  test("LexerError.UnterminatedHexNumber.01") {
    val input =
    s"""
        |def f(): Int32 = 0x
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedHexNumber](result)
  }

  test("LexerError.UnterminatedHexNumber.02") {
    val input =
    s"""
        |def f(): Int32 = 0xF_
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedHexNumber](result)
  }

  test("LexerError.UnterminatedHexNumber.03") {
    val input =
      s"""
         |def f(): Int32 = 0x1_
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedHexNumber](result)
  }

  test("LexerError.Int8.01") {
    val input =
      s"""
         |def f(): Int8 = 1_i8
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.Int8.02") {
    val input =
      s"""
         |def f(): Int8 = 1_000_i8
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.Int16.01") {
    val input =
      s"""
         |def f(): Int16 = 1_i16
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.Int16.02") {
    val input =
      s"""
         |def f(): Int16 = 1_000_i16
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.Int32.01") {
    val input =
      s"""
         |def f(): Int32 = 1_i32
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.Int32.02") {
    val input =
      s"""
         |def f(): Int32 = 1_000_i32
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.Int64.01") {
    val input =
      s"""
         |def f(): Int64 = 1_i64
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.Int64.02") {
    val input =
      s"""
         |def f(): Int64 = 1_000_i64
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.BigInt.01") {
    val input =
      s"""
         |def f(): BigInt = 1_ii
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.BigInt.02") {
    val input =
      s"""
         |def f(): BigInt = 1_000_ii
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.Float.01") {
    val input =
      s"""
         |def f(): Float = 1_.0
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.Float.02") {
    val input =
      s"""
         |def f(): Float = 1_000_.0
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.Float.03") {
    val input =
      s"""
         |def f(): Float = 1.0_
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.Float32.01") {
    val input =
      s"""
         |def f(): Float32 = 1_.0f32
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.Float32.02") {
    val input =
      s"""
         |def f(): Float32 = 1_000_.0f32
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.Float32.03") {
    val input =
      s"""
         |def f(): Float32 = 1.0_f32
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.Float64.01") {
    val input =
      s"""
         |def f(): Float64 = 1_.0f64
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.Float64.02") {
    val input =
      s"""
         |def f(): Float64 = 1_000_.0f64
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.Float64.03") {
    val input =
      s"""
         |def f(): Float64 = 1.0_f64
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.BigDecimal.01") {
    val input =
      s"""
         |def f(): BigDecimal = 1_.0ff
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.BigDecimal.02") {
    val input =
      s"""
         |def f(): BigDecimal = 1_000_.0ff
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.BigDecimal.03") {
    val input =
      s"""
         |def f(): Float64 = 1.0_ff
           """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.EOI.01") {
    val input = """def foo(): String = """"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.EOI.02") {
    val input =
      """def foo(): Char = '"""
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.EOI.03") {
    val input =
      """def foo (): String = "\"""
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("LexerError.EOI.04") {
    val input = """def foo (): Char = "\"""
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }
}
