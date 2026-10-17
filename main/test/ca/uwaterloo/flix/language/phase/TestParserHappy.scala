package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

/**
  * Note that CompilerSuite and LibrarySuite covers the positive testing of the parser well.
  */
class TestParserHappy extends AnyFunSuite with TestUtils {
  test("DetectRecord.01") {
    val input =
      """
        |pub def foo(): { x = Int32 } = {
        |    // This is a comment
        |    x = 1000
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectSuccess(result)
  }

  test("ExtensibleType.01") {
    val input =
      """
        |pub def foo(): #| A(Int32) |# = ???
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectSuccess(result)
  }

  test("Comment.IfThenElse.01") {
    val input =
      """
        |def f(): Int32 = if /* c */ (true) 1 else 2
        |def g(): Int32 = if (true) /* c */ 1 else 2
        |def h(): Int32 = if (true) 1 /* c */ else 2
        |def i(): Int32 = if (true) 1 else /* c */ 2
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectSuccess(result)
  }

  test("Comment.Lambda.01") {
    val input =
      """
        |def f(): Int32 -> Int32 = x /* c */ -> x
        |def g(): Int32 -> Int32 = x -> /* c */ x
        |def h(): Int32 -> Int32 = /* c */ x -> x
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectSuccess(result)
  }

  test("Comment.Let.01") {
    val input =
      """
        |def f(): Int32 =
        |    let /* c */ x = 1; // trailing
        |    // leading
        |    x
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectSuccess(result)
  }

  test("Comment.Parameters.01") {
    val input =
      """
        |def f(x: Int32 /* c */, /* c */ y: Int32, z /* c */ : /* c */ Int32): (Int32, Int32, Int32) = (x, y, z)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectSuccess(result)
  }

  test("Comment.Match.01") {
    val input =
      """
        |def f(x: Int32): Int32 = match x {
        |    // leading
        |    case 1 => /* c */ 1
        |    /* between */
        |    case _ /* c */ => 2
        |    // trailing
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectSuccess(result)
  }

  test("Comment.Block.01") {
    val input =
      """
        |def f(): Int32 = {
        |    // only a comment before the expression
        |    1
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectSuccess(result)
  }

  test("Comment.Type.01") {
    val input =
      """
        |def f(): (/* c */ Int32 /* c */, Int32) = (1, 2)
        |def g() /* c */ : /* c */ Int32 /* c */ = 1
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectSuccess(result)
  }

  test("Comment.Enum.01") {
    val input =
      """
        |enum Color {
        |    /// doc on Red
        |    case Red, // trailing after Red
        |    // line comment before Green
        |    case Green,
        |    /// doc on Blue after a comma
        |    case Blue // trailing after Blue
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectSuccess(result)
  }

  test("Comment.Struct.01") {
    val input =
      """
        |struct S[r] {
        |    // comment before field
        |    x: Int32, // trailing field comment
        |    /// doc on field
        |    y: Int32
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectSuccess(result)
  }

  test("Comment.Decl.01") {
    val input =
      """
        |/// Doc line one
        |// a line comment inside a doc comment
        |/* a block comment inside a doc comment */
        |/// Doc line two
        |pub /* c */ def f(): Int32 = 1
        |
        |/// Doc
        |@Lazy // trailing after an annotation
        |def g(): Int32 = 1
        |
        |mod M {
        |    // only a comment in this module
        |}
        |
        |trait T[a] {
        |    /// doc on a signature
        |    pub def t(x: a): Int32
        |    // trailing comment in a trait
        |}
        |
        |// trailing comment at the end of the file
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectSuccess(result)
  }

  test("Comment.LocalDef.01") {
    val input =
      """
        |def f(): Int32 =
        |    /// doc on a local def
        |    def g(x: Int32): Int32 = x;
        |    g(1)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectSuccess(result)
  }

  test("Comment.OnlyComments.01") {
    val input =
      """
        |// a file with nothing but comments
        |/* and a block comment */
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectSuccess(result)
  }
}
