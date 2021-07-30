// MATT license
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.KindError
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestKinder extends FunSuite with TestUtils {

  private val DefaultOptions = Options.TestWithLibNix

  test("MismatchedTypeParamKind.Implicit.01") {
    val input = "def f(g: Int -> o & o): Int = 123"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Implicit.02") {
    val input = "def f(g: Int -> Int & e): e = g(123)"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  ignore("MismatchedTypeParamKind.Implicit.03") { // MATT reenable after Kind ascriptions
    val input = "def f(s: #{| a}, r: {| a}): Int = 123" // MATT #{| } is just a syntactic hint; doesn't make it to the kinder
    val result = compile(input, Options.TestWithLibNix)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Implicit.04") {
    val input = "def f(s: #{X(Int) | a}, r: {x: Int | a}): Int = 123"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Implicit.05") {
    val input = "def f(a: e): Int & not e = 123"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Implicit.06") {
    val input =
      """
        |enum E[a] {
        |  case E1(a)
        |}
        |
        |def f(g: E[a -> b & e]): Int & not (a or b) = 123
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }


  test("MismatchedTypeParamKind.Enum.01") {
    val input =
      """
        |enum E[o] {
        |    case A(Int -> o & o)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Enum.02") {
    val input =
      """
        |enum E[e] {
        |    case A((Int -> Int & e) -> e)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  ignore("MismatchedTypeParamKind.Enum.03") { // MATT reenable after kind ascriptions
    val input =
      """
        |enum E[a] {
        |    case A(#{| a}, {| a})
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Enum.04") { // MATT error caused by subkinding
    val input =
      """
        |enum E[a] {
        |    case A(#{X(Int) | a}, {x: Int | a})
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Enum.05") {
    val input =
      """
        |enum E[e] {
        |    case A(e -> Int & not e)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Enum.06") {
    val input =
      """
        |enum D[a] {
        |  case D1(a)
        |}
        |enum E[a, b, e] {
        |    case A(D[a -> b & e] -> Int & not (a or b))
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.01") {
    val input = "type alias T[o] = Int -> o & o"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.02") {
    val input = "type alias T[e] = (Int -> Int & e) -> e"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  ignore("MismatchedTypeParamKind.TypeAlias.03") { // MATT depends on kind ascriptions
    val input = "type alias T[a] = (#{| a}, {| a})"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.04") {
    val input = "type alias T[a] = (#{X(Int) | a}, {x: Int | a})"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.05") {
    val input = "type alias T[e] = e -> Int & not e"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.06") {
    val input =
      """
        |enum Option[a] {
        |  case Some(a)
        |  case None
        |}
        |
        |type alias T[a, b, e] = Option[a -> b & e] -> Int & not (a or b)
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.01") {
    val input =
      """
        |enum P[a, b] {
        |  case C(a, b)
        |}
        |
        |def f(p: P[Int]): Int = 123
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.02") {
    val input =
      """
        |enum P[a, b] {
        |  case C(a, b)
        |}
        |
        |enum E {
        |  case A(P[Int])
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }


  test("IllegalUninhabitedType.03") {
    val input =
      """
        |enum P[a, b] {
        |  case C(a, b)
        |}
        |
        |def f(p: P): Int = 123
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.04") {
    val input =
      """
        |enum P[a, b] {
        |  case C(a, b)
        |}
        |
        |enum E {
        |  case A(P)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.05") {
    val input =
      """
        |enum P[a, b, c] {
        |  case C(a, b, c)
        |}
        |
        |def f(p: P[Int, Int]): Int = 123
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.06") {
    val input =
      """
        |enum P[a, b, c] {
        |  case C(a, b, c)
        |}
        |
        |enum E {
        |  case A(P[Int, Int])
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.07") {
    val input = """def f(x: true): Int = 123"""
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.08") {
    val input = "def f(): Int = 1 as Pure"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.09") {
    val input =
      """
        |enum E[a, b] {
        |  case C(a, b)
        |}
        |
        |def f(): Int = 1 as E[Int]""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.10") { // MATT stack overflow
    val input = "def f(): Int = 1: Pure"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.11") {
    val input =
      """
        |enum E[a, b] {
        |  case C(a, b)
        |}
        |
        |def f(): Int = 1: E[Int]""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalEffect.01") {
    val input = "def f(): Int = 1 as & Int"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalEffect.02") {
    val input = "def f(): Int = 1 as Int & Int"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalEffect.03") {
    val input = "def f(): Int = 1: & Int"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalEffect.04") {
    val input = "def f(): Int = 1: Int & Int"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.01") {
    val input =
      """
        |enum P[a, b] {
        |  case C(a, b)
        |}
        |
        |def f(p: P[Int, String, String]): Int = 123
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.02") {
    val input =
      """
        |type alias R = {x: Int}
        |
        |def f(p: R[Int]): Int = 123
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.03") {
    val input =
      """
        |rel A(a: Int)
        |
        |type alias S = #{ A }
        |
        |def f(p: S[Int]): Int = 123
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.04") {
    val input = "def f(p: String[Int]): Int = 123"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.05") {
    val input = "def f(): Int = 1 as Int & Int and true"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.06") {
    val input = "def f(): Int = 1 as Int & true or Int"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.07") {
    val input = "def f(): Int = 1 as Int & not Int"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.08") {
    val input = "def f(a: (Int, true)): Int = 1"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }
}
