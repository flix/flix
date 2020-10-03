/*
 * Copyright 2020 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.util.{Options, Validation}
import org.scalatest.FunSuite

class TestTyper extends FunSuite with TestUtils {

  val DefaultOptions: Options = Options.DefaultTest.copy(core = true)

  test("TestLeq01") {
    val input =
      """
        |def f(): a = 21
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq02") {
    val input =
      """
        |def f(): List[a] = 21 :: Nil
        |
        |enum List[t] {
        |    case Nil,
        |    case Cons(t, List[t])
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq03") {
    val input =
      """
        |def f(): Result[a, Int] = Ok(21)
        |
        |enum Result[t, e] {
        |    case Ok(t),
        |    case Err(e)
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq04") {
    val input =
      """
        |def f(): Result[Int, a] = Err(21)
        |
        |enum Result[t, e] {
        |    case Ok(t),
        |    case Err(e)
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq05") {
    val input =
      """
        |def f(): a -> a = x -> 21
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq06") {
    val input =
      """
        |def f(): a -> a = (x: Int32) -> x
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq07") {
    val input =
      """
        |def f(): {x: Int | r} = {x = 21}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq08") {
    val input =
      """
        |def f(): {x: Int, y: Int | r} = {y = 42, x = 21}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestOccurs01") {
    val input =
      """
        |rel A(v: Int)
        |rel B(v: Int)
        |
        |def f(a: #{A | r}, b: #{B | r}): #{A, B} = solve (a <+> b)
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.OccursCheckError](result)
  }

  test("TestLeq.Null.01") {
    val input =
      """
        |pub def testNullableThreeVar11(x: String ? false, y: String ? true, z: String ? false): Bool =
        |    match? (x, y, z) {
        |        case (a, b, _) => a == "Hello" && b == "World"
        |        case (_, b, c) => b == "World" && c == "!"
        |    }
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestLeq.Null.02") {
    val input =
      """
        |pub def testNullableThreeVar11(x: String ? true, y: String ? false, z: String ? true): Bool =
        |    match? (x, y, z) {
        |        case (a, b, _) => a == "Hello" && b == "World"
        |        case (_, b, c) => b == "World" && c == "!"
        |    }
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestLeq.Null.03") {
    val input =
      """
        |def f(x: String ? n, y: String ? n): Bool =
        |    match? (x, y) {
        |        case (a, _) => a == "Hello"
        |    }
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestMismatchedKinds.01") {
    val input = "def f(): {| x} = {a = 2} <+> {a = 2}"
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedKinds](result)
  }

  test("TestMismatchedKinds.02") {
    val input = "def f(): #{| x} = {a = 2} <+> {a = 2}"
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedKinds](result)
  }

  test("TestMismatchedKinds.03") {
    val input = "def f(): {a: Int} = {a = 2} <+> {a = 2}"
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedKinds](result)
  }

  test("TestMismatchedKinds.04") {
    val input = "def f(): Str = 1 |= 2"
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedKinds](result)
  }

  test("TestMismatchedKinds.05") {
    val input = "def f(): Str = solve \"hello\""
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedKinds](result)
  }

  test("TestMismatchedKinds.06") {
    val input =
      """
        |rel A(a: Int)
        |
        |def f(): Int = {
        |  let b = "b";
        |  let c = "c";
        |  let d = "d";
        |  fold A b c d
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError](result) // TODO use more specific error once TypeError logic is more defined
  }

  test("TestMismatchedKinds.07") {
    val input =
      """
        |rel A(a: Int)
        |
        |def f(): Int = {
        |  let b = "b";
        |  project A b
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError](result) // TODO use more specific error once TypeError logic is more defined
  }

  test("TestLeq.Wildcard.01") {
    val input = "def f(a: _): _ = a"
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq.Wildcard.02") {
    val input = "def f(a: Int): _ = a"
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq.Wildcard.03") {
    val input = "def f(a: Int): Int & _ = a"
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq.Wildcard.04") {
    val input = "def f(a: Int): Int & _ = a"
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq.Wildcard.05") {
    val input = "def f(g: Int -> Int & _): Int & _ = g(1)"
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq.Class.01") {
    val input =
      """
        |class Show[a] {
        |  def show(x: a): String
        |}
        |def foo(x: a): String = show(x)
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }
}
