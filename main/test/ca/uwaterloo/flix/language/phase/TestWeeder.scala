/*
 * Copyright 2015-2016 Magnus Madsen
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
import ca.uwaterloo.flix.language.errors.WeederError
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestWeeder extends FunSuite with TestUtils {

  val DefaultOptions: Options = Options.DefaultTest.copy(core = true)

  test("DuplicateAnnotation.01") {
    val input =
      """@test @test
        |def foo(x: Int): Int = 42
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[WeederError.DuplicateAnnotation](result)
  }

  test("DuplicateFormal.01") {
    val input = "def f(x: Int, x: Int): Int = 42"
    val result = compile(input, DefaultOptions)
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormal.02") {
    val input = "def f(x: Int, y: Int, x: Int): Int = 42"
    val result = compile(input, DefaultOptions)
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormal.03") {
    val input = "def f(x: Bool, x: Int, x: Str): Int = 42"
    val result = compile(input, DefaultOptions)
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormal.04") {
    val input = "def f(): (Int, Int) -> Int = (x, x) -> x"
    val result = compile(input, DefaultOptions)
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormal.05") {
    val input = "def f(): (Int, Int, Int) -> Int = (x, y, x) -> x"
    val result = compile(input, DefaultOptions)
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormal.06") {
    val input = "def f(): Bool = ∀(x: E, x: E). true"
    val result = compile(input, DefaultOptions)
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormal.07") {
    val input = "def f(): Bool = ∃(x: E, x: E). true"
    val result = compile(input, DefaultOptions)
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateTag.01") {
    val input =
      """enum Color {
        |  case Red,
        |  case Red
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[WeederError.DuplicateTag](result)
  }

  test("DuplicateTag.02") {
    val input =
      """enum Color {
        |  case Red,
        |  case Blu,
        |  case Red
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[WeederError.DuplicateTag](result)
  }

  test("IllegalFieldName.01") {
    val input = "def f(): { length: Int } = { length = 123 }"
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalFieldName](result)
  }

  test("IllegalFieldName.02") {
    val input = "def f(): { length: Int } = { +length = 123 | {} }"
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalFieldName](result)
  }

  test("IllegalFieldName.03") {
    val input = "def f(): { length: Int } = { -length | {} }"
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalFieldName](result)
  }

  test("IllegalFieldName.04") {
    val input = "def f(): { length: Int } = { length = 123 | {} }"
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalFieldName](result)
  }

  test("IllegalExistential.01") {
    val input = "def f(): Bool = ∃(). true"
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalExistential](result)
  }

  test("IllegalInt8.01") {
    val input = "def f(): Int8 = -1000i8"
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalInt8.02") {
    val input = "def f(): Int8 = 1000i8"
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalInt16.01") {
    val input = "def f(): Int16 = -100000i16"
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalInt16.02") {
    val input = "def f(): Int16 = 100000i16"
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalInt32.01") {
    val input = "def f(): Int32 = -10000000000i32"
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalInt32.02") {
    val input = "def f(): Int32 = 10000000000i32"
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalInt64.01") {
    val input = "def f(): Int64 = -100000000000000000000i64"
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalInt64.02") {
    val input = "def f(): Int64 = 100000000000000000000i64"
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalUniversal.01") {
    val input = "def f(): Prop = ∀(). true"
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalUniversal](result)
  }

  test("IllegalNullPattern.01") {
    val input =
      s"""
         |def f(): Int = match null {
         |    case null => 123
         |    case _    => 456
         |}
         |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalNullPattern](result)
  }

  test("IllegalJvmFieldOrMethodName.01") {
    val input =
      s"""
         |def f(): Unit =
         |    import foo() as bar;
         |    ()
         |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalJvmFieldOrMethodName](result)
  }

  test("MismatchedArity.01") {
    val input =
      """def f(): Bool =
        |    choose 123 {
        |        case (Present(x), Present(y)) => x == y
        |    }
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[WeederError.MismatchedArity](result)
  }

  test("MismatchedArity.02") {
    val input =
      """def f(): Bool =
        |    choose (123, 456) {
        |        case Present(x) => x == x
        |    }
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[WeederError.MismatchedArity](result)
  }

  test("NonLinearPattern.01") {
    val input =
      """def f(): Bool = match (21, 42) {
        |  case (x, x) => true
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[WeederError.NonLinearPattern](result)
  }

  test("NonLinearPattern.02") {
    val input =
      """def f(): Bool = match (21, 42, 84) {
        |  case (x, x, x) => true
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[WeederError.NonLinearPattern](result)
  }

  test("NonLinearPattern.03") {
    val input =
      """def f(): Bool = match (1, (2, (3, 4))) {
        |  case (x, (y, (z, x))) => true
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[WeederError.NonLinearPattern](result)
  }

  test("UndefinedAnnotation.01") {
    val input =
      """@abc
        |def foo(x: Int): Int = 42
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[WeederError.UndefinedAnnotation](result)
  }

  test("UndefinedAnnotation.02") {
    val input =
      """@foobarbaz
        |def foo(x: Int): Int = 42
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[WeederError.UndefinedAnnotation](result)
  }

  test("IllegalPrivateDeclaration.01") {
    val input =
      """
        |class C[a] {
        |    def f(): a
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalPrivateDeclaration](result)
  }

  test("IllegalPrivateDeclaration.02") {
    val input =
      """
        |instance C[Int] {
        |    def f(): Int = 1
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalPrivateDeclaration](result)
  }

  test("IllegalTypeConstraintParameter.01") {
    val input =
      """
        |class C[a] with D[Int]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalTypeConstraintParameter](result)
  }

  test("IllegalTypeConstraintParameter.02") {
    val input =
      """
        |instance C[a] with D[Some[a]]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalTypeConstraintParameter](result)
  }

  test("IllegalTypeConstraint.01") {
    val input =
      """
        |enum CBox[a has C] {
        |    case CBox(a)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalTypeConstraint](result)
  }

  test("IllegalTypeConstraint.02") {
    val input =
      """
        |opaque type Box[a has C] = a
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalTypeConstraint](result)
  }

  test("IllegalTypeConstraint.03") {
    val input =
      """
        |type alias Box[a has C] = a
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalTypeConstraint](result)
  }

  test("IllegalTypeConstraint.04") {
    val input =
      """
        |class C[a has D]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalTypeConstraint](result)
  }

  test("IllegalTypeConstraint.05") {
    val input =
      """
        |rel R[a has D](x: a)
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalTypeConstraint](result)
  }

  test("IllegalTypeConstraint.06") {
    val input =
      """
        |lat L[a has D](x: a, y: Int)
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[WeederError.IllegalTypeConstraint](result)
  }
}
