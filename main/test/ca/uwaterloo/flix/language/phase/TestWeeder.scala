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
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.errors.WeederError
import org.scalatest.FunSuite

class TestWeeder extends FunSuite with TestUtils {

  test("DuplicateAnnotation.01") {
    val input =
      """@test @test
        |def foo(x: Int): Int = 42
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.DuplicateAnnotation](result)
  }

  test("DuplicateAttribute.01") {
    val input = "rel A(x: Int, x: Int)"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.DuplicateAttribute](result)
  }

  test("DuplicateAttribute.02") {
    val input = "rel A(x: Int, y: Int, x: Int)   "
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.DuplicateAttribute](result)
  }

  test("DuplicateAttribute.03") {
    val input = "rel A(x: Bool, x: Int, x: Str)"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.DuplicateAttribute](result)
  }

  test("DuplicateFormal.01") {
    val input = "def f(x: Int, x: Int): Int = 42"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormal.02") {
    val input = "def f(x: Int, y: Int, x: Int): Int = 42"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormal.03") {
    val input = "def f(x: Bool, x: Int, x: Str): Int = 42"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormal.04") {
    val input = "def f(): (Int, Int) -> Int = (x, x) -> x"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormal.05") {
    val input = "def f(): (Int, Int, Int) -> Int = (x, y, x) -> x"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormal.06") {
    val input = "def f(): Bool = ∀(x: E, x: E). true"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormal.07") {
    val input = "def f(): Bool = ∃(x: E, x: E). true"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateTag.01") {
    val input =
      """enum Color {
        |  case Red,
        |  case Red
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
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
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.DuplicateTag](result)
  }

  test("IllegalExistential.01") {
    val input = "def f(): Bool = ∃(). true"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.IllegalExistential](result)
  }

  test("IllegalInt8.01") {
    val input = "def f(): Int8 = -1000i8"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalInt8.02") {
    val input = "def f(): Int8 = 1000i8"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalInt16.01") {
    val input = "def f(): Int16 = -100000i16"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalInt16.02") {
    val input = "def f(): Int16 = 100000i16"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalInt32.01") {
    val input = "def f(): Int32 = -10000000000i32"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalInt32.02") {
    val input = "def f(): Int32 = 10000000000i32"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalInt64.01") {
    val input = "def f(): Int64 = -100000000000000000000i64"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalInt64.02") {
    val input = "def f(): Int64 = 100000000000000000000i64"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalLattice.01") {
    val input = "let Foo<> = (1, 2)"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.IllegalLattice](result)
  }

  test("IllegalLattice.02") {
    val input = "let Foo<> = (1, 2, 3, 4, 5, 6, 7, 8, 9)"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.IllegalLattice](result)
  }

  test("IllegalNativeFieldOrMethodName.01") {
    val input = "def f(): Int = native field java"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.IllegalNativeFieldOrMethodName](result)
  }

  test("IllegalNativeFieldOrMethodName.02") {
    val input = "def f(): Int = native field com"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.IllegalNativeFieldOrMethodName](result)
  }

  test("IllegalNativeFieldOrMethodName.03") {
    val input = "def f(): Int = native method java()"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.IllegalNativeFieldOrMethodName](result)
  }

  test("IllegalNativeFieldOrMethodName.04") {
    val input = "def f(): Int = native method com()"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.IllegalNativeFieldOrMethodName](result)
  }

  test("IllegalUniversal.01") {
    val input = "def f(): Prop = ∀(). true"
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.IllegalUniversal](result)
  }

  test("NonLinearPattern.01") {
    val input =
      """def f(): Bool = match (21, 42) with {
        |  case (x, x) => true
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.NonLinearPattern](result)
  }

  test("NonLinearPattern.02") {
    val input =
      """def f(): Bool = match (21, 42, 84) with {
        |  case (x, x, x) => true
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.NonLinearPattern](result)
  }

  test("NonLinearPattern.03") {
    val input =
      """def f(): Bool = match (1, (2, (3, 4))) with {
        |  case (x, (y, (z, x))) => true
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.NonLinearPattern](result)
  }

  test("UndefinedAnnotation.01") {
    val input =
      """@abc
        |def foo(x: Int): Int = 42
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.UndefinedAnnotation](result)
  }

  test("UndefinedAnnotation.02") {
    val input =
      """@foobarbaz
        |def foo(x: Int): Int = 42
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[WeederError.UndefinedAnnotation](result)
  }

}
