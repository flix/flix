/*
 * Copyright 2020 Matthew Lutze
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
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.errors.InstanceError
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestInstances extends FunSuite with TestUtils {

  val DefaultOptions: Options = Options.DefaultTest.copy(core = true)

  test("Test.OverlappingInstance.01") {
    val input =
      """
        |class C[a]
        |
        |instance C[Int]
        |
        |instance C[Int]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.OverlappingInstances](result)
  }


  test("Test.OverlappingInstance.02") {
    val input =
      """
        |class C[a]
        |
        |instance C[a]
        |
        |instance C[Int]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.OverlappingInstances](result)
  }

  test("Test.OverlappingInstance.03") {
    val input =
      """
        |class C[a]
        |
        |instance C[(a, Int)]
        |
        |instance C[(Bool, b)]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.OverlappingInstances](result)
  }

  test("Test.OverlappingInstance.04") {
    val input =
      """
        |class C[a]
        |
        |instance C[() -> a]
        |
        |instance C[a -> ()]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.OverlappingInstances](result)
  }

  test("Test.OverlappingInstance.05") {
    val input =
      """
        |enum Box[a] {
        |    case Box(a)
        |}
        |
        |class C[a]
        |
        |instance C[Box[a]]
        |
        |instance C[Box[Int]]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.OverlappingInstances](result)
  }

  test("Test.OverlappingInstance.06") {
    val input =
      """
        |class C[a]
        |
        |instance C[() -> a]
        |
        |instance C[a -> ()]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.OverlappingInstances](result)
  }

  test("Test.OverlappingInstance.07") {
    val input =
      """
        |class C[a]
        |
        |instance C[() -> a & e]
        |
        |instance C[a -> () & e]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.OverlappingInstances](result)
  }


  test("Test.OverlappingInstance.08") {
    val input =
      """
        |enum Box[a] {
        |    case Box(a)
        |}
        |
        |class C[a]
        |
        |instance C[Box[a] -> b & e]
        |
        |instance C[Box[Int] ~> ()]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.OverlappingInstances](result)
  }

  test("Test.MissingImplementation.01") {
    val input =
      """
        |class C[a] {
        |    def get(): a
        |}
        |
        |instance C[Bool] {
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.MissingImplementation](result)
  }

  test("Test.MismatchedSignatures.01") {
    val input =
      """
        |class C[a] {
        |    def get(): a
        |}
        |
        |instance C[Bool] {
        |    def get(_i: Int): Bool = false
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.MismatchedSignatures](result)
  }

  test("Test.MismatchedSignatures.02") {
    val input =
      """
        |class C[a] {
        |    def f(x: a): Bool
        |}
        |
        |enum Box[a] {
        |    case Box(a)
        |}
        |
        |instance C[Box[a]] {
        |    def f[a: C](_x: Box[a]): Bool = false
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.MismatchedSignatures](result)
  }

  test("Test.MismatchSignatures.03") {
    val input =
      """
        |class C[a] {
        |    pub def f(x: a): String
        |}
        |
        |enum Box[a] {
        |    case Box(a)
        |}
        |
        |instance C[Int] {
        |    pub def f(x: Int): String = ""
        |}
        |
        |instance C[Box[a]] {
        |    def f[a: C](x: Box[a]): String = match x {
        |        case Box(y) => f(y)
        |    }
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[CompilationError](result) // TODO should be MismatchedSignature
  }

  test("Test.MismatchedSignatures.04") {
    val input =
      """
        |class C[a] {
        |    def f[b : D](x: b): a
        |}
        |
        |class D[a]
        |
        |instance C[Bool] {
        |    def f(x: b): Bool = false
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.MismatchedSignatures](result)
  }

  test("Test.MismatchedSignatures.05") {
    val input =
      """
        |class C[a] {
        |    def f(x: a, y: Int): Int
        |}
        |
        |instance C[Bool] {
        |    def f(x: Bool, y: Int): Int & Impure = 123 as & Impure
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.MismatchedSignatures](result)
  }

  test("Test.MismatchedSignatures.06") {
    val input =
      """
        |class C[a] {
        |    def f(x: a, y: Int): Int & e
        |}
        |
        |instance C[Bool] {
        |    def f(x: Bool, y: Int): Int & Impure = 123 as & Impure
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.MismatchedSignatures](result)
  }

  test("Test.ExtraneousDefinition.01") {
    val input =
      """
        |class C[a]
        |
        |instance C[Bool] {
        |    def get(): Bool = false
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.ExtraneousDefinition](result)
  }
}
