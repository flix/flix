/*
 * Copyright 2022 Matthew Lutze
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
import ca.uwaterloo.flix.language.errors.EntryPointError
import ca.uwaterloo.flix.util.Options
import ca.uwaterloo.flix.language.ast.Symbol
import org.scalatest.FunSuite

class TestEntryPoint extends FunSuite with TestUtils {

  test("Test.IllegalEntryPointArg.Main.01") {
    val input =
      """
        |def main(blah: Array[String]): Int32 & Impure = ??? as & Impure
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.IllegalEntryPointArgs](result)
  }

  test("Test.IllegalEntryPointArg.Main.02") {
    val input =
      """
        |def main(blah: Array[a]): Int32 & Impure = ??? as & Impure
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.IllegalEntryPointArgs](result)
  }

  test("Test.IllegalEntryPointArg.Main.03") {
    val input =
      """
        |class C[a]
        |
        |def main(blah: Array[a]): Int32 & Impure with C[a] = ??? as & Impure
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.IllegalEntryPointArgs](result)
  }

  test("Test.IllegalEntryPointArg.Main.04") {
    val input =
      """
        |def main(arg1: Array[String], arg2: Array[String]): Unit = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.IllegalEntryPointArgs](result)
  }

  test("Test.IllegalEntryPointArgs.Main.05") {
    val input =
      """
        |def main(arg1: String, arg2: String): Unit = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.IllegalEntryPointArgs](result)
  }

  test("Test.IllegalEntryPointArgs.Other.01") {
    val input =
      """
        |def f(x: Bool): Unit = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin.copy(entryPoint = Some(Symbol.mkDefnSym("f"))))
    expectError[EntryPointError.IllegalEntryPointArgs](result)
  }

  test("Test.IllegalEntryPointResult.Main.01") {
    val input =
      """
        |def main(): a & Impure = ??? as & Impure
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.IllegalEntryPointResult](result)
  }

  test("Test.IllegalEntryPointResult.Main.02") {
    val input =
      """
        |enum E
        |def main(): E = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.IllegalEntryPointResult](result)
  }

  test("Test.IllegalEntryPointResult.Other.01") {
    val input =
      """
        |enum E
        |def f(): E = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin.copy(entryPoint = Some(Symbol.mkDefnSym("f"))))
    expectError[EntryPointError.IllegalEntryPointResult](result)
  }
    test("Test.ValidEntryPoint.Main.01") {
      val input =
        """
          |def main(): Int32 & ef = ??? as & ef
          |""".stripMargin
      val result = compile(input, Options.TestWithLibMin)
      expectSuccess(result)
    }

    test("Test.ValidEntryPoint.Main.02") {
      val input =
        """
          |def main(): Int32 = ???
          |""".stripMargin
      val result = compile(input, Options.TestWithLibMin)
      expectSuccess(result)
    }

    test("Test.ValidEntryPoint.Main.03") {
      val input =
        """
          |def main(): Int64 & Impure = ??? as & Impure
          |""".stripMargin
      val result = compile(input, Options.TestWithLibMin)
      expectSuccess(result)
    }

  test("Test.ValidEntryPoint.Main.04") {
    val input =
      """
        |def main(): Unit = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectSuccess(result)
  }

  test("Test.ValidEntryPoint.Other.01") {
    val input =
      """
        |def f(): Unit = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin.copy(entryPoint = Some(Symbol.mkDefnSym("f"))))
    expectSuccess(result)
  }
}
