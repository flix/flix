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
import org.scalatest.FunSuite

class TestEntryPoint extends FunSuite with TestUtils {

  test("Test.UnexpectedEntryPointArg.Main.01") {
    val input =
      """
        |def main(blah: Array[Char]): Int32 & Impure = ??? as & Impure
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.UnexpectedEntryPointArg](result)
  }

  test("Test.UnexpectedEntryPointArg.Main.02") {
    val input =
      """
        |def main(blah: Array[a]): Int32 & Impure = ??? as & Impure
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.UnexpectedEntryPointArg](result)
  }

  test("Test.UnexpectedEntryPointArg.Main.03") {
    val input =
      """
        |class C[a]
        |
        |def main(blah: Array[a]): Int32 & Impure with C[a] = ??? as & Impure
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.UnexpectedEntryPointArg](result)
  }

  test("Test.TooManyEntryPointArgs.Main.01") {
    val input =
      """
        |def main(arg1: Array[String], arg2: Array[String]): Unit = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.TooManyEntryPointArgs](result)
  }

  test("Test.TooManyEntryPointArgs.Main.02") {
    val input =
      """
        |def main(arg1: String, arg2: String): Unit = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.TooManyEntryPointArgs](result)
  }

  test("Test.UnexpectedEntryPointResult.Main.01") {
    val input =
      """
        |def main(blah: Array[String]): a & Impure = ??? as & Impure
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.UnexpectedEntryPointResult](result)
  }

  test("Test.UnexpectedEntryPointResult.Main.02") {
    val input =
      """
        |enum E
        |def main(blah: Array[String]): E = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.UnexpectedEntryPointResult](result)
  }

  //  test("Test.IllegalMain.07") { // MATT ok
  //    val input =
  //      """
  //        |def main(blah: Array[String]): Int32 & ef = ???
  //        |""".stripMargin
  //    val result = compile(input, Options.TestWithLibMin)
  //  }
  //
  //  test("Test.IllegalMain.01") { // MATT move to happy test
  //    val input =
  //      """
  //        |def main(blah: Array[String]): Int32 = ???
  //        |""".stripMargin
  //    val result = compile(input, Options.TestWithLibMin)
  //    expectError[TypeError.IllegalMain](result)
  //  }
  //
  //  test("Test.IllegalMain.03") { // MATT ok
  //    val input =
  //      """
  //        |def main(blah: Array[String]): Int64 & Impure = ???
  //        |""".stripMargin
  //    val result = compile(input, Options.TestWithLibMin)
  //    expectError[TypeError.IllegalMain](result)
  //  }


}
