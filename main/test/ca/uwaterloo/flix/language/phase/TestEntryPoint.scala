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
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.errors.EntryPointError
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestEntryPoint extends AnyFunSuite with TestUtils {

  test("Test.IllegalEntryPointArg.Main.01") {
    val input =
      """
        |def main(blah: Array[String, _]): Unit \ IO = checked_ecast(())
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.IllegalRunnableEntryPointArgs](result)
  }

  test("Test.IllegalEntryPointArg.Main.02") {
    val input =
      """
        |def main(blah: Array[a, _]): Unit \ IO = checked_ecast(())
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.IllegalRunnableEntryPointArgs](result)
  }

  test("Test.IllegalEntryPointArg.Main.03") {
    val input =
      """
        |trait C[a]
        |
        |def main(blah: Array[a, _]): Unit \ IO with C[a] = checked_ecast(())
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.IllegalRunnableEntryPointArgs](result)
  }

  test("Test.IllegalEntryPointArg.Main.04") {
    val input =
      """
        |def main(arg1: Array[String, _], arg2: Array[String, _]): Unit = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.IllegalRunnableEntryPointArgs](result)
  }

  test("Test.IllegalRunnableEntryPointArgs.Main.05") {
    val input =
      """
        |def main(arg1: String, arg2: String): Unit = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.IllegalRunnableEntryPointArgs](result)
  }

  test("Test.IllegalRunnableEntryPointArgs.Other.01") {
    val input =
      """
        |def f(x: Bool): Unit = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin.copy(entryPoint = Some(Symbol.mkDefnSym("f"))))
    expectError[EntryPointError.IllegalRunnableEntryPointArgs](result)
  }

  test("Test.IllegalEntryPointEffect.Main.01") {
    val input =
      """
        |eff Exc {
        |    pub def raise(): Unit
        |}
        |
        |def main(): Unit \ Exc = Exc.raise()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.IllegalEntryPointEffect](result)
  }

  test("Test.IllegalEntryPointEffect.Main.02") {
    val input =
      """
        |eff Print {
        |    pub def print(): Unit
        |}
        |
        |eff Exc {
        |    pub def raise(): Unit
        |}
        |
        |def main(): Unit \ Print + Exc  =
        |    Print.print();
        |    Exc.raise()
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.IllegalEntryPointEffect](result)
  }

  test("Test.IllegalEntryPointEffect.Main.03") {
    val input =
      """
        |eff Print {
        |    pub def print(): Unit
        |}
        |
        |def main(): Unit \ Print + IO  =
        |    Print.print();
        |    println("Hello, World!")
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.IllegalEntryPointEffect](result)
  }

  test("Test.IllegalMainEntryPointResult.Main.01") {
    val input =
      """
        |def main(): a \ IO = checked_ecast(???)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.IllegalMainEntryPointResult](result)
  }

  test("Test.IllegalMainEntryPointResult.Main.02") {
    val input =
      """
        |enum E
        |def main(): E = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.IllegalMainEntryPointResult](result)
  }

  test("Test.IllegalMainEntryPointResult.Other.01") {
    val input =
      """
        |enum E
        |def f(): E = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin.copy(entryPoint = Some(Symbol.mkDefnSym("f"))))
    expectError[EntryPointError.IllegalMainEntryPointResult](result)
  }

  test("Test.IllegalSignature.Main.01") {
    val input =
      """
        |enum E
        |eff Exc {
        |    pub def raise(): Unit
        |}
        |def main(a: Int32): E \ Exc = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.IllegalRunnableEntryPointArgs](result)
    expectError[EntryPointError.IllegalMainEntryPointResult](result)
    expectError[EntryPointError.IllegalEntryPointEffect](result)
  }

  test("Test.MainEntryPointNotFound.01") {
    val input =
      """
        |def notF(): String = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin.copy(entryPoint = Some(Symbol.mkDefnSym("f"))))
    expectError[EntryPointError.MainEntryPointNotFound](result)
  }

  test("Test.MainEntryPointNotFound.02") {
    val input =
      """
        |def main(): String = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin.copy(entryPoint = Some(Symbol.mkDefnSym("f"))))
    expectError[EntryPointError.MainEntryPointNotFound](result)
  }

  test("Test.ValidEntryPoint.Main.01") {
    val input =
      """
        |def main(): Unit = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectSuccess(result)
  }

  test("Test.ValidEntryPoint.Main.02") {
    val input =
      """
        |def main(): Int64 \ IO = checked_ecast(42i64)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectSuccess(result)
  }

  test("Test.ValidEntryPoint.Main.03") {
    val input =
      """
        |def main(): Int64 \ Exec = checked_ecast(42i64)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectSuccess(result)
  }

  test("Test.ValidEntryPoint.Main.04") {
    val input =
      """
        |def main(): Int64 \ FsRead = checked_ecast(42i64)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectSuccess(result)
  }

  test("Test.ValidEntryPoint.Main.05") {
    val input =
      """
        |def main(): Int64 \ FsWrite = checked_ecast(42i64)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectSuccess(result)
  }

  test("Test.ValidEntryPoint.Main.06") {
    val input =
      """
        |def main(): Int64 \ Net = checked_ecast(42i64)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectSuccess(result)
  }

  test("Test.ValidEntryPoint.Main.07") {
    val input =
      """
        |def main(): Int64 \ NonDet = checked_ecast(42i64)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectSuccess(result)
  }

  test("Test.ValidEntryPoint.Main.08") {
    val input =
      """
        |def main(): Int64 \ Sys = checked_ecast(42i64)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectSuccess(result)
  }

  test("Test.ValidEntryPoint.Main.09") {
    val input =
      """
        |def main(): Int64 \ Sys + Exec + NonDet = checked_ecast(42i64)
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
