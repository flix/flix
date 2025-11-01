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

class TestEntryPoints extends AnyFunSuite with TestUtils {

  test("Test.IllegalEntryPointArg.Main.01") {
    val input =
      """
        |def main(_blah: Array[String, _]): Unit \ IO = checked_ecast(())
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.IllegalEntryPointTypeVariables](result)
  }

  test("Test.IllegalEntryPointArg.Main.02") {
    val input =
      """
        |def main(_blah: Array[a, Static]): Unit \ IO = checked_ecast(())
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.IllegalEntryPointTypeVariables](result)
  }

  test("Test.IllegalEntryPointArg.Main.03") {
    val input =
      """
        |trait C[a]
        |
        |def main(_blah: Array[a, Static]): Unit \ IO with C[a] = checked_ecast(())
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.IllegalEntryPointTypeVariables](result)
  }

  test("Test.IllegalEntryPointArg.Main.04") {
    val input =
      """
        |def main(_arg1: Array[String, _], _arg2: Array[String, _]): Unit = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.IllegalEntryPointTypeVariables](result)
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
    expectError[EntryPointError.IllegalEntryPointTypeVariables](result)
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
    expectError[EntryPointError.MainEntryPointNotFound](result, allowUnknown = true)
  }

  test("Test.MainEntryPointNotFound.02") {
    val input =
      """
        |def main(): String = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin.copy(entryPoint = Some(Symbol.mkDefnSym("f"))))
    expectError[EntryPointError.MainEntryPointNotFound](result, allowUnknown = true)
  }

  test("Test.DefaultHandlerNotInEffectModule.01") {
    val input =
      """
        |pub eff E{
        |   def op(): Unit
        |}
        |@DefaultHandler
        |pub def runWithIO(f: Unit -> a \ ef): a \ (ef - E) + IO =
        |            run {
        |                f()
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |def main(): Unit = ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.DefaultHandlerNotInEffectModule](result)
  }

  test("Test.WrongSignatureForDefaultHandler.01") {
    val input =
      """
        |pub eff E{
        |   def op(): Unit
        |}
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(): a \ (ef - E) + IO =
        |            run {
        |                f()
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.WrongSignatureForDefaultHandler](result)
  }

  test("Test.WrongSignatureForDefaultHandler.02") {
    val input =
      """
        |pub eff E{
        |   def op(): Unit
        |}
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> a \ ef, u: a): a \ (ef - E) + IO =
        |            run {
        |                f()
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.WrongSignatureForDefaultHandler](result)
  }

  test("Test.WrongSignatureForDefaultHandler.03") {
    val input =
      """
        |pub eff E{
        |   def op(): Unit
        |}
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: a): a \ (ef - E) + IO =
        |            checked_ecast(f)
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.WrongSignatureForDefaultHandler](result)
  }

  test("Test.WrongSignatureForDefaultHandler.04") {
    val input =
      """
        |pub eff E{
        |   def op(): Unit
        |}
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> a \ ef): Bool \ (ef - E) + IO =
        |            run {
        |                true
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.WrongSignatureForDefaultHandler](result)
  }

  test("Test.WrongSignatureForDefaultHandler.05") {
    val input =
      """
        |pub eff E{
        |   def op(): Unit
        |}
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> a \ {}): a \ IO =
        |            run {
        |                f()
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.WrongSignatureForDefaultHandler](result)
  }

  test("Test.WrongSignatureForDefaultHandler.06") {
    val input =
      """
        |pub eff E{
        |   def op(): Unit
        |}
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Bool -> a \ ef, u: a): a \ (ef - E) + IO =
        |            run {
        |                f(true)
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.WrongSignatureForDefaultHandler](result)
  }

  test("Test.WrongSignatureForDefaultHandler.07") {
    val input =
      """
        |pub eff E1{
        |   def op(): Unit
        |}
        |pub eff E2{
        |   def op(): Unit
        |}
        |mod E1 {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - E1) + IO + E2 =
        |            run {
        |                f()
        |            } with handler E1 {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.WrongSignatureForDefaultHandler](result)
  }

  test("Test.DuplicatedDefaultHandlers.01") {
    val input =
      """
        |pub eff E{
        |   def op(): Unit
        |}
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - E) + IO =
        |            run {
        |                f()
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |    @DefaultHandler
        |    pub def runWithIO2(f: Unit -> a \ ef): a \ (ef - E) + IO =
        |            run {
        |                f()
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour 2");
        |                    k()
        |                }
        |            }
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[EntryPointError.DuplicatedDefaultHandlers](result)
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
        |def main(): Int64 \ NonDet = checked_ecast(42i64)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectSuccess(result)
  }

  test("Test.ValidEntryPoint.Main.04") {
    val input =
      """
        |def main(): Int64 \ {NonDet, IO} = checked_ecast(42i64)
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

  test("Test.ValidEntryPoint.LibNix.01") {
    val input =
      """
        |def main(): Unit = ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectSuccess(result)
  }

  test("Test.ValidEntryPoint.DefaultHandler.01") {
    val input =
      """
        |pub eff E{
        |   def op(): Unit
        |}
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - E) + IO =
        |            run {
        |                f()
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectSuccess(result)
  }
}
