/*
 * Copyright 2026 Magnus Madsen
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
import ca.uwaterloo.flix.language.errors.DefaultHandlerError
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestDefaultHandlers extends AnyFunSuite with TestUtils {

  test("Test.DefaultHandlerNotInModule.01") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
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
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DefaultHandlerError.DefaultHandlerNotInModule](result)
  }

  test("Test.IllegalDefaultHandlerArity.01") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
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
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DefaultHandlerError.IllegalDefaultHandlerArity](result)
  }

  test("Test.IllegalDefaultHandlerArity.02") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> a \ ef, _x: Int32, _y: Int32): a \ (ef - E) + IO =
        |            run {
        |                f()
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DefaultHandlerError.IllegalDefaultHandlerArity](result)
  }

  test("Test.IllegalDefaultHandlerParameter.01") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
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
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DefaultHandlerError.IllegalDefaultHandlerParameter](result)
  }

  test("Test.IllegalDefaultHandlerParameter.02") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: a): a \ (ef - E) + IO =
        |            checked_ecast(f)
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DefaultHandlerError.IllegalDefaultHandlerParameter](result)
  }

  test("Test.IllegalDefaultHandlerParameter.03") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
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
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DefaultHandlerError.IllegalDefaultHandlerParameter](result)
  }

  test("Test.IllegalDefaultHandlerParameter.04") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Bool -> a \ ef): a \ (ef - E) + IO =
        |            run {
        |                f(true)
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DefaultHandlerError.IllegalDefaultHandlerParameter](result)
  }

  test("Test.IllegalDefaultHandlerParameter.05") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> Int32 \ ef): Int32 \ (ef - E) + IO =
        |            run {
        |                f()
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DefaultHandlerError.IllegalDefaultHandlerParameter](result)
  }

  test("Test.IllegalDefaultHandlerReturnType.01") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
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
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DefaultHandlerError.IllegalDefaultHandlerReturnType](result)
  }

  test("Test.IllegalDefaultHandlerReturnType.02") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> a \ ef): b \ (ef - E) + IO =
        |            run {
        |                unchecked_cast(f() as b)
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DefaultHandlerError.IllegalDefaultHandlerReturnType](result)
  }

  test("Test.DefaultHandlerDoesNotHandleEffect.01") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> a \ ef): a \ ef + IO = {
        |        println("Default behaviour");
        |        f()
        |    }
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DefaultHandlerError.DefaultHandlerDoesNotHandleEffect](result)
  }

  test("Test.DefaultHandlerDoesNotHandleEffect.02") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> a \ ef): a \ IO =
        |            run {
        |                f()
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DefaultHandlerError.DefaultHandlerDoesNotHandleEffect](result)
  }

  test("Test.IllegalDefaultHandlerEffectArguments.01") {
    val input =
      """
        |pub eff E[t] {
        |   def op(x: t): Unit
        |}
        |
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - E[Int32]) + IO =
        |        run f() with handler E {
        |            def op(_x, k) = k()
        |        }
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DefaultHandlerError.IllegalDefaultHandlerEffectArguments](result)
  }

  test("Test.IllegalDefaultHandlerEffectArguments.02") {
    val input =
      """
        |pub eff E[t] {
        |   def op(x: t): Unit
        |}
        |
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - E[a]) + IO =
        |        run f() with handler E {
        |            def op(_x, k) = k()
        |        }
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DefaultHandlerError.IllegalDefaultHandlerEffectArguments](result)
  }

  test("Test.IllegalDefaultHandlerEffectArguments.03") {
    val input =
      """
        |pub eff E[s, t] {
        |   def op(x: s, y: t): Unit
        |}
        |
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - E[t, t]) + IO =
        |        run f() with handler E {
        |            def op(_x, _y, k) = k()
        |        }
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DefaultHandlerError.IllegalDefaultHandlerEffectArguments](result)
  }

  test("Test.IllegalDefaultHandlerEffect.01") {
    val input =
      """
        |pub eff E1 {
        |   def op(): Unit
        |}
        |
        |pub eff E2 {
        |   def op(): Unit
        |}
        |
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
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DefaultHandlerError.IllegalDefaultHandlerEffect](result)
  }

  test("Test.IllegalDefaultHandlerEffect.02") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - E) =
        |        run f() with handler E {
        |            def op(k) = k()
        |        }
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DefaultHandlerError.IllegalDefaultHandlerEffect](result)
  }

  test("Test.IllegalDefaultHandlerEffect.03") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> a \ ef): a \ ef + E + IO =
        |            run {
        |                f()
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DefaultHandlerError.IllegalDefaultHandlerEffect](result)
  }

  test("Test.IllegalDefaultHandlerConstraint.01") {
    val input =
      """
        |pub eff E[t] {
        |   def op(x: t): Unit
        |}
        |
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - E[t]) + IO with Eq[t] =
        |        run f() with handler E {
        |            def op(_x, k) = k()
        |        }
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DefaultHandlerError.IllegalDefaultHandlerConstraint](result)
  }

  test("Test.IllegalDefaultHandlerConstraint.02") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - E) + IO with ToString[a] =
        |            run {
        |                f()
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DefaultHandlerError.IllegalDefaultHandlerConstraint](result)
  }

  test("Test.IllegalDefaultHandlerConstraint.03") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
        |pub trait C[a] {
        |    type T: Type
        |}
        |
        |mod E {
        |    @DefaultHandler
        |    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - E) + IO with C[a] where C.T[a] ~ Int32 =
        |            run {
        |                f()
        |            } with handler E {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DefaultHandlerError.IllegalDefaultHandlerConstraint](result)
  }

  test("Test.NonPublicDefaultHandler.01") {
    val input =
      """
        |pub eff E1 {
        |   def op(): Unit
        |}
        |
        |mod E1 {
        |    @DefaultHandler
        |    def runWithIO(f: Unit -> a \ ef): a \ (ef - E1) + IO =
        |            run {
        |                f()
        |            } with handler E1 {
        |                def op(k) = {
        |                    println("Default behaviour");
        |                    k()
        |                }
        |            }
        |}
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DefaultHandlerError.NonPublicDefaultHandler](result)
  }

  test("Test.DuplicateDefaultHandler.01") {
    val input =
      """
        |pub eff E {
        |   def op(): Unit
        |}
        |
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
        |
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
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DefaultHandlerError.DuplicateDefaultHandler](result)
  }

}
