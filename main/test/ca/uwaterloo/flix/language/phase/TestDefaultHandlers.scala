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

  test("Test.DuplicateHandler.01") {
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
    expectError[DefaultHandlerError.DuplicateHandler](result)
  }

  test("Test.IllegalArity.01") {
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
    expectError[DefaultHandlerError.IllegalArity](result)
  }

  test("Test.IllegalArity.02") {
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
    expectError[DefaultHandlerError.IllegalArity](result)
  }

  test("Test.IllegalConstraint.01") {
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
    expectError[DefaultHandlerError.IllegalConstraint](result)
  }

  test("Test.IllegalConstraint.02") {
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
    expectError[DefaultHandlerError.IllegalConstraint](result)
  }

  test("Test.IllegalConstraint.03") {
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
    expectError[DefaultHandlerError.IllegalConstraint](result)
  }

  test("Test.IllegalEffect.01") {
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
    expectError[DefaultHandlerError.IllegalEffect](result)
  }

  test("Test.IllegalEffect.02") {
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
    expectError[DefaultHandlerError.IllegalEffect](result)
  }

  test("Test.IllegalEffect.03") {
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
    expectError[DefaultHandlerError.IllegalEffect](result)
  }

  test("Test.IllegalEffectArguments.01") {
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
    expectError[DefaultHandlerError.IllegalEffectArguments](result)
  }

  test("Test.IllegalEffectArguments.02") {
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
    expectError[DefaultHandlerError.IllegalEffectArguments](result)
  }

  test("Test.IllegalEffectArguments.03") {
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
    expectError[DefaultHandlerError.IllegalEffectArguments](result)
  }

  test("Test.IllegalParameterType.01") {
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
    expectError[DefaultHandlerError.IllegalParameterType](result)
  }

  test("Test.IllegalParameterType.02") {
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
    expectError[DefaultHandlerError.IllegalParameterType](result)
  }

  test("Test.IllegalParameterType.03") {
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
    expectError[DefaultHandlerError.IllegalParameterType](result)
  }

  test("Test.IllegalParameterType.04") {
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
    expectError[DefaultHandlerError.IllegalParameterType](result)
  }

  test("Test.IllegalParameterType.05") {
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
    expectError[DefaultHandlerError.IllegalParameterType](result)
  }

  test("Test.IllegalReturnType.01") {
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
    expectError[DefaultHandlerError.IllegalReturnType](result)
  }

  test("Test.IllegalReturnType.02") {
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
    expectError[DefaultHandlerError.IllegalReturnType](result)
  }

  test("Test.MissingHandledEffect.01") {
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
    expectError[DefaultHandlerError.MissingHandledEffect](result)
  }

  test("Test.MissingHandledEffect.02") {
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
    expectError[DefaultHandlerError.MissingHandledEffect](result)
  }

  test("Test.NonPublicHandler.01") {
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
    expectError[DefaultHandlerError.NonPublicHandler](result)
  }

  test("Test.NotInCompanionModule.01") {
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
    expectError[DefaultHandlerError.NotInCompanionModule](result)
  }

}
