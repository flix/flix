/*
 * Copyright 2021 Matthew Lutze
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
import ca.uwaterloo.flix.language.errors.ScopeError
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestScoper extends FunSuite with TestUtils {

  private val DefaultOptions = Options.TestWithLibNix

  test("Test.Scoped.01") {
    val input =
      """
        |def f(scoped x: a): a = x
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ScopeError](result)
  }

  test("Test.Scoped.02") {
    val input =
      """
        |def f(): a = {
        |  let scoped x = default;
        |  x
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ScopeError](result)
  }

  test("Test.Scoped.03") {
    val input =
      """
        |def f(): a -> a = scoped x -> x
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ScopeError](result)
  }

  test("Test.Scoped.Ref.01") {
    val input =
      """
        |def f(scoped x: a): a & Impure =
        |  let r = ref x;
        |  deref r
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ScopeError](result)
  }

  test("Test.Scoped.Ref.02") {
    val input =
      """
        |def f(scoped x: a): a & Impure = {
        |  let r = ref default;
        |  r := x;
        |  deref r
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ScopeError](result)
  }

  test("Test.Scoped.Array.01") {
    val input =
      """
        |def f(scoped x: a): a & Impure = {
        |  let ar = [x];
        |  ar[0]
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ScopeError](result)
  }

  test("Test.Scoped.Array.02") {
    val input =
      """
        |def f(scoped x: a): a & Impure = {
        |  let ar = [x; 5];
        |  ar[0]
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ScopeError](result)
  }

  test("Test.Scoped.Array.03") {
    val input =
      """
        |def f(scoped x: a): a & Impure = {
        |  let ar = [default; 5];
        |  ar[0] = x;
        |  ar[0]
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ScopeError](result)
  }

  test("Test.Scoped.Channel.01") {
    val input =
      """
        |def f(scoped x: a): a & Impure = {
        |  let c = chan a 0;
        |  spawn c <- x;
        |  <- c
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ScopeError](result)
  }

  test("Test.Scoped.Tuple.01") {
    val input =
      """
        |def f(scoped x: a): (a, a) = (x, x)
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ScopeError](result)
  }

  test("Test.Scoped.Tuple.02") {
    val input =
      """
        |def f(scoped x: (a, a)): a = match x {
        |  case (y, z) => y
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ScopeError](result)
  }

  test("Test.Scoped.Enum.01") {
    val input =
      """
        |enum Box[a] {
        |  case Box(a)
        |}
        |
        |def f(scoped x: a): Box[a] = Box(x)
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ScopeError](result)
  }

  test("Test.Scoped.Enum.02") {
    val input =
      """
        |enum Box[a] {
        |  case Box(a)
        |}
        |
        |def f(scoped x: Box[a]): a = match x {
        |  case Box(y) => y
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ScopeError](result)
  }
}
