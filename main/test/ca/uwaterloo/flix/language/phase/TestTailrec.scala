/*
 * Copyright 2016 Magnus Madsen
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestTailrec extends FunSuite {

  val opts = Options.DefaultTest

  test("Tailrec.Countdown (of 1)") {
    val input =
      """def r(): Int = f(1)
        |
        |def f(i: Int): Int =
        |  if (i == 0) 0 else f(i - 1)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(0)(result.get.getConstant("r"))
  }

  test("Tailrec.Countdown (of 5)") {
    val input =
      """def r(): Int = f(5)
        |
        |def f(i: Int): Int =
        |  if (i == 0) 0 else f(i - 1)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(0)(result.get.getConstant("r"))
  }

  test("Tailrec.Countdown (of 1_000_000)") {
    val input =
      """def r(): Int = f(1000000)
        |
        |def f(i: Int): Int =
        |  if (i == 0) 0 else f(i - 1)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(0)(result.get.getConstant("r"))
  }

  test("Tailrec.Countover (of 1)") {
    val input =
      """def r(): Int = f(1, 0)
        |
        |def f(i: Int, j: Int): Int =
        |  if (i == 0) j else f(i - 1, j + 1)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(1)(result.get.getConstant("r"))
  }

  test("Tailrec.Countover (of 5)") {
    val input =
      """def r(): Int = f(5, 0)
        |
        |def f(i: Int, j: Int): Int =
        |  if (i == 0) j else f(i - 1, j + 1)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(5)(result.get.getConstant("r"))
  }

  test("Tailrec.Countover (of 1_000_000)") {
    val input =
      """def r(): Int = f(1000000, 0)
        |
        |def f(i: Int, j: Int): Int =
        |  if (i == 0) j else f(i - 1, j + 1)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(1000000)(result.get.getConstant("r"))
  }

  test("Tailrec.Sum (of 1)") {
    val input =
      """def r(): Int = f(1, 0)
        |
        |def f(n: Int, m: Int): Int =
        |  if (n == 0) m else f(n - 1, m + n)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(1)(result.get.getConstant("r"))
  }

  test("Tailrec.Sum (of 2)") {
    val input =
      """def r(): Int = f(2, 0)
        |
        |def f(n: Int, m: Int): Int =
        |  if (n == 0) m else f(n - 1, m + n)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(2 + 1)(result.get.getConstant("r"))
  }

  test("Tailrec.Sum (of 3)") {
    val input =
      """def r(): Int = f(3, 0)
        |
        |def f(n: Int, m: Int): Int =
        |  if (n == 0) m else f(n - 1, m + n)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(3 + 2 + 1)(result.get.getConstant("r"))
  }

  test("Tailrec.Sum (of 4)") {
    val input =
      """def r(): Int = f(4, 0)
        |
        |def f(n: Int, m: Int): Int =
        |  if (n == 0) m else f(n - 1, m + n)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(4 + 3 + 2 + 1)(result.get.getConstant("r"))
  }

  test("Tailrec.Sum (of 5)") {
    val input =
      """def r(): Int = f(5, 0)
        |
        |def f(n: Int, m: Int): Int =
        |  if (n == 0) m else f(n - 1, m + n)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(5 + 4 + 3 + 2 + 1)(result.get.getConstant("r"))
  }

  test("Tailrec.Sum (of 10)") {
    val input =
      """def r(): Int = f(10, 0)
        |
        |def f(n: Int, m: Int): Int =
        |  if (n == 0) m else f(n - 1, m + n)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(10 + 9 + 8 + 7 + 6 + 5 + 4 + 3 + 2 + 1)(result.get.getConstant("r"))
  }

  test("Tailrec.Factorial (of 1)") {
    val input =
      """def r(): Int = f(1, 1)
        |
        |def f(n: Int, m: Int): Int =
        |  if (n <= 1) m else f(n - 1, m * n)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(1)(result.get.getConstant("r"))
  }

  test("Tailrec.Factorial (of 2)") {
    val input =
      """def r(): Int = f(2, 1)
        |
        |def f(n: Int, m: Int): Int =
        |  if (n <= 1) m else f(n - 1, m * n)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(2 * 1)(result.get.getConstant("r"))
  }

  test("Tailrec.Factorial (of 3)") {
    val input =
      """def r(): Int = f(3, 1)
        |
        |def f(n: Int, m: Int): Int =
        |  if (n <= 1) m else f(n - 1, m * n)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(3 * 2 * 1)(result.get.getConstant("r"))
  }

  test("Tailrec.Factorial (of 4)") {
    val input =
      """def r(): Int = f(4, 1)
        |
        |def f(n: Int, m: Int): Int =
        |  if (n <= 1) m else f(n - 1, m * n)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(4 * 3 * 2 * 1)(result.get.getConstant("r"))
  }

  test("Tailrec.Factorial (of 5)") {
    val input =
      """def r(): Int = f(5, 1)
        |
        |def f(n: Int, m: Int): Int =
        |  if (n <= 1) m else f(n - 1, m * n)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(5 * 4 * 3 * 2 * 1)(result.get.getConstant("r"))
  }

  test("Tailrec.Factorial (of 10)") {
    val input =
      """def r(): Int = f(10, 1)
        |
        |def f(n: Int, m: Int): Int =
        |  if (n <= 1) m else f(n - 1, m * n)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1)(result.get.getConstant("r"))
  }

  test("Tailrec.GreatestCommonDivisor (of 8, 16)") {
    val input =
      """def r(): Int = gcd(8, 16)
        |
        |def gcd(n: Int, m: Int): Int = switch {
        |  case n == m => n
        |  case n >= m => gcd(n - m, m)
        |  case n <= m => gcd(n, m - n)
        |}
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(8)(result.get.getConstant("r"))
  }

  test("Tailrec.GreatestCommonDivisor (of 16, 8)") {
    val input =
      """def r(): Int = gcd(16, 8)
        |
        |def gcd(n: Int, m: Int): Int = switch {
        |  case n == m => n
        |  case n >= m => gcd(n - m, m)
        |  case n <= m => gcd(n, m - n)
        |}
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(8)(result.get.getConstant("r"))
  }

  test("Tailrec.GreatestCommonDivisor (of 42, 56)") {
    val input =
      """def r(): Int = gcd(42, 56)
        |
        |def gcd(n: Int, m: Int): Int = switch {
        |  case n == m => n
        |  case n >= m => gcd(n - m, m)
        |  case n <= m => gcd(n, m - n)
        |}
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(14)(result.get.getConstant("r"))
  }

  test("Tailrec.GreatestCommonDivisor (of 56, 42)") {
    val input =
      """def r(): Int = gcd(56, 42)
        |
        |def gcd(n: Int, m: Int): Int = switch {
        |  case n == m => n
        |  case n >= m => gcd(n - m, m)
        |  case n <= m => gcd(n, m - n)
        |}
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(14)(result.get.getConstant("r"))
  }

  test("Tailrec.GreatestCommonDivisor (of 24, 468)") {
    val input =
      """def r(): Int = gcd(24, 468)
        |
        |def gcd(n: Int, m: Int): Int = switch {
        |  case n == m => n
        |  case n >= m => gcd(n - m, m)
        |  case n <= m => gcd(n, m - n)
        |}
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(12)(result.get.getConstant("r"))
  }

  test("Tailrec.GreatestCommonDivisor (of 468, 24)") {
    val input =
      """def r(): Int = gcd(468, 24)
        |
        |def gcd(n: Int, m: Int): Int = switch {
        |  case n == m => n
        |  case n >= m => gcd(n - m, m)
        |  case n <= m => gcd(n, m - n)
        |}
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(12)(result.get.getConstant("r"))
  }

  test("Tailrec.Fibonacci (of 1)") {
    val input =
      """def r(): Int = fib(1, 0, 1)
        |
        |def fib(n: Int, a: Int, b: Int): Int =
        |  if (n == 0) b else fib(n - 1, a + b, a)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(0)(result.get.getConstant("r"))
  }

  test("Tailrec.Fibonacci (of 2)") {
    val input =
      """def r(): Int = fib(2, 0, 1)
        |
        |def fib(n: Int, a: Int, b: Int): Int =
        |  if (n == 0) b else fib(n - 1, a + b, a)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(1)(result.get.getConstant("r"))
  }

  test("Tailrec.Fibonacci (of 3)") {
    val input =
      """def r(): Int = fib(3, 0, 1)
        |
        |def fib(n: Int, a: Int, b: Int): Int =
        |  if (n == 0) b else fib(n - 1, a + b, a)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(1)(result.get.getConstant("r"))
  }

  test("Tailrec.Fibonacci (of 4)") {
    val input =
      """def r(): Int = fib(4, 0, 1)
        |
        |def fib(n: Int, a: Int, b: Int): Int =
        |  if (n == 0) b else fib(n - 1, a + b, a)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(2)(result.get.getConstant("r"))
  }

  test("Tailrec.Fibonacci (of 5)") {
    val input =
      """def r(): Int = fib(5, 0, 1)
        |
        |def fib(n: Int, a: Int, b: Int): Int =
        |  if (n == 0) b else fib(n - 1, a + b, a)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(3)(result.get.getConstant("r"))
  }

  test("Tailrec.Fibonacci (of 10)") {
    val input =
      """def r(): Int = fib(10, 0, 1)
        |
        |def fib(n: Int, a: Int, b: Int): Int =
        |  if (n == 0) b else fib(n - 1, a + b, a)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(34)(result.get.getConstant("r"))
  }

  test("Tailrec.Fibonacci (of 25)") {
    val input =
      """def r(): Int = fib(25, 0, 1)
        |
        |def fib(n: Int, a: Int, b: Int): Int =
        |  if (n == 0) b else fib(n - 1, a + b, a)
      """.stripMargin
    val result = new Flix().setOptions(opts).addStr(input).solve()
    assertResult(46368)(result.get.getConstant("r"))
  }

}
