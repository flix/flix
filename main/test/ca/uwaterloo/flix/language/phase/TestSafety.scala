/*
 * Copyright 2018 Magnus Madsen
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
import ca.uwaterloo.flix.language.errors.SafetyError
import ca.uwaterloo.flix.language.errors.SafetyError.{IllegalNegativelyBoundWildVariable, IllegalNegativelyBoundWildcard, IllegalNonPositivelyBoundVariable, IllegalRelationalUseOfLatticeVariable}
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestSafety extends FunSuite with TestUtils {

  val DefaultOptions: Options = Options.TestWithLibMin

  test("NonPositivelyBoundVariable.01") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Int32) } = #{
        |    A(x) :- not B(x).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[IllegalNonPositivelyBoundVariable](result)
  }

  test("NonPositivelyBoundVariable.02") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Int32), R(Int32) } = solve #{
        |    R(x) :- not A(x), not B(x).
        |}
    """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[IllegalNonPositivelyBoundVariable](result)
  }

  test("NonPositivelyBoundVariable.03") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Int32), R(Int32) } = #{
        |    R(x) :- not A(x), B(12), if x > 5.
        |}
    """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[IllegalNonPositivelyBoundVariable](result)
  }

  test("NegativelyBoundWildVariable.01") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Int32), R(Int32) } = #{
        |    R(x) :- A(x), not B(_y).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[IllegalNegativelyBoundWildVariable](result)
  }


  test("NegativelyBoundWildVariable.02") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Int32), R(Int32) } = solve #{
        |    R(1) :- not A(_x), not B(_y).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[IllegalNegativelyBoundWildVariable](result)
  }

  test("NegativelyBoundWildVariable.03") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Int32), R(Int32) } = #{
        |    R(1) :- A(y), not A(_y), not B(y).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[IllegalNegativelyBoundWildVariable](result)
  }

  test("NegativelyBoundWildcard.01") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Int32) } = #{
        |    A(1) :- not B(_).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[IllegalNegativelyBoundWildcard](result)
  }

  test("NegativelyBoundWildcard.02") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Int32) } = solve #{
        |    A(1) :- not B(_), A(_).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[IllegalNegativelyBoundWildcard](result)
  }

  test("NegativelyBoundWildcard.03") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Int32) } = #{
        |    A(1) :- not B(z), A(z), not B(_).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[IllegalNegativelyBoundWildcard](result)
  }

  test("UseOfLatticeVariable.01") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Int32; Int32) } = #{
        |    A(x: Int32) :- B(12; x).
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[IllegalRelationalUseOfLatticeVariable](result)
  }

  test("UseOfLatticeVariable.02") {
    val input =
      """
        |pub def f(): #{A(Int32), B(Int32; Int32), C(Int32, Int32) } = #{
        |    A(x) :- B(x; l), C(x, l).
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[IllegalRelationalUseOfLatticeVariable](result)
  }

  test("UseOfLatticeVariable.03") {
    val input =
      """
        |pub def f(): #{A(Int32; Int32), B(Int32; Int32), C(Int32, Int32) } = #{
        |    A(x; l) :- B(x; l), C(x, l).
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[IllegalRelationalUseOfLatticeVariable](result)
  }

  test("UseOfLatticeVariable.04") {
    val input =
      """
        |pub def f(): #{A(Int32, Int32), B(Int32; Int32), C(Int32; Int32) } = #{
        |    A(12, l) :- B(12; l), fix C(12; l).
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[IllegalRelationalUseOfLatticeVariable](result)
  }

  test("UseOfLatticeVariable.05") {
    val input =
      """
        |pub def f(): #{A(Int32), B(Int32, Int32), C(Int32; Int32) } = #{
        |    A(12) :- B(12, l), fix C(12; l).
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[IllegalRelationalUseOfLatticeVariable](result)
  }

  test("TestIllegalDerivation.01") {
    val input =
      """
        |def f(): ##java.lang.Thread & Impure = object ##java.lang.Thread {}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.IllegalObjectDerivation](result)
  }

  test("TestInvalidThis.01") {
    val input =
      """
        |def f(): ##java.lang.Runnable & Impure = 
        |  object ##java.lang.Runnable {
        |    def run(): Unit & Impure = ()
        |  }
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.IllegalThisType](result)
  }

  test("TestInvalidThis.02") {
    val input =
      """
        |def f(): ##java.lang.Runnable & Impure = 
        |  object ##java.lang.Runnable {
        |    def run(_this: Int32): Unit & Impure = ()
        |  }
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.IllegalThisType](result)
  }

  test("TestUnimplementedMethod.01") {
    val input =
      """
        |def f(): ##java.lang.Runnable & Impure = object ##java.lang.Runnable {}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.UnimplementedMethod](result)
  }

  test("TestExtraMethod.01") {
    val input =
      """
        |def f(): ##java.lang.Runnable & Impure = 
        |  object ##java.lang.Runnable {
        |    def run(_this: ##java.lang.Runnable): Unit & Impure = ()
        |    def anExtraMethod(_this: ##java.lang.Runnable): Unit & Impure = ()
        |  }
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ExtraMethod](result)
  }
}
