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
import ca.uwaterloo.flix.language.errors.SafetyError.{IllegalNegativelyBoundWildcard, IllegalNonPositivelyBoundVariable, IllegalRelationalUseOfLatticeVariable, UnexpectedPatternInBodyAtom}
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestSafety extends FunSuite with TestUtils {

  val DefaultOptions: Options = Options.TestWithLibMin

  test("UnexpectedBodyAtomPattern.01") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Option[Int32]) } = #{
        |    A(x) :- B(Some(x)).
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[UnexpectedPatternInBodyAtom](result)
  }

  test("UnexpectedBodyAtomPattern.02") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Option[Int32]) } = #{
        |    A(1) :- B(Some(2)).
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[UnexpectedPatternInBodyAtom](result)
  }

  test("UnexpectedBodyAtomPattern.03") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Option[Int32]) } = #{
        |    A(1) :- B(None).
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[UnexpectedPatternInBodyAtom](result)
  }

  test("UnexpectedBodyAtomPattern.04") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Unit), C(List[Int32]) } = #{
        |    A(x) :- B(()), C(x::_).
        |}
    """.stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[UnexpectedPatternInBodyAtom](result)
  }

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

  test("NonPositivelyBoundVariable.04") {
    val input =
      """
        |def main(): Unit =
        |    let f = (x, y) -> Vector#{(x, y)};
        |    let _ = #{
        |        R(0, 0, 0, 0).
        |        A(1, 2).
        |        R(x, y, u, v) :- A(x, y), let (u, v) = f(u, v).
        |    };
        |    ()
    """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[IllegalNonPositivelyBoundVariable](result)
  }

  // TODO NS-REFACTOR find out if wildcard and wild variable are different
  test("NegativelyBoundWildVariable.01") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Int32), R(Int32) } = #{
        |    R(x) :- A(x), not B(_y).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[IllegalNegativelyBoundWildcard](result)
  }


  // TODO NS-REFACTOR find out if wildcard and wild variable are different
  test("NegativelyBoundWildVariable.02") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Int32), R(Int32) } = solve #{
        |    R(1) :- not A(_x), not B(_y).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[IllegalNegativelyBoundWildcard](result)
  }

  // TODO NS-REFACTOR find out if wildcard and wild variable are different
  test("NegativelyBoundWildVariable.03") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Int32), R(Int32) } = #{
        |    R(1) :- A(y), not A(_y), not B(y).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[IllegalNegativelyBoundWildcard](result)
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

  test("TestInvalidThis.01") {
    val input =
      """
        |def f(): ##java.lang.Runnable \ IO =
        |  new ##java.lang.Runnable {
        |    def run(): Unit \ IO = ()
        |  }
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.MissingThis](result)
  }

  test("TestInvalidThis.02") {
    val input =
      """
        |def f(): ##java.lang.Runnable \ IO =
        |  new ##java.lang.Runnable {
        |    def run(_this: Int32): Unit \ IO = ()
        |  }
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.IllegalThisType](result)
  }

  test("TestUnimplementedMethod.01") {
    val input =
      """
        |def f(): ##java.lang.Runnable \ IO = new ##java.lang.Runnable {}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.UnimplementedMethod](result)
  }

  test("TestExtraMethod.01") {
    val input =
      """
        |def f(): ##java.lang.Runnable \ IO =
        |  new ##java.lang.Runnable {
        |    def run(_this: ##java.lang.Runnable): Unit \ IO = ()
        |    def anExtraMethod(_this: ##java.lang.Runnable): Unit \ IO = ()
        |  }
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.ExtraMethod](result)
  }

  test("TestNonDefaultConstructor.01") {
    val input =
      """
        |def f(): ##dev.flix.test.TestClassWithNonDefaultConstructor \ IO =
        |  new ##dev.flix.test.TestClassWithNonDefaultConstructor {
        |  }
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.MissingPublicZeroArgConstructor](result)
  }

  test("TestNonPublicInterface.01") {
    val input =
      """
        |def f(): ##dev.flix.test.TestNonPublicInterface \ IO =
        |  new ##dev.flix.test.TestNonPublicInterface {
        |  }
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.NonPublicClass](result)
  }

  test("TestIllegalParExpression.01") {
    val input =
      """
        |def f(): Int32 =
        |    par 1
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.IllegalParExpression](result)
  }

  test("TestIllegalParExpression.02") {
    val input =
      """
        |def f(): Int32 =
        |    par par f()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.IllegalParExpression](result)
  }

  test("TestMissingDefaultMatchTypeCase.01") {
    val input =
      """
        |def f(): Bool = typematch () {
        |    case _: Unit => true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.MissingDefaultMatchTypeCase](result)
  }

  test("TestMissingDefaultMatchTypeCase.02") {
    val input =
      """
        |def f(x: a): Bool = typematch x {
        |    case _: a => true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.MissingDefaultMatchTypeCase](result)
  }

  test("UnableToDeriveSendable.01") {
    val input =
      """
        |enum Enum1[r: Region](Array[Int32, r]) with Sendable
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.SendableError](result)
  }

  test("UnableToDeriveSendable.02") {
    val input =
      """
        |instance Sendable[Array[a, r]]
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.SendableError](result)
  }

  test("ImpossibleCast.01") {
    val input =
      """
        |def f(): Bool = unchecked_cast("true" as Bool)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleCast](result)
  }

  test("ImpossibleCast.02") {
    val input =
      """
        |def f(): String = unchecked_cast(true as String)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleCast](result)
  }

  test("ImpossibleCast.03") {
    val input =
      """
        |enum A(Bool)
        |def f(): A = unchecked_cast(true as A)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleCast](result)
  }

  test("ImpossibleCast.04") {
    val input =
      """
        |enum A(Bool)
        |def f(): Bool = unchecked_cast(A.A(false) as Bool)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleCast](result)
  }

  test("ImpossibleCast.05") {
    val input =
      """
        |enum A(Int32)
        |def f(): A = unchecked_cast(1 as A)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleCast](result)
  }

  test("ImpossibleCast.06") {
    val input =
      """
        |enum A(Int32)
        |def f(): Int32 = unchecked_cast(A.A(1) as Int32)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleCast](result)
  }

  test("ImpossibleCast.07") {
    val input =
      """
        |enum A(String)
        |def f(): A = unchecked_cast("a" as A)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleCast](result)
  }

  test("ImpossibleCast.08") {
    val input =
      """
        |enum A(String)
        |def f(): String = unchecked_cast(A.A("a") as String)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleCast](result)
  }

}
