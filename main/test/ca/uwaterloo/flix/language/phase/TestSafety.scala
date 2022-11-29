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

  test("TestUpcast.01") {
    val input =
      """
        |def f(): Unit =
        |    let _ =
        |        if (true)
        |            upcast ()
        |        else
        |            1;
        |    ()
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.UnsafeUpcast](result)
  }

  test("TestUpcast.02") {
    val input =
      """
        |def f(): Unit =
        |    let _ =
        |        if (true)
        |            upcast x -> { println(x); x + 1 }
        |        else
        |            x -> x + 1;
        |    ()
        |""".stripMargin

    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.UnsafeUpcast](result)
  }

  test("TestUpcast.03") {
    val input =
      """
        |def f(): Unit & ef =
        |    let f =
        |        if (true)
        |            upcast x -> (unsafe_cast x + 1 as & ef)
        |        else
        |            upcast x -> x + 1;
        |    let _ = f(1);
        |    ()
        |""".stripMargin

    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.UnsafeUpcast](result)
  }

  test("TestUpcast.04") {
    val input =
      """
        |def f(): Unit =
        |    let _ =
        |        if (true)
        |            upcast (1, "a")
        |        else
        |            (1, 1);
        |    ()
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.UnsafeUpcast](result)
  }

  test("TestUpcast.05") {
    val input =
      """
        |def f(): Unit =
        |    let _ =
        |        if (true)
        |            upcast (1, "a")
        |        else
        |            upcast (1, 1);
        |    ()
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.UnsafeUpcast](result)
  }

  test("TestUpcast.06") {
    val input =
      """
        |def f(): Unit & Impure =
        |    import new java.lang.StringBuilder(): ##java.lang.StringBuilder & Impure as newStringBuilder;
        |    import new java.lang.Object(): ##java.lang.Object & Impure as newObject;
        |    let _ =
        |        if (true)
        |            upcast (newObject(), newObject())
        |        else
        |            (newObject(), newStringBuilder());
        |    ()
        |""".stripMargin

    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.UnsafeUpcast](result)
  }

  test("TestUpcast.07") {
    val input =
      """
        |pub eff A
        |pub eff B
        |pub eff C
        |
        |def f(): Unit =
        |    let f = () -> ();
        |    let g = () -> unsafe_cast () as \ { A, B, C };
        |    let _ =
        |        if (true)
        |            f
        |        else
        |            upcast g;
        |    ()
        |
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.UnsafeUpcast](result)
  }

  test("TestUpcast.08") {
    val input =
      """
        |pub eff A
        |pub eff B
        |pub eff C
        |
        |def f(): Unit =
        |    let f = () -> unsafe_cast () as \ A;
        |    let g = () -> unsafe_cast () as \ { A, B, C };
        |    let _ =
        |        if (true)
        |            f
        |        else
        |            upcast g;
        |    ()
        |
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.UnsafeUpcast](result)
  }

  test("TestUpcast.09") {
    val input =
      """
        |pub eff A
        |pub eff B
        |pub eff C
        |
        |def f(): Unit =
        |    let f = () -> unsafe_cast () as & Impure \ A;
        |    let g = () -> unsafe_cast () as \ { A, B, C };
        |    let _ =
        |        if (true)
        |            upcast f
        |        else
        |            g;
        |    ()
        |
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.UnsafeUpcast](result)
  }

  test("TestUpcast.10") {
    val input =
      """
        |pub eff A
        |pub eff B
        |pub eff C
        |
        |def f(): Unit =
        |    let f = () -> unsafe_cast () as \ A;
        |    let g = () -> unsafe_cast () as \ { A, B, C };
        |    let _ =
        |        if (true)
        |            upcast f
        |        else
        |            upcast g;
        |    ()
        |
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.UnsafeUpcast](result)
  }

  test("TestUpcast.11") {
    val input =
      """
        |pub eff A
        |pub eff B
        |pub eff C
        |
        |def f(): Unit =
        |    let f = () -> unsafe_cast () as & Impure \ A;
        |    let g = () -> unsafe_cast () as \ { A, B, C };
        |    let _ =
        |        if (true)
        |            upcast f
        |        else
        |            upcast g;
        |    ()
        |
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.UnsafeUpcast](result)
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

  test("TestSupercast.01") {
    val input =
      """
        |def f(): Unit \ Impure =
        |    import new java.lang.Object(): ##java.lang.Object \ Impure as newObject;
        |    let _ =
        |        if (true)
        |            super_cast (unsafe_cast 1 as \ Impure)
        |        else
        |            newObject();
        |    ()
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.FromNonJavaTypeSupercast](result)
  }

  test("TestSupercast.02") {
    val input =
      """
        |def f(): Unit \ Impure =
        |    import new java.lang.Object(): ##java.lang.Object \ Impure as newObject;
        |    let _ =
        |        if (true)
        |            unsafe_cast 1 as \ Impure
        |        else
        |            super_cast newObject();
        |    ()
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ToNonJavaTypeSupercast](result)
  }

  test("TestSupercast.03") {
    val input =
      """
        |def f(): Unit \ Impure =
        |    import new java.lang.Object(): ##java.lang.Object \ Impure as newObject;
        |    import new java.lang.StringBuilder(): ##java.lang.StringBuilder \ Impure as newStringBuilder;
        |    let _ =
        |        if (true)
        |            super_cast newObject()
        |        else
        |            newStringBuilder();
        |    ()
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.UnsafeSupercast](result)
  }

  test("TestSupercast.04") {
    val input =
      """
        |def f(): Unit \ Impure =
        |    import new java.lang.Object(): ##java.lang.Object \ Impure as newObject;
        |    import new java.lang.StringBuilder(): ##java.lang.StringBuilder \ Impure as newStringBuilder;
        |    let _ =
        |        if (true)
        |            super_cast newObject()
        |        else
        |            super_cast newStringBuilder();
        |    ()
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ToTypeVariableSupercast](result)
  }

  test("TestSupercast.05") {
    val input =
      """
        |def f(): Unit \ Impure =
        |    import new java.lang.StringBuilder(): ##java.lang.StringBuilder \ Impure as newStringBuilder;
        |    let _ =
        |        if (true)
        |            super_cast newStringBuilder()
        |        else
        |            super_cast newStringBuilder();
        |    ()
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ToTypeVariableSupercast](result)
  }

  test("TestSupercast.06") {
    val input = "def f(): ##java.lang.Object = super_cast \"hello\""
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ToTypeVariableSupercast](result)
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
        |def f(): Bool = unsafe_cast "true" as Bool
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleCast](result)
  }

  test("ImpossibleCast.02") {
    val input =
      """
        |def f(): String = unsafe_cast true as String
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleCast](result)
  }

  test("ImpossibleCast.03") {
    val input =
      """
        |enum A(Bool)
        |def f(): A = unsafe_cast true as A
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleCast](result)
  }

  test("ImpossibleCast.04") {
    val input =
      """
        |enum A(Bool)
        |def f(): Bool = unsafe_cast A(false) as Bool
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleCast](result)
  }

  test("ImpossibleCast.05") {
    val input =
      """
        |enum A(Int32)
        |def f(): A = unsafe_cast 1 as A
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleCast](result)
  }

  test("ImpossibleCast.06") {
    val input =
      """
        |enum A(Int32)
        |def f(): Int32 = unsafe_cast A(1) as Int32
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleCast](result)
  }

  test("ImpossibleCast.07") {
    val input =
      """
        |enum A(String)
        |def f(): A = unsafe_cast "a" as A
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleCast](result)
  }

  test("ImpossibleCast.08") {
    val input =
      """
        |enum A(String)
        |def f(): String = unsafe_cast A("a") as String
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleCast](result)
  }

}
