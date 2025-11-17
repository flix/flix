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
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.errors.{EntryPointError, SafetyError}
import ca.uwaterloo.flix.language.errors.SafetyError.{Forbidden, IllegalCatchType, IllegalMethodEffect, IllegalNegativelyBoundWildCard, IllegalNonPositivelyBoundVar, IllegalPatternInBodyAtom, IllegalRelationalUseOfLatticeVar, IllegalThrowType}
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestSafety extends AnyFunSuite with TestUtils {

  val DefaultOptions: Options = Options.TestWithLibMin

  test("IllegalCatchType.01") {
    val input =
      """
        |import java.lang.Object
        |
        |pub def f(): String =
        |    try {
        |        "abc"
        |    } catch {
        |        case _s: Object => "fail"
        |    }
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[IllegalCatchType](result)
  }

  test("IllegalCatchType.02") {
    val input =
      """
        |import java.lang.Exception
        |import java.lang.String
        |
        |pub def f(): String =
        |    try {
        |        "abc"
        |    } catch {
        |        case _e1: Exception => "ok"
        |        case _e2: String => "not ok"
        |    }
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[IllegalCatchType](result)
  }

  test("IllegalThrowType.01") {
    val input =
      """
        |def f(): String \ IO = throw "hello"
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[IllegalThrowType](result)
  }

  test("IllegalThrowType.02") {
    val input =
      """
        |import java.io.IOException
        |
        |def f(): String \ IO = throw (throw new IOException())
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[IllegalThrowType](result)
  }

  test("IllegalThrowType.03") {
    val input =
      """
        |import java.io.IOException
        |
        |pub def f(): String \ IO = throw Comparison.LessThan
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[IllegalThrowType](result)
  }

  test("UnexpectedBodyAtomPattern.01") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Option[Int32]) } = #{
        |    A(x) :- B(Some(x)).
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[IllegalPatternInBodyAtom](result)
  }

  test("UnexpectedBodyAtomPattern.02") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Option[Int32]) } = #{
        |    A(1) :- B(Some(2)).
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[IllegalPatternInBodyAtom](result)
  }

  test("UnexpectedBodyAtomPattern.03") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Option[Int32]) } = #{
        |    A(1) :- B(None).
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[IllegalPatternInBodyAtom](result)
  }

  test("UnexpectedBodyAtomPattern.04") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Unit), C(List[Int32]) } = #{
        |    A(x) :- B(()), C(x::_).
        |}
    """.stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[IllegalPatternInBodyAtom](result)
  }

  test("NonPositivelyBoundVariable.01") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Int32) } = #{
        |    A(x) :- not B(x).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[IllegalNonPositivelyBoundVar](result)
  }

  test("NonPositivelyBoundVariable.02") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Int32), R(Int32) } = solve #{
        |    R(x) :- not A(x), not B(x).
        |}
    """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[IllegalNonPositivelyBoundVar](result)
  }

  test("NonPositivelyBoundVariable.03") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Int32), R(Int32) } = #{
        |    R(x) :- not A(x), B(12), if x > 5.
        |}
    """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[IllegalNonPositivelyBoundVar](result)
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
    expectError[IllegalNonPositivelyBoundVar](result)
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
    expectError[IllegalNegativelyBoundWildCard](result)
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
    expectError[IllegalNegativelyBoundWildCard](result)
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
    expectError[IllegalNegativelyBoundWildCard](result)
  }

  test("NegativelyBoundWildcard.01") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Int32) } = #{
        |    A(1) :- not B(_).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[IllegalNegativelyBoundWildCard](result)
  }

  test("NegativelyBoundWildcard.02") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Int32) } = solve #{
        |    A(1) :- not B(_), A(_).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[IllegalNegativelyBoundWildCard](result)
  }

  test("NegativelyBoundWildcard.03") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Int32) } = #{
        |    A(1) :- not B(z), A(z), not B(_).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[IllegalNegativelyBoundWildCard](result)
  }

  test("UseOfLatticeVariable.01") {
    val input =
      """
        |pub def f(): #{ A(Int32), B(Int32; Int32) } = #{
        |    A((x: Int32)) :- B(12; x).
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[IllegalRelationalUseOfLatticeVar](result)
  }

  test("UseOfLatticeVariable.02") {
    val input =
      """
        |pub def f(): #{A(Int32), B(Int32; Int32), C(Int32, Int32) } = #{
        |    A(x) :- B(x; l), C(x, l).
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[IllegalRelationalUseOfLatticeVar](result)
  }

  test("UseOfLatticeVariable.03") {
    val input =
      """
        |pub def f(): #{A(Int32; Int32), B(Int32; Int32), C(Int32, Int32) } = #{
        |    A(x; l) :- B(x; l), C(x, l).
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[IllegalRelationalUseOfLatticeVar](result)
  }

  test("UseOfLatticeVariable.04") {
    val input =
      """
        |pub def f(): #{A(Int32, Int32), B(Int32; Int32), C(Int32; Int32) } = #{
        |    A(12, l) :- B(12; l), fix C(12; l).
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[IllegalRelationalUseOfLatticeVar](result)
  }

  test("UseOfLatticeVariable.05") {
    val input =
      """
        |pub def f(): #{A(Int32), B(Int32, Int32), C(Int32; Int32) } = #{
        |    A(12) :- B(12, l), fix C(12; l).
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[IllegalRelationalUseOfLatticeVar](result)
  }

  test("TestInvalidThis.01") {
    val input =
      """
        |import java.lang.Runnable
        |
        |def f(): Runnable \ IO =
        |  new Runnable {
        |    def $run(): Unit = ()
        |  }
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.NewObjectMissingThisArg](result)
  }

  test("TestInvalidThis.02") {
    val input =
      """
        |import java.lang.Runnable
        |
        |def f(): Runnable \ IO =
        |  new Runnable {
        |    def $run(_this: Int32): Unit = ()
        |  }
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.NewObjectIllegalThisType](result)
  }

  test("TestUnimplementedMethod.01") {
    val input =
      """
        |import java.lang.Runnable
        |
        |def f(): Runnable \ IO = new Runnable {}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.NewObjectMissingMethod](result)
  }

  test("TestExtraMethod.01") {
    val input =
      """
        |import java.lang.Runnable
        |
        |def f(): Runnable \ IO =
        |  new Runnable {
        |    def $run(_this: Runnable): Unit = ()
        |    def anExtraMethod(_this: Runnable): Unit = ()
        |  }
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.NewObjectUndefinedMethod](result)
  }

  test("TestNonDefaultConstructor.01") {
    val input =
      """
        |import dev.flix.test.TestClassWithNonDefaultConstructor
        |
        |def f(): TestClassWithNonDefaultConstructor \ IO =
        |  new TestClassWithNonDefaultConstructor {
        |  }
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.NewObjectMissingPublicZeroArgConstructor](result)
  }

  test("TestNonPublicInterface.01") {
    val input =
      """
        |import dev.flix.test.TestNonPublicInterface
        |
        |def f(): TestNonPublicInterface \ IO =
        |  new TestNonPublicInterface {
        |  }
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.NewObjectNonPublicClass](result)
  }

  test("TestMissingDefaultTypeMatchCase.01") {
    val input =
      """
        |def f(): Bool = typematch () {
        |    case _: Unit => true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.MissingDefaultTypeMatchCase](result)
  }

  test("TestMissingDefaultTypeMatchCase.02") {
    val input =
      """
        |def f(x: a): Bool = typematch x {
        |    case _: a => true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.MissingDefaultTypeMatchCase](result)
  }

  test("TestBaseEffectInRunWith.01") {
    val input =
      """
        |def f(): Unit =
        |    run println("Hello, World!") with handler IO {}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.PrimitiveEffectInRunWith](result)
  }

  test("TestBaseEffectInRunWith.02") {
    val input =
      """
        |def f(): Unit =
        |    run g() with handler NonDet {}
        |
        |def g(): Unit \ NonDet = ???
    """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.PrimitiveEffectInRunWith](result)
  }

  test("ImpossibleCast.01") {
    val input =
      """
        |def f(): Bool = unchecked_cast("true" as Bool)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleUncheckedCast](result)
  }

  test("ImpossibleCast.02") {
    val input =
      """
        |def f(): String = unchecked_cast(true as String)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleUncheckedCast](result)
  }

  test("ImpossibleCast.03") {
    val input =
      """
        |enum A(Bool)
        |def f(): A = unchecked_cast(true as A)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleUncheckedCast](result)
  }

  test("ImpossibleCast.04") {
    val input =
      """
        |enum A(Bool)
        |def f(): Bool = unchecked_cast(A.A(false) as Bool)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleUncheckedCast](result)
  }

  test("ImpossibleCast.05") {
    val input =
      """
        |enum A(Int32)
        |def f(): A = unchecked_cast(1 as A)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleUncheckedCast](result)
  }

  test("ImpossibleCast.06") {
    val input =
      """
        |enum A(Int32)
        |def f(): Int32 = unchecked_cast(A.A(1) as Int32)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleUncheckedCast](result)
  }

  test("ImpossibleCast.07") {
    val input =
      """
        |enum A(String)
        |def f(): A = unchecked_cast("a" as A)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleUncheckedCast](result)
  }

  test("ImpossibleCast.08") {
    val input =
      """
        |enum A(String)
        |def f(): String = unchecked_cast(A.A("a") as String)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleUncheckedCast](result)
  }

  test("ImpossibleCast.09") {
    val input =
      """
        |import java.lang.String
        |
        |def f(): String =
        |    unchecked_cast(('a', 'b', false) as String)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleUncheckedCast](result)
  }

  test("ImpossibleCast.10") {
    val input =
      """
        |struct A[r] {
        |    a: Bool
        |}
        |def f(): A[r] = unchecked_cast(true as A[r])
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleUncheckedCast](result)
  }

  test("ImpossibleCast.11") {
    val input =
      """
        |struct A[r] {
        |    a: Int32
        |}
        |def f(): A[r] = unchecked_cast(1 as A[r])
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleUncheckedCast](result)
  }

  test("ImpossibleCast.12") {
    val input =
      """
        |struct A[r] {
        |    a: String
        |}
        |def f(): A[r] = unchecked_cast("a" as A[r])
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.ImpossibleUncheckedCast](result)
  }

  test("IllegalCastFromNonJava.01") {
    val input =
      """
        |import java.lang.Object
        |
        |def f(): Object =
        |    checked_cast(10i64)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.IllegalCheckedCastFromNonJava](result)
  }

  test("IllegalCastFromNonJava.02") {
    val input =
      """
        |import java.lang.Boolean
        |def f(): Boolean =
        |    checked_cast(true)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.IllegalCheckedCastFromNonJava](result)
  }

  test("IllegalCastFromNonJava.03") {
    val input =
      """
        |import java.lang.StringBuilder
        |def f(): StringBuilder =
        |    checked_cast(false)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.IllegalCheckedCastFromNonJava](result)
  }

  test("IllegalCastFromNonJava.04") {
    val input =
      """
        |import java.lang.{Boolean => JBoolean}
        |
        |enum Boolean(Bool)
        |def f(): JBoolean =
        |    checked_cast(Boolean.Boolean(true))
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.IllegalCheckedCastFromNonJava](result)
  }

  test("IllegalCastFromVar.01") {
    val input =
      """
        |def f(x: a): String = checked_cast(x)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.IllegalCheckedCastFromVar](result)
  }

  test("IllegalCastToVar.01") {
    val input =
      """
        |def f(): b =
        |    g("ABC")
        |
        |def g(x: String): a = checked_cast(x)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.IllegalCheckedCastToVar](result)
  }

  test("IllegalCastToVar.02") {
    val input =
      """
        |def f(x: Int32): b =
        |    checked_cast(x)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.IllegalCheckedCastToVar](result)
  }

  test("IllegalCastToVar.03") {
    val input =
      """
        |def f(): Unit =
        |    let _ =
        |        if (true)
        |            checked_cast("abc")
        |        else
        |            checked_cast("def");
        |    ()
    """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.IllegalCheckedCastToVar](result)
  }

  test("IllegalEntryPointSignature.01") {
    val input =
      """
        |@test
        |def f(x: Int32): Int32 = x
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[EntryPointError.IllegalRunnableEntryPointArgs](result)
  }

  test("IllegalEntryPointSignature.02") {
    val input =
      """
        |@test
        |def g(x: Int32, _y: Int32, _a: Float64): Int32 = x
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[EntryPointError.IllegalRunnableEntryPointArgs](result)
  }

  test("IllegalEntryPointSignature.03") {
    val input =
      """
        |@test
        |def f(_x: Int32, _y: Int32, a: Float64): Float64 = a
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[EntryPointError.IllegalRunnableEntryPointArgs](result)
  }

  test("IllegalEntryPointSignature.04") {
    val input =
      """
        |@test
        |def f(_x: Int32, _y: Int32, _a: Float64): Float64 = 1.0f64
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[EntryPointError.IllegalRunnableEntryPointArgs](result)
  }

  test("IllegalEntryPointSignature.05") {
    val input =
      """
        |eff Print {
        |    pub def println(): Unit
        |}
        |
        |@Test
        |def foo(): Unit \ Print = Print.println()
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[EntryPointError.IllegalEntryPointEffect](result)
  }

  test("IllegalExportFunction.01") {
    val input =
      """
        |mod Mod { @Export def id(x: Int32): Int32 = x }
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[EntryPointError.NonPublicExport](result)
  }

  test("IllegalExportFunction.02") {
    val input =
      """
        |@Export pub def id(x: Int32): Int32 = x
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[EntryPointError.IllegalExportNamespace](result)
  }

  test("IllegalExportFunction.03") {
    val input =
      """
        |mod Mod { @Export pub def <><(x: Int32, _y: Int32): Int32 = x }
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[EntryPointError.IllegalExportName](result)
  }

  test("IllegalExportFunction.04") {
    val input =
      """
        |eff Print
        |def println(x: t): t \ Print = ???()
        |mod Mod { @Export pub def id(x: Int32): Int32 \ Print = println(x) }
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[EntryPointError.IllegalEntryPointEffect](result)
  }

  test("IllegalExportFunction.05") {
    val input =
      """
        |enum Option[t] {
        |  case Some(t)
        |  case None
        |}
        |mod Mod { @Export pub def id(x: Int32): Option[Int32] = Some(x) }
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[EntryPointError.IllegalExportType](result)
  }

  test("IllegalExportFunction.06") {
    val input =
      """
        |enum Option[t] {
        |  case Some(t)
        |  case None
        |}
        |mod Mod { @Export pub def id(x: Int32, _y: Option[Int32]): Int32 = x }
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[EntryPointError.IllegalExportType](result)
  }

  test("IllegalExportFunction.07") {
    val input =
      """
        |mod Mod { @Export pub def id[t](x: t): t = x }
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[EntryPointError.IllegalEntryPointTypeVariables](result)
  }

  test("IllegalExportFunction.08") {
    val input =
      """
        |struct S[t, r] {
        |    v: t
        |}
        |mod Mod { @Export pub def id(x: Int32): S[Int32, r] = ??? }
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[EntryPointError.IllegalEntryPointTypeVariables](result)
  }

  test("IllegalExportFunction.09") {
    val input =
      """
        |struct S[t, r] {
        |    v: t
        |}
        |mod Mod { @Export pub def id(x: Int32, _y: S[Int32, r]): Int32 = x }
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[EntryPointError.IllegalEntryPointTypeVariables](result)
  }

  test("IllegalMethodEffect.01") {
    val input =
      """
        |import java.lang.Runnable
        |
        |eff Ask {
        |    def ask(): String
        |}
        |
        |def newRunnable(): Runnable \ IO = new Runnable {
        |    def $run(_this: Runnable): Unit \ Ask =
        |        Ask.ask(); ()
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[IllegalMethodEffect](result)
  }

  test("SecurityContext.Paranoid.Def.01") {
    val input =
      """
        |pub def f(_: (Unit -> Unit \ IO) -> Unit \ IO): Unit = ()
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Paranoid)
    expectError[Forbidden](result)
  }

  test("SecurityContext.Paranoid.Def.02") {
    val input =
      """
        |pub def f(_: Unit -> Unit \ IO): Unit = ()
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Paranoid)
    expectError[Forbidden](result)
  }

  test("SecurityContext.Paranoid.Def.03") {
    val input =
      """
        |pub def f(g: Unit -> Unit \ IO): Unit \ IO = g()
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Paranoid)
    expectError[Forbidden](result)
  }

  test("SecurityContext.Paranoid.Def.04") {
    val input =
      """
        |pub def f(): (Unit -> Unit \ IO) = _ -> println("hello")
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Paranoid)
    expectError[Forbidden](result)
  }

  test("SecurityContext.Paranoid.Sig.01") {
    val input =
      """
        |trait A[t: Type] {
        |    pub def f(x: t): Unit \ IO
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Paranoid)
    expectError[Forbidden](result)
  }

  test("SecurityContext.Paranoid.Sig.02") {
    val input =
      """
        |trait A[t: Type] {
        |    pub def f(g: t -> Unit \ IO, x: t): Unit \ IO
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Paranoid)
    expectError[Forbidden](result)
  }

  test("SecurityContext.Paranoid.Sig.03") {
    val input =
      """
        |trait A[t: Type] {
        |    pub def f(x: t): Unit
        |    pub def g(h: Unit -> Unit \ IO, x: t): Unit \ IO = h(A.f(x))
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Paranoid)
    expectError[Forbidden](result)
  }

  test("SecurityContext.Paranoid.Sig.04") {
    val input =
      """
        |trait A[t: Type] {
        |    type Aef: Eff = IO
        |    pub def f(x: t): Unit \ A.Aef[t]
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Paranoid)
    expectError[Forbidden](result)
  }

  test("SecurityContext.Paranoid.Sig.05") {
    val input =
      """
        |trait A[t: Type] {
        |    type Aef: Eff = {}
        |    pub def f(x: t): Unit \ A.Aef[t]
        |}
        |
        |instance A[B] {
        |    type Aef = IO
        |    pub def f(x: B): Unit \ IO = match x { case B.N => println("N") }
        |}
        |
        |pub enum B {
        |    case N
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Paranoid)
    expectError[Forbidden](result)
  }

  test("SecurityContext.Paranoid.Sig.06") {
    val input =
      """
        |trait A[t: Type] {
        |    type Aef: Eff
        |    pub def f(x: t): String
        |    pub def g(x: t, h: String -> Unit \ A.Aef[t]): Unit \ A.Aef[t] = A.f(x) |> h
        |}
        |
        |instance A[B[a]] {
        |    type Aef = IO
        |    pub def f(x: B[a]): String = match x { case B.N(_) => "B.N" }
        |}
        |
        |pub enum B[a] {
        |    case N(a)
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Paranoid)
    expectError[Forbidden](result)
  }

  test("SecurityContext.Paranoid.Sig.07") {
    val input =
      """
        |trait A[t: Type] {
        |    type Aef: Eff = IO
        |    pub def f(x: t): String
        |    pub def g(x: t, h: String -> Unit \ A.Aef[t]): Unit \ A.Aef[t] = A.f(x) |> h
        |}
        |
        |instance A[B[a]] {
        |    type Aef = IO
        |    pub def f(x: B[a]): String = match x { case B.N(_) => "B.N" }
        |}
        |
        |pub enum B[a] {
        |    case N(a)
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Paranoid)
    expectError[Forbidden](result)
  }

  test("SecurityContext.Paranoid.Sig.08") {
    val input =
      """
        |trait A[t: Type] {
        |    type Aef: Eff = IO
        |    pub def f(x: t): String
        |    pub def g(x: t, h: String -> Unit \ A.Aef[t]): Unit \ A.Aef[t] = A.f(x) |> h
        |    pub def q(x: t, h: String -> Unit \ A.Aef[t]): (Unit -> Unit \ A.Aef[t]) = _ -> A.g(x, h)
        |}
        |
        |instance A[B[a]] {
        |    type Aef = {}
        |    pub def f(x: B[a]): String = match x { case B.N(_) => "B.N" }
        |}
        |
        |pub enum B[a] {
        |    case N(a)
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Paranoid)
    expectError[Forbidden](result)
  }

  test("SecurityContext.Paranoid.UncheckedCast.01") {
    val input =
      """
        |pub def f(): Unit = unchecked_cast(println(42) as _ \ {})
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Paranoid)
    expectError[Forbidden](result)
  }

  test("SecurityContext.Paranoid.Java.01") {
    val input =
      """
        |mod A {
        |    import java.lang.StringBuilder
        |    pub def f(): Unit = ()
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)(SecurityContext.Paranoid)
    expectError[Forbidden](result)
  }

  test("SecurityContext.Paranoid.Java.02") {
    val input =
      """
        |mod A {
        |    import java.lang.StringBuilder
        |    pub def f(): StringBuilder \ IO = new StringBuilder()
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Paranoid)
    expectError[Forbidden](result)
  }

  test("SecurityContext.Paranoid.Java.03") {
    val input =
      """
        |mod A {
        |    import java.lang.StringBuilder
        |    pub def f(): StringBuilder = unsafe new StringBuilder()
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)(SecurityContext.Paranoid)
    expectError[Forbidden](result)
  }

  test("SecurityContext.Paranoid.Java.04") {
    val input =
      """
        |mod A {
        |    import java.lang.StringBuilder
        |    pub def f(sb: StringBuilder): Unit \ IO = {
        |        sb.append("hello");
        |        ()
        |    }
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Paranoid)
    expectError[Forbidden](result)
  }

  test("SecurityContext.Plain.Def.01") {
    val input =
      """
        |pub def f(_: (Unit -> Unit \ IO) -> Unit \ IO): Unit = ()
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Plain)
    expectSuccess(result)
  }

  test("SecurityContext.Plain.Def.02") {
    val input =
      """
        |pub def f(_: Unit -> Unit \ IO): Unit = ()
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Plain)
    expectSuccess(result)
  }

  test("SecurityContext.Plain.Def.03") {
    val input =
      """
        |pub def f(g: Unit -> Unit \ IO): Unit \ IO = g()
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Plain)
    expectSuccess(result)
  }

  test("SecurityContext.Plain.Def.04") {
    val input =
      """
        |pub def f(): (Unit -> Unit \ IO) = _ -> println("hello")
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Plain)
    expectSuccess(result)
  }

  test("SecurityContext.Plain.Sig.01") {
    val input =
      """
        |trait A[t: Type] {
        |    pub def f(x: t): Unit \ IO
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Plain)
    expectSuccess(result)
  }

  test("SecurityContext.Plain.Sig.02") {
    val input =
      """
        |trait A[t: Type] {
        |    pub def f(g: t -> Unit \ IO, x: t): Unit \ IO
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Plain)
    expectSuccess(result)
  }

  test("SecurityContext.Plain.Sig.03") {
    val input =
      """
        |trait A[t: Type] {
        |    pub def f(x: t): Unit
        |    pub def g(h: Unit -> Unit \ IO, x: t): Unit \ IO = h(A.f(x))
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Plain)
    expectSuccess(result)
  }

  test("SecurityContext.Plain.Sig.04") {
    val input =
      """
        |trait A[t: Type] {
        |    type Aef: Eff = IO
        |    pub def f(x: t): Unit \ A.Aef[t]
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Plain)
    expectSuccess(result)
  }

  test("SecurityContext.Plain.Sig.05") {
    val input =
      """
        |trait A[t: Type] {
        |    type Aef: Eff = {}
        |    pub def f(x: t): Unit \ A.Aef[t]
        |}
        |
        |instance A[B] {
        |    type Aef = IO
        |    pub def f(x: B): Unit \ IO = match x { case B.N => println("N") }
        |}
        |
        |pub enum B {
        |    case N
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Plain)
    expectSuccess(result)
  }

  test("SecurityContext.Plain.Sig.06") {
    val input =
      """
        |trait A[t: Type] {
        |    type Aef: Eff
        |    pub def f(x: t): String
        |    pub def g(x: t, h: String -> Unit \ A.Aef[t]): Unit \ A.Aef[t] = A.f(x) |> h
        |}
        |
        |instance A[B[a]] {
        |    type Aef = IO
        |    pub def f(x: B[a]): String = match x { case B.N(_) => "B.N" }
        |}
        |
        |pub enum B[a] {
        |    case N(a)
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Plain)
    expectSuccess(result)
  }

  test("SecurityContext.Plain.Sig.07") {
    val input =
      """
        |trait A[t: Type] {
        |    type Aef: Eff = IO
        |    pub def f(x: t): String
        |    pub def g(x: t, h: String -> Unit \ A.Aef[t]): Unit \ A.Aef[t] = A.f(x) |> h
        |}
        |
        |instance A[B[a]] {
        |    type Aef = IO
        |    pub def f(x: B[a]): String = match x { case B.N(_) => "B.N" }
        |}
        |
        |pub enum B[a] {
        |    case N(a)
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Plain)
    expectSuccess(result)
  }

  test("SecurityContext.Plain.Sig.08") {
    val input =
      """
        |trait A[t: Type] {
        |    type Aef: Eff = IO
        |    pub def f(x: t): String
        |    pub def g(x: t, h: String -> Unit \ A.Aef[t]): Unit \ A.Aef[t] = A.f(x) |> h
        |    pub def q(x: t, h: String -> Unit \ A.Aef[t]): (Unit -> Unit \ A.Aef[t]) = _ -> A.g(x, h)
        |}
        |
        |instance A[B[a]] {
        |    type Aef = {}
        |    pub def f(x: B[a]): String = match x { case B.N(_) => "B.N" }
        |}
        |
        |pub enum B[a] {
        |    case N(a)
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Plain)
    expectSuccess(result)
  }

  test("SecurityContext.Plain.UncheckedCast.01") {
    val input =
      """
        |pub def f(): Unit = unchecked_cast(println(42) as _ \ {})
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Plain)
    expectError[Forbidden](result)
  }

  test("SecurityContext.Plain.Java.01") {
    val input =
      """
        |mod A {
        |    import java.lang.StringBuilder
        |    pub def f(): Unit = ()
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)(SecurityContext.Plain)
    expectError[Forbidden](result)
  }

  test("SecurityContext.Plain.Java.02") {
    val input =
      """
        |mod A {
        |    import java.lang.StringBuilder
        |    pub def f(): StringBuilder \ IO = new StringBuilder()
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Plain)
    expectError[Forbidden](result)
  }

  test("SecurityContext.Plain.Java.03") {
    val input =
      """
        |mod A {
        |    import java.lang.StringBuilder
        |    pub def f(): StringBuilder = unsafe new StringBuilder()
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)(SecurityContext.Plain)
    expectError[Forbidden](result)
  }

  test("SecurityContext.Plain.Java.04") {
    val input =
      """
        |mod A {
        |    import java.lang.StringBuilder
        |    pub def f(sb: StringBuilder): Unit \ IO = {
        |        sb.append("hello");
        |        ()
        |    }
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Plain)
    expectError[Forbidden](result)
  }

  test("SecurityContext.Unrestricted.Def.01") {
    val input =
      """
        |pub def f(_: (Unit -> Unit \ IO) -> Unit \ IO): Unit = ()
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Unrestricted)
    expectSuccess(result)
  }

  test("SecurityContext.Unrestricted.Def.02") {
    val input =
      """
        |pub def f(_: Unit -> Unit \ IO): Unit = ()
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Unrestricted)
    expectSuccess(result)
  }

  test("SecurityContext.Unrestricted.Def.03") {
    val input =
      """
        |pub def f(g: Unit -> Unit \ IO): Unit \ IO = g()
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Unrestricted)
    expectSuccess(result)
  }

  test("SecurityContext.Unrestricted.Def.04") {
    val input =
      """
        |pub def f(): (Unit -> Unit \ IO) = _ -> println("hello")
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Unrestricted)
    expectSuccess(result)
  }

  test("SecurityContext.Unrestricted.Sig.01") {
    val input =
      """
        |trait A[t: Type] {
        |    pub def f(x: t): Unit \ IO
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Unrestricted)
    expectSuccess(result)
  }

  test("SecurityContext.Unrestricted.Sig.02") {
    val input =
      """
        |trait A[t: Type] {
        |    pub def f(g: t -> Unit \ IO, x: t): Unit \ IO
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Unrestricted)
    expectSuccess(result)
  }

  test("SecurityContext.Unrestricted.Sig.03") {
    val input =
      """
        |trait A[t: Type] {
        |    pub def f(x: t): Unit
        |    pub def g(h: Unit -> Unit \ IO, x: t): Unit \ IO = h(A.f(x))
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Unrestricted)
    expectSuccess(result)
  }

  test("SecurityContext.Unrestricted.Sig.04") {
    val input =
      """
        |trait A[t: Type] {
        |    type Aef: Eff = IO
        |    pub def f(x: t): Unit \ A.Aef[t]
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Unrestricted)
    expectSuccess(result)
  }

  test("SecurityContext.Unrestricted.Sig.05") {
    val input =
      """
        |trait A[t: Type] {
        |    type Aef: Eff = {}
        |    pub def f(x: t): Unit \ A.Aef[t]
        |}
        |
        |instance A[B] {
        |    type Aef = IO
        |    pub def f(x: B): Unit \ IO = match x { case B.N => println("N") }
        |}
        |
        |pub enum B {
        |    case N
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Unrestricted)
    expectSuccess(result)
  }

  test("SecurityContext.Unrestricted.Sig.06") {
    val input =
      """
        |trait A[t: Type] {
        |    type Aef: Eff
        |    pub def f(x: t): String
        |    pub def g(x: t, h: String -> Unit \ A.Aef[t]): Unit \ A.Aef[t] = A.f(x) |> h
        |}
        |
        |instance A[B[a]] {
        |    type Aef = IO
        |    pub def f(x: B[a]): String = match x { case B.N(_) => "B.N" }
        |}
        |
        |pub enum B[a] {
        |    case N(a)
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Unrestricted)
    expectSuccess(result)
  }

  test("SecurityContext.Unrestricted.Sig.07") {
    val input =
      """
        |trait A[t: Type] {
        |    type Aef: Eff = IO
        |    pub def f(x: t): String
        |    pub def g(x: t, h: String -> Unit \ A.Aef[t]): Unit \ A.Aef[t] = A.f(x) |> h
        |}
        |
        |instance A[B[a]] {
        |    type Aef = IO
        |    pub def f(x: B[a]): String = match x { case B.N(_) => "B.N" }
        |}
        |
        |pub enum B[a] {
        |    case N(a)
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Unrestricted)
    expectSuccess(result)
  }

  test("SecurityContext.Unrestricted.Sig.08") {
    val input =
      """
        |trait A[t: Type] {
        |    type Aef: Eff = IO
        |    pub def f(x: t): String
        |    pub def g(x: t, h: String -> Unit \ A.Aef[t]): Unit \ A.Aef[t] = A.f(x) |> h
        |    pub def q(x: t, h: String -> Unit \ A.Aef[t]): (Unit -> Unit \ A.Aef[t]) = _ -> A.g(x, h)
        |}
        |
        |instance A[B[a]] {
        |    type Aef = {}
        |    pub def f(x: B[a]): String = match x { case B.N(_) => "B.N" }
        |}
        |
        |pub enum B[a] {
        |    case N(a)
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Unrestricted)
    expectSuccess(result)
  }

  test("SecurityContext.Unrestricted.UncheckedCast.01") {
    val input =
      """
        |pub def f(): Unit = unchecked_cast(println(42) as _ \ {})
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Unrestricted)
    expectSuccess(result)
  }

  test("SecurityContext.Unrestricted.Java.01") {
    val input =
      """
        |mod A {
        |    import java.lang.StringBuilder
        |    pub def f(): Unit = ()
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)(SecurityContext.Unrestricted)
    expectSuccess(result)
  }

  test("SecurityContext.Unrestricted.Java.02") {
    val input =
      """
        |mod A {
        |    import java.lang.StringBuilder
        |    pub def f(): StringBuilder \ IO = new StringBuilder()
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Unrestricted)
    expectSuccess(result)
  }

  test("SecurityContext.Unrestricted.Java.03") {
    val input =
      """
        |mod A {
        |    import java.lang.StringBuilder
        |    pub def f(): StringBuilder = unsafe new StringBuilder()
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)(SecurityContext.Unrestricted)
    expectSuccess(result)
  }

  test("SecurityContext.Unrestricted.Java.04") {
    val input =
      """
        |mod A {
        |    import java.lang.StringBuilder
        |    pub def f(sb: StringBuilder): Unit \ IO = {
        |        sb.append("hello");
        |        ()
        |    }
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)(SecurityContext.Unrestricted)
    expectSuccess(result)
  }
}
