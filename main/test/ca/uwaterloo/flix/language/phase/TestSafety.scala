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
import ca.uwaterloo.flix.language.errors.{EntryPointError, SafetyError}
import ca.uwaterloo.flix.language.errors.SafetyError.{IllegalCatchType, IllegalMethodEffect, IllegalNegativelyBoundWildCard, IllegalNestedTryCatch, IllegalNonPositivelyBoundVar, IllegalPatternInBodyAtom, IllegalRelationalUseOfLatticeVar, IllegalSpawnEffect, IllegalThrowType}
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
    val result = compile(input, Options.DefaultTest)
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
    val result = compile(input, Options.DefaultTest)
    expectError[IllegalCatchType](result)
  }

  test("IllegalThrowType.01") {
    val input =
      """
        |def f(): String = throw "hello"
      """.stripMargin
    val result = compile(input, Options.DefaultTest)
    expectError[IllegalThrowType](result)
  }

  test("IllegalThrowType.02") {
    val input =
      """
        |import java.io.IOException
        |def f(): String = throw (throw new IOException())
      """.stripMargin
    val result = compile(input, Options.DefaultTest)
    expectError[IllegalThrowType](result)
  }

  test("IllegalThrowType.03") {
    val input =
      """
        |import java.io.IOException
        |pub def f(): String = throw None
      """.stripMargin
    val result = compile(input, Options.DefaultTest)
    expectError[IllegalThrowType](result)
  }

  test("IllegalNestedTryCatch.01") {
    val input =
      """
        |import java.lang.Exception
        |
        |pub def f(): String =
        |    try {
        |        try {
        |            "abc"
        |        } catch {
        |            case _e1: Exception => "ok"
        |        }
        |    } catch {
        |        case _e1: Exception => "ok"
        |    }
      """.stripMargin
    val result = compile(input, Options.DefaultTest)
    expectError[IllegalNestedTryCatch](result)
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
    val result = compile(input, Options.TestWithLibAll)
    expectError[IllegalRelationalUseOfLatticeVar](result)
  }

  test("UseOfLatticeVariable.02") {
    val input =
      """
        |pub def f(): #{A(Int32), B(Int32; Int32), C(Int32, Int32) } = #{
        |    A(x) :- B(x; l), C(x, l).
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[IllegalRelationalUseOfLatticeVar](result)
  }

  test("UseOfLatticeVariable.03") {
    val input =
      """
        |pub def f(): #{A(Int32; Int32), B(Int32; Int32), C(Int32, Int32) } = #{
        |    A(x; l) :- B(x; l), C(x, l).
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[IllegalRelationalUseOfLatticeVar](result)
  }

  test("UseOfLatticeVariable.04") {
    val input =
      """
        |pub def f(): #{A(Int32, Int32), B(Int32; Int32), C(Int32; Int32) } = #{
        |    A(12, l) :- B(12; l), fix C(12; l).
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[IllegalRelationalUseOfLatticeVar](result)
  }

  test("UseOfLatticeVariable.05") {
    val input =
      """
        |pub def f(): #{A(Int32), B(Int32, Int32), C(Int32; Int32) } = #{
        |    A(12) :- B(12, l), fix C(12; l).
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[IllegalRelationalUseOfLatticeVar](result)
  }

  test("TestInvalidThis.01") {
    val input =
      """
        |import java.lang.Runnable
        |
        |def f(): Runnable \ IO =
        |  new Runnable {
        |    def run(): Unit = ()
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
        |    def run(_this: Int32): Unit = ()
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
        |    def run(_this: Runnable): Unit = ()
        |    def anExtraMethod(_this: Runnable): Unit = ()
        |  }
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.NewObjectUnreachableMethod](result)
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

  test("TestBaseEffectInTryWith.01") {
    val input =
      """
        |def f(): Unit =
        |    try println("Hello, World!") with IO {}
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.PrimitiveEffectInTryWith](result)
  }

  test("TestBaseEffectInTryWith.02") {
    val input =
      """
        |def f(): Unit =
        |    try g() with Exec {}
        |
        |def g(): Unit \ Exec = ???
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.PrimitiveEffectInTryWith](result)
  }

  test("TestBaseEffectInTryWith.03") {
    val input =
      """
        |def f(): Unit =
        |    try g() with FsRead {}
        |
        |def g(): Unit \ FsRead = ???
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.PrimitiveEffectInTryWith](result)
  }

  test("TestBaseEffectInTryWith.04") {
    val input =
      """
        |def f(): Unit =
        |    try g() with FsWrite {}
        |
        |def g(): Unit \ FsWrite = ???
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.PrimitiveEffectInTryWith](result)
  }

  test("TestBaseEffectInTryWith.05") {
    val input =
      """
        |def f(): Unit =
        |    try g() with Net {}
        |
        |def g(): Unit \ Net = ???
    """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.PrimitiveEffectInTryWith](result)
  }

  test("TestBaseEffectInTryWith.06") {
    val input =
      """
        |def f(): Unit =
        |    try g() with NonDet {}
        |
        |def g(): Unit \ NonDet = ???
    """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.PrimitiveEffectInTryWith](result)
  }

  test("TestBaseEffectInTryWith.07") {
    val input =
      """
        |def f(): Unit =
        |    try g() with Sys {}
        |
        |def g(): Unit \ Sys = ???
    """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.PrimitiveEffectInTryWith](result)
  }

  test("UnableToDeriveSendable.01") {
    val input =
      """
        |enum Enum1[r: Region](Array[Int32, r]) with Sendable
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.IllegalSendableInstance](result)
  }

  test("UnableToDeriveSendable.02") {
    val input =
      """
        |instance Sendable[Array[a, r]]
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[SafetyError.IllegalSendableInstance](result)
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
        |def f(): String =
        |    g("ABC")
        |
        |def g(x: a): a = checked_cast(x)
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[SafetyError.IllegalCheckedCastFromVar](result)
  }

  test("IllegalCastFromVar.02") {
    val input =
      """
        |def f(x: a): String =
        |    checked_cast(x)
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
    expectError[EntryPointError.IllegalEntryPointPolymorphism](result)
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
    expectError[EntryPointError.IllegalEntryPointPolymorphism](result)
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
    expectError[EntryPointError.IllegalEntryPointPolymorphism](result)
  }

  test("IllegalMethodEffect.01") {
    val input =
      """
        |import java.lang.Runnable
        |
        |eff Ask {
        |    pub def ask(): String
        |}
        |
        |def newRunnable(): Runnable \ IO = new Runnable {
        |    def run(_this: Runnable): Unit \ Ask =
        |        Ask.ask(); ()
        |}
      """.stripMargin
    val result = compile(input, Options.DefaultTest)
    expectError[IllegalMethodEffect](result)
  }

  test("IllegalSpawnEffect.01") {
    val input =
      """
        |eff Ask {
        |    pub def ask(): String
        |}
        |
        |def main(): Unit \ IO =
        |    region rc {
        |        spawn Ask.ask() @ rc
        |    }
        |
      """.stripMargin
    val result = compile(input, Options.DefaultTest)
    expectError[IllegalSpawnEffect](result)
  }

}
