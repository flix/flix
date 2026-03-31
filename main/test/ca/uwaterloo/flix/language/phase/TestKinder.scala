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
import ca.uwaterloo.flix.language.errors.KindError
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestKinder extends AnyFunSuite with TestUtils {

  private val DefaultOptions = Options.TestWithLibNix

  // ---------------------------------------------------------------------------
  // --- KindError (base trait, no specific subtype) ---
  // ---------------------------------------------------------------------------

  test("MismatchedTypeParamKind.Implicit.01") {
    val input = raw"def f(g: Int32 -> o \ o): Int32 = 123"
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Implicit.02") {
    val input = raw"def f(g: Int32 -> Int32 \ e): e = g(123)"
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Implicit.03") {
    val input = "def f(s: #{| a}, r: {| a}): Int32 = 123"
    val result = check(input, Options.TestWithLibNix)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Implicit.04") {
    val input = "def f(s: #{X(Int32) | a}, r: {x = Int32 | a}): Int32 = 123"
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Implicit.05") {
    val input = raw"def f(a: e): Int32 \ ~e = 123"
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Implicit.06") {
    val input =
      """
        |enum E[a] {
        |  case E1(a)
        |}
        |
        |def f(g: E[a -> b \ e]): Int32 \ ~(a & b) = 123
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Implicit.07") {
    val input =
      """
        |struct E[a, r] {
        |    e1: a
        |}
        |
        |def f(g: E[a -> b \ e, r]): Int32 \ ~(a & b) = 123
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Enum.01") {
    val input =
      """
        |enum E[o] {
        |    case A(Int32 -> o \ o)
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Enum.02") {
    val input =
      """
        |enum E[e] {
        |    case A((Int32 -> Int32 \ e) -> e)
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Enum.03") {
    val input =
      """
        |enum E[a] {
        |    case A(#{| a}, {| a})
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Enum.04") {
    val input =
      """
        |enum E[a] {
        |    case A(#{X(Int32) | a}, {x = Int32 | a})
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Enum.05") {
    val input =
      """
        |enum E[e] {
        |    case A(e -> Int32 \ ~e)
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Enum.06") {
    val input =
      """
        |enum D[a] {
        |  case D1(a)
        |}
        |enum E[a, b, e] {
        |    case A(D[a -> b \ e] -> Int32 \ ~(a & b))
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Struct.01") {
    val input =
      """
        |struct S[o, r] {
        |    a: Int32 -> o \ o
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Struct.02") {
    val input =
      """
        |struct S[e, r] {
        |    a: (Int32 -> Int32 \ e) -> e
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Struct.03") {
    val input =
      """
        |struct S[a, r] {
        |    a: #{| a}, {| a}
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Struct.04") {
    val input =
      """
        |struct S[a, r] {
        |    a: #{X(Int32) | a}, {x = Int32 | a}
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Struct.05") {
    val input =
      """
        |struct S[e, r] {
        |    a: e -> Int32 \ ~e
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Struct.06") {
    val input =
      """
        |struct D[a, r] {
        |    d1: a
        |}
        |struct E[a, b, e, r] {
        |    a: D[a -> b \ e, r] -> Int32 \ ~(a & b)
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Struct.07") {
    val input =
      """
        |struct D[r] {
        |    d1: r
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Struct.08") {
    val input =
      """
        |struct D[r] { }
        |def f(d: D[Int32]): Int32 = 123
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Struct.09") {
    val input =
      """
        |struct D[a, r] { }
        |def f(d: D[Int32, Int32]): Int32 = 123
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.01") {
    val input = raw"type alias T[o] = Int32 -> o \ o"
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.02") {
    val input = raw"type alias T[e] = (Int32 -> Int32 \ e) -> e"
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.03") {
    val input = "type alias T[a] = (#{| a}, {| a})"
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.04") {
    val input = "type alias T[a] = (#{X(Int32) | a}, {x = Int32 | a})"
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.05") {
    val input = raw"type alias T[e] = e -> Int32 \ ~e"
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.06") {
    val input =
      """
        |enum Option[a] {
        |  case Some(a)
        |  case None
        |}
        |
        |type alias T[a, b, e] = Option[a -> b \ e] -> Int32 \ ~(a & b)
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.07") {
    val input =
      """
        |struct S[a, r] {
        |    field1: a
        |    field2: Int32
        |}
        |
        |type alias T[a, b, e, r] = S[a -> b \ e, r] -> Int32 \ ~(a & b)
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.01") {
    val input =
      """
        |enum P[a, b] {
        |  case C(a, b)
        |}
        |
        |def f(p: P[Int32]): Int32 = 123
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.02") {
    val input =
      """
        |enum P[a, b] {
        |  case C(a, b)
        |}
        |
        |enum E {
        |  case A(P[Int32])
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.03") {
    val input =
      """
        |enum P[a, b] {
        |  case C(a, b)
        |}
        |
        |def f(p: P): Int32 = 123
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.04") {
    val input =
      """
        |enum P[a, b] {
        |  case C(a, b)
        |}
        |
        |enum E {
        |  case A(P)
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.05") {
    val input =
      """
        |enum P[a, b, c] {
        |  case C(a, b, c)
        |}
        |
        |def f(p: P[Int32, Int32]): Int32 = 123
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.06") {
    val input =
      """
        |enum P[a, b, c] {
        |  case C(a, b, c)
        |}
        |
        |enum E {
        |  case A(P[Int32, Int32])
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.07") {
    val input = """def f(x: true): Int32 = 123"""
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.08") {
    val input = "def f(): Int32 = unchecked_cast(1 as {})"
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.09") {
    val input =
      """
        |enum E[a, b] {
        |  case C(a, b)
        |}
        |
        |def f(): Int32 = unchecked_cast(1 as E[Int32])""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.10") {
    val input = "def f(): Int32 = (1: {})"
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.11") {
    val input =
      """
        |enum E[a, b] {
        |  case C(a, b)
        |}
        |
        |def f(): Int32 = (1: E[Int32])""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.12") {
    val input =
      """
        |struct P[a, b, r] {}
        |
        |def f(p: P[Int32]): Int32 = 123
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalEffect.01") {
    val input = raw"def f(): Int32 = unchecked_cast(1 as _ \ Int32)"
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalEffect.02") {
    val input = raw"def f(): Int32 = unchecked_cast(1 as Int32 \ Int32)"
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalEffect.03") {
    val input = raw"def f(): Int32 = (1: _ \ Int32)"
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalEffect.04") {
    val input = raw"def f(): Int32 = (1: Int32 \ Int32)"
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.01") {
    val input =
      """
        |enum P[a, b] {
        |  case C(a, b)
        |}
        |
        |def f(p: P[Int32, String, String]): Int32 = 123
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.02") {
    val input =
      """
        |type alias R = {x = Int32}
        |
        |def f(p: R[Int32]): Int32 = 123
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.03") {
    val input =
      """
        |type alias S = #{ A(Int32) }
        |
        |def f(p: S[Int32]): Int32 = 123
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.04") {
    val input = "def f(p: String[Int32]): Int32 = 123"
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.05") {
    val input = raw"def f(): Int32 = unchecked_cast(1 as Int32 \ Int32 + true)"
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.06") {
    val input = raw"def f(): Int32 = unchecked_cast(1 as Int32 \ true & Int32)"
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.07") {
    val input = raw"def f(): Int32 = unchecked_cast(1 as Int32 \ ~Int32)"
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.08") {
    val input = "def f(a: (Int32, true)): Int32 = 1"
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.09") {
    val input =
      """
        |struct P[a, b, r] {}
        |
        |def f(p: P[Int32, String, String, Region]): Int32 = 123
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError](result)
  }

  // ---------------------------------------------------------------------------
  // --- KindError.MismatchedKinds ---
  // ---------------------------------------------------------------------------

  test("KindError.MismatchedKinds.Def.01") {
    val input =
      """
        |def f(x: a): Int32 \ a = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedKinds](result)
  }

  test("KindError.MismatchedKinds.Def.02") {
    val input =
      """
        |trait C[a: Type -> Type]
        |
        |def f(x: a): Int32 with C[a] = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedKinds](result)
  }

  test("KindError.MismatchedKinds.Def.03") {
    val input =
      """
        |def f(x: a -> a \ a): Int32 = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedKinds](result)
  }

  test("KindError.MismatchedKinds.Struct.01") {

    val input =
      """
        |struct S[a: Type, r: Type] {
        |    c: a
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedKinds](result)
  }

  test("KindError.MismatchedKinds.CaseSet.01") {
    val input =
      """
        |restrictable enum E[s] {
        |    case C1
        |    case C2
        |}
        |
        |restrictable enum F[s] {
        |    case D1
        |    case D2
        |}
        |def f(x: a[<E.C1, F.D1>]): String = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedKinds](result)
  }

  test("KindError.MismatchedKinds.CaseSet.02") {
    val input =
      """
        |restrictable enum E[s] {
        |    case C1
        |    case C2
        |}
        |
        |restrictable enum F[s] {
        |    case D1
        |    case D2
        |}
        |
        |def f(x: F[s], y: E[s]): String = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedKinds](result)
  }

  test("KindError.MismatchedKinds.CaseSet.03") {
    val input =
      """
        |restrictable enum E[s] {
        |    case C1
        |    case C2
        |}
        |
        |restrictable enum F[s] {
        |    case D1
        |    case D2
        |}
        |
        |def f(x: F[s rvadd d], y: E[rvnot d]): String = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedKinds](result)
  }

  // ---------------------------------------------------------------------------
  // --- KindError.MismatchedArityOfEnum ---
  // ---------------------------------------------------------------------------

  test("KindError.MismatchedArityOfEnum.Def.Ascribe.01") {
    val input =
      """
        |enum E
        |
        |pub def foo(): Int32 =
        |    let x: E[Int32] = ???; 0
        |
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedArityOfEnum](result)
  }

  test("KindError.MismatchedArityOfEnum.Def.Ascribe.02") {
    val input =
      """
        |enum E[a, b]
        |
        |pub def foo(): Int32 =
        |    let x: E[Int32] = ???; 0
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedArityOfEnum](result)
  }

  test("KindError.MismatchedArityOfEnum.Def.Cast.01") {
    val input =
      """
        |enum E
        |
        |pub def foo(): Int32 = unchecked_cast(0 as E[Int32])
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedArityOfEnum](result)
  }

  test("KindError.MismatchedArityOfEnum.Def.Cast.02") {
    val input =
      """
        |enum E[a, b]
        |
        |pub def foo(): Int32 = unchecked_cast(0 as E[Int32])
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedArityOfEnum](result)
  }

  test("KindError.MismatchedArityOfEnum.Def.LocalDef.Type.01") {
    val input =
      """
        |enum E[a]
        |
        |def g(): Int32 =
        |    def f(x: E[Int32, Int32]): Int32 = ???;
        |    f(???)
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedArityOfEnum](result)
  }

  test("KindError.MismatchedArityOfEnum.Def.LocalDef.Param.01") {
    val input =
      """
        |enum E[a]
        |
        |def g(): Int32 =
        |    def f(x: E): Int32 = ???;
        |    f(???)
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedArityOfEnum](result)
  }

  test("KindError.MismatchedArityOfEnum.Def.LocalDef.Return.01") {
    val input =
      """
        |enum E[a]
        |
        |def g(): Int32 =
        |    def f(): E = ???;
        |    let _ = f();
        |    1
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedArityOfEnum](result)
  }

  test("KindError.MismatchedArityOfEnum.Def.Type.01") {
    val input =
      """
        |enum E[a]
        |
        |def f(x: E[Int32, Int32]): Int32 = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedArityOfEnum](result)
  }

  test("KindError.MismatchedArityOfEnum.Def.Param.01") {
    val input =
      """
        |enum E[a]
        |
        |def f(x: E): Int32 = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedArityOfEnum](result)
  }

  test("KindError.MismatchedArityOfEnum.Def.Return.01") {
    val input =
      """
        |enum E[a]
        |
        |def f(): E = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedArityOfEnum](result)
  }

  test("KindError.MismatchedArityOfEnum.Enum.01") {
    val input =
      """
        |enum F[a]
        |
        |enum E {
        |  case C(F)
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedArityOfEnum](result)
  }

  test("KindError.MismatchedArityOfEnum.Instance.Def.01") {
    val input =
      """
        |trait C[a] {
        |  pub def f(x: a): a
        |}
        |
        |enum E[a]
        |
        |instance C[E[a]] {
        |  pub def f(x: E): E = ???
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedArityOfEnum](result)
  }

  test("KindError.MismatchedArityOfEnum.Instance.TypeParam.01") {
    val input =
      """
        |trait C[a]
        |
        |enum E[a]
        |
        |instance C[E]
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedArityOfEnum](result)
  }

  // ---------------------------------------------------------------------------
  // --- KindError.MismatchedArityOfStruct ---
  // ---------------------------------------------------------------------------

  test("KindError.MismatchedArityOfStruct.Def.Ascribe.01") {
    val input =
      """
        |struct S[a, b, r]
        |
        |pub def foo(): Int32 =
        |    let x: S[Int32] = ???; 0
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedArityOfStruct](result)
  }

  test("KindError.MismatchedArityOfStruct.Def.Cast.01") {
    val input =
      """
        |struct S [r] { }
        |
        |pub def foo(): Int32 = unchecked_cast(0 as S[Int32, Region])
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedArityOfStruct](result)
  }

  test("KindError.MismatchedArityOfStruct.Def.LocalDef.Type.01") {
    val input =
      """
        |struct S[a, r]
        |
        |def g(): Int32 =
        |    def f(x: S[Int32, Int32, Region]): Int32 = ???;
        |    f(???)
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedArityOfStruct](result)
  }

  test("KindError.MismatchedArityOfStruct.Def.LocalDef.Param.01") {
    val input =
      """
        |struct S[a, r]
        |
        |def g(): Int32 =
        |    def f(x: S): Int32 = ???;
        |    f(???)
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedArityOfStruct](result)
  }

  test("KindError.MismatchedArityOfStruct.Def.LocalDef.Return.01") {
    val input =
      """
        |struct S[a, r] {}
        |
        |def g(): Int32 =
        |    def f(): S = ???;
        |    let _ = f();
        |    1
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedArityOfStruct](result)
  }

  test("KindError.MismatchedArityOfStruct.Def.Type.01") {
    val input =
      """
        |struct S[a, r]
        |
        |def f(x: S[Int32, Int32, Region]): Int32 = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedArityOfStruct](result)
  }

  test("KindError.MismatchedArityOfStruct.Def.Param.01") {
    val input =
      """
        |struct S[a, r]
        |
        |def f(x: S): Int32 = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedArityOfStruct](result)
  }

  test("KindError.MismatchedArityOfStruct.Def.Return.01") {
    val input =
      """
        |struct S[a, r] {}
        |
        |def f(): S = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedArityOfStruct](result)
  }

  test("KindError.MismatchedArityOfStruct.Struct.01") {
    val input =
      """
        |struct F[a, r]
        |
        |struct E[r] {
        |    c: F
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.MismatchedArityOfStruct](result)
  }

  // ---------------------------------------------------------------------------
  // --- KindError.UnexpectedKind ---
  // ---------------------------------------------------------------------------

  test("KindError.UnexpectedKind.Def.Ascribe.01") {
    val input =
      """
        |def foo(x: a[Int32]): Int32 =
        |  let _x: a = ???;
        |  123
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.Def.Ascribe.02") {
    val input =
      """
        |pub def foo(): Int32 =
        |    let x : Int32 : (Type -> Type) = 123;
        |    x
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.Def.LocalDef.Type.01") {
    val input =
      """
        |def g(): Int32 =
        |    def f(x: Int32[Int32]): Int32 = ???;
        |    f(???)
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.Def.LocalDef.Type.02") {
    val input =
      """
        |def g(): Int32 =
        |    def f(x: {} -> Int32 \ Int32): Int32 = ???;
        |    f(_ -> 1)
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.Def.LocalDef.Return.01") {
    val input =
      """
        |def g(): Int32 =
        |    def f(): Int32[Int32] = ???;
        |    let _ = f();
        |    1
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.Def.LocalDef.Return.02") {
    val input =
      """
        |def g(): Int32 =
        |    def f(): () = ???;
        |    let _ = f();
        |    1
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.Def.Type.01") {
    val input =
      """
        |def f(x: Int32[Int32]): Int32 = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.Def.Type.02") {
    val input =
      """
        |def f(x: {} -> Int32 \ Int32): Int32 = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.Def.Type.03") {
    val input =
      """
        |def f[r: Type](x: {name = Int32 | r} ): Int32 = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.Def.Type.04") {
    val input =
      """
        |def f[r: Type](x: #{| r} ): Int32 = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.Def.Return.01") {
    val input =
      """
        |def f(): Int32[Int32] = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.Def.Return.02") {
    val input =
      """
        |def f(): () = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.Def.TraitConstraint.01") {
    val input =
      """
        |trait C[a: Type -> Type]
        |
        |def f[a: Type](): a with C[a] = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.Enum.Case.01") {
    val input =
      """
        |enum E[a: Type -> Type] {
        |  case C(a)
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.Enum.Case.02") {
    val input =
      """
        |enum E[a] {
        |  case C({i = Int32 | a})
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.Enum.Type.01") {
    val input =
      """
        |enum E {
        |  case C(Int32[Int32])
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.Struct.Field.01") {
    val input =
      """
        |struct S[a, r] {
        |    c: {i = Int32 | a}
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.Struct.Type.01") {
    val input =
      """
        |struct S[r]{
        |    c: Int32[Int32
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.Instance.TraitConstraint.01") {
    val input =
      """
        |trait C[a]
        |
        |trait D[a: Type -> Type]
        |
        |enum E[a]
        |
        |instance C[E[a]] with D[a]
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.TypeAlias.01") {
    val input =
      """
        |type alias T = {} -> Int32
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.TypeAlias.02") {
    val input = "type alias Z[r] = #{ A(Int32) | r }"
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.Trait.Law.01") {
    val input =
      """
        |trait C[a: Type -> Type] {
        |  law l: forall (x: a) ???
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.Trait.Sig.01") {
    val input =
      """
        |trait C[a: Type -> Type] {
        |  pub def f(x: a): Int32 = ???
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.Trait.Sig.02") {
    val input =
      """
        |trait C[a] {
        |  pub def f(x: {l =  Int32 | a}): Int32 = ???
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.Trait.TraitConstraint.01") {
    val input =
      """
        |trait C[a]
        |
        |trait D[a: Type -> Type] with C[a]
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.CaseSet.01") {
    val input =
      """
        |restrictable enum E[s] {
        |    case C1
        |    case C2
        |}
        |
        |def f[a: E](x: a): String = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.CaseSet.02") {
    val input =
      """
        |restrictable enum E[s] {
        |    case C1
        |    case C2
        |}
        |
        |restrictable enum F[s] {
        |    case D1
        |    case D2
        |}
        |
        |def f(x: F[<E.C1>]): String = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.AssocType.01") {
    val input =
      """
        |mod Foo {
        |    trait Add[t] {
        |        type Aef: Eff
        |        pub def add(lhs: t, rhs: t): t \ Add.Aef[t]
        |    }
        |    instance Add[String] {
        |        type Aef = {|}
        |        pub def add(x: String, y: String): String = x + y
        |    }
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.AssocType.02") {
    val input =
      """
        |trait Foo[t] {
        |   type K: Type
        |   type E: Type
        |   pub def f(x: t): Foo.K[t][Foo.E[t]]
        |}
        |
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.AssocType.03") {
    val input =
      """
        |trait C[a] {
        |    type S : Type
        |    type T : Type -> Type
        |    pub def f(x: a): C.T[a][C.S[a]]
        |}
        |
        |instance C[Int32] {
        |    type S = Int32
        |    type T = Int32
        |    pub def f(x: Int32): Int32 = x
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.AssocType.04") {
    val input =
      """
        |trait B[a] {
        |    type Q : Type -> Type
        |    type R : Type
        |    pub def g(x: a): B.Q[a][B.R[a]]
        |}
        |
        |trait C[a] with B[a] {
        |    type S : Type
        |    type T : Type -> Type
        |    pub def f(x: a): C.T[a][B.Q[a][C.T[a]]]
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.AssocType.05") {
    val input =
      """
        |trait B[a] {
        |    type Q : Type -> Type
        |    type R : Type
        |    pub def g(x: a): B.Q[a][B.R[a]]
        |}
        |
        |trait C[a] with B[a] {
        |    type S : Type
        |    type T : Type -> Type
        |    pub def f(x: a): C.T[a][B.Q[a][C.T[a][C.S[a][B.R[a]]]]]
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.AssocType.06") {
    val input =
      """
        |trait A[a] {
        |    type A : Type -> Type
        |    type B : Type
        |    pub def f(x: a): A.A[a][A.A[a][A.A[a][A.A[a]]]]
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.AssocType.07") {
    val input =
      """
        |trait C[a] {
        |    type T : Type -> Eff
        |    type S : Eff
        |    pub def f(x: a): a \ C.T[a][C.S[a]]
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.UnexpectedKind.AssocType.08") {
    val input =
      """
        |trait C[a] {
        |    type T : Type -> Eff
        |    type S : Eff
        |    pub def f(x: C.T[a][a]): a
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  // ---------------------------------------------------------------------------
  // --- KindError.UnexpectedType ---
  // ---------------------------------------------------------------------------

  test("KindError.UnexpectedType.Def.Effect.01") {
    val input =
      """
        |def f(): Unit \ Unit = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedType](result)
  }

  test("KindError.UnexpectedType.Def.Effect.02") {
    val input =
      """
        |def f[a: Type](): Unit \ a = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedType](result)
  }

  test("KindError.UnexpectedType.Def.Ascribe.01") {
    val input =
      """
        |def f(): Int32 = (1: _ \ Unit)
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedType](result)
  }

  test("KindError.UnexpectedType.Def.Cast.01") {
    val input =
      """
        |def f(): Int32 = unchecked_cast(1 as _ \ Unit)
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedType](result)
  }

  test("KindError.UnexpectedType.Def.LocalDef.Effect.01") {
    val input =
      """
        |def g(): Unit =
        |    def f(): Unit \ Unit = ???;
        |    f()
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedType](result)
  }

  test("KindError.UnexpectedType.Def.LocalDef.Type.01") {
    val input =
      """
        |def g(): Int32 =
        |    def f(x: Int32 -> Int32 \ Int32): Int32 = ???;
        |    f(x -> x)
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedType](result)
  }

  test("KindError.UnexpectedType.Def.Type.01") {
    val input =
      """
        |def f(x: Int32 -> Int32 \ Int32): Int32 = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedType](result)
  }

  test("KindError.UnexpectedType.Enum.01") {
    val input =
      """
        |enum E {
        |  case C(Int32 -> Int32 \ Int32)
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedType](result)
  }

  test("KindError.UnexpectedType.Enum.02") {
    val input =
      """
        |enum E[a] {
        |  case C(Int32 -> Int32 \ a)
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedType](result)
  }

  test("KindError.UnexpectedType.Struct.01") {
    val input =
      """
        |struct S [r] {
        |    c: Int32 -> Int32 \ Int32
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedType](result)
  }

  test("KindError.UnexpectedType.Struct.02") {
    val input =
      """
        |struct S[a, r] {
        |    c: Int32-> Int32 \ a
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedType](result)
  }

  test("KindError.UnexpectedType.TypeAlias.01") {
    val input =
      """
        |type alias T[a] = Int32 -> Int32 \ a
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedType](result)
  }

  // ---------------------------------------------------------------------------
  // --- KindError.UnexpectedEffect ---
  // ---------------------------------------------------------------------------

  test("KindError.UnexpectedEffect.Def.Ascribe.01") {
    val input =
      """
        |def f(): Int32 = (1: {})
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedEffect](result)
  }

  test("KindError.UnexpectedEffect.Def.Ascribe.02") {
    val input =
      """
        |def foo(): Int32 \ ef =
        |  let _x: ef = ???;
        |  123
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedEffect](result)
  }

  test("KindError.UnexpectedEffect.Def.Cast.01") {
    val input =
      """
        |def f(): Int32 = unchecked_cast(1 as {})
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedEffect](result)
  }

  test("KindError.UnexpectedEffect.Def.LocalDef.Param.01") {
    val input =
      """
        |def g(): Int32 =
        |    def f(x: {}): Int32 = ???;
        |    f({})
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedEffect](result)
  }

  test("KindError.UnexpectedEffect.Def.LocalDef.Return.01") {
    val input =
      """
        |def g(): Int32 =
        |    def f(): {} = ???;
        |    let _ = f();
        |    1
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedEffect](result)
  }

  test("KindError.UnexpectedEffect.Def.Param.01") {
    val input =
      """
        |def f(x: {}): Int32 = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedEffect](result)
  }

  test("KindError.UnexpectedEffect.Def.Return.01") {
    val input =
      """
        |def f(): {} = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedEffect](result)
  }

  test("KindError.UnexpectedEffect.Enum.01") {
    val input =
      """
        |enum E {
        |  case C({})
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedEffect](result)
  }

  test("KindError.UnexpectedEffect.Struct.01") {
    val input =
      """
        |struct S [r] {
        |    c: { }
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedEffect](result)
  }

  test("KindError.UnexpectedEffect.AssocType.01") {
    val input =
      """
        |trait C[a] {
        |    type T
        |}
        |
        |instance C[String] {
        |    type T = {}
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedEffect](result)
  }

  test("KindError.UnexpectedEffect.AssocType.02") {
    val input =
      """
        |trait C[a] {
        |    type T
        |}
        |
        |instance C[String] {
        |    type T[{}] = String
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedEffect](result)
  }

  test("KindError.UnexpectedEffect.AssocType.03") {
    val input =
      """
        |mod Foo {
        |    enum Set[_]
        |
        |    trait Order[a]
        |
        |    trait Add[t] {
        |        type Rhs: Type
        |        pub def add(lhs: t, rhs: Add.Rhs[t]): t
        |    }
        |
        |    instance Add[Set[t]] with Order[t] {
        |        type Rhs = {}
        |        pub def add(lhs: Set[t], rhs: t): Set[t] = ???
        |
        |    }
        |
        |}
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UnexpectedEffect](result)
  }

  // ---------------------------------------------------------------------------
  // --- KindError.UninferrableKind ---
  // ---------------------------------------------------------------------------

  test("KindError.UninferrableKind.CaseSet.01") {
    val input =
      """
        |restrictable enum E[s] {
        |    case C1
        |    case C2
        |}
        |
        |def f(x: a[< >]): String = ???
        |""".stripMargin
    val result = check(input, DefaultOptions)
    expectError[KindError.UninferrableKind](result)
  }

}
