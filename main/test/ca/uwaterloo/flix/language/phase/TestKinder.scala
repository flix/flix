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

  test("MismatchedTypeParamKind.Implicit.01") {
    val input = raw"def f(g: Int32 -> o \ o): Int32 = 123"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Implicit.02") {
    val input = raw"def f(g: Int32 -> Int32 \ e): e = g(123)"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Implicit.03") {
    val input = "def f(s: #{| a}, r: {| a}): Int32 = 123"
    val result = compile(input, Options.TestWithLibNix)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Implicit.04") {
    val input = "def f(s: #{X(Int32) | a}, r: {x = Int32 | a}): Int32 = 123"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Implicit.05") {
    val input = raw"def f(a: e): Int32 \ ~e = 123"
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }


  test("MismatchedTypeParamKind.Enum.01") {
    val input =
      """
        |enum E[o] {
        |    case A(Int32 -> o \ o)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Enum.02") {
    val input =
      """
        |enum E[e] {
        |    case A((Int32 -> Int32 \ e) -> e)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Enum.03") {
    val input =
      """
        |enum E[a] {
        |    case A(#{| a}, {| a})
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Enum.04") {
    val input =
      """
        |enum E[a] {
        |    case A(#{X(Int32) | a}, {x = Int32 | a})
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.Enum.05") {
    val input =
      """
        |enum E[e] {
        |    case A(e -> Int32 \ ~e)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.01") {
    val input = raw"type alias T[o] = Int32 -> o \ o"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.02") {
    val input = raw"type alias T[e] = (Int32 -> Int32 \ e) -> e"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.03") {
    val input = "type alias T[a] = (#{| a}, {| a})"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.04") {
    val input = "type alias T[a] = (#{X(Int32) | a}, {x = Int32 | a})"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.05") {
    val input = raw"type alias T[e] = e -> Int32 \ ~e"
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.07") {
    val input = """def f(x: true): Int32 = 123"""
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.08") {
    val input = "def f(): Int32 = unchecked_cast(1 as Pure)"
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.10") {
    val input = "def f(): Int32 = 1: Pure"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalUninhabitedType.11") {
    val input =
      """
        |enum E[a, b] {
        |  case C(a, b)
        |}
        |
        |def f(): Int32 = 1: E[Int32]""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalEffect.01") {
    val input = raw"def f(): Int32 = unchecked_cast(1 as _ \ Int32)"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalEffect.02") {
    val input = raw"def f(): Int32 = unchecked_cast(1 as Int32 \ Int32)"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalEffect.03") {
    val input = raw"def f(): Int32 = 1: _ \ Int32"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalEffect.04") {
    val input = raw"def f(): Int32 = 1: Int32 \ Int32"
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.02") {
    val input =
      """
        |type alias R = {x = Int32}
        |
        |def f(p: R[Int32]): Int32 = 123
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.03") {
    val input =
      """
        |type alias S = #{ A(Int32) }
        |
        |def f(p: S[Int32]): Int32 = 123
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.04") {
    val input = "def f(p: String[Int32]): Int32 = 123"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.05") {
    val input = raw"def f(): Int32 = unchecked_cast(1 as Int32 \ Int32 + true)"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.06") {
    val input = raw"def f(): Int32 = unchecked_cast(1 as Int32 \ true & Int32)"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.07") {
    val input = raw"def f(): Int32 = unchecked_cast(1 as Int32 \ ~Int32)"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalTypeApplication.08") {
    val input = "def f(a: (Int32, true)): Int32 = 1"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("KindError.Def.Effect.01") {
    val input =
      """
        |def f(): Unit \ Unit = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Effect.02") {
    val input =
      """
        |def f[a: Type](): Unit \ a = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Expression.Ascribe.01") {
    val input =
      """
        |def f(): Int32 = 1: Pure
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Expression.Ascribe.02") {
    val input =
      """
        |def f(): Int32 = 1: _ \ Unit
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Expression.Ascribe.03") {
    val input =
      """
        |def foo(): Int32 \ ef =
        |  let _x: ef = ???;
        |  123
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Expression.Ascribe.04") {
    val input =
      """
        |def foo(x: a[Int32]): Int32 =
        |  let _x: a = ???;
        |  123
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Expression.Ascribe.05") {
    val input =
      """
        |pub def foo(): Int32 =
        |    let x : Int32 : (Type -> Type) = 123;
        |    x
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Expression.Ascribe.06") {
    val input =
      """
        |enum E
        |
        |pub def foo(): Int32 =
        |    let x: E[Int32] = ???; 0
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Expression.Ascribe.07") {
    val input =
      """
        |enum E[a, b]
        |
        |pub def foo(): Int32 =
        |    let x: E[Int32] = ???; 0
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Expression.Cast.01") {
    val input =
      """
        |def f(): Int32 = unchecked_cast(1 as Pure)
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Expression.Cast.02") {
    val input =
      """
        |def f(): Int32 = unchecked_cast(1 as _ \ Unit)
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Expression.Cast.03") {
    val input =
      """
        |enum E
        |
        |pub def foo(): Int32 = unchecked_cast(0 as E[Int32])
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Expression.Cast.04") {
    val input =
      """
        |enum E[a, b]
        |
        |pub def foo(): Int32 = unchecked_cast(0 as E[Int32])
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }
  test("KindError.Def.Type.01") {
    val input =
      """
        |def f(x: Int32[Int32]): Int32 = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Type.02") {
    val input =
      """
        |def f(x: Int32 -> Int32 \ Int32): Int32 = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Type.03") {
    val input =
      """
        |def f(x: Pure -> Int32 \ Int32): Int32 = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Type.04") {
    val input =
      """
        |def f[r: Type](x: {name = Int32 | r} ): Int32 = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Type.05") {
    val input =
      """
        |def f[r: Type](x: #{| r} ): Int32 = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Type.06") {
    val input =
      """
        |enum E[a]
        |
        |def f(x: E[Int32, Int32]): Int32 = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Type.07") {
    val input =
      """
        |def f(x: Int32[Int32]): Int32 = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Parameter.01") {
    val input =
      """
        |def f(x: Pure): Int32 = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Parameter.02") {
    val input =
      """
        |enum E[a]
        |
        |def f(x: E): Int32 = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Return.01") {
    val input =
      """
        |def f(): Pure = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Return.02") {
    val input =
      """
        |enum E[a]
        |
        |def f(): E = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Return.03") {
    val input =
      """
        |def f(): Int32[Int32] = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Return.04") {
    val input =
      """
        |def f(): () = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.TypeConstraint.01") {
    val input =
      """
        |class C[a: Type -> Type]

        |def f[a: Type](): a with C[a] = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Mismatch.01") {
    val input =
      """
        |def f(x: a): Int32 \ a = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.MismatchedKinds](result)
  }

  test("KindError.Def.Mismatch.02") {
    val input =
      """
        |class C[a: Type -> Type]
        |
        |def f(x: a): Int32 with C[a] = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.MismatchedKinds](result)
  }

  test("KindError.Def.Mismatch.03") {
    val input =
      """
        |def f(x: a -> a \ a): Int32 = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.MismatchedKinds](result)
  }

  test("KindError.Enum.Case.01") {
    val input =
      """
        |enum E {
        |  case C(Pure)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Enum.Case.02") {
    val input =
      """
        |enum F[a]
        |
        |enum E {
        |  case C(F)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Enum.Case.03") {
    val input =
      """
        |enum E[a: Type -> Type] {
        |  case C(a)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Enum.Case.04") {
    val input =
      """
        |enum E[a] {
        |  case C({i = Int32 | a})
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Enum.Type.01") {
    val input =
      """
        |enum E {
        |  case C(Int32 -> Int32 \ Int32)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Enum.Type.02") {
    val input =
      """
        |enum E[a] {
        |  case C(Int32 -> Int32 \ a)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Enum.Type.05") {
    val input =
      """
        |enum E {
        |  case C(Int32[Int32])
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Instance.Def.01") {
    val input =
      """
        |class C[a] {
        |  pub def f(x: a): a
        |}
        |
        |enum E[a]
        |
        |instance C[E[a]] {
        |  pub def f(x: E): E = ???
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Instance.TypeConstraint.01") {
    val input =
      """
        |class C[a]
        |
        |class D[a: Type -> Type]
        |
        |enum E[a]
        |
        |instance C[E[a]] with D[a]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Instance.TypeParameter.01") {
    val input =
      """
        |class C[a]
        |
        |enum E[a]
        |
        |instance C[E]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.TypeAlias.Type.01") {
    val input =
      """
        |type alias T = Pure -> Int32
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.TypeAlias.Type.02") {
    val input =
      """
        |type alias T[a] = Int32 -> Int32 \ a
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.TypeAlias.Type.03") {
    val input = "type alias Z[r] = #{ A(Int32) | r }"
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Class.Law.01") {
    val input =
      """
        |class C[a: Type -> Type] {
        |  law l: forall (x: a) . ???
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Class.Sig.01") {
    val input =
      """
        |class C[a: Type -> Type] {
        |  pub def f(x: a): Int32 = ???
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Class.Sig.02") {
    val input =
      """
        |class C[a] {
        |  pub def f(x: {l =  Int32 | a}): Int32 = ???
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Class.TypeConstraint.01") {
    val input =
      """
        |class C[a]
        |
        |class D[a: Type -> Type] with C[a]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.TypeMatch.01") {
    val input =
      """
        |def f(x: a[b]): Unit = typematch x {
        |    case _: b[a] => ()
        |    case _: _ => ()
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.TypeMatch.02") {
    val input =
      """
        |
        |enum E[_]
        |
        |def f(x: a): Unit = typematch x {
        |    case _: E => ()
        |    case _: _ => ()
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.CaseSet.01") {
    val input =
      """
        |restrictable enum E[s] {
        |    case C1
        |    case C2
        |}
        |
        |def f[a: E](x: a): String = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.CaseSet.02") {
    val input =
      """
        |restrictable enum E[s] {
        |    case C1
        |    case C2
        |}
        |
        |def f(x: a[<>]): String = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UninferrableKind](result)
  }

  test("KindError.CaseSet.03") {
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
    val result = compile(input, DefaultOptions)
    expectError[KindError.MismatchedKinds](result)
  }

  test("KindError.CaseSet.04") {
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
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.CaseSet.05") {
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
    val result = compile(input, DefaultOptions)
    expectError[KindError.MismatchedKinds](result)
  }

  test("KindError.CaseSet.06") {
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
        |def f(x: F[s ++ d], y: E[~~d]): String = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.MismatchedKinds](result)
  }

  test("KindError.MissingConstraint.01") {
    val input =
      """
        |class C[a] {
        |    type T
        |}
        |
        |def foo(): C.T[a] = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.MissingConstraint](result)
  }
}
