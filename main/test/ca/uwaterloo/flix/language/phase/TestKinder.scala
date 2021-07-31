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
import org.scalatest.FunSuite

class TestKinder extends FunSuite with TestUtils {

  private val DefaultOptions = Options.TestWithLibNix

  test("KindError.Def.Effect.01") {
    val input =
      """
        |def f(): Unit & Unit = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Effect.02") {
    val input =
      """
        |def f[a: Type](): Unit & a = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Expression.Ascribe.01") {
    val input =
      """
        |def f(): Int = 1: Pure
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Expression.Ascribe.02") {
    val input =
      """
        |def f(): Int = 1: & Unit
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Expression.Cast.01") {
    val input =
      """
        |def f(): Int = 1 as Pure
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Expression.Cast.02") {
    val input =
      """
        |def f(): Int = 1 as & Unit
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Type.01") {
    val input =
      """
        |def f(x: Int[Int]): Int = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Type.02") {
    val input =
      """
        |def f(x: Int -> Int & Int): Int = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Type.03") {
    val input =
      """
        |def f(x: Pure -> Int & Int): Int = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Type.04") {
    val input =
      """
        |def f[r: Type](x: {name: Int | r} ): Int = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Type.05") {
    val input =
      """
        |def f[r: Type](x: #{| r} ): Int = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Parameter.01") {
    val input =
      """
        |def f(x: Pure): Int = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Def.Parameter.02") {
    val input =
      """
        |enum E[a]
        |
        |def f(x: E): Int = ???
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
        |def f(x: a): Int & a = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.MismatchedKinds](result)
  }

  test("KindError.Def.Mismatch.02") {
    val input =
      """
        |class C[a: Type -> Type]
        |
        |def f(x: a): Int with C[a] = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.MismatchedKinds](result)
  }

  test("KindError.Def.Mismatch.03") {
    val input =
      """
        |def f(x: a -> a & a): Int = ???
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

  test("KindError.Enum.Type.01") {
    val input =
      """
        |enum E {
        |  case C(Int -> Int & Int)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.Enum.Type.02") {
    val input =
      """
        |enum E[a] {
        |  case C(Int -> Int & a)
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
        |type alias T = Pure -> Int
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
  }

  test("KindError.TypeAlias.Type.02") {
    val input =
      """
        |type alias T[a] = Int -> Int & a
        |""".stripMargin
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
    expectError[KindError.MismatchedKinds](result)
  }

  test("KindError.Class.Sig.01") {
    val input =
      """
        |class C[a: Type -> Type] {
        |  pub def f(x: a): Int = ???
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.MismatchedKinds](result)
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
}
