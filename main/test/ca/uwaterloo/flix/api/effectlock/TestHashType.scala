/*
 * Copyright 2026 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.ast.{Type, TypedAst}
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestHashType extends AnyFunSuite with TestUtils {

  test("determinism.01") {
    val input =
      """
        |pub def f(x: Int8): Int8 = x
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    val t = getTypeOf("f", result.get).get
    val h1 = HashType.hashType(t)
    val h2 = HashType.hashType(t)
    assert(h1 sameElements h2)
  }

  test("determinism.02") {
    val input =
      """
        |pub def f(x: Int16): Int16 = x
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    val t = getTypeOf("f", result.get).get
    val h1 = HashType.hashType(t)
    val h2 = HashType.hashType(t)
    assert(h1 sameElements h2)
  }

  test("determinism.03") {
    val input =
      """
        |pub def f(x: Int32): Int32 = x
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    val t = getTypeOf("f", result.get).get
    val h1 = HashType.hashType(t)
    val h2 = HashType.hashType(t)
    assert(h1 sameElements h2)
  }

  test("determinism.04") {
    val input =
      """
        |pub def f(x: Int64): Int64 = x
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    val t = getTypeOf("f", result.get).get
    val h1 = HashType.hashType(t)
    val h2 = HashType.hashType(t)
    assert(h1 sameElements h2)
  }

  test("determinism.05") {
    val input =
      """
        |pub def f(x: Char): Char = x
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    val t = getTypeOf("f", result.get).get
    val h1 = HashType.hashType(t)
    val h2 = HashType.hashType(t)
    assert(h1 sameElements h2)
  }

  test("determinism.06") {
    val input =
      """
        |pub def f(x: Bool): Bool = x
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    val t = getTypeOf("f", result.get).get
    val h1 = HashType.hashType(t)
    val h2 = HashType.hashType(t)
    assert(h1 sameElements h2)
  }

  test("determinism.07") {
    val input =
      """
        |pub def f(x: Float32): Float32 = x
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    val t = getTypeOf("f", result.get).get
    val h1 = HashType.hashType(t)
    val h2 = HashType.hashType(t)
    assert(h1 sameElements h2)
  }

  test("determinism.08") {
    val input =
      """
        |pub def f(x: Float64): Float64 = x
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    val t = getTypeOf("f", result.get).get
    val h1 = HashType.hashType(t)
    val h2 = HashType.hashType(t)
    assert(h1 sameElements h2)
  }

  test("determinism.09") {
    val input =
      """
        |pub def f(x: String): String = x
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    val t = getTypeOf("f", result.get).get
    val h1 = HashType.hashType(t)
    val h2 = HashType.hashType(t)
    assert(h1 sameElements h2)
  }

  test("determinism.10") {
    val input =
      """
        |pub def f(x: Unit): Unit = x
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    val t = getTypeOf("f", result.get).get
    val h1 = HashType.hashType(t)
    val h2 = HashType.hashType(t)
    assert(h1 sameElements h2)
  }

  test("determinism.11") {
    val input =
      s"""
         |pub def f(): (Int32, (Int32, Int32)) = ???
         |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    val t = getTypeOf("f", result.get).get
    val h1 = HashType.hashType(t)
    val h2 = HashType.hashType(t)
    assert(h1 sameElements h2)
  }

  test("determinism.12") {
    val input =
      s"""
         |pub def f(x: Array[Int32, r]): Int32 \\ r = ???
         |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    val t = getTypeOf("f", result.get).get
    val h1 = HashType.hashType(t)
    val h2 = HashType.hashType(t)
    assert(h1 sameElements h2)
  }

  test("determinism.13") {
    val input =
      s"""
         |pub def f(): { a = { x = Bool }, x = Int32 | r } = ???
         |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    val t = getTypeOf("f", result.get).get
    val h1 = HashType.hashType(t)
    val h2 = HashType.hashType(t)
    assert(h1 sameElements h2)
  }

  test("determinism.14") {
    val input =
      s"""
         |enum A {
         |    case T
         |}
         |pub def f(): A = A.T
         |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    val t = getTypeOf("f", result.get).get
    val h1 = HashType.hashType(t)
    val h2 = HashType.hashType(t)
    assert(h1 sameElements h2)
  }

  test("determinism.15") {
    val input =
      s"""
         |enum A[a] {
         |    case T(a)
         |}
         |pub def f(x: A[a]): A[a] = x
         |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    val t = getTypeOf("f", result.get).get
    val h1 = HashType.hashType(t)
    val h2 = HashType.hashType(t)
    assert(h1 sameElements h2)
  }

  test("determinism.16") {
    val input =
      s"""
         |struct A[r] {
         |    name: String
         |}
         |pub def f(rc: Region[r]): A[r] \\ r = ???
         |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    val t = getTypeOf("f", result.get).get
    val h1 = HashType.hashType(t)
    val h2 = HashType.hashType(t)
    assert(h1 sameElements h2)
  }

  test("determinism.17") {
    val input =
      s"""
         |eff A {
         |    def g(): Void
         |}
         |pub def f(): Void \\ A = A.g()
         |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    val t = getTypeOf("f", result.get).get
    val h1 = HashType.hashType(t)
    val h2 = HashType.hashType(t)
    assert(h1 sameElements h2)
  }

  test("determinism.18") {
    val input =
      s"""
         |eff A {
         |    def g(): Void
         |}
         |pub def f(h: a -> b \\ ef1 & ef2): Void \\ (ef1 & ef2) - A = ???
         |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    val t = getTypeOf("f", result.get).get
    val h1 = HashType.hashType(t)
    val h2 = HashType.hashType(t)
    assert(h1 sameElements h2)
  }

  test("injective.01") {
    val input =
      """
        |pub def f(x: Int8): Int8 = x
        |pub def g(x: Int8): Bool = ???
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    val t1 = getTypeOf("f", result.get).get
    val t2 = getTypeOf("g", result.get).get
    val h1 = HashType.hashType(t1)
    val h2 = HashType.hashType(t2)
    assert(!(h1 sameElements h2))
  }

  // TODO: Traits, region syms, assoc types, datalog, java class

  private def getTypeOf(defn0: String, root0: TypedAst.Root): Option[Type] = {
    root0.defs.find {
      case (sym, _) => sym.text == defn0
    }.map {
      case (_, defn) => defn.spec.declaredScheme.base
    }
  }
}
