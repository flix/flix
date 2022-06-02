/*
 * Copyright 2022 Magnus Madsen
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
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.errors.TypeError.MismatchedTypes
import org.scalatest.{BeforeAndAfter, FunSuite}

import java.nio.file.Path

class TestIncremental extends FunSuite with BeforeAndAfter with TestUtils {

  private val FileA = "FileA.flix"
  private val FileB = "FileB.flix"
  private val FileC = "FileC.flix"
  private val FileD = "FileD.flix"
  private val FileE = "FileE.flix"
  private val FileF = "FileF.flix"
  private val FileG = "FileG.flix"
  private val FileH = "FileH.flix"

  // A new Flix instance is created and initialized with some source code for each test.

  private var flix: Flix = _

  before {
    flix = new Flix()
    flix.addSourceCode(FileA,
      s"""
         |pub def f(x: Bool): Bool = not x
         |
         |""".stripMargin)
    flix.addSourceCode(FileB,
      s"""
         |def main(): Unit & Impure =
         |    println(f(true));
         |    println(C.cd(1) |> C.cda)
         |""".stripMargin)
    flix.addSourceCode(FileC,
      s"""
         |pub class C[a] {
         |    pub def cf(x: Bool, y: a, z: a): a = if (f(x) == x) y else z
         |    pub def cd(x: a): L[a] = DA(x)
         |    pub def cda(l: L[a]): a = match l {
         |        case DA(x) => x
         |    }
         |    pub def cga(g: G[a]): a =
         |        let G(r) = g;
         |        r.el
         |}
         |""".stripMargin)
    flix.addSourceCode(FileD,
      s"""
         |pub enum D[a] {
         |    case DA(a)
         |}
         |""".stripMargin)
    flix.addSourceCode(FileE,
      s"""
         |instance C[Int32] {}
         |""".stripMargin)
    flix.addSourceCode(FileF,
      s"""
         |pub type alias L[a] = D[a]
         |""".stripMargin)
    flix.addSourceCode(FileG,
      s"""
         |pub enum G[a]({ el :: a })
         |""".stripMargin)
    flix.addSourceCode(FileH,
      s"""
         |pub class H[a] with C[a] {
         |    pub def cf(x: Bool, y: a, z: a): a = C.cf(x, y, z)
         |}
         |""".stripMargin)

    flix.compile().get
  }

  test("Incremental.01") {
    flix.addSourceCode(FileA,
      s"""
         |pub def f(x: Int32): Int32 = x + 1i32
         |
         |""".stripMargin)
    flix.addSourceCode(FileB,
      s"""
         |def main(): Unit & Impure =
         |    println(f(123))
         |""".stripMargin)
    flix.addSourceCode(FileC,
      s"""
         |pub class C[a] {
         |    pub def cf(x: Int32, y: a, z: a): a = if (f(x) == x) y else z
         |    pub def cg(x: a): a
         |}
         |""".stripMargin)
    flix.remSourcePath(Path.of(FileE))
    flix.remSourcePath(Path.of(FileD))
    flix.remSourcePath(Path.of(FileF))
    flix.remSourcePath(Path.of(FileG))
    flix.remSourcePath(Path.of(FileH))

    flix.compile().get
  }

  test("Incremental.02") {
    flix.addSourceCode(FileA,
      s"""
         |pub def f(x: String): String = String.toUpperCase(x)
         |""".stripMargin)
    flix.addSourceCode(FileB,
      s"""
         |def main(): Unit & Impure =
         |    println(f("Hello World"))
         |""".stripMargin)
    flix.addSourceCode(FileC,
      s"""
         |pub class C[a] {
         |    pub def cf(x: String, y: a, z: a): a = if (f(x) == x) y else z
         |}
         |""".stripMargin)
    flix.addSourceCode(FileH,
      s"""
         |pub class H[a] with C[a] {
         |    pub def cf(x: String, y: a, z: a): a = C.cf(x, y, z)
         |}
         |""".stripMargin)
    flix.compile().get
  }

  test("Incremental.03") {
    flix.addSourceCode(FileA,
      s"""
         |pub class C[a] {
         |    pub def cf(x: Bool, y: a, z: a): a = if (f(x) == x) y else z
         |    pub def cd(x: a): D[a] = DA(x)
         |    pub def cda(d: D[a]): a = match d {
         |        case DA(x) => x
         |    }
         |}
         |""".stripMargin)
    flix.addSourceCode(FileC,
      s"""
         |pub def f(x: Bool): Bool = not x
         |
         |""".stripMargin)
    flix.compile().get
  }

  test("Incremental.04") {
    flix.addSourceCode(FileA,
      s"""
         |pub def f(x: Int32): Bool = x == 0
         |
         |""".stripMargin)
    expectError[MismatchedTypes](flix.compile())
  }

  test("Incremental.05") {
    flix.addSourceCode(FileA,
      s"""
         |pub def f(x: Int64, y: Int64): Bool = x == y
         |
         |""".stripMargin)
    flix.addSourceCode(FileB,
      s"""
         |def main(): Unit & Impure =
         |    println(f(1i64, 2i64));
         |    println(C.cd(1i64) |> C.cda)
         |""".stripMargin)
    flix.addSourceCode(FileC,
      s"""
         |pub class C[a] {
         |    pub def cf(x: Int64, b: Int64, y: a, z: a): a = if (f(x, b)) y else z
         |    pub def cd(x: a): D[a]
         |    pub def cda(d: D[a]): a = match d {
         |        case DA(x, _)    => x
         |        case DB(x, _, _) => x
         |    }
         |    pub def cdaf(x: a, y: a, d: D[a]): Bool
         |}
         |""".stripMargin)
    flix.addSourceCode(FileD,
      s"""
         |pub enum D[a] {
         |    case DA(a, a)
         |    case DB(a, a, a)
         |}
         |""".stripMargin)
    flix.addSourceCode(FileE,
      s"""
         |instance C[Int64] {
         |    pub def cd(x: Int64): D[Int64] = DB(x, x, x)
         |    pub def cdaf(x: Int64, y: Int64, d: D[Int64]): Bool = match d {
         |        case DA(a, b)    => f(x, a) and f(y, b)
         |        case DB(_, _, _) => false
         |    }
         |}
         |""".stripMargin)
    flix.addSourceCode(FileH,
      s"""
         |pub class H[a] with C[a] {
         |    pub def cf(x: Int64, b: Int64, y: a, z: a): a = C.cf(x, b, y, z)
         |}
         |""".stripMargin)
    flix.compile().get
  }

  test("Incremental.06") {
    flix.addSourceCode(FileA,
      s"""
         |namespace F {
         |    pub def f(x: Bool): Bool = not x
         |}
         |""".stripMargin)
    flix.addSourceCode(FileB,
      s"""
         |def main(): Unit & Impure =
         |    println(F.f(true));
         |    println(C.cd(1i8) |> C.cda)
         |""".stripMargin)
    flix.addSourceCode(FileC,
      s"""
         |pub class C[a] {
         |    pub def cf(x: Bool, y: a, z: a): a = if (F.f(x) == x) y else z
         |    pub def cd(x: a): DDD.D[a] =
         |        use DDD.D;
         |        D.DA(x)
         |    pub def cda(d: DDD.D[a]): a =
         |        use DDD.D;
         |        match d {
         |          case D.DA(x) => x
         |        }
         |}
         |""".stripMargin)
    flix.addSourceCode(FileD,
      s"""
         |namespace DDD {
         |    pub enum D[a] {
         |        case DA(a)
         |    }
         |}
         |""".stripMargin)
    flix.addSourceCode(FileE,
      s"""
         |instance C[Int8] {}
         |""".stripMargin)
    flix.addSourceCode(FileF,
      s"""
         |pub type alias L[a] = DDD.D[a]
         |""".stripMargin)

    flix.compile().get
  }

  test("Incremental.07") {
    flix.addSourceCode(FileC,
      s"""
         |pub class C[a] {
         |    pub def cf(x: Bool, y: a, z: a): a = if (f(x) == x) y else z
         |    pub def cd(x: a): L[a] = { x = x }
         |    pub def cda(l: L[a]): a = l.x
         |}
         |""".stripMargin)
    flix.addSourceCode(FileF,
      s"""
         |pub type alias L[a] = { x :: a }
         |""".stripMargin)

    flix.compile().get
  }

  test("Incremental.08") {
    flix.addSourceCode(FileC,
      s"""
         |pub class C[a] {
         |    pub def cf(x: Bool, y: a, z: a): a = if (f(x) == x) y else z
         |    pub def cd(x: a): L[a] = DA(x)
         |    pub def cda(l: L[a]): a = match l {
         |        case DA(x) => x
         |    }
         |    pub def cga(g: G[a]): a =
         |        let G(d) = g;
         |        match d {
                      case DA(x) => x
         |        }
         |}
         |""".stripMargin)
    flix.addSourceCode(FileG,
      s"""
         |pub opaque type G[a] = D[a]
         |""".stripMargin)

    flix.compile().get
  }
}
