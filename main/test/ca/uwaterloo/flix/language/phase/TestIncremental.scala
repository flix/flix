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
import ca.uwaterloo.flix.language.ast.shared.{SecurityContext, SourceName}
import ca.uwaterloo.flix.language.errors.ResolutionError
import ca.uwaterloo.flix.language.errors.TypeError.UnexpectedArg
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}

class TestIncremental extends AnyFunSuite with BeforeAndAfter with TestUtils {

  private val FileA = Path.of("FileA.flix")
  private val FileB = Path.of("FileB.flix")
  private val FileC = Path.of("FileC.flix")
  private val FileD = Path.of("FileD.flix")
  private val FileE = Path.of("FileE.flix")
  private val FileF = Path.of("FileF.flix")
  private val FileG = Path.of("FileG.flix")
  private val FileH = Path.of("FileH.flix")
  private val FileJ = Path.of("FileJ.flix")

  // A new Flix instance is created and initialized with some source code for each test.
  private var flix: Flix = _

  before {
    flix = new Flix()
    flix.addSource(FileA,
      s"""
         |pub def f(x: Bool): Bool = not x
         |
         |""".stripMargin, sctx)
    flix.addSource(FileB,
      raw"""
           |def main(): Unit \ IO =
           |    println(f(true));
           |    println(C.cd(1) |> C.cda)
           |""".stripMargin, sctx)
    flix.addSource(FileC,
      s"""
         |pub trait C[a] {
         |    pub def cf(x: Bool, y: a, z: a): a = if (f(x) == x) y else z
         |    pub def cd(x: a): L[a] = D.DA(x)
         |    pub def cda(l: L[a]): a = match l {
         |        case D.DA(x) => x
         |    }
         |    pub def cga(g: G[a]): a =
         |        let G.G(r) = g;
         |        r#el
         |}
         |""".stripMargin, sctx)
    flix.addSource(FileD,
      s"""
         |pub enum D[a] {
         |    case DA(a)
         |}
         |""".stripMargin, sctx)
    flix.addSource(FileE,
      s"""
         |instance C[Int32] {}
         |""".stripMargin, sctx)
    flix.addSource(FileF,
      s"""
         |pub type alias L[a] = D[a]
         |""".stripMargin, sctx)
    flix.addSource(FileG,
      s"""
         |pub enum G[a]({ el = a })
         |""".stripMargin, sctx)
    flix.addSource(FileH,
      s"""
         |pub trait H[a] with C[a] {
         |    pub def cf(x: Bool, y: a, z: a): a = C.cf(x, y, z)
         |}
         |""".stripMargin, sctx)

    flix.compile().unsafeGet
  }

  test("Incremental.01") {
    flix.addSource(FileA,
      s"""
         |pub def f(x: Int32): Int32 = x + 1i32
         |
         |""".stripMargin, sctx)
    flix.addSource(FileB,
      raw"""
           |def main(): Unit \ IO =
           |    println(f(123))
           |""".stripMargin, sctx)
    flix.addSource(FileC,
      s"""
         |pub trait C[a] {
         |    pub def cf(x: Int32, y: a, z: a): a = if (f(x) == x) y else z
         |    pub def cg(x: a): a
         |}
         |""".stripMargin, sctx)
    flix.remFile(FileE)
    flix.remFile(FileD)
    flix.remFile(FileF)
    flix.remFile(FileG)
    flix.remFile(FileH)

    flix.compile().unsafeGet
  }

  test("Incremental.02") {
    flix.addSource(FileA,
      s"""
         |pub def f(x: String): String = String.toUpperCase(x)
         |""".stripMargin, sctx)
    flix.addSource(FileB,
      raw"""
           |def main(): Unit \ IO =
           |    println(f("Hello World"))
           |""".stripMargin, sctx)
    flix.addSource(FileC,
      s"""
         |pub trait C[a] {
         |    pub def cf(x: String, y: a, z: a): a = if (f(x) == x) y else z
         |}
         |""".stripMargin, sctx)
    flix.addSource(FileH,
      s"""
         |pub trait H[a] with C[a] {
         |    pub def cf(x: String, y: a, z: a): a = C.cf(x, y, z)
         |}
         |""".stripMargin, sctx)
    flix.compile().unsafeGet
  }

  test("Incremental.03") {
    flix.addSource(FileA,
      s"""
         |pub trait C[a] {
         |    pub def cf(x: Bool, y: a, z: a): a = if (f(x) == x) y else z
         |    pub def cd(x: a): D[a] = D.DA(x)
         |    pub def cda(d: D[a]): a = match d {
         |        case D.DA(x) => x
         |    }
         |}
         |""".stripMargin, sctx)
    flix.addSource(FileC,
      s"""
         |pub def f(x: Bool): Bool = not x
         |
         |""".stripMargin, sctx)
    flix.compile().unsafeGet
  }

  test("Incremental.04") {
    flix.addSource(FileA,
      s"""
         |pub def f(x: Int32): Bool = x == 0
         |
         |""".stripMargin, sctx)
    expectError[UnexpectedArg](flix.check())
  }

  test("Incremental.05") {
    flix.addSource(FileA,
      s"""
         |pub def f(x: Int64, y: Int64): Bool = x == y
         |
         |""".stripMargin, sctx)
    flix.addSource(FileB,
      raw"""
           |def main(): Unit \ IO =
           |    println(f(1i64, 2i64));
           |    println(C.cd(1i64) |> C.cda)
           |""".stripMargin, sctx)
    flix.addSource(FileC,
      s"""
         |pub trait C[a] {
         |    pub def cf(x: Int64, b: Int64, y: a, z: a): a = if (f(x, b)) y else z
         |    pub def cd(x: a): D[a]
         |    pub def cda(d: D[a]): a = match d {
         |        case D.DA(x, _)    => x
         |        case D.DB(x, _, _) => x
         |    }
         |    pub def cdaf(x: a, y: a, d: D[a]): Bool
         |}
         |""".stripMargin, sctx)
    flix.addSource(FileD,
      s"""
         |pub enum D[a] {
         |    case DA(a, a)
         |    case DB(a, a, a)
         |}
         |""".stripMargin, sctx)
    flix.addSource(FileE,
      s"""
         |instance C[Int64] {
         |    pub def cd(x: Int64): D[Int64] = D.DB(x, x, x)
         |    pub def cdaf(x: Int64, y: Int64, d: D[Int64]): Bool = match d {
         |        case D.DA(a, b)    => f(x, a) and f(y, b)
         |        case D.DB(_, _, _) => false
         |    }
         |}
         |""".stripMargin, sctx)
    flix.addSource(FileH,
      s"""
         |pub trait H[a] with C[a] {
         |    pub def cf(x: Int64, b: Int64, y: a, z: a): a = C.cf(x, b, y, z)
         |}
         |""".stripMargin, sctx)
    flix.compile().unsafeGet
  }

  test("Incremental.06") {
    flix.addSource(FileA,
      s"""
         |mod F {
         |    pub def f(x: Bool): Bool = not x
         |}
         |""".stripMargin, sctx)
    flix.addSource(FileB,
      raw"""
           |def main(): Unit \ IO =
           |    println(F.f(true));
           |    println(C.cd(1i8) |> C.cda)
           |""".stripMargin, sctx)
    flix.addSource(FileC,
      s"""
         |pub trait C[a] {
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
         |""".stripMargin, sctx)
    flix.addSource(FileD,
      s"""
         |mod DDD {
         |    pub enum D[a] {
         |        case DA(a)
         |    }
         |}
         |""".stripMargin, sctx)
    flix.addSource(FileE,
      s"""
         |instance C[Int8] {}
         |""".stripMargin, sctx)
    flix.addSource(FileF,
      s"""
         |pub type alias L[a] = DDD.D[a]
         |""".stripMargin, sctx)

    flix.compile().unsafeGet
  }

  test("Incremental.07") {
    flix.addSource(FileC,
      s"""
         |pub trait C[a] {
         |    pub def cf(x: Bool, y: a, z: a): a = if (f(x) == x) y else z
         |    pub def cd(x: a): L[a] = { x = x }
         |    pub def cda(l: L[a]): a = l#x
         |}
         |""".stripMargin, sctx)
    flix.addSource(FileF,
      s"""
         |pub type alias L[a] = { x = a }
         |""".stripMargin, sctx)

    flix.compile().unsafeGet
  }

  test("Incremental.08") {
    flix.addSource(FileC,
      s"""
         |pub trait C[a] {
         |    pub def cf(x: Bool, y: a, z: a): a = if (f(x) == x) y else z
         |    pub def cd(x: a): L[a] = D.DA(x)
         |    pub def cda(l: L[a]): a = match l {
         |        case D.DA(x) => x
         |    }
         |    pub def cga(g: G[a]): a =
         |        let G.G(d) = g;
         |        match d {
                      case D.DA(x) => x
         |        }
         |}
         |""".stripMargin, sctx)
    flix.addSource(FileG,
      s"""
         |pub enum G[a](D[a])
         |""".stripMargin, sctx)

    flix.compile().unsafeGet
  }

  test("Incremental.AddFile.ReadWhenAdded") {
    // A file is read when it is added. A later change on disk is not seen until the file is re-added.
    val dir = Files.createTempDirectory("flix-incremental")
    val file = dir.resolve("FileI.flix")
    Files.writeString(file, "pub def i(): Int32 = 1")
    flix.addFile(file, sctx)
    flix.addSource(FileJ,
      s"""
         |def useI(): Int32 = i()
         |""".stripMargin, sctx)
    flix.compile().unsafeGet

    Files.writeString(file, "pub def notI(): Int32 = 1")
    flix.compile().unsafeGet

    flix.addFile(file, sctx)
    expectError[ResolutionError.UndefinedName](flix.check())
  }

  test("Incremental.RemFile.NormalizedPath") {
    // A file added under a path with `..` segments must be removable under that same path.
    val dir = Files.createTempDirectory("flix-incremental")
    Files.createDirectory(dir.resolve("sub"))
    val file = dir.resolve("FileI.flix")
    Files.writeString(file, "pub def i(): Int32 = 1")
    val unnormalized = dir.resolve("sub").resolve("..").resolve("FileI.flix")
    flix.addFile(unnormalized, sctx)
    flix.addSource(FileJ,
      s"""
         |def useI(): Int32 = i()
         |""".stripMargin, sctx)
    flix.compile().unsafeGet

    flix.remFile(unnormalized)
    expectError[ResolutionError.UndefinedName](flix.check())
  }

  test("Incremental.RemSource.Forgotten") {
    // A removed source is forgotten, not kept as an empty source.
    flix.remSource(FileH)
    val (optRoot, errors) = flix.check()
    assert(errors.isEmpty)
    assert(!optRoot.get.sources.keys.exists(_.sourceName == SourceName.PathName(FileH)))
  }

  test("Incremental.RemSource.Dependents") {
    // Removing a source recompiles its dependents, and adding it back repairs them.
    flix.remSource(FileA)
    expectError[ResolutionError.UndefinedName](flix.check())
    flix.addSource(FileA, "pub def f(x: Bool): Bool = not x", sctx)
    val (_, errors) = flix.check()
    assert(errors.isEmpty)
  }

  test("Incremental.DependencyGraph.SurvivesUnchangedCheck") {
    // The dependency graph must survive a check in which nothing changed, which a language server
    // performs constantly, so that a later change to a source still recompiles its dependents.
    val (optRoot, errors) = flix.check()
    assert(errors.isEmpty)
    val dg = optRoot.get.dependencyGraph
    assert(dg.dirty(SourceName.PathName(FileA)).contains(SourceName.PathName(FileB)))
  }

  test("Incremental.RemSource.AfterUnchangedCheck") {
    // Removing a source after a check in which nothing changed must still recompile its dependents.
    // The typed body of `main` in FileB must hold the resolution error, not a stale use of `f`.
    flix.check()
    flix.remSource(FileA)
    val (optRoot, errors) = flix.check()
    expectError[ResolutionError.UndefinedName]((optRoot, errors))
    val main = optRoot.get.defs.collectFirst { case (sym, defn) if sym.text == "main" => defn }.get
    assert(main.exp.toString.contains("UndefinedName"))
  }
}
