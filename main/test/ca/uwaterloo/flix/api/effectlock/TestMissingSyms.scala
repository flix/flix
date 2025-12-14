/*
 * Copyright 2025 Jakob Schneider Villumsen
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
import ca.uwaterloo.flix.api.effectlock.MissingSyms.MissingSyms
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestMissingSyms extends AnyFunSuite with TestUtils {

  test("MissingSyms.01") {
    val input =
      """
        |pub def f(): Unit = ()
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val result = MissingSyms.run(root)
    assert(result.isEmpty)
  }

  test("MissingSyms.02") {
    val input =
      """
        |pub def f(): Unit = ()
        |pub def g(): Unit = ()
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val result = MissingSyms.run(root)
    assert(result.isEmpty)
  }

  test("MissingSyms.03") {
    val input =
      """
        |pub def f(): Unit = ()
        |pub def g(): Unit = ()
        |pub def h(): Int32 = 42
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val result = MissingSyms.run(root)
    assert(result.isEmpty)
  }

  test("MissingSyms.04") {
    val input =
      """
        |pub def f(): Unit = g()
        |pub def g(): Unit = ()
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val filtered = excludeDef("g", root)
    val result = MissingSyms.run(filtered)
    assert(containsDef("g", result))
  }

  test("MissingSyms.05") {
    val input =
      """
        |pub def f(): Unit = g()
        |pub def h(): Unit = g()
        |pub def g(): Unit = ()
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val filtered = excludeDef("g", root)
    val result = MissingSyms.run(filtered)
    assert(containsDef("g", result))
  }

  test("MissingSyms.06") {
    val input =
      """
        |pub def f(): Unit = a()
        |pub def g(): Unit = b()
        |pub def h(): Unit = c()
        |
        |pub def a(): Unit = ()
        |pub def b(): Unit = ()
        |pub def c(): Unit = ()
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val filtered = excludeDefs(List("a", "b", "c"), root)
    val result = MissingSyms.run(filtered)
    assert(
      containsDef("a", result) &&
        containsDef("b", result) &&
        containsDef("c", result)
    )
  }

  test("MissingSyms.07") {
    val input =
      """
        |pub def f(x: a): String with ToString[a] = ToString.toString(x)
        |
        |trait ToString[a: Type] {
        |    pub def toString(x: a): String
        |}
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val filtered = excludeSig("ToString.toString", root)
    val result = MissingSyms.run(filtered)
    assert(containsSig("ToString.toString", result))
  }

  test("MissingSyms.08") {
    val input =
      """
        |pub def f(g: a -> b, x: a): String with ToString[b] = ToString.toString(g(x))
        |
        |trait ToString[a: Type] {
        |    pub def toString(x: a): String
        |}
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val result = MissingSyms.run(root)
    assert(!containsDef("g", result))
  }

  test("MissingSyms.09") {
    val input =
      """
        |pub def f(x: a): Unit with ToString[a], Bla[a] = Bla.write(ToString.toString(x))
        |pub def g(x: a): Unit with Bla[a] = h(x)
        |pub def h(x: a): Unit with Bla[a] = Bla.write(x)
        |
        |trait ToString[a: Type] {
        |    pub def toString(x: a): String
        |}
        |
        |trait Bla[a: Type] {
        |    pub def write(x: a): Unit
        |}
        |
        |instance Bla[String] {
        |    pub def write(_: String): Unit = ()
        |}
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val filtered = excludeDef("h", excludeSigs(List("ToString.toString", "Bla.write"), root))
    val result = MissingSyms.run(filtered)
    assert(
      containsDef("h", result) &&
        containsSig("ToString.toString", result) &&
        containsSig("Bla.write", result)
    )
  }

  test("MissingSyms.10") {
    val input =
      """
        |pub def f(x: a): Unit with ToString[a], Bla[a] = Bla.write(ToString.toString(x))
        |pub def g(x: a): Unit with Bla[a] = h(x)
        |pub def h(x: a): Unit with Bla[a] = Bla.write(x)
        |
        |trait ToString[a: Type] {
        |    pub def toString(x: a): String
        |}
        |
        |trait Bla[a: Type] {
        |    pub def write(x: a): Unit
        |}
        |
        |instance Bla[String] {
        |    pub def write(_: String): Unit = ()
        |}
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val filtered = excludeSigs(List("ToString.toString", "Bla.write"), root)
    val result = MissingSyms.run(filtered)
    assert(
      !containsDef("h", result) &&
        containsSig("ToString.toString", result) &&
        containsSig("Bla.write", result)
    )
  }

  test("MissingSyms.11") {
    val input =
      """
        |pub def f(x: a): String with ToString[a], Bla[a] = ToString.toString(x)
        |pub def g(x: a): Unit with Bla[a] = h(x)
        |pub def h(x: a): Unit with Bla[a] = Bla.write(x)
        |
        |trait ToString[a: Type] {
        |    pub def toString(x: a): String
        |}
        |
        |trait Bla[a: Type] {
        |    pub def write(x: a): Unit
        |}
        |
        |instance Bla[String] {
        |    pub def write(_: String): Unit = ()
        |}
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val filtered = excludeSigs(List("ToString.toString", "Bla.write"), root)
    val result = MissingSyms.run(filtered)
    assert(
      !containsDef("h", result) &&
        containsSig("ToString.toString", result) &&
        containsSig("Bla.write", result)
    )
  }

  test("MissingSyms.12") {
    val input =
      """
        |pub def f(x: a): String with ToString[a], Bla[a] = ToString.toString(x)
        |pub def g(x: a): Unit with Bla[a] = h(x)
        |pub def h(x: a): Unit with Bla[a] = Bla.write(x)
        |
        |trait ToString[a: Type] {
        |    pub def toString(x: a): String
        |}
        |
        |trait Bla[a: Type] {
        |    pub def write(x: a): Unit
        |}
        |
        |instance Bla[String] {
        |    pub def write(_: String): Unit = ()
        |}
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val filtered = excludeSigs(List("ToString.toString"), root)
    val result = MissingSyms.run(filtered)
    assert(
      !containsDef("h", result) &&
        containsSig("ToString.toString", result) &&
        !containsSig("Bla.write", result)
    )
  }

  test("MissingSyms.13") {
    val input =
      """
        |pub def f(x: a): String with ToString[a] = Bla.write(ToString.toString(x))
        |pub def g(x: a): Unit with Bla[a] = h(x)
        |pub def h(x: a): Unit with Bla[a] = Bla.write(x)
        |
        |trait ToString[a: Type] {
        |    pub def toString(x: a): String
        |}
        |
        |trait Bla[a: Type] {
        |    pub def write(x: a): Unit
        |}
        |
        |instance Bla[String] {
        |    pub def write(_: String): Unit = ()
        |}
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val filtered = excludeDef("h", excludeSigs(List("ToString.toString"), root))
    val result = MissingSyms.run(filtered)
    assert(
      containsDef("h", result) &&
        containsSig("ToString.toString", result) &&
        containsSig("Bla.write", result)
    )
  }

  test("MissingSyms.14") {
    val input =
      """
        |pub def f(x: a): String with ToString[a] = ToString.toString(x)
        |
        |trait ToString[a: Type] {
        |    pub def toString(x: a): String
        |}
        |
        |trait Unused[a: Type] {
        |    pub def unreachable(x: a): String
        |}
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val result = MissingSyms.run(root)
    assert(!containsSig("Unused.unreachable", result))
  }

  test("MissingSyms.15") {
    val input =
      """
        |pub def f(x: a): String with ToString[a] = ToString.toString(x)
        |
        |trait ToString[a: Type] {
        |    pub def toString(x: a): String
        |}
        |
        |trait Unused[a: Type] {
        |    pub def unreachable(x: a): String
        |}
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val filtered = excludeSig("Unused.unreachable", root)
    val result = MissingSyms.run(filtered)
    assert(!containsSig("Unused.unreachable", result))
  }

  private def excludeDef(defn0: String, root: TypedAst.Root): TypedAst.Root = {
    root.copy(defs = root.defs.filter { case (sym, _) => sym.toString != defn0 })
  }

  private def excludeDefs(defs0: List[String], root: TypedAst.Root): TypedAst.Root = {
    defs0.foldLeft(root)((r, defn) => excludeDef(defn, r))
  }

  private def excludeSig(sig0: String, root: TypedAst.Root): TypedAst.Root = {
    root.copy(sigs = root.sigs.filter { case (sym, _) => sym.toString != sig0 })
  }

  private def excludeSigs(sigs0: List[String], root: TypedAst.Root): TypedAst.Root = {
    sigs0.foldLeft(root)((r, defn) => excludeSig(defn, r))
  }

  private def containsDef(defn0: String, root: MissingSyms): Boolean = {
    root.defs.exists(g => g.toString == defn0)
  }

  private def containsSig(sig0: String, root: MissingSyms): Boolean = {
    root.sigs.exists(g => g.toString == sig0)
  }

}
