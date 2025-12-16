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
import ca.uwaterloo.flix.api.effectlock.UseGraph.ReachableSym
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestUseGraph extends AnyFunSuite with TestUtils {

  test("UseGraph.01") {
    val input =
      """
        |pub def f(): Unit = ()
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val actual = UseGraph.computeGraph(root).map(mkString).toList
    val expected = List.empty
    assert(actual == expected)
  }

  test("UseGraph.02") {
    val input =
      """
        |pub def f(): Unit = ()
        |pub def g(): Unit = ()
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val actual = UseGraph.computeGraph(root).map(mkString).toList
    val expected = List.empty
    assert(actual == expected)
  }

  test("UseGraph.03") {
    val input =
      """
        |pub def f(): Unit = ()
        |pub def g(): Unit = ()
        |pub def h(): Int32 = 42
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val actual = UseGraph.computeGraph(root).map(mkString).toList
    val expected = List.empty
    assert(actual == expected)
  }

  test("UseGraph.04") {
    val input =
      """
        |pub def f(): Unit = g()
        |pub def g(): Unit = ()
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val actual = UseGraph.computeGraph(root).map(mkString).toList
    val expected = List("f" -> "g")
    assert(actual == expected)
  }

  test("UseGraph.05") {
    val input =
      """
        |pub def f(): Unit = g()
        |pub def h(): Unit = g()
        |pub def g(): Unit = ()
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val actual = UseGraph.computeGraph(root).map(mkString).toList.sorted
    val expected = List("f" -> "g", "h" -> "g").sorted
    assert(actual == expected)
  }

  test("UseGraph.06") {
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
    val actual = UseGraph.computeGraph(root).map(mkString).toList.sorted
    val expected = List("f" -> "a", "g" -> "b", "h" -> "c").sorted
    assert(actual == expected)
  }

  test("UseGraph.07") {
    val input =
      """
        |pub def f(x: a): String with ToString[a] = ToString.toString(x)
        |
        |trait ToString[a: Type] {
        |    pub def toString(x: a): String
        |}
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val actual = UseGraph.computeGraph(root).map(mkString).toList
    val expected = List("f" -> "ToString.toString")
    assert(actual == expected)
  }

  test("UseGraph.08") {
    val input =
      """
        |pub def f(g: a -> b, x: a): String with ToString[b] = ToString.toString(g(x))
        |
        |trait ToString[a: Type] {
        |    pub def toString(x: a): String
        |}
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val actual = UseGraph.computeGraph(root).map(mkString).toList
    val expected = List("f" -> "ToString.toString")
    assert(actual == expected)
  }

  test("UseGraph.09") {
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
    val actual = UseGraph.computeGraph(root).map(mkString).toList.sorted
    val expected = List("f" -> "Bla.write", "f" -> "ToString.toString", "g" -> "h", "h" -> "Bla.write").sorted
    assert(actual == expected)
  }

  test("UseGraph.10") {
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
    val actual = UseGraph.computeGraph(root).map(mkString).toList
    val expected = List("f" -> "ToString.toString")
    assert(actual == expected)
  }

  test("UseGraph.11") {
    val input =
      """
        |pub def f(x: a): Unit = g(x)
        |pub def g(x: a): Unit = f(x)
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val actual = UseGraph.computeGraph(root).map(mkString).toList.sorted
    val expected = List("f" -> "g", "g" -> "f").sorted
    assert(actual == expected)
  }

  test("UseGraph.12") {
    val input =
      """
        |pub def f(x: a): Unit = f(x)
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val actual = UseGraph.computeGraph(root).map(mkString).toList
    val expected = List("f" -> "f")
    assert(actual == expected)
  }

  private def mkString(kv: (ReachableSym, ReachableSym)): (String, String) = kv match {
    case (ReachableSym.DefnSym(source), ReachableSym.DefnSym(dest)) => source.toString -> dest.toString
    case (ReachableSym.DefnSym(source), ReachableSym.SigSym(dest)) => source.toString -> dest.toString
    case (ReachableSym.SigSym(source), ReachableSym.DefnSym(dest)) => source.toString -> dest.toString
    case (ReachableSym.SigSym(source), ReachableSym.SigSym(dest)) => source.toString -> dest.toString
  }

}
