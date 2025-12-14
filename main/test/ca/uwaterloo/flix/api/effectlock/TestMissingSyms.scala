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
    val filtered = exclude("g", root)
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
    val filtered = exclude("g", root)
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
    val filtered = exclude("a", exclude("b", exclude("c", root)))
    val result = MissingSyms.run(filtered)
    assert(
      containsDef("a", result) &&
        containsDef("b", result) &&
        containsDef("c", result)
    )
  }

  private def exclude(defn0: String, root: TypedAst.Root): TypedAst.Root = {
    root.copy(defs = root.defs.filter { case (sym, _) => sym.toString != defn0 })
  }

  private def containsDef(defn0: String, root: MissingSyms): Boolean = {
    root.defs.exists(g => g.toString == defn0)
  }

}
