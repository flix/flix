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
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestEffectSafeUpgrade extends AnyFunSuite with TestUtils {

  test("IsEffectSafeUpgrade.01") {
    val input =
      """
        |pub def f(x: Int32): Int32 = x
        |
        |pub def g(x: a): a = x
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("IsEffectSafeUpgrade.02") {
    val input =
      """
        |pub def f(): Unit = ???
        |
        |pub def g(): Unit = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("IsEffectSafeUpgrade.03") {
    val input =
      """
        |pub def f(): Bool -> Unit = ???
        |
        |pub def g(): Bool -> Unit = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("IsEffectSafeUpgrade.04") {
    val input =
      """
        |pub eff E {
        |    def a(): Unit
        |}
        |
        |pub def f(): (Bool -> Unit \ E) = unchecked_cast(() as Bool -> Unit \ E)
        |
        |pub def g(): Bool -> Unit = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("IsEffectSafeUpgrade.05") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |
        |pub def f(_: a -> b \ E): b \ E = unchecked_cast(() as b \ E)
        |
        |pub def g(_: a -> b \ ef): b \ ef = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("IsEffectSafeUpgrade.06") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |
        |pub def f(_: String): String \ E = unchecked_cast("str" as String \ E)
        |
        |pub def g(_: String): String = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("IsEffectSafeUpgrade.07") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: String): String \ {A, E} = unchecked_cast("str" as String \ {A, E})
        |
        |pub def g(_: String): String = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("IsEffectSafeUpgrade.08") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: String): String \ {A, E} = unchecked_cast("str" as String \ {A, E})
        |
        |pub def g(_: String): String \ A = unchecked_cast("str" as String \ A)
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("IsEffectSafeUpgrade.09") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: a -> b \ ef): b \ {ef, A, E} = unchecked_cast(() as b \ {ef, A, E})
        |
        |pub def g(_: a -> b \ ef): b \ {ef, A} = unchecked_cast(() as b \ {ef, A})
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("IsEffectSafeUpgrade.10") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: a -> b \ ef): String \ {ef, A, E} = unchecked_cast("str" as String \ {ef, A, E})
        |
        |pub def g(_: a -> b \ ef): String \ {ef, A, E} = unchecked_cast("str" as String \ {ef, A, E})
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("IsEffectSafeUpgrade.11") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: a -> b \ ef): String \ {ef, A, E} = unchecked_cast("str" as String \ {ef, A, E})
        |
        |pub def g(_: a -> b \ ef): String \ {A, E} = unchecked_cast("str" as String \ {A, E})
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("IsEffectSafeUpgrade.12") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: a -> b \ ef): String \ {ef, A, E} = unchecked_cast("str" as String \ {ef, A, E})
        |
        |pub def g(_: a -> b): String \ {A, E} = unchecked_cast("str" as String \ {A, E})
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  ignore("IsEffectSafeUpgrade.13") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: a -> b \ ef): String \ {ef, A, E} = unchecked_cast("str" as String \ {ef, A, E})
        |
        |pub def g(_: a -> b): String \ {A, E} = unchecked_cast("str" as String \ {A, E})
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("g", "f", result.get))
  }

  ignore("IsEffectSafeUpgrade.14") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: a -> b \ ef): String \ {ef, A, E} = unchecked_cast("str" as String \ {ef, A, E})
        |
        |pub def g(_: a -> b \ ef): String \ {A, E} = unchecked_cast("str" as String \ {A, E})
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("g", "f", result.get))
  }

  ignore("IsEffectSafeUpgrade.15") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: a -> b \ {ef1, ef2}): String \ {ef1, ef2, A, E} = unchecked_cast("str" as String \ {ef1, ef2, A, E})
        |
        |pub def g(_: a -> b \ ef): String \ {ef, A, E} = unchecked_cast("str" as String \ {ef, A, E})
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("g", "f", result.get))
  }

  test("IsEffectSafeUpgrade.16") {
    val input =
      """
        |pub def f(_: Unit -> Unit): Unit = ()
        |
        |pub def g(_: a -> b): Unit = ()
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("IsEffectSafeUpgrade.17") {
    val input =
      """
        |pub def f(_: Unit -> Unit \ ef): Unit = ()
        |
        |pub def g(_: a -> b \ ef): Unit = ()
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("IsEffectSafeUpgrade.18") {
    val input =
      """
        |pub def f(_: Unit -> Unit): Unit = ()
        |
        |pub def g(_: a -> b \ ef): Unit = ()
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("IsEffectSafeUpgrade.19") {
    val input =
      """
        |pub eff E {
        |    def a(): Unit
        |}
        |
        |pub def f(_: Unit -> Unit \ E): Unit = ()
        |
        |pub def g(_: a -> b \ ef): Unit = ()
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("IsEffectSafeUpgrade.20") {
    val input =
      """
        |pub eff E {
        |    def a(): Unit
        |}
        |
        |pub def f(_: Unit -> b \ ef): Unit = ()
        |
        |pub def g(_: a -> b \ ef): Unit = ()
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("IsEffectSafeUpgrade.21") {
    val input =
      """
        |pub def f(_: a -> b): Unit = ()
        |
        |pub def g(_: a -> b \ ef): Unit \ ef = checked_ecast(())
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result.get))
  }

  test("IsEffectSafeUpgrade.Negative.01") {
    val input =
      """
        |pub eff E {
        |    def a(): Unit
        |}
        |
        |pub def f(): (Bool -> Unit \ E) = unchecked_cast(() as Bool -> Unit \ E)
        |
        |pub def g(): Bool -> Unit = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  test("IsEffectSafeUpgrade.Negative.02") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |
        |pub def f(_: a -> b \ E): b \ E = unchecked_cast(() as b \ E)
        |
        |pub def g(_: a -> b \ ef): b \ ef = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  test("IsEffectSafeUpgrade.Negative.03") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |
        |pub def f(_: String): String \ E = unchecked_cast("str" as String \ E)
        |
        |pub def g(_: String): String = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  test("IsEffectSafeUpgrade.Negative.04") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: String): String \ {A, E} = unchecked_cast("str" as String \ {A, E})
        |
        |pub def g(_: String): String = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  test("IsEffectSafeUpgrade.Negative.05") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: String): String \ {A, E} = unchecked_cast("str" as String \ {A, E})
        |
        |pub def g(_: String): String \ A = unchecked_cast("str" as String \ A)
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  test("IsEffectSafeUpgrade.Negative.06") {
    val input =
      """
        |pub eff E {
        |    def e(): Unit
        |}
        |pub eff A {
        |    def e(): Unit
        |}
        |
        |pub def f(_: a -> b \ ef): String \ {ef, A, E} = unchecked_cast("str" as String \ {ef, A, E})
        |
        |pub def g(_: a -> b \ ef): String \ {ef, A} = unchecked_cast("str" as String \ {ef, A})
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  test("IsEffectSafeUpgrade.Negative.07") {
    val input =
      """
        |pub def f(_: Unit -> Unit \ ef): Unit = ()
        |
        |pub def g(_: Int32 -> Unit \ ef): Unit = ()
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  test("IsEffectSafeUpgrade.Negative.08") {
    val input =
      """
        |pub def f(_: Unit -> Unit): Unit = ()
        |
        |pub def g(_: a -> b): Unit = ()
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  test("IsEffectSafeUpgrade.Negative.09") {
    val input =
      """
        |pub def f(_: Unit -> Unit \ ef): Unit = ()
        |
        |pub def g(_: a -> b \ ef): Unit = ()
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  test("IsEffectSafeUpgrade.Negative.10") {
    val input =
      """
        |pub def f(_: Unit -> Unit): Unit = ()
        |
        |pub def g(_: a -> b \ ef): Unit = ()
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  test("IsEffectSafeUpgrade.Negative.11") {
    val input =
      """
        |pub eff E {
        |    def a(): Unit
        |}
        |
        |pub def f(_: Unit -> Unit \ E): Unit = ()
        |
        |pub def g(_: a -> b \ ef): Unit = ()
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  test("IsEffectSafeUpgrade.Negative.12") {
    val input =
      """
        |pub eff E {
        |    def a(): Unit
        |}
        |
        |pub def f(_: Unit -> b \ ef): Unit = ()
        |
        |pub def g(_: a -> b \ ef): Unit = ()
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("g", "f", result.get))
  }

  private def checkIsSafe(original: String, upgrade: String, root: TypedAst.Root): Boolean = {
    implicit val flix: Flix = new Flix()
    val orig = stringToDef(original, root).get
    val upgr = stringToDef(upgrade, root).get
    EffectUpgrade.isEffSafeUpgrade(orig.spec.declaredScheme, upgr.spec.declaredScheme)
  }

  private def stringToDef(defn: String, root: TypedAst.Root): Option[TypedAst.Def] = {
    root.defs.find {
      case (sym, _) => sym.text == defn
    }.map(_._2)
  }

}
