/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestFormatSignature extends AnyFunSuite with TestUtils {

  private val fmt: FormatOptions = FormatOptions(FormatOptions.VarName.NameBased)

  /**
    * The options the programs of this suite are checked with.
    *
    * The chaos monkey is off. It randomly permutes the trait and equality constraints of a
    * declaration in [[ca.uwaterloo.flix.language.phase.Namer]], and the tests below check that
    * the constraints are written in the order they were declared.
    */
  private val TestOptions: Options = Options.TestWithLibMin.copy(xchaosMonkey = false)

  test("formatSpec.01") {
    assert(declarationOf("f", "pub def f(): Unit = ()") == "def f(): Unit")
  }

  test("formatSpec.02") {
    assert(declarationOf("f", "pub def f(x: Int32): Int32 = x") == "def f(x: Int32): Int32")
  }

  test("formatSpec.03") {
    assert(declarationOf("f", "pub def f(x: Int32, y: Bool): Int32 = if (y) x else x") ==
      "def f(x: Int32, y: Bool): Int32")
  }

  test("formatSpec.04") {
    // The name of a parameter is part of the declaration, and a type scheme does not have it.
    assert(declarationOf("f", "pub def f(needle: a, haystack: Vector[a]): Bool = ???") ==
      "def f(needle: a, haystack: Vector[a]): Bool")
  }

  test("formatSpec.05") {
    val input =
      """
        |eff Ef { def op(): Unit }
        |pub def f(): Unit \ Ef = Ef.op()
        |""".stripMargin
    assert(declarationOf("f", input) == "def f(): Unit \\ Ef")
  }

  test("formatSpec.06") {
    // A trait constraint is part of the signature, and is written as the declaration writes it.
    val input =
      """
        |pub trait Sellable[a] {
        |    pub def price(x: a): Int32
        |}
        |pub def f(x: a): Int32 with Sellable[a] = Sellable.price(x)
        |""".stripMargin
    assert(declarationOf("f", input) == "def f(x: a): Int32 with Sellable[a]")
  }

  test("formatSpec.07") {
    // Several trait constraints are written in the order they were declared.
    val input =
      """
        |pub trait Sellable[a] { pub def price(x: a): Int32 }
        |pub trait Buyable[a] { pub def cost(x: a): Int32 }
        |pub def f(x: a): Int32 with Sellable[a], Buyable[a] = Sellable.price(x) + Buyable.cost(x)
        |""".stripMargin
    assert(declarationOf("f", input) == "def f(x: a): Int32 with Sellable[a], Buyable[a]")
  }

  test("formatSpec.08") {
    // An equality constraint is written after the trait constraints.
    val input =
      """
        |pub trait Collection[a] {
        |    type Elm: Type
        |    pub def head(x: a): Collection.Elm[a]
        |}
        |pub def f(x: a): Int32 with Collection[a] where Collection.Elm[a] ~ Int32 = Collection.head(x)
        |""".stripMargin
    assert(declarationOf("f", input) ==
      "def f(x: a): Int32 with Collection[a] where Elm[a] ~ Int32")
  }

  /**
    * Returns the declaration of the def named `name` in `input`, which must compile.
    */
  private def declarationOf(name: String, input: String): String = {
    val (root, errors) = check(input, TestOptions)
    expectSuccess((root, errors))
    val defn = root.get.defs.collectFirst { case (sym, defn) if sym.text == name => defn }.get
    FormatSignature.formatSpecWithOptions(name, defn.spec, fmt)
  }

}
