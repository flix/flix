/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.ast.TypedAst
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

  /**
    * A program whose def `f` has both a trait constraint and an equality constraint.
    */
  private val CollectionInput: String =
    """
      |pub trait Collection[a] {
      |    type Elm: Type
      |    pub def head(x: a): Collection.Elm[a]
      |}
      |pub def f(x: a): Int32 with Collection[a] where Collection.Elm[a] ~ Int32 = Collection.head(x)
      |""".stripMargin

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
    // An equality constraint is written after the trait constraints, and its associated type is
    // written with its trait.
    val input =
      """
        |pub trait Collection[a] {
        |    type Elm: Type
        |    pub def head(x: a): Collection.Elm[a]
        |}
        |pub def f(x: a): Int32 with Collection[a] where Collection.Elm[a] ~ Int32 = Collection.head(x)
        |""".stripMargin
    assert(declarationOf("f", input) ==
      "def f(x: a): Int32 with Collection[a] where Collection.Elm[a] ~ Int32")
  }

  test("formatSpec.09") {
    // The universe is written `Univ`. `IO` is an effect of its own.
    assert(declarationOf("f", "pub def f(): Unit \\ Univ = ???") == "def f(): Unit \\ Univ")
  }

  test("formatSpec.10") {
    assert(declarationOf("f", "pub def f(): Unit \\ IO = ???") == "def f(): Unit \\ IO")
  }

  test("formatSpec.11") {
    // An associated type in a type is written with its trait.
    val input =
      """
        |pub trait Collection[a] {
        |    type Elm: Type
        |    pub def head(x: a): Collection.Elm[a]
        |}
        |pub def f(x: a): Collection.Elm[a] with Collection[a] = Collection.head(x)
        |""".stripMargin
    assert(declarationOf("f", input) == "def f(x: a): Collection.Elm[a] with Collection[a]")
  }

  test("formatSpec.12") {
    // The trait of a constraint is written by its name alone, without the module it is declared in.
    val input =
      """
        |mod Shop {
        |    pub trait Sellable[a] {
        |        pub def price(x: a): Int32
        |    }
        |}
        |pub def f(x: a): Int32 with Shop.Sellable[a] = Shop.Sellable.price(x)
        |""".stripMargin
    assert(declarationOf("f", input) == "def f(x: a): Int32 with Sellable[a]")
  }

  test("specHead.01") {
    // The head of a signature without constraints is the whole signature.
    val defn = defOf("f", "pub def f(x: Int32): Int32 = x")
    assert(Pretty.specHead(defn.sym, defn.spec)(fmt).plain == "def f(x: Int32): Int32")
  }

  test("specHead.02") {
    // The head of a signature ends before its constraints, and after its effect.
    val input =
      """
        |eff Ef { def op(): Unit }
        |pub trait Sellable[a] {
        |    pub def price(x: a): Int32
        |}
        |pub def f(x: a): Int32 \ Ef with Sellable[a] = { Ef.op(); Sellable.price(x) }
        |""".stripMargin
    val defn = defOf("f", input)
    assert(Pretty.specHead(defn.sym, defn.spec)(fmt).plain == "def f(x: a): Int32 \\ Ef")
  }

  test("withClause.01") {
    // A declaration without trait constraints has no `with` clause.
    assert(withClauseOf("f", "pub def f(): Unit = ()").isEmpty)
  }

  test("withClause.02") {
    // The clause starts at its keyword and holds every trait constraint, in the order declared.
    val input =
      """
        |pub trait Sellable[a] { pub def price(x: a): Int32 }
        |pub trait Buyable[a] { pub def cost(x: a): Int32 }
        |pub def f(x: a): Int32 with Sellable[a], Buyable[a] = Sellable.price(x) + Buyable.cost(x)
        |""".stripMargin
    assert(withClauseOf("f", input).contains("with Sellable[a], Buyable[a]"))
  }

  test("withClause.03") {
    // The `with` clause does not hold the equality constraints.
    assert(withClauseOf("f", CollectionInput).contains("with Collection[a]"))
  }

  test("whereClause.01") {
    // A declaration without equality constraints has no `where` clause.
    val input =
      """
        |pub trait Sellable[a] {
        |    pub def price(x: a): Int32
        |}
        |pub def f(x: a): Int32 with Sellable[a] = Sellable.price(x)
        |""".stripMargin
    assert(whereClauseOf("f", input).isEmpty)
  }

  test("whereClause.02") {
    // The clause starts at its keyword and holds the equality constraints alone.
    assert(whereClauseOf("f", CollectionInput).contains("where Collection.Elm[a] ~ Int32"))
  }

  test("specParts.01") {
    // The head and the two clauses, with a space between each, are the signature.
    val defn = defOf("f", CollectionInput)
    val sc = defn.spec.declaredScheme
    val head = Pretty.specHead(defn.sym, defn.spec)(fmt)
    val clauses = Pretty.withClause(sc.tconstrs)(fmt).toList ::: Pretty.whereClause(sc.econstrs)(fmt).toList
    assert((head :: clauses).map(_.plain).mkString(" ") == Pretty.spec(defn.sym, defn.spec)(fmt).plain)
  }

  /**
    * Returns the declaration of the def named `name` in `input`, which must compile.
    */
  private def declarationOf(name: String, input: String): String = {
    val defn = defOf(name, input)
    Pretty.spec(defn.sym, defn.spec)(fmt).plain
  }

  /**
    * Returns the `with` clause of the def named `name` in `input`, which must compile, if it has one.
    */
  private def withClauseOf(name: String, input: String): Option[String] =
    Pretty.withClause(defOf(name, input).spec.declaredScheme.tconstrs)(fmt).map(_.plain)

  /**
    * Returns the `where` clause of the def named `name` in `input`, which must compile, if it has one.
    */
  private def whereClauseOf(name: String, input: String): Option[String] =
    Pretty.whereClause(defOf(name, input).spec.declaredScheme.econstrs)(fmt).map(_.plain)

  /**
    * Returns the def named `name` in `input`, which must compile.
    */
  private def defOf(name: String, input: String): TypedAst.Def = {
    val (root, errors) = check(input, TestOptions)
    expectSuccess((root, errors))
    root.get.defs.collectFirst { case (sym, defn) if sym.text == name => defn }.get
  }

}
