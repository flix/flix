/*
 * Copyright 2026 Ry Wiese
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.ast.shared.Origin
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestHtmlDocumentor extends AnyFunSuite with TestUtils {

  /**
    * Returns the fully qualified names of every item that `HtmlDocumentor.checkCoverage` finds
    * to have no doc comment when `s` is compiled as the sole source of a user project.
    */
  private def missingNames(s: String): List[String] = {
    val (optRoot, errors) = check(s, Options.TestWithLibNix)
    if (errors.nonEmpty) {
      fail(s"Expected the input to compile without errors, but got: ${errors.mkString(", ")}")
    }
    HtmlDocumentor.checkCoverage(optRoot.get, Origin.User).map(_.qualifiedName)
  }

  test("HtmlDocumentor.checkCoverage - reports an undocumented public def") {
    val input =
      """
        |mod M {
        |    pub def f(): Int32 = 1
        |}
        |""".stripMargin
    assert(missingNames(input).contains("M.f"))
  }

  test("HtmlDocumentor.checkCoverage - does not report a documented public def") {
    val input =
      """
        |mod M {
        |    /// Documented.
        |    pub def f(): Int32 = 1
        |}
        |""".stripMargin
    assert(!missingNames(input).contains("M.f"))
  }

  test("HtmlDocumentor.checkCoverage - does not report a private def") {
    val input =
      """
        |mod M {
        |    def f(): Int32 = 1
        |    pub def g(): Int32 = f()
        |}
        |""".stripMargin
    assert(!missingNames(input).contains("M.f"))
  }

  test("HtmlDocumentor.checkCoverage - reports an undocumented public module") {
    val input =
      """
        |mod M {
        |    pub def f(): Int32 = 1
        |}
        |""".stripMargin
    assert(missingNames(input).contains("M"))
  }

  test("HtmlDocumentor.checkCoverage - does not report a documented public module") {
    val input =
      """
        |/// Documented.
        |mod M {
        |    pub def f(): Int32 = 1
        |}
        |""".stripMargin
    assert(!missingNames(input).contains("M"))
  }

  test("HtmlDocumentor.checkCoverage - reports an undocumented trait and its undocumented signature") {
    val input =
      """
        |mod M {
        |    pub trait Foo[a] {
        |        pub def bar(x: a): a
        |    }
        |}
        |""".stripMargin
    val missing = missingNames(input)
    assert(missing.contains("M.Foo"))
    assert(missing.contains("M.Foo.bar"))
  }

  test("HtmlDocumentor.checkCoverage - reports an undocumented enum") {
    val input =
      """
        |mod M {
        |    pub enum Color {
        |        case Red
        |    }
        |}
        |""".stripMargin
    assert(missingNames(input).contains("M.Color"))
  }

  test("HtmlDocumentor.checkCoverage - a fully documented project reports nothing") {
    val input =
      """
        |/// M.
        |mod M {
        |    /// f.
        |    pub def f(): Int32 = 1
        |}
        |""".stripMargin
    assert(missingNames(input).isEmpty)
  }
}
