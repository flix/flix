/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.api.lsp.provider.HoverProvider
import ca.uwaterloo.flix.api.{CompilerConstants, Flix}
import ca.uwaterloo.flix.language.ast.shared.{SecurityContext, SourceName}
import ca.uwaterloo.flix.util.{LibLevel, Options}
import org.scalatest.funsuite.AnyFunSuite

class TestHoverProvider extends AnyFunSuite {

  /**
    * The Flix object used across all the tests.
    *
    * The tests do not use the library, so it is left out.
    */
  private val Flix: Flix = new Flix().setOptions(Options.Default.copy(lib = LibLevel.Nix))

  /**
    * The uri of the test source.
    *
    * Every test uses the same uri so that adding a new source with this uri replaces the old one.
    */
  private val Name = SourceName.PathName(CompilerConstants.VirtualTestFile)

  /**
    * Returns the first line of the code that the hover shows at the given one-indexed `line` and
    * `col` of the given `program`.
    */
  private def hover(program: String, line: Int, col: Int): String = {
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted
    Flix.addSource(CompilerConstants.VirtualTestFile, program, sctx)
    Flix.check() match {
      case (Some(root), _) => HoverProvider.processHover(Name, Position(line, col))(root) match {
        // The hover is a fenced code block: its first line opens the fence.
        case Some(h) => h.contents.value.linesIterator.drop(1).next()
        case None => fail("A hover is expected.")
      }
      case (None, _) => fail("Compilation failed: a root is expected.")
    }
  }

  test("Hover.Binder.01") {
    // A type variable with no source text is numbered, not written by its id.
    val program =
      """pub def f(): Unit =
        |    let _g = x -> x;
        |    ()
        |""".stripMargin
    assertResult("t0 -> t0")(hover(program, 2, 9))
  }

  test("Hover.FormalParam.01") {
    val program =
      """pub def f(): Unit =
        |    let _g = x -> x;
        |    ()
        |""".stripMargin
    assertResult("t0")(hover(program, 2, 14))
  }

  test("Hover.Expr.01") {
    val program =
      """pub def f(): Unit =
        |    let _g = x -> x;
        |    ()
        |""".stripMargin
    assertResult("t0 -> t0")(hover(program, 2, 16))
  }

  test("Hover.Expr.02") {
    // The type and the effect of an expression are numbered together.
    val program =
      """pub def f(): Unit =
        |    let _h = (g, x) -> g(x);
        |    ()
        |""".stripMargin
    assertResult("t0 \\ e1")(hover(program, 2, 25))
  }

  test("Hover.Expr.03") {
    // A type variable with a source text keeps it.
    val program =
      """pub def f(x: a): a = x
        |""".stripMargin
    assertResult("a")(hover(program, 1, 22))
  }

}
