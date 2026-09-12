/*
 * Copyright 2026 Flix Authors
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
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.api.lsp.provider.FoldingRangeProvider
import ca.uwaterloo.flix.api.{CompilerConstants, Flix}
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestFoldingRangeProvider extends AnyFunSuite {

  /**
    * The Flix object used across all the tests.
    *
    * We first compile the stdlib so that every further compilation will be incremental.
    */
  private val Flix: Flix = {
    val flix = new Flix().setOptions(Options.Default)
    flix.check()
    flix
  }

  /**
    * The uri of the test source.
    *
    * Every test uses the same uri so that adding a new source with this uri replaces the old one.
    */
  private val Uri = CompilerConstants.VirtualTestFile.toString

  /**
    * Returns the folding ranges for the given `program`.
    */
  private def foldingRanges(program: String): List[FoldingRange] = {
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted
    Flix.addVirtualPath(CompilerConstants.VirtualTestFile, program)
    Flix.check() match {
      case (Some(root), _) => FoldingRangeProvider.getFoldingRanges(Uri)(root)
      case (None, _) => fail("Compilation failed: a root is expected.")
    }
  }

  test("FoldingRange.DocComment.Multi") {
    val program =
      """/// Hello
        |/// Doc
        |/// Comment
        |pub def f(): Int32 = 1
        |""".stripMargin
    assertResult(List(FoldingRange(1, 3, FoldingRangeKind.Comment)))(foldingRanges(program))
  }

  test("FoldingRange.DocComment.Single") {
    val program =
      """/// Hello
        |pub def f(): Int32 = 1
        |""".stripMargin
    assertResult(Nil)(foldingRanges(program))
  }

  test("FoldingRange.LineComment.Multi") {
    val program =
      """// one
        |// two
        |// three
        |pub def f(): Int32 = 1
        |""".stripMargin
    assertResult(List(FoldingRange(1, 3, FoldingRangeKind.Comment)))(foldingRanges(program))
  }

  test("FoldingRange.LineComment.Single") {
    val program =
      """// one
        |pub def f(): Int32 = 1
        |""".stripMargin
    assertResult(Nil)(foldingRanges(program))
  }

  test("FoldingRange.LineComment.NonAdjacent") {
    val program =
      """// one
        |pub def f(): Int32 = 1
        |// two
        |pub def g(): Int32 = 2
        |""".stripMargin
    assertResult(Nil)(foldingRanges(program))
  }

  test("FoldingRange.BlockComment.Multi") {
    val program =
      """/* one
        |   two
        |   three */
        |pub def f(): Int32 = 1
        |""".stripMargin
    assertResult(List(FoldingRange(1, 3, FoldingRangeKind.Comment)))(foldingRanges(program))
  }

  test("FoldingRange.BlockComment.Single") {
    val program =
      """/* one line */
        |pub def f(): Int32 = 1
        |""".stripMargin
    assertResult(Nil)(foldingRanges(program))
  }

  test("FoldingRange.DocComment.TwoBlocks") {
    val program =
      """/// a
        |/// b
        |pub def f(): Int32 = 1
        |
        |/// c
        |/// d
        |pub def g(): Int32 = 2
        |""".stripMargin
    assertResult(List(FoldingRange(1, 2, FoldingRangeKind.Comment), FoldingRange(5, 6, FoldingRangeKind.Comment)))(foldingRanges(program))
  }

  test("FoldingRange.DocAndLineComment.NotMerged") {
    // A doc comment immediately followed by a line comment must not be merged into a single range.
    val program =
      """/// doc
        |// line
        |pub def f(): Int32 = 1
        |""".stripMargin
    assertResult(Nil)(foldingRanges(program))
  }

  test("FoldingRange.DocAndLineComment.Separate") {
    // A run of doc comments and an adjacent run of line comments fold into two separate ranges.
    val program =
      """/// a
        |/// b
        |// c
        |// d
        |pub def f(): Int32 = 1
        |""".stripMargin
    assertResult(List(FoldingRange(1, 2, FoldingRangeKind.Comment), FoldingRange(3, 4, FoldingRangeKind.Comment)))(foldingRanges(program))
  }

}
