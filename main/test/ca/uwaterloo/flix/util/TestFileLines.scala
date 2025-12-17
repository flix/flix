/*
 * Copyright 2025 Google LLC
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
package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.language.ast.SourcePosition
import ca.uwaterloo.flix.util.FileLines.LineInfo
import org.scalatest.funsuite.AnyFunSuite

class TestFileLines extends AnyFunSuite {

  test("empty file") {
    val chars = FileLines.fromChars(Array.emptyCharArray)
    assertResult(None)(chars.nthLineInfo(0))
    assertResult(Some(LineInfo(0, 0)))(chars.nthLineInfo(1))
    assertResult(None)(chars.nthLineInfo(2))
  }

  test("empty lines") {
    val input = List(
      "",
      "",
      ""
    ).mkString("\n")
    val chars = FileLines.fromChars(input.toCharArray)
    assertResult(None)(chars.nthLineInfo(0))
    assertResult(Some(LineInfo(0, 0)))(chars.nthLineInfo(1))
    assertResult(Some(LineInfo(1, 0)))(chars.nthLineInfo(2))
    assertResult(Some(LineInfo(2, 0)))(chars.nthLineInfo(3))
    assertResult(None)(chars.nthLineInfo(4))
  }

  test("single line") {
    val input = "1234"
    val chars = FileLines.fromChars(input.toCharArray)
    assertResult(None)(chars.nthLineInfo(0))
    assertResult(Some(LineInfo(0, 4)))(chars.nthLineInfo(1))
    assertResult(None)(chars.nthLineInfo(2))
  }

  test("single line with newline") {
    val input = "1234\n"
    val chars = FileLines.fromChars(input.toCharArray)
    assertResult(None)(chars.nthLineInfo(0))
    assertResult(Some(LineInfo(0, 4)))(chars.nthLineInfo(1))
    assertResult(Some(LineInfo(5, 0)))(chars.nthLineInfo(2))
    assertResult(None)(chars.nthLineInfo(3))
  }

  test("trailing line") {
    val input = List(
      "1234",
      "223",
      "3",
      ""
    ).mkString("\n")
    val chars = FileLines.fromChars(input.toCharArray)
    assertResult(None)(chars.nthLineInfo(0))
    assertResult(Some(LineInfo(0, 4)))(chars.nthLineInfo(1))
    assertResult(Some(LineInfo(5, 3)))(chars.nthLineInfo(2))
    assertResult(Some(LineInfo(9, 1)))(chars.nthLineInfo(3))
    assertResult(Some(LineInfo(11, 0)))(chars.nthLineInfo(4))
    assertResult(None)(chars.nthLineInfo(5))
  }

  test("non-trailing line") {
    val input = List(
      "1234",
      "223",
      "3",
      "  X"
    ).mkString("\n")
    val chars = FileLines.fromChars(input.toCharArray)
    assertResult(None)(chars.nthLineInfo(0))
    assertResult(Some(LineInfo(0, 4)))(chars.nthLineInfo(1))
    assertResult(Some(LineInfo(5, 3)))(chars.nthLineInfo(2))
    assertResult(Some(LineInfo(9, 1)))(chars.nthLineInfo(3))
    assertResult(Some(LineInfo(11, 3)))(chars.nthLineInfo(4))
    assertResult(None)(chars.nthLineInfo(5))
  }

  test("get first position") {
    val input = List(
      "ABC",
      "B",
      "123"
    ).mkString("\n")
    val chars = FileLines.fromChars(input.toCharArray)
    assertResult(SourcePosition(1, 1))(chars.getPosition(0))
  }

  test("get middle position") {
    val input = List(
      "0123",
      "56X8",
      "ABCD"
    ).mkString("\n")
    val chars = FileLines.fromChars(input.toCharArray)
    assertResult(SourcePosition(2, 3))(chars.getPosition(7))
  }

  test("get end position") {
    val input = List(
      "0123",
      "5678",
      "ABCD"
    ).mkString("\n")
    val chars = FileLines.fromChars(input.toCharArray)
    assertResult(SourcePosition(3, 4))(chars.getPosition(13))
  }

  test("get start of line position") {
    val input = List(
      "0123",
      "5678",
      "ABCD"
    ).mkString("\n")
    val chars = FileLines.fromChars(input.toCharArray)
    assertResult(SourcePosition(2, 1))(chars.getPosition(5))
  }

  test("line count") {
    val input = List(
      "0123",
      "5678",
      ""
    ).mkString("\n")
    val chars = FileLines.fromChars(input.toCharArray)
    assertResult(3)(chars.lineCount)
  }


  test("exclusive end position at start of line") {
    val input = List(
      "0123",
      "5678",
      ""
    ).mkString("\n")
    val chars = FileLines.fromChars(input.toCharArray)
    assertResult(SourcePosition(1, 5))(chars.getPositionExclusive(5))
  }

  test("exclusive end position") {
    val input = List(
      "0123",
      "5678",
      ""
    ).mkString("\n")
    val chars = FileLines.fromChars(input.toCharArray)
    assertResult(SourcePosition(2, 2))(chars.getPositionExclusive(6))
  }

}
