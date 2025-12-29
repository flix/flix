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
    val lines = FileLines.fromChars(Array.emptyCharArray)
    assertResult(None)(lines.nthLineInfo(0))
    assertResult(Some(LineInfo(0, 0)))(lines.nthLineInfo(1))
    assertResult(None)(lines.nthLineInfo(2))
  }

  test(s"empty lines") {
    val input = List(
      "",
      "",
      ""
    ).mkString("\n")
    val lines = FileLines.fromChars(input.toCharArray)
    assertResult(None)(lines.nthLineInfo(0))
    assertResult(Some(LineInfo(0, 0)))(lines.nthLineInfo(1))
    assertResult(Some(LineInfo(1, 0)))(lines.nthLineInfo(2))
    assertResult(Some(LineInfo(2, 0)))(lines.nthLineInfo(3))
    assertResult(None)(lines.nthLineInfo(4))
  }

  test(s"empty lines (w. \\r)") {
    val input = List(
      "",
      "",
      ""
    ).mkString("\r\n")
    val lines = FileLines.fromChars(input.toCharArray)
    assertResult(None)(lines.nthLineInfo(0))
    assertResult(Some(LineInfo(0, 0)))(lines.nthLineInfo(1))
    assertResult(Some(LineInfo(2, 0)))(lines.nthLineInfo(2))
    assertResult(Some(LineInfo(4, 0)))(lines.nthLineInfo(3))
    assertResult(None)(lines.nthLineInfo(4))
  }

  test("single line") {
    val input = "1234"
    val lines = FileLines.fromChars(input.toCharArray)
    assertResult(None)(lines.nthLineInfo(0))
    assertResult(Some(LineInfo(0, 4)))(lines.nthLineInfo(1))
    assertResult(None)(lines.nthLineInfo(2))
  }

  test(s"single line with newline") {
    val input = s"1234\n"
    val lines = FileLines.fromChars(input.toCharArray)
    assertResult(None)(lines.nthLineInfo(0))
    assertResult(Some(LineInfo(0, 4)))(lines.nthLineInfo(1))
    assertResult(Some(LineInfo(5, 0)))(lines.nthLineInfo(2))
    assertResult(None)(lines.nthLineInfo(3))
  }

  test(s"single line with newline (w. \\r)") {
    val input = s"1234\r\n"
    val lines = FileLines.fromChars(input.toCharArray)
    assertResult(None)(lines.nthLineInfo(0))
    assertResult(Some(LineInfo(0, 4)))(lines.nthLineInfo(1))
    assertResult(Some(LineInfo(6, 0)))(lines.nthLineInfo(2))
    assertResult(None)(lines.nthLineInfo(3))
  }

  test("trailing line") {
    val input = List(
      "1234",
      "223",
      "3",
      ""
    ).mkString("\n")
    val lines = FileLines.fromChars(input.toCharArray)
    assertResult(None)(lines.nthLineInfo(0))
    assertResult(Some(LineInfo(0, 4)))(lines.nthLineInfo(1))
    assertResult(Some(LineInfo(5, 3)))(lines.nthLineInfo(2))
    assertResult(Some(LineInfo(9, 1)))(lines.nthLineInfo(3))
    assertResult(Some(LineInfo(11, 0)))(lines.nthLineInfo(4))
    assertResult(None)(lines.nthLineInfo(5))
  }

  test("trailing line (w. \\r)") {
    val input = List(
      "1234",
      "223",
      "3",
      ""
    ).mkString("\r\n")
    val lines = FileLines.fromChars(input.toCharArray)
    assertResult(None)(lines.nthLineInfo(0))
    assertResult(Some(LineInfo(0, 4)))(lines.nthLineInfo(1))
    assertResult(Some(LineInfo(6, 3)))(lines.nthLineInfo(2))
    assertResult(Some(LineInfo(11, 1)))(lines.nthLineInfo(3))
    assertResult(Some(LineInfo(14, 0)))(lines.nthLineInfo(4))
    assertResult(None)(lines.nthLineInfo(5))
  }

  test("non-trailing line") {
    val input = List(
      "1234",
      "223",
      "3",
      "  X"
    ).mkString("\n")
    val lines = FileLines.fromChars(input.toCharArray)
    assertResult(None)(lines.nthLineInfo(0))
    assertResult(Some(LineInfo(0, 4)))(lines.nthLineInfo(1))
    assertResult(Some(LineInfo(5, 3)))(lines.nthLineInfo(2))
    assertResult(Some(LineInfo(9, 1)))(lines.nthLineInfo(3))
    assertResult(Some(LineInfo(11, 3)))(lines.nthLineInfo(4))
    assertResult(None)(lines.nthLineInfo(5))
  }

  test("non-trailing line (w. \\r)") {
    val input = List(
      "1234",
      "223",
      "3",
      "  X"
    ).mkString("\r\n")
    val lines = FileLines.fromChars(input.toCharArray)
    assertResult(None)(lines.nthLineInfo(0))
    assertResult(Some(LineInfo(0, 4)))(lines.nthLineInfo(1))
    assertResult(Some(LineInfo(6, 3)))(lines.nthLineInfo(2))
    assertResult(Some(LineInfo(11, 1)))(lines.nthLineInfo(3))
    assertResult(Some(LineInfo(14, 3)))(lines.nthLineInfo(4))
    assertResult(None)(lines.nthLineInfo(5))
  }

  test("get first position") {
    val input = List(
      "ABC",
      "B",
      "123"
    ).mkString("\n")
    val lines = FileLines.fromChars(input.toCharArray)
    assertResult(SourcePosition(1, 1))(lines.getPosition(0))
  }

  test("get first position (w. \\r)") {
    val input = List(
      "ABC",
      "B",
      "123"
    ).mkString("\r\n")
    val lines = FileLines.fromChars(input.toCharArray)
    assertResult(SourcePosition(1, 1))(lines.getPosition(0))
  }

  test("get middle position") {
    val input = List(
      "0123",
      "56X8",
      "ABCD"
    ).mkString("\n")
    val lines = FileLines.fromChars(input.toCharArray)
    assertResult(SourcePosition(2, 3))(lines.getPosition(7))
  }

  test("get middle position (w. \\r)") {
    val input = List(
      "0123",
      "6789",
      "CDEF"
    ).mkString("\r\n")
    val lines = FileLines.fromChars(input.toCharArray)
    assertResult(SourcePosition(2, 3))(lines.getPosition(8))
  }

  test("get end position") {
    val input = List(
      "0123",
      "5678",
      "ABCD"
    ).mkString("\n")
    val lines = FileLines.fromChars(input.toCharArray)
    assertResult(SourcePosition(3, 4))(lines.getPosition(13))
  }

  test("get end position (w. \\r)") {
    val input = List(
      "0123",
      "6789",
      "CDEF"
    ).mkString("\r\n")
    val lines = FileLines.fromChars(input.toCharArray)
    assertResult(SourcePosition(3, 4))(lines.getPosition(15))
  }

  test("get start of line position") {
    val input = List(
      "0123",
      "5678",
      "ABCD"
    ).mkString("\n")
    val lines = FileLines.fromChars(input.toCharArray)
    assertResult(SourcePosition(2, 1))(lines.getPosition(5))
  }

  test("get start of line position (w. \\r)") {
    val input = List(
      "0123",
      "6789",
      "CDEF"
    ).mkString("\r\n")
    val lines = FileLines.fromChars(input.toCharArray)
    assertResult(SourcePosition(2, 1))(lines.getPosition(6))
  }

  test("line count") {
    val input = List(
      "0123",
      "5678",
      ""
    ).mkString("\n")
    val lines = FileLines.fromChars(input.toCharArray)
    assertResult(3)(lines.lineCount)
  }

  test("line count (w. \\r)") {
    val input = List(
      "0123",
      "5678",
      ""
    ).mkString("\r\n")
    val lines = FileLines.fromChars(input.toCharArray)
    assertResult(3)(lines.lineCount)
  }

  test("exclusive end position at start of line") {
    val input = List(
      "0123",
      "5678",
      ""
    ).mkString("\n")
    val lines = FileLines.fromChars(input.toCharArray)
    assertResult(SourcePosition(1, 5))(lines.getPositionExclusive(5))
  }

  test("exclusive end position at start of line (w. \\r)") {
    val input = List(
      "0123",
      "6789",
      ""
    ).mkString("\r\n")
    val lines = FileLines.fromChars(input.toCharArray)
    assertResult(SourcePosition(1, 5))(lines.getPositionExclusive(6))
  }

  test("exclusive end position") {
    val input = List(
      "0123",
      "5678",
      ""
    ).mkString("\n")
    val lines = FileLines.fromChars(input.toCharArray)
    assertResult(SourcePosition(2, 2))(lines.getPositionExclusive(6))
  }

  test("exclusive end position (w. \\r)") {
    val input = List(
      "0123",
      "6789",
      ""
    ).mkString("\r\n")
    val lines = FileLines.fromChars(input.toCharArray)
    assertResult(SourcePosition(2, 2))(lines.getPositionExclusive(7))
  }

}
