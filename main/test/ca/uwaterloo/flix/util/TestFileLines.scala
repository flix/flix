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

import ca.uwaterloo.flix.util.FileLines.LineInfo
import org.scalatest.funsuite.AnyFunSuite

class TestFileLines extends AnyFunSuite {

  test("example1") {
    val input =
      """
        |1234
        |223
        |3
        |
        |""".stripMargin.replace("\r", "")
    val fileLines = FileLines.fromChars(input.toCharArray)
    assertResult(Some(LineInfo(0, 4)))(fileLines.nthLineInfo(1))
    assertResult(Some(LineInfo(5, 3)))(fileLines.nthLineInfo(2))
    assertResult(Some(LineInfo(9, 1)))(fileLines.nthLineInfo(3))
    assertResult(Some(LineInfo(11, 0)))(fileLines.nthLineInfo(4))
    assertResult(None)(fileLines.nthLineInfo(5))
  }

}
