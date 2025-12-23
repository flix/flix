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
package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.{Input, Source}
import org.scalatest.funsuite.AnyFunSuite

class SourceTest extends AnyFunSuite {

  test("get single line") {
    val input = "1234"
    val src = Source.fromString(Input.Unknown, input)
    assertResult(input)(src.getData(0, 4))
  }

  test("get multiple lines") {
    val input = "1234\nABCD\n"
    val src = Source.fromString(Input.Unknown, input)
    assertResult(input)(src.getData(0, 10))
  }

  test("get middle line") {
    val input = "1234\nABCD\n"
    val src = Source.fromString(Input.Unknown, input)
    assertResult("ABCD")(src.getLine(2))
  }

  test("get middle line (with \\r)") {
    val input = "1234\r\nABCD\r\n"
    val src = Source.fromString(Input.Unknown, input)
    assertResult("ABCD")(src.getLine(2))
  }
}
