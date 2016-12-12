/*
 *  Copyright 2016 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.library

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.runtime.Value
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestPrelude extends FunSuite {

  val options = Options.DefaultTest

  def runTest(input: String, output: AnyRef) {
    val flix = new Flix().setOptions(options).addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Logical And                                                             //
  /////////////////////////////////////////////////////////////////////////////
  test("∧.01") {
    val input = "def r: Bool = true ∧ true"
    runTest(input, Value.True)
  }

  test("∧.02") {
    val input = "def r: Bool = true ∧ false"
    runTest(input, Value.False)
  }

  test("∧.03") {
    val input = "def r: Bool = false ∧ true"
    runTest(input, Value.False)
  }

  test("∧.04") {
    val input = "def r: Bool = false ∧ false"
    runTest(input, Value.False)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Logical Or                                                              //
  /////////////////////////////////////////////////////////////////////////////
  test("∨.01") {
    val input = "def r: Bool = true ∨ true"
    runTest(input, Value.True)
  }

  test("∨.02") {
    val input = "def r: Bool = true ∨ false"
    runTest(input, Value.True)
  }

  test("∨.03") {
    val input = "def r: Bool = false ∨ true"
    runTest(input, Value.True)
  }

  test("∨.04") {
    val input = "def r: Bool = false ∨ false"
    runTest(input, Value.False)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Logical Implication                                                     //
  /////////////////////////////////////////////////////////////////////////////
  test("→.01") {
    val input = "def r: Bool = true → true"
    runTest(input, Value.True)
  }

  test("→.02") {
    val input = "def r: Bool = true → false"
    runTest(input, Value.False)
  }

  test("→.03") {
    val input = "def r: Bool = false → true"
    runTest(input, Value.True)
  }

  test("→.04") {
    val input = "def r: Bool = false → false"
    runTest(input, Value.True)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Logical Biconditional                                                   //
  /////////////////////////////////////////////////////////////////////////////
  test("↔.01") {
    val input = "def r: Bool = true ↔ true"
    runTest(input, Value.True)
  }

  test("↔.02") {
    val input = "def r: Bool = true ↔ false"
    runTest(input, Value.False)
  }

  test("↔.03") {
    val input = "def r: Bool = false ↔ true"
    runTest(input, Value.False)
  }

  test("↔.04") {
    val input = "def r: Bool = false ↔ false"
    runTest(input, Value.True)
  }

}
