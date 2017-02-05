/*
 * Copyright 2016 Liam Palmer
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

package ca.uwaterloo.flix.library

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestFloat32 extends FunSuite {

  val options = Options.DefaultTest

  def runTest(input: String, output: Float) {
    val flix = new Flix().setOptions(options).addPath("main/src/library/Float32.flix").addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  // Used for testing equality with the Float.NaN type
  def runEqualsTest(input: String, output: Float) {
    val flix = new Flix().setOptions(options).addPath("main/src/library/Float32.flix").addStr(input)
    assert(output.equals(flix.solve().get.getConstant("r")))
  }

  test("size.01") {
    val input = "def r: Int32 = Float32.size()"
    runTest(input, 32)
  }

  test("minExponent.01") {
    val input = "def r: Int32 = Float32.minExponent()"
    runTest(input, -126)
  }

  test("maxExponent.01") {
    val input = "def r: Int32 = Float32.maxExponent()"
    runTest(input, 127)
  }

  test("maxValue.01") {
    val input = "def r: Float32 = Float32.maxValue()"
    runTest(input, Float.MaxValue)
  }

  test("minValue.01") {
    val input = "def r: Float32 = Float32.minValue()"
    runTest(input, Float.MinValue)
  }

  test("minPositiveValue.01") {
    val input = "def r: Float32 = Float32.minPositiveValue()"
    runTest(input, Float.MinPositiveValue)
  }

  test("undefined.01") {
    val input = "def r: Float32 = Float32.undefined()"
    runEqualsTest(input, Float.NaN)
  }

  test("positiveInfinity.01") {
    val input = "def r: Float32 = Float32.positiveInfinity()"
    runTest(input, Float.PositiveInfinity)
  }

  test("negativeInfinity.01") {
    val input = "def r: Float32 = Float32.negativeInfinity()"
    runTest(input, Float.NegativeInfinity)
  }

  test("min.01") {
    val input = "def r: Float32 = Float32.min(1.0f32, 2.0f32)"
    runTest(input, 1f)
  }

  test("min.02") {
    val input = "def r: Float32 = Float32.min(2.0f32, -1.0f32)"
    runTest(input, -1f)
  }

  test("min.03") {
    val input = "def r: Float32 = Float32.min(-33.0f32, -66.0f32)"
    runTest(input, -66f)
  }

  test("max.01") {
    val input = "def r: Float32 = Float32.max(48.0f32, 49.0f32)"
    runTest(input, 49f)
  }

  test("max.02") {
    val input = "def r: Float32 = Float32.max(4.0f32, -16.0f32)"
    runTest(input, 4f)
  }

  test("max.03") {
    val input = "def r: Float32 = Float32.max(-34.0f32, -16.0f32)"
    runTest(input, -16f)
  }
}