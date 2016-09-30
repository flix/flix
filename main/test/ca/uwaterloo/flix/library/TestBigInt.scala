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
import ca.uwaterloo.flix.runtime.Value
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestBigInt extends FunSuite {

  val options = Options.DefaultTest

  def runTest(input: String, output: AnyRef) {
    val flix = new Flix().setOptions(options).addPath("main/src/library/BigInt.flix").addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  def runIntTest(input: String, output: Int) {
    val flix = new Flix().setOptions(options).addPath("main/src/library/BigInt.flix").addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  test("min.01") {
    val input = "def r: BigInt = BigInt/min(1ii, 2ii)"
    runTest(input, Value.mkBigInt(1))
  }

  test("min.02") {
    val input = "def r: BigInt = BigInt/min(2ii, -1ii)"
    runTest(input, Value.mkBigInt(-1))
  }

  test("min.03") {
    val input = "def r: BigInt = BigInt/min(-33ii, -66ii)"
    runTest(input, Value.mkBigInt(-66))
  }

  test("min.04") {
    val input = "def r: BigInt = BigInt/min(-44ii, -9223372036854775808ii)"
    runTest(input, Value.mkBigInt(Long.MinValue))
  }

  test("max.01") {
    val input = "def r: BigInt = BigInt/max(48ii, 49ii)"
    runTest(input, Value.mkBigInt(49))
  }

  test("max.02") {
    val input = "def r: BigInt = BigInt/max(4ii, -16ii)"
    runTest(input, Value.mkBigInt(4))
  }

  test("max.03") {
    val input = "def r: BigInt = BigInt/max(-34ii, -16ii)"
    runTest(input, Value.mkBigInt(-16))
  }

  test("max.04") {
    val input = "def r: BigInt = BigInt/max(-34ii, 9223372036854775807ii)"
    runTest(input, Value.mkBigInt(Long.MaxValue))
  }

  test("abs.01") {
    val input = "def r: BigInt = BigInt/abs(311111111111ii)"
    runTest(input, Value.mkBigInt(311111111111L))
  }

  test("abs.02") {
    val input = "def r: BigInt = BigInt/abs(-311111111111ii)"
    runTest(input, Value.mkBigInt(311111111111L))
  }

  test("abs.03") {
    val input = "def r: BigInt = BigInt/abs(0ii)"
    runTest(input, Value.mkBigInt(0))
  }

  test("abs.04") {
    val input = "def r: BigInt = BigInt/abs(9223372036854775807ii)"
    runTest(input, Value.mkBigInt(Long.MaxValue))
  }

  test("abs.05") {
    val input = "def r: BigInt = BigInt/abs(-9223372036854775807ii)"
    runTest(input, Value.mkBigInt(Long.MaxValue))
  }

  test("dist.01") {
    val input = "def r: BigInt = BigInt/dist(31ii, -7ii)"
    runTest(input, Value.mkBigInt(38))
  }

  test("dist.02") {
    val input = "def r: BigInt = BigInt/dist(-44ii, -1ii)"
    runTest(input, Value.mkBigInt(43))
  }

  test("dist.03") {
    val input = "def r: BigInt = BigInt/dist(-2ii, -2ii)"
    runTest(input, Value.mkBigInt(0))
  }

  test("dist.04") {
    val input = "def r: BigInt = BigInt/dist(9223372036854775807ii, 0ii)"
    runTest(input, Value.mkBigInt(Long.MaxValue))
  }

  test("dist.05") {
    val input = "def r: BigInt = BigInt/dist(-9223372036854775807ii, 0ii)"
    runTest(input, Value.mkBigInt(Long.MaxValue))
  }

  test("compare.01") {
    val input = "def r: Int32 = BigInt/compare(-1ii, 44ii)"
    runIntTest(input, -1)
  }

  test("compare.02") {
    val input = "def r: Int32 = BigInt/compare(-1ii, -44ii)"
    runIntTest(input, 1)
  }

  test("compare.03") {
    val input = "def r: Int32 = BigInt/compare(88ii, 88ii)"
    runIntTest(input, 0)
  }

  test("signum.01") {
    val input = "def r: Int32 = BigInt/signum(-22ii)"
    runIntTest(input, -1)
  }

  test("signum.02") {
    val input = "def r: Int32 = BigInt/signum(22ii)"
    runIntTest(input, 1)
  }

  test("signum.03") {
    val input = "def r: Int32 = BigInt/signum(0ii)"
    runIntTest(input, 0)
  }

  test("signum.04") {
    val input = "def r: Int32 = BigInt/signum(9223372036854775807ii)"
    runIntTest(input, 1)
  }

  test("signum.05") {
    val input = "def r: Int32 = BigInt/signum(-9223372036854775808ii)"
    runIntTest(input, -1)
  }
}