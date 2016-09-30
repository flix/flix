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

class TestInt16 extends FunSuite {

  val options = Options.DefaultTest

  def runTest(input: String, output: Short) {
    val flix = new Flix().setOptions(options).addPath("main/src/library/Int16.flix").addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  test("maxValue.01") {
    val input = "def r: Int16 = Int16/maxValue()"
    runTest(input, Short.MaxValue)
  }

  test("minValue.01") {
    val input = "def r: Int16 = Int16/minValue()"
    runTest(input, Short.MinValue)
  }

  test("size.01") {
    val input = "def r: Int32 = Int16/size()"
    runTest(input, 16:Short)
  }

  test("min.01") {
    val input = "def r: Int16 = Int16/min(1i16, 2i16)"
    runTest(input, 1:Short)
  }

  test("min.02") {
    val input = "def r: Int16 = Int16/min(2i16, -1i16)"
    runTest(input, -1:Short)
  }

  test("min.03") {
    val input = "def r: Int16 = Int16/min(-33i16, -66i16)"
    runTest(input, -66:Short)
  }

  test("max.01") {
    val input = "def r: Int16 = Int16/max(48i16, 49i16)"
    runTest(input, 49:Short)
  }

  test("max.02") {
    val input = "def r: Int16 = Int16/max(4i16, -16i16)"
    runTest(input, 4:Short)
  }

  test("max.03") {
    val input = "def r: Int16 = Int16/max(-34i16, -16i16)"
    runTest(input, -16:Short)
  }

  test("abs.01") {
    val input = "def r: Int16 = Int16/abs(31i16)"
    runTest(input, 31:Short)
  }

  test("abs.02") {
    val input = "def r: Int16 = Int16/abs(-31i16)"
    runTest(input, 31:Short)
  }

  test("abs.03") {
    val input = "def r: Int16 = Int16/abs(0i16)"
    runTest(input, 0:Short)
  }

  test("abs.04") {
    val input = "def r: Int16 = Int16/abs(32767i16)"
    runTest(input, Short.MaxValue)
  }

  test("abs.05") {
    val input = "def r: Int16 = Int16/abs(-32767i16)"
    runTest(input, Short.MaxValue)
  }

  test("abs.06") {
    val input = "def r: Int16 = Int16/abs(-32768i16)"
    runTest(input, -1:Short)
  }

  test("dist.01") {
    val input = "def r: Int16 = Int16/dist(31i16, -7i16)"
    runTest(input, 38:Short)
  }

  test("dist.02") {
    val input = "def r: Int16 = Int16/dist(-44i16, -1i16)"
    runTest(input, 43:Short)
  }

  test("dist.03") {
    val input = "def r: Int16 = Int16/dist(-2i16, -2i16)"
    runTest(input, 0:Short)
  }

  test("dist.04") {
    val input = "def r: Int16 = Int16/dist(-32768i16, -1i16)"
    runTest(input, Short.MaxValue)
  }

  test("dist.05") {
    val input = "def r: Int16 = Int16/dist(-32768i16, 0i16)"
    runTest(input, -1:Short)
  }

  test("dist.06") {
    val input = "def r: Int16 = Int16/dist(-32763i16, 4i16)"
    runTest(input, Short.MaxValue)
  }

  test("dist.07") {
    val input = "def r: Int16 = Int16/dist(-32763i16, 5i16)"
    runTest(input, -1:Short)
  }

  test("dist.08") {
    val input = "def r: Int16 = Int16/dist(-32666i16, 101i16)"
    runTest(input, Short.MaxValue)
  }

  test("dist.09") {
    val input = "def r: Int16 = Int16/dist(-32666i16, 102i16)"
    runTest(input, -1:Short)
  }

  test("dist.10") {
    val input = "def r: Int16 = Int16/dist(32767i16, 0i16)"
    runTest(input, Short.MaxValue)
  }

  test("dist.11") {
    val input = "def r: Int16 = Int16/dist(32767i16, 1i16)"
    runTest(input, 32766:Short)
  }

  test("compare.01") {
    val input = "def r: Int32 = Int16/compare(-1i16, 44i16)"
    runTest(input, -1:Short)
  }

  test("compare.02") {
    val input = "def r: Int32 = Int16/compare(-1i16, -44i16)"
    runTest(input, 1:Short)
  }

  test("compare.03") {
    val input = "def r: Int32 = Int16/compare(88i16, 88i16)"
    runTest(input, 0:Short)
  }

  test("signum.01") {
    val input = "def r: Int32 = Int16/signum(-22i16)"
    runTest(input, -1:Short)
  }

  test("signum.02") {
    val input = "def r: Int32 = Int16/signum(22i16)"
    runTest(input, 1:Short)
  }

  test("signum.03") {
    val input = "def r: Int32 = Int16/signum(0i16)"
    runTest(input, 0:Short)
  }

  test("highestOneBitPosition.01") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(-1i16)"
    runTest(input, 15:Short)
  }

  test("highestOneBitPosition.02") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(-32768i16)"
    runTest(input, 15:Short)
  }

  test("highestOneBitPosition.03") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(1i16)"
    runTest(input, 0:Short)
  }

  test("highestOneBitPosition.04") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(2i16)"
    runTest(input, 1:Short)
  }

  test("highestOneBitPosition.05") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(3i16)"
    runTest(input, 1:Short)
  }

  test("highestOneBitPosition.06") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(20i16)"
    runTest(input, 4:Short)
  }

  test("highestOneBitPosition.07") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(2i16 ** 14i16)"
    runTest(input, 14:Short)
  }

  test("highestOneBitPosition.08") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(32767i16)"
    runTest(input, 14:Short)
  }

  test("highestOneBitPosition.09") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(0i16)"
    runTest(input, -1:Short)
  }

  test("highestOneBitPosition.10") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(2i16 ** 8i16 + 2i16 ** 11i16)"
    runTest(input, 11:Short)
  }

  test("highestOneBitPosition.11") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(2i16 ** 3i16 + 2i16 ** 7i16)"
    runTest(input, 7:Short)
  }

  test("highestOneBitPosition.12") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(-32767i16)"
    runTest(input, 15:Short)
  }

  test("highestOneBit.01") {
    val input = "def r: Int16 = Int16/highestOneBit(-1i16)"
    runTest(input, Short.MinValue)
  }

  test("highestOneBit.02") {
    val input = "def r: Int16 = Int16/highestOneBit(-32768i16)"
    runTest(input, Short.MinValue)
  }

  test("highestOneBit.03") {
    val input = "def r: Int16 = Int16/highestOneBit(-77i16)"
    runTest(input, Short.MinValue)
  }

  test("highestOneBit.04") {
    val input = "def r: Int16 = Int16/highestOneBit(2i16)"
    runTest(input, 2:Short)
  }

  test("highestOneBit.05") {
    val input = "def r: Int16 = Int16/highestOneBit(3i16)"
    runTest(input, 2:Short)
  }

  test("highestOneBit.06") {
    val input = "def r: Int16 = Int16/highestOneBit(20i16)"
    runTest(input, 16:Short)
  }

  test("highestOneBit.07") {
    val input = "def r: Int16 = Int16/highestOneBit(2i16 ** 11i16 + 2i16 ** 13i16)"
    runTest(input, 8192:Short)
  }

  test("highestOneBit.08") {
    val input = "def r: Int16 = Int16/highestOneBit(32767i16)"
    runTest(input, 16384:Short)
  }

  test("highestOneBit.09") {
    val input = "def r: Int16 = Int16/highestOneBit(0i16)"
    runTest(input, 0:Short)
  }

  test("highestOneBit.10") {
    val input = "def r: Int16 = Int16/highestOneBit(2i16 ** 6i16 + 2i16 ** 9i16)"
    runTest(input, 512:Short)
  }

  test("numberOfLeadingZeros.01") {
    val input = "def r: Int32 = Int16/numberOfLeadingZeros(0i16)"
    runTest(input, 16:Short)
  }

  test("numberOfLeadingZeros.02") {
    val input = "def r: Int32 = Int16/numberOfLeadingZeros(-32768i16)"
    runTest(input, 0:Short)
  }

  test("numberOfLeadingZeros.03") {
    val input = "def r: Int32 = Int16/numberOfLeadingZeros(-1i16)"
    runTest(input, 0:Short)
  }

  test("numberOfLeadingZeros.04") {
    val input = "def r: Int32 = Int16/numberOfLeadingZeros(32767i16)"
    runTest(input, 1:Short)
  }

  test("numberOfLeadingZeros.05") {
    val input = "def r: Int32 = Int16/numberOfLeadingZeros(2i16 ** 14i16)"
    runTest(input, 1:Short)
  }

  test("numberOfLeadingZeros.06") {
    val input = "def r: Int32 = Int16/numberOfLeadingZeros(2i16 ** 6i16 + 2i16 ** 11i16)"
    runTest(input, 4:Short)
  }

  test("numberOfLeadingZeros.07") {
    val input = "def r: Int32 = Int16/numberOfLeadingZeros(2i16 ** 7i16 + 2i16 ** 6i16)"
    runTest(input, 8:Short)
  }

  test("numberOfLeadingZeros.08") {
    val input = "def r: Int32 = Int16/numberOfLeadingZeros(20i16)"
    runTest(input, 11:Short)
  }

  test("numberOfLeadingZeros.09") {
    val input = "def r: Int32 = Int16/numberOfLeadingZeros(1i16)"
    runTest(input, 15:Short)
  }

  test("lowestOneBitPosition.01") {
    val input = "def r: Int32 = Int16/lowestOneBitPosition(-1i16)"
    runTest(input, 0:Short)
  }

  test("lowestOneBitPosition.02") {
    val input = "def r: Int32 = Int16/lowestOneBitPosition(-32768i16)"
    runTest(input, 15:Short)
  }

  test("lowestOneBitPosition.03") {
    val input = "def r: Int32 = Int16/lowestOneBitPosition(1i16)"
    runTest(input, 0:Short)
  }

  test("lowestOneBitPosition.04") {
    val input = "def r: Int32 = Int16/lowestOneBitPosition(2i16)"
    runTest(input, 1:Short)
  }

  test("lowestOneBitPosition.05") {
    val input = "def r: Int32 = Int16/lowestOneBitPosition(3i16)"
    runTest(input, 0:Short)
  }

  test("lowestOneBitPosition.06") {
    val input = "def r: Int32 = Int16/lowestOneBitPosition(4i16)"
    runTest(input, 2:Short)
  }

  test("lowestOneBitPosition.07") {
    val input = "def r: Int32 = Int16/lowestOneBitPosition(12i16)"
    runTest(input, 2:Short)
  }

  test("lowestOneBitPosition.08") {
    val input = "def r: Int32 = Int16/lowestOneBitPosition(2i16 ** 11i16)"
    runTest(input, 11:Short)
  }

  test("lowestOneBitPosition.09") {
    val input = "def r: Int32 = Int16/lowestOneBitPosition(2i16 ** 11i16 + 2i16 ** 6i16)"
    runTest(input, 6:Short)
  }

  test("lowestOneBitPosition.10") {
    val input = "def r: Int32 = Int16/lowestOneBitPosition(32767i16)"
    runTest(input, 0:Short)
  }

  test("lowestOneBitPosition.11") {
    val input = "def r: Int32 = Int16/lowestOneBitPosition(0i16)"
    runTest(input, -1:Short)
  }

  test("lowestOneBit.01") {
    val input = "def r: Int16 = Int16/lowestOneBit(-1i16)"
    runTest(input, 1:Short)
  }

  test("lowestOneBit.02") {
    val input = "def r: Int16 = Int16/lowestOneBit(-32768i16)"
    runTest(input, Short.MinValue)
  }

  test("lowestOneBit.03") {
    val input = "def r: Int16 = Int16/lowestOneBit(1i16)"
    runTest(input, 1:Short)
  }

  test("lowestOneBit.04") {
    val input = "def r: Int16 = Int16/lowestOneBit(2i16)"
    runTest(input, 2:Short)
  }

  test("lowestOneBit.05") {
    val input = "def r: Int16 = Int16/lowestOneBit(6i16)"
    runTest(input, 2:Short)
  }

  test("lowestOneBit.06") {
    val input = "def r: Int16 = Int16/lowestOneBit(12i16)"
    runTest(input, 4:Short)
  }

  test("lowestOneBit.07") {
    val input = "def r: Int16 = Int16/lowestOneBit(2i16 ** 6i16 + 2i16 ** 9i16 + 2i16 ** 14i16)"
    runTest(input, 64:Short)
  }

  test("lowestOneBit.08") {
    val input = "def r: Int16 = Int16/lowestOneBit(2i16 ** 13i16)"
    runTest(input, 8192:Short)
  }

  test("lowestOneBit.09") {
    val input = "def r: Int16 = Int16/lowestOneBit(2i16 ** 3i16 + 2i16 ** 5i16)"
    runTest(input, 8:Short)
  }

  test("lowestOneBit.10") {
    val input = "def r: Int16 = Int16/lowestOneBit(32767i16)"
    runTest(input, 1:Short)
  }

  test("lowestOneBit.11") {
    val input = "def r: Int16 = Int16/lowestOneBit(0i16)"
    runTest(input, 0:Short)
  }

  test("numberOfTrailingZeros.01") {
    val input = "def r: Int32 = Int16/numberOfTrailingZeros(0i16)"
    runTest(input, 16:Short)
  }

  test("numberOfTrailingZeros.02") {
    val input = "def r: Int32 = Int16/numberOfTrailingZeros(3i16)"
    runTest(input, 0:Short)
  }

  test("numberOfTrailingZeros.03") {
    val input = "def r: Int32 = Int16/numberOfTrailingZeros(42i16)"
    runTest(input, 1:Short)
  }

  test("numberOfTrailingZeros.04") {
    val input = "def r: Int32 = Int16/numberOfTrailingZeros(2i16 ** 10i16 + 2i16 ** 14i16)"
    runTest(input, 10:Short)
  }

  test("numberOfTrailingZeros.05") {
    val input = "def r: Int32 = Int16/numberOfTrailingZeros(2i16 ** 8i16)"
    runTest(input, 8:Short)
  }

  test("numberOfTrailingZeros.06") {
    val input = "def r: Int32 = Int16/numberOfTrailingZeros(-1i16)"
    runTest(input, 0:Short)
  }

  test("numberOfTrailingZeros.07") {
    val input = "def r: Int32 = Int16/numberOfTrailingZeros(-32768i16)"
    runTest(input, 15:Short)
  }

  test("bit.01") {
    val input = "def r: Int32 = Int16/bit(-1i16, 5)"
    runTest(input, 1:Short)
  }

  test("bit.02") {
    val input = "def r: Int32 = Int16/bit(-10000i16, 15)"
    runTest(input, 1:Short)
  }

  test("bit.03") {
    val input = "def r: Int32 = Int16/bit(2i16 ** 12i16, 12)"
    runTest(input, 1:Short)
  }

  test("bit.04") {
    val input = "def r: Int32 = Int16/bit(2i16 ** 12i16, 13)"
    runTest(input, 0:Short)
  }

  test("bit.05") {
    val input = "def r: Int32 = Int16/bit(2i16 ** 12i16, 11)"
    runTest(input, 0:Short)
  }

  test("bit.06") {
    val input = "def r: Int32 = Int16/bit(-1i16 - 2i16 ** 9i16, 9)"
    runTest(input, 0:Short)
  }

  test("bit.07") {
    val input = "def r: Int32 = Int16/bit(-1i16 - 2i16 ** 9i16, 10)"
    runTest(input, 1:Short)
  }

  test("bit.08") {
    val input = "def r: Int32 = Int16/bit(0i16, 15)"
    runTest(input, 0:Short)
  }
}