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

  def runTest(input: String, output: Long) {
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
    runTest(input, 16)
  }

  test("min.01") {
    val input = "def r: Int16 = Int16/min(1i16, 2i16)"
    runTest(input, 1)
  }

  test("min.02") {
    val input = "def r: Int16 = Int16/min(2i16, -1i16)"
    runTest(input, -1)
  }

  test("min.03") {
    val input = "def r: Int16 = Int16/min(-33i16, -66i16)"
    runTest(input, -66)
  }

  test("max.01") {
    val input = "def r: Int16 = Int16/max(48i16, 49i16)"
    runTest(input, 49)
  }

  test("max.02") {
    val input = "def r: Int16 = Int16/max(4i16, -16i16)"
    runTest(input, 4)
  }

  test("max.03") {
    val input = "def r: Int16 = Int16/max(-34i16, -16i16)"
    runTest(input, -16)
  }

  test("abs.01") {
    val input = "def r: Int16 = Int16/abs(31i16)"
    runTest(input, 31)
  }

  test("abs.02") {
    val input = "def r: Int16 = Int16/abs(-31i16)"
    runTest(input, 31)
  }

  test("abs.03") {
    val input = "def r: Int16 = Int16/abs(0i16)"
    runTest(input, 0)
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
    runTest(input, -1)
  }

  test("dist.01") {
    val input = "def r: Int16 = Int16/dist(31i16, -7i16)"
    runTest(input, 38)
  }

  test("dist.02") {
    val input = "def r: Int16 = Int16/dist(-44i16, -1i16)"
    runTest(input, 43)
  }

  test("dist.03") {
    val input = "def r: Int16 = Int16/dist(-2i16, -2i16)"
    runTest(input, 0)
  }

  test("dist.04") {
    val input = "def r: Int16 = Int16/dist(-32768i16, -1i16)"
    runTest(input, Short.MaxValue)
  }

  test("dist.05") {
    val input = "def r: Int16 = Int16/dist(-32768i16, 0i16)"
    runTest(input, -1)
  }

  test("dist.06") {
    val input = "def r: Int16 = Int16/dist(-32763i16, 4i16)"
    runTest(input, Short.MaxValue)
  }

  test("dist.07") {
    val input = "def r: Int16 = Int16/dist(-32763i16, 5i16)"
    runTest(input, -1)
  }

  test("dist.08") {
    val input = "def r: Int16 = Int16/dist(-32666i16, 101i16)"
    runTest(input, Short.MaxValue)
  }

  test("dist.09") {
    val input = "def r: Int16 = Int16/dist(-32666i16, 102i16)"
    runTest(input, -1)
  }

  test("dist.10") {
    val input = "def r: Int16 = Int16/dist(32767i16, 0i16)"
    runTest(input, Short.MaxValue)
  }

  test("dist.11") {
    val input = "def r: Int16 = Int16/dist(32767i16, 1i16)"
    runTest(input, Short.MaxValue - 1)
  }

  test("compare.01") {
    val input = "def r: Int32 = Int16/compare(-1i16, 44i16)"
    runTest(input, -1)
  }

  test("compare.02") {
    val input = "def r: Int32 = Int16/compare(-1i16, -44i16)"
    runTest(input, 1)
  }

  test("compare.03") {
    val input = "def r: Int32 = Int16/compare(88i16, 88i16)"
    runTest(input, 0)
  }

  test("signum.01") {
    val input = "def r: Int32 = Int16/signum(-22i16)"
    runTest(input, -1)
  }

  test("signum.02") {
    val input = "def r: Int32 = Int16/signum(22i16)"
    runTest(input, 1)
  }

  test("signum.03") {
    val input = "def r: Int32 = Int16/signum(0i16)"
    runTest(input, 0)
  }

  test("highestOneBitPosition.01") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(-1i16)"
    runTest(input, 15)
  }

  test("highestOneBitPosition.02") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(-32768i16)"
    runTest(input, 15)
  }

  test("highestOneBitPosition.03") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(1i16)"
    runTest(input, 0)
  }

  test("highestOneBitPosition.04") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(2i16)"
    runTest(input, 1)
  }

  test("highestOneBitPosition.05") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(3i16)"
    runTest(input, 1)
  }

  test("highestOneBitPosition.06") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(20i16)"
    runTest(input, 4)
  }

  test("highestOneBitPosition.07") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(2i16 ** 14i16)"
    runTest(input, 14)
  }

  test("highestOneBitPosition.08") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(32767i16)"
    runTest(input, 14)
  }

  test("highestOneBitPosition.09") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(0i16)"
    runTest(input, -1)
  }

  test("highestOneBitPosition.10") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(2i16 ** 8i16 + 2i16 ** 11i16)"
    runTest(input, 11)
  }

  test("highestOneBitPosition.11") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(2i16 ** 3i16 + 2i16 ** 7i16)"
    runTest(input, 7)
  }

  test("highestOneBitPosition.12") {
    val input = "def r: Int32 = Int16/highestOneBitPosition(-32767i16)"
    runTest(input, 15)
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
    runTest(input, 2)
  }

  test("highestOneBit.05") {
    val input = "def r: Int16 = Int16/highestOneBit(3i16)"
    runTest(input, 2)
  }

  test("highestOneBit.06") {
    val input = "def r: Int16 = Int16/highestOneBit(20i16)"
    runTest(input, 16)
  }

  test("highestOneBit.07") {
    val input = "def r: Int16 = Int16/highestOneBit(2i16 ** 11i16 + 2i16 ** 13i16)"
    runTest(input, 8192)
  }

  test("highestOneBit.08") {
    val input = "def r: Int16 = Int16/highestOneBit(32767i16)"
    runTest(input, 16384)
  }

  test("highestOneBit.09") {
    val input = "def r: Int16 = Int16/highestOneBit(0i16)"
    runTest(input, 0L)
  }

  test("highestOneBit.10") {
    val input = "def r: Int16 = Int16/highestOneBit(2i16 ** 6i16 + 2i16 ** 9i16)"
    runTest(input, 512)
  }

  test("numberOfLeadingZeros.01") {
    val input = "def r: Int32 = Int16/numberOfLeadingZeros(0i16)"
    runTest(input, 16)
  }

  test("numberOfLeadingZeros.02") {
    val input = "def r: Int32 = Int16/numberOfLeadingZeros(-32768i16)"
    runTest(input, 0)
  }

  test("numberOfLeadingZeros.03") {
    val input = "def r: Int32 = Int16/numberOfLeadingZeros(-1i16)"
    runTest(input, 0)
  }

  test("numberOfLeadingZeros.04") {
    val input = "def r: Int32 = Int16/numberOfLeadingZeros(32767i16)"
    runTest(input, 1)
  }

  test("numberOfLeadingZeros.05") {
    val input = "def r: Int32 = Int16/numberOfLeadingZeros(2i16 ** 14i16)"
    runTest(input, 1)
  }

  test("numberOfLeadingZeros.06") {
    val input = "def r: Int32 = Int16/numberOfLeadingZeros(2i16 ** 6i16 + 2i16 ** 11i16)"
    runTest(input, 4)
  }

  test("numberOfLeadingZeros.07") {
    val input = "def r: Int32 = Int16/numberOfLeadingZeros(2i16 ** 7i16 + 2i16 ** 6i16)"
    runTest(input, 8)
  }

  test("numberOfLeadingZeros.08") {
    val input = "def r: Int32 = Int16/numberOfLeadingZeros(20i16)"
    runTest(input, 11)
  }

  test("numberOfLeadingZeros.09") {
    val input = "def r: Int32 = Int16/numberOfLeadingZeros(1i16)"
    runTest(input, 15)
  }

  test("lowestOneBitPosition.01") {
    val input = "def r: Int32 = Int16/lowestOneBitPosition(-1i16)"
    runTest(input, 0)
  }

  test("lowestOneBitPosition.02") {
    val input = "def r: Int32 = Int16/lowestOneBitPosition(-32768i16)"
    runTest(input, 15)
  }

  test("lowestOneBitPosition.03") {
    val input = "def r: Int32 = Int16/lowestOneBitPosition(1i16)"
    runTest(input, 0)
  }

  test("lowestOneBitPosition.04") {
    val input = "def r: Int32 = Int16/lowestOneBitPosition(2i16)"
    runTest(input, 1)
  }

  test("lowestOneBitPosition.05") {
    val input = "def r: Int32 = Int16/lowestOneBitPosition(3i16)"
    runTest(input, 0)
  }

  test("lowestOneBitPosition.06") {
    val input = "def r: Int32 = Int16/lowestOneBitPosition(4i16)"
    runTest(input, 2)
  }

  test("lowestOneBitPosition.07") {
    val input = "def r: Int32 = Int16/lowestOneBitPosition(12i16)"
    runTest(input, 2)
  }

  test("lowestOneBitPosition.08") {
    val input = "def r: Int32 = Int16/lowestOneBitPosition(2i16 ** 11i16)"
    runTest(input, 11)
  }

  test("lowestOneBitPosition.09") {
    val input = "def r: Int32 = Int16/lowestOneBitPosition(2i16 ** 11i16 + 2i16 ** 6i16)"
    runTest(input, 6)
  }

  test("lowestOneBitPosition.10") {
    val input = "def r: Int32 = Int16/lowestOneBitPosition(32767i16)"
    runTest(input, 0)
  }

  test("lowestOneBitPosition.11") {
    val input = "def r: Int32 = Int16/lowestOneBitPosition(0i16)"
    runTest(input, -1)
  }

  test("lowestOneBit.01") {
    val input = "def r: Int16 = Int16/lowestOneBit(-1i16)"
    runTest(input, 1)
  }

  test("lowestOneBit.02") {
    val input = "def r: Int16 = Int16/lowestOneBit(-32768i16)"
    runTest(input, Short.MinValue)
  }

  test("lowestOneBit.03") {
    val input = "def r: Int16 = Int16/lowestOneBit(1i16)"
    runTest(input, 1)
  }

  test("lowestOneBit.04") {
    val input = "def r: Int16 = Int16/lowestOneBit(2i16)"
    runTest(input, 2)
  }

  test("lowestOneBit.05") {
    val input = "def r: Int16 = Int16/lowestOneBit(6i16)"
    runTest(input, 2)
  }

  test("lowestOneBit.06") {
    val input = "def r: Int16 = Int16/lowestOneBit(12i16)"
    runTest(input, 4)
  }

  test("lowestOneBit.07") {
    val input = "def r: Int16 = Int16/lowestOneBit(2i16 ** 6i16 + 2i16 ** 9i16 + 2i16 ** 14i16)"
    runTest(input, 64)
  }

  test("lowestOneBit.08") {
    val input = "def r: Int16 = Int16/lowestOneBit(2i16 ** 13i16)"
    runTest(input, 8192)
  }

  test("lowestOneBit.09") {
    val input = "def r: Int16 = Int16/lowestOneBit(2i16 ** 3i16 + 2i16 ** 5i16)"
    runTest(input, 8)
  }

  test("lowestOneBit.10") {
    val input = "def r: Int16 = Int16/lowestOneBit(32767i16)"
    runTest(input, 1)
  }

  test("lowestOneBit.11") {
    val input = "def r: Int16 = Int16/lowestOneBit(0i16)"
    runTest(input, 0)
  }

  test("numberOfTrailingZeros.01") {
    val input = "def r: Int32 = Int16/numberOfTrailingZeros(0i16)"
    runTest(input, 16)
  }

  test("numberOfTrailingZeros.02") {
    val input = "def r: Int32 = Int16/numberOfTrailingZeros(3i16)"
    runTest(input, 0)
  }

  test("numberOfTrailingZeros.03") {
    val input = "def r: Int32 = Int16/numberOfTrailingZeros(42i16)"
    runTest(input, 1)
  }

  test("numberOfTrailingZeros.04") {
    val input = "def r: Int32 = Int16/numberOfTrailingZeros(2i16 ** 10i16 + 2i16 ** 14i16)"
    runTest(input, 10)
  }

  test("numberOfTrailingZeros.05") {
    val input = "def r: Int32 = Int16/numberOfTrailingZeros(2i16 ** 8i16)"
    runTest(input, 8)
  }

  test("numberOfTrailingZeros.06") {
    val input = "def r: Int32 = Int16/numberOfTrailingZeros(-1i16)"
    runTest(input, 0)
  }

  test("numberOfTrailingZeros.07") {
    val input = "def r: Int32 = Int16/numberOfTrailingZeros(-32768i16)"
    runTest(input, 15)
  }

  test("bit.01") {
    val input = "def r: Int32 = Int16/bit(-1i16, 5)"
    runTest(input, 1)
  }

  test("bit.02") {
    val input = "def r: Int32 = Int16/bit(-10000i16, 15)"
    runTest(input, 1)
  }

  test("bit.03") {
    val input = "def r: Int32 = Int16/bit(2i16 ** 12i16, 12)"
    runTest(input, 1)
  }

  test("bit.04") {
    val input = "def r: Int32 = Int16/bit(2i16 ** 12i16, 13)"
    runTest(input, 0)
  }

  test("bit.05") {
    val input = "def r: Int32 = Int16/bit(2i16 ** 12i16, 11)"
    runTest(input, 0)
  }

  test("bit.06") {
    val input = "def r: Int32 = Int16/bit(-1i16 - 2i16 ** 9i16, 9)"
    runTest(input, 0)
  }

  test("bit.07") {
    val input = "def r: Int32 = Int16/bit(-1i16 - 2i16 ** 9i16, 10)"
    runTest(input, 1)
  }

  test("bit.08") {
    val input = "def r: Int32 = Int16/bit(0i16, 15)"
    runTest(input, 0)
  }
}