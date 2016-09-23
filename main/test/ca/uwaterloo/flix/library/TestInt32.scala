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

class TestInt32 extends FunSuite {

  val options = Options.DefaultTest

  def runTest(input: String, output: Int) {
    val flix = new Flix().setOptions(options).addPath("main/src/library/Int32.flix").addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  test("maxValue.01") {
    val input = "def r: Int32 = Int32/maxValue()"
    runTest(input, Int.MaxValue)
  }

  test("minValue.01") {
    val input = "def r: Int32 = Int32/minValue()"
    runTest(input, Int.MinValue)
  }

  test("size.01") {
    val input = "def r: Int32 = Int32/size()"
    runTest(input, 32)
  }

  test("min.01") {
    val input = "def r: Int32 = Int32/min(1, 2)"
    runTest(input, 1)
  }

  test("min.02") {
    val input = "def r: Int32 = Int32/min(2, -1)"
    runTest(input, -1)
  }

  test("min.03") {
    val input = "def r: Int32 = Int32/min(-33, -66)"
    runTest(input, -66)
  }

  test("max.01") {
    val input = "def r: Int32 = Int32/max(48, 49)"
    runTest(input, 49)
  }

  test("max.02") {
    val input = "def r: Int32 = Int32/max(4, -16)"
    runTest(input, 4)
  }

  test("max.03") {
    val input = "def r: Int32 = Int32/max(-34, -16)"
    runTest(input, -16)
  }

  test("abs.01") {
    val input = "def r: Int32 = Int32/abs(31)"
    runTest(input, 31)
  }

  test("abs.02") {
    val input = "def r: Int32 = Int32/abs(-31)"
    runTest(input, 31)
  }

  test("abs.03") {
    val input = "def r: Int32 = Int32/abs(0)"
    runTest(input, 0)
  }

  test("dist.01") {
    val input = "def r: Int32 = Int32/dist(31, -7)"
    runTest(input, 38)
  }

  test("dist.02") {
    val input = "def r: Int32 = Int32/dist(-44, -1)"
    runTest(input, 43)
  }

  test("dist.03") {
    val input = "def r: Int32 = Int32/dist(-2, -2)"
    runTest(input, 0)
  }

  test("dist.04") {
    val input = "def r: Int32 = Int32/dist(-2147483648, -1)"
    runTest(input, 2147483647)
  }

  test("dist.05") {
    val input = "def r: Int32 = Int32/dist(-2147483648, 0)"
    runTest(input, -1)
  }

  test("dist.06") {
    val input = "def r: Int32 = Int32/dist(-2147483643, 4)"
    runTest(input, 2147483647)
  }

  test("dist.07") {
    val input = "def r: Int32 = Int32/dist(-2147483643, 5)"
    runTest(input, -1)
  }

  test("dist.08") {
    val input = "def r: Int32 = Int32/dist(-1147473643, 1000010004)"
    runTest(input, 2147483647)
  }

  test("dist.09") {
    val input = "def r: Int32 = Int32/dist(-1147473643, 1000010005)"
    runTest(input, -1)
  }

  test("compare.01") {
    val input = "def r: Int32 = Int32/compare(-1, 44)"
    runTest(input, -1)
  }

  test("compare.02") {
    val input = "def r: Int32 = Int32/compare(-1, -44)"
    runTest(input, 1)
  }

  test("compare.03") {
    val input = "def r: Int32 = Int32/compare(88, 88)"
    runTest(input, 0)
  }

  test("signum.01") {
    val input = "def r: Int32 = Int32/signum(-22)"
    runTest(input, -1)
  }

  test("signum.02") {
    val input = "def r: Int32 = Int32/signum(22)"
    runTest(input, 1)
  }

  test("signum.03") {
    val input = "def r: Int32 = Int32/signum(0)"
    runTest(input, 0)
  }

  test("logicalRightShift.01") {
    val input = "def r: Int32 = Int32/logicalRightShift(2233, 4)"
    runTest(input, 2233 >>> 4)
  }

  test("logicalRightShift.02") {
    val input = "def r: Int32 = Int32/logicalRightShift(223366, 33)"
    runTest(input, 223366 >>> 33)
  }

  test("logicalRightShift.03") {
    val input = "def r: Int32 = Int32/logicalRightShift(2233, -8)"
    runTest(input, 2233 >>> -8)
  }

  test("logicalRightShift.04") {
    val input = "def r: Int32 = Int32/logicalRightShift(2233, 4)"
    runTest(input, 2233 >>> 4)
  }

  test("logicalRightShift.05") {
    val input = "def r: Int32 = Int32/logicalRightShift(-1, 1)"
    runTest(input, -1 >>> 1)
  }

  test("logicalRightShift.06") {
    val input = "def r: Int32 = Int32/logicalRightShift(-35, 4)"
    runTest(input, -35 >>> 4)
  }

  test("logicalRightShift.07") {
    val input = "def r: Int32 = Int32/logicalRightShift(-2777, -2777)"
    runTest(input, -2777 >>> -2777)
  }

  test("logicalRightShift.08") {
    val input = "def r: Int32 = Int32/logicalRightShift(-233, 64)"
    runTest(input, -233 >>> 64)
  }

  test("logicalRightShift.09") {
    val input = "def r: Int32 = Int32/logicalRightShift(-27, 0)"
    runTest(input, -27 >>> 0)
  }

  test("logicalRightShift.10") {
    val input = "def r: Int32 = Int32/logicalRightShift(2147483647, 1)"
    runTest(input, 2147483647 >>> 1)
  }

  test("bitCount.01") {
    val input = "def r: Int32 = Int32/bitCount(19909)"
    runTest(input, Integer.bitCount(19909))
  }

  test("bitCount.02") {
    val input = "def r: Int32 = Int32/bitCount(-1)"
    runTest(input, Integer.bitCount(-1))
  }

  test("bitCount.03") {
    val input = "def r: Int32 = Int32/bitCount(-999888888)"
    runTest(input, Integer.bitCount(-999888888))
  }

  test("bitCount.04") {
    val input = "def r: Int32 = Int32/bitCount(0)"
    runTest(input, Integer.bitCount(0))
  }

  test("bitCount.05") {
    val input = "def r: Int32 = Int32/bitCount(2147483647)"
    runTest(input, Integer.bitCount(2147483647))
  }

  test("bitCount.06") {
    val input = "def r: Int32 = Int32/bitCount(-2147483648)"
    runTest(input, Integer.bitCount(-2147483648))
  }

  test("rotateLeft.01") {
    val input = "def r: Int32 = Int32/rotateLeft(-1698265258, 4)"
    runTest(input, Integer.rotateLeft(-1698265258, 4))
  }

  test("rotateLeft.02") {
    val input = "def r: Int32 = Int32/rotateLeft(-1836677290, 5)"
    runTest(input, Integer.rotateLeft(-1836677290, 5))
  }

  test("rotateLeft.03") {
    val input = "def r: Int32 = Int32/rotateLeft(1478293001, 24)"
    runTest(input, Integer.rotateLeft(1478293001, 24))
  }

  test("rotateLeft.04") {
    val input = "def r: Int32 = Int32/rotateLeft(-12267711, -36)"
    runTest(input, Integer.rotateLeft(-12267711, -36))
  }

  test("rotateRight.01") {
    val input = "def r: Int32 = Int32/rotateRight(-1698265258, 4)"
    runTest(input, Integer.rotateRight(-1698265258, 4))
  }

  test("rotateRight.02") {
    val input = "def r: Int32 = Int32/rotateRight(-1836677290, 5)"
    runTest(input, Integer.rotateRight(-1836677290, 5))
  }

  test("rotateRight.03") {
    val input = "def r: Int32 = Int32/rotateRight(1478293001, 24)"
    runTest(input, Integer.rotateRight(1478293001, 24))
  }

  test("rotateRight.04") {
    val input = "def r: Int32 = Int32/rotateRight(-12267711, -36)"
    runTest(input, Integer.rotateRight(-12267711, -36))
  }

  test("highestOneBitPosition.01") {
    val input = "def r: Int32 = Int32/highestOneBitPosition(-1)"
    runTest(input, 31)
  }

  test("highestOneBitPosition.02") {
    val input = "def r: Int32 = Int32/highestOneBitPosition(-2147483648)"
    runTest(input, 31)
  }

  test("highestOneBitPosition.03") {
    val input = "def r: Int32 = Int32/highestOneBitPosition(1)"
    runTest(input, 0)
  }

  test("highestOneBitPosition.04") {
    val input = "def r: Int32 = Int32/highestOneBitPosition(2)"
    runTest(input, 1)
  }

  test("highestOneBitPosition.05") {
    val input = "def r: Int32 = Int32/highestOneBitPosition(3)"
    runTest(input, 1)
  }

  test("highestOneBitPosition.06") {
    val input = "def r: Int32 = Int32/highestOneBitPosition(20)"
    runTest(input, 4)
  }

  test("highestOneBitPosition.07") {
    val input = "def r: Int32 = Int32/highestOneBitPosition(2 ** 30)"
    runTest(input, 30)
  }

  test("highestOneBitPosition.08") {
    val input = "def r: Int32 = Int32/highestOneBitPosition(2147483647)"
    runTest(input, 30)
  }

  test("highestOneBitPosition.09") {
    val input = "def r: Int32 = Int32/highestOneBitPosition(0)"
    runTest(input, -1)
  }

  test("highestOneBitPosition.10") {
    val input = "def r: Int32 = Int32/highestOneBitPosition(2 ** 23 + 2 ** 18)"
    runTest(input, 23)
  }

  test("highestOneBit.01") {
    val input = "def r: Int32 = Int32/highestOneBit(-1)"
    runTest(input, Int.MinValue)
  }

  test("highestOneBit.02") {
    val input = "def r: Int32 = Int32/highestOneBit(-2147483000)"
    runTest(input, Int.MinValue)
  }

  test("highestOneBit.03") {
    val input = "def r: Int32 = Int32/highestOneBit(1 + 2 ** 24)"
    runTest(input, 16777216)
  }

  test("highestOneBit.04") {
    val input = "def r: Int32 = Int32/highestOneBit(2)"
    runTest(input, 2)
  }

  test("highestOneBit.05") {
    val input = "def r: Int32 = Int32/highestOneBit(3)"
    runTest(input, 2)
  }

  test("highestOneBit.06") {
    val input = "def r: Int32 = Int32/highestOneBit(20)"
    runTest(input, 16)
  }

  test("highestOneBit.07") {
    val input = "def r: Int32 = Int32/highestOneBit(2 ** 30)"
    runTest(input, 1073741824)
  }

  test("highestOneBit.08") {
    val input = "def r: Int32 = Int32/highestOneBit(2147483647)"
    runTest(input, 1073741824)
  }

  test("highestOneBit.09") {
    val input = "def r: Int32 = Int32/highestOneBit(0)"
    runTest(input, 0)
  }

  test("highestOneBit.10") {
    val input = "def r: Int32 = Int32/highestOneBit(2 ** 24 + 2 ** 18)"
    runTest(input, 16777216)
  }

  test("numberOfLeadingZeros.01") {
    val input = "def r: Int32 = Int32/numberOfLeadingZeros(0)"
    runTest(input, 32)
  }

  test("numberOfLeadingZeros.02") {
    val input = "def r: Int32 = Int32/numberOfLeadingZeros(- 2 ** 31)"
    runTest(input, 0)
  }

  test("numberOfLeadingZeros.03") {
    val input = "def r: Int32 = Int32/numberOfLeadingZeros(-1)"
    runTest(input, 0)
  }

  test("numberOfLeadingZeros.04") {
    val input = "def r: Int32 = Int32/numberOfLeadingZeros((2 ** 30 - 1) * 2)"
    runTest(input, 1)
  }

  test("numberOfLeadingZeros.05") {
    val input = "def r: Int32 = Int32/numberOfLeadingZeros(2 ** 29)"
    runTest(input, 2)
  }

  test("numberOfLeadingZeros.06") {
    val input = "def r: Int32 = Int32/numberOfLeadingZeros(2 ** 24 + 2 ** 18)"
    runTest(input, 7)
  }

  test("numberOfLeadingZeros.07") {
    val input = "def r: Int32 = Int32/numberOfLeadingZeros(20)"
    runTest(input, 27)
  }

  test("numberOfLeadingZeros.08") {
    val input = "def r: Int32 = Int32/numberOfLeadingZeros(1)"
    runTest(input, 31)
  }

  test("lowestOneBitPosition.01") {
    val input = "def r: Int32 = Int32/lowestOneBitPosition(-1)"
    runTest(input, 0)
  }

  test("lowestOneBitPosition.02") {
    val input = "def r: Int32 = Int32/lowestOneBitPosition(-2147483648)"
    runTest(input, 31)
  }

  test("lowestOneBitPosition.03") {
    val input = "def r: Int32 = Int32/lowestOneBitPosition(1)"
    runTest(input, 0)
  }

  test("lowestOneBitPosition.04") {
    val input = "def r: Int32 = Int32/lowestOneBitPosition(2)"
    runTest(input, 1)
  }

  test("lowestOneBitPosition.05") {
    val input = "def r: Int32 = Int32/lowestOneBitPosition(3)"
    runTest(input, 0)
  }

  test("lowestOneBitPosition.06") {
    val input = "def r: Int32 = Int32/lowestOneBitPosition(4)"
    runTest(input, 2)
  }

  test("lowestOneBitPosition.07") {
    val input = "def r: Int32 = Int32/lowestOneBitPosition(12)"
    runTest(input, 2)
  }

  test("lowestOneBitPosition.08") {
    val input = "def r: Int32 = Int32/lowestOneBitPosition(2 ** 30)"
    runTest(input, 30)
  }

  test("lowestOneBitPosition.09") {
    val input = "def r: Int32 = Int32/lowestOneBitPosition(2 ** 23 + 2 ** 18)"
    runTest(input, 18)
  }

  test("lowestOneBitPosition.10") {
    val input = "def r: Int32 = Int32/lowestOneBitPosition(2147483647)"
    runTest(input, 0)
  }

  test("lowestOneBitPosition.11") {
    val input = "def r: Int32 = Int32/lowestOneBitPosition(0)"
    runTest(input, -1)
  }

  test("lowestOneBit.01") {
    val input = "def r: Int32 = Int32/lowestOneBit(-1)"
    runTest(input, 1)
  }

  test("lowestOneBit.02") {
    val input = "def r: Int32 = Int32/lowestOneBit(-2147483648)"
    runTest(input, Int.MinValue)
  }

  test("lowestOneBit.03") {
    val input = "def r: Int32 = Int32/lowestOneBit(1)"
    runTest(input, 1)
  }

  test("lowestOneBit.04") {
    val input = "def r: Int32 = Int32/lowestOneBit(2)"
    runTest(input, 2)
  }

  test("lowestOneBit.05") {
    val input = "def r: Int32 = Int32/lowestOneBit(6)"
    runTest(input, 2)
  }

  test("lowestOneBit.06") {
    val input = "def r: Int32 = Int32/lowestOneBit(12)"
    runTest(input, 4)
  }

  test("lowestOneBit.07") {
    val input = "def r: Int32 = Int32/lowestOneBit(2 ** 6 + 2 ** 18 + 2 ** 27)"
    runTest(input, 64)
  }

  test("lowestOneBit.08") {
    val input = "def r: Int32 = Int32/lowestOneBit(2 ** 30)"
    runTest(input, 1073741824)
  }

  test("lowestOneBit.09") {
    val input = "def r: Int32 = Int32/lowestOneBit(2 ** 23 + 2 ** 18)"
    runTest(input, 262144)
  }

  test("lowestOneBit.10") {
    val input = "def r: Int32 = Int32/lowestOneBit(2147483647)"
    runTest(input, 1)
  }

  test("lowestOneBit.11") {
    val input = "def r: Int32 = Int32/lowestOneBit(0)"
    runTest(input, 0)
  }

  test("numberOfTrailingZeros.01") {
    val input = "def r: Int32 = Int32/numberOfTrailingZeros(0)"
    runTest(input, 32)
  }

  test("numberOfTrailingZeros.02") {
    val input = "def r: Int32 = Int32/numberOfTrailingZeros(3)"
    runTest(input, 0)
  }

  test("numberOfTrailingZeros.03") {
    val input = "def r: Int32 = Int32/numberOfTrailingZeros(42)"
    runTest(input, 1)
  }

  test("numberOfTrailingZeros.04") {
    val input = "def r: Int32 = Int32/numberOfTrailingZeros(2 ** 16 + 2 ** 22)"
    runTest(input, 16)
  }

  test("numberOfTrailingZeros.05") {
    val input = "def r: Int32 = Int32/numberOfTrailingZeros(2 ** 30)"
    runTest(input, 30)
  }

  test("numberOfTrailingZeros.06") {
    val input = "def r: Int32 = Int32/numberOfTrailingZeros(-1)"
    runTest(input, 0)
  }

  test("numberOfTrailingZeros.07") {
    val input = "def r: Int32 = Int32/numberOfTrailingZeros(-2147483648)"
    runTest(input, 31)
  }

  test("bit.01") {
    val input = "def r: Int32 = Int32/bit(-1, 5)"
    runTest(input, 1)
  }

  test("bit.02") {
    val input = "def r: Int32 = Int32/bit(-1000000, 31)"
    runTest(input, 1)
  }

  test("bit.03") {
    val input = "def r: Int32 = Int32/bit(2 ** 23, 23)"
    runTest(input, 1)
  }

  test("bit.04") {
    val input = "def r: Int32 = Int32/bit(-1 - 2 ** 7, 7)"
    runTest(input, 0)
  }

  test("bit.05") {
    val input = "def r: Int32 = Int32/bit(2 ** 23, 22)"
    runTest(input, 0)
  }

  test("bit.06") {
    val input = "def r: Int32 = Int32/bit(2 ** 23, 24)"
    runTest(input, 0)
  }

  test("bit.07") {
    val input = "def r: Int32 = Int32/bit(0, 17)"
    runTest(input, 0)
  }
}