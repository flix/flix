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

class TestInt64 extends FunSuite {

  val options = Options.DefaultTest

  def runTest(input: String, output: Long) {
    val flix = new Flix().setOptions(options).addPath("main/src/library/Int64.flix").addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  test("maxValue.01") {
    val input = "def r: Int64 = Int64/maxValue()"
    runTest(input, Long.MaxValue)
  }

  test("minValue.01") {
    val input = "def r: Int64 = Int64/minValue()"
    runTest(input, Long.MinValue)
  }

  test("size.01") {
    val input = "def r: Int32 = Int64/size()"
    runTest(input, 64L)
  }

  test("min.01") {
    val input = "def r: Int64 = Int64/min(1i64, 2i64)"
    runTest(input, 1L)
  }

  test("min.02") {
    val input = "def r: Int64 = Int64/min(2i64, -1i64)"
    runTest(input, -1L)
  }

  test("min.03") {
    val input = "def r: Int64 = Int64/min(-33i64, -66i64)"
    runTest(input, -66L)
  }

  test("max.01") {
    val input = "def r: Int64 = Int64/max(48i64, 49i64)"
    runTest(input, 49L)
  }

  test("max.02") {
    val input = "def r: Int64 = Int64/max(4i64, -16i64)"
    runTest(input, 4L)
  }

  test("max.03") {
    val input = "def r: Int64 = Int64/max(-34i64, -16i64)"
    runTest(input, -16L)
  }

  test("abs.01") {
    val input = "def r: Int64 = Int64/abs(31i64)"
    runTest(input, 31L)
  }

  test("abs.02") {
    val input = "def r: Int64 = Int64/abs(-31i64)"
    runTest(input, 31L)
  }

  test("abs.03") {
    val input = "def r: Int64 = Int64/abs(0i64)"
    runTest(input, 0L)
  }

  test("abs.04") {
    val input = "def r: Int64 = Int64/abs(9223372036854775807i64)"
    runTest(input, Long.MaxValue)
  }

  test("abs.05") {
    val input = "def r: Int64 = Int64/abs(-9223372036854775807i64)"
    runTest(input, Long.MaxValue)
  }

  test("abs.06") {
    val input = "def r: Int64 = Int64/abs(-9223372036854775808i64)"
    runTest(input, -1L)
  }

  test("dist.01") {
    val input = "def r: Int64 = Int64/dist(31i64, -7i64)"
    runTest(input, 38L)
  }

  test("dist.02") {
    val input = "def r: Int64 = Int64/dist(-44i64, -1i64)"
    runTest(input, 43L)
  }

  test("dist.03") {
    val input = "def r: Int64 = Int64/dist(-2i64, -2i64)"
    runTest(input, 0L)
  }

  test("dist.04") {
    val input = "def r: Int64 = Int64/dist(-9223372036854775808i64, -1i64)"
    runTest(input, Long.MaxValue)
  }

  test("dist.05") {
    val input = "def r: Int64 = Int64/dist(-9223372036854775808i64, 0i64)"
    runTest(input, -1L)
  }

  test("dist.06") {
    val input = "def r: Int64 = Int64/dist(-9223372036854775803i64, 4i64)"
    runTest(input, Long.MaxValue)
  }

  test("dist.07") {
    val input = "def r: Int64 = Int64/dist(-9223372036854775803i64, 5i64)"
    runTest(input, -1L)
  }

  test("dist.08") {
    val input = "def r: Int64 = Int64/dist(-9223372035854765803i64, 1000010004i64)"
    runTest(input, Long.MaxValue)
  }

  test("dist.09") {
    val input = "def r: Int64 = Int64/dist(-9223372035854765803i64, 1000010005i64)"
    runTest(input, -1L)
  }

  test("compare.01") {
    val input = "def r: Int32 = Int64/compare(-1i64, 44i64)"
    runTest(input, -1)
  }

  test("compare.02") {
    val input = "def r: Int32 = Int64/compare(-1i64, -44i64)"
    runTest(input, 1)
  }

  test("compare.03") {
    val input = "def r: Int32 = Int64/compare(88i64, 88i64)"
    runTest(input, 0)
  }

  test("signum.01") {
    val input = "def r: Int32 = Int64/signum(-22i64)"
    runTest(input, -1)
  }

  test("signum.02") {
    val input = "def r: Int32 = Int64/signum(22i64)"
    runTest(input, 1)
  }

  test("signum.03") {
    val input = "def r: Int32 = Int64/signum(0i64)"
    runTest(input, 0)
  }

  test("logicalRightShift.01") {
    val input = "def r: Int64 = Int64/logicalRightShift(2233i64, 4)"
    runTest(input, 2233L >>> 4)
  }

  test("logicalRightShift.02") {
    val input = "def r: Int64 = Int64/logicalRightShift(22336688888i64, 33)"
    runTest(input, 22336688888L >>> 33)
  }

  test("logicalRightShift.03") {
    val input = "def r: Int64 = Int64/logicalRightShift(2233i64, -8)"
    runTest(input, 2233L >>> -8)
  }

  test("logicalRightShift.04") {
    val input = "def r: Int64 = Int64/logicalRightShift(2233i64, 4)"
    runTest(input, 2233L >>> 4)
  }

  test("logicalRightShift.05") {
    val input = "def r: Int64 = Int64/logicalRightShift(-1i64, 1)"
    runTest(input, -1L >>> 1)
  }

  test("logicalRightShift.06") {
    val input = "def r: Int64 = Int64/logicalRightShift(-358888899999i64, 4)"
    runTest(input, -358888899999L >>> 4)
  }

  test("logicalRightShift.07") {
    val input = "def r: Int64 = Int64/logicalRightShift(-277732323232i64, -2777)"
    runTest(input, -277732323232L >>> -2777)
  }

  test("logicalRightShift.08") {
    val input = "def r: Int64 = Int64/logicalRightShift(-23328384858i64, 64)"
    runTest(input, -23328384858L >>> 64)
  }

  test("logicalRightShift.09") {
    val input = "def r: Int64 = Int64/logicalRightShift(-27000000000000i64, 0)"
    runTest(input, -27000000000000L >>> 0)
  }

  test("logicalRightShift.10") {
    val input = "def r: Int64 = Int64/logicalRightShift(9223372036854775807i64, 1)"
    runTest(input, Long.MaxValue >>> 1)
  }

  test("logicalRightShift.11") {
    val input = "def r: Int64 = Int64/logicalRightShift(-9223372036854775808i64, 1)"
    runTest(input, Long.MinValue >>> 1)
  }

  test("bitCount.01") {
    val input = "def r: Int32 = Int64/bitCount(1990911889900134i64)"
    runTest(input, 25)
  }

  test("bitCount.02") {
    val input = "def r: Int32 = Int64/bitCount(-1i64)"
    runTest(input, 64)
  }

  test("bitCount.03") {
    val input = "def r: Int32 = Int64/bitCount(-999888888114322199i64)"
    runTest(input, 35)
  }

  test("bitCount.04") {
    val input = "def r: Int32 = Int64/bitCount(0i64)"
    runTest(input, 0)
  }

  test("bitCount.05") {
    val input = "def r: Int32 = Int64/bitCount(-9223372036854775808i64)"
    runTest(input, 1)
  }

  test("bitCount.06") {
    val input = "def r: Int32 = Int64/bitCount(-9223372036854775805i64)"
    runTest(input, 3)
  }

  test("bitCount.07") {
    val input = "def r: Int32 = Int64/bitCount(9223372036854775807i64)"
    runTest(input, 63)
  }

  test("bitCount.08") {
    val input = "def r: Int32 = Int64/bitCount(9223372036854775806i64)"
    runTest(input, 62)
  }

  test("bitCount.09") {
    val input = "def r: Int32 = Int64/bitCount(9223372036854775791i64)"
    runTest(input, 62)
  }

  test("rotateLeft.01") {
    val input = "def r: Int64 = Int64/rotateLeft(-99991001265258663i64, 7)"
    runTest(input, 5647895911756442879L)
  }

  test("rotateLeft.02") {
    val input = "def r: Int64 = Int64/rotateLeft(-983667729064i64, 24)"
    runTest(input, 1943538110990123007L)
  }

  test("rotateLeft.03") {
    val input = "def r: Int64 = Int64/rotateLeft(-983667729064i64, -104)"
    runTest(input, 1943538110990123007L)
  }

  test("rotateLeft.04") {
    val input = "def r: Int64 = Int64/rotateLeft(1478293001i64, 23)"
    runTest(input, 12400820494532608L)
  }

  test("rotateLeft.05") {
    val input = "def r: Int64 = Int64/rotateLeft(1478293001i64, -41)"
    runTest(input, 12400820494532608L)
  }

  test("rotateLeft.06") {
    val input = "def r: Int64 = Int64/rotateLeft(1478293001i64, 87)"
    runTest(input, 12400820494532608L)
  }

  test("rotateLeft.07") {
    val input = "def r: Int64 = Int64/rotateLeft(-1i64, -36678)"
    runTest(input, -1L)
  }

  test("rotateLeft.08") {
    val input = "def r: Int64 = Int64/rotateLeft(-1i64, 667)"
    runTest(input, -1L)
  }

  test("rotateLeft.09") {
    val input = "def r: Int64 = Int64/rotateLeft(-1i64, -11111)"
    runTest(input, -1L)
  }

  test("rotateLeft.10") {
    val input = "def r: Int64 = Int64/rotateLeft(-9223372036854775808i64, 3)"
    runTest(input, 4L)
  }

  test("rotateLeft.11") {
    val input = "def r: Int64 = Int64/rotateLeft(9223372036854775807i64, 2)"
    runTest(input, -3L)
  }

  test("rotateRight.01") {
    val input = "def r: Int64 = Int64/rotateRight(-99991001265258663i64, 57)"
    runTest(input, 5647895911756442879L)
  }

  test("rotateRight.02") {
    val input = "def r: Int64 = Int64/rotateRight(-983667729064i64, 40)"
    runTest(input, 1943538110990123007L)
  }

  test("rotateRight.03") {
    val input = "def r: Int64 = Int64/rotateRight(-983667729064i64, -88)"
    runTest(input, 1943538110990123007L)
  }

  test("rotateRight.04") {
    val input = "def r: Int64 = Int64/rotateRight(1478293001i64, 41)"
    runTest(input, 12400820494532608L)
  }

  test("rotateRight.05") {
    val input = "def r: Int64 = Int64/rotateRight(1478293001i64, -23)"
    runTest(input, 12400820494532608L)
  }

  test("rotateRight.06") {
    val input = "def r: Int64 = Int64/rotateRight(1478293001i64, 105)"
    runTest(input, 12400820494532608L)
  }

  test("rotateRight.07") {
    val input = "def r: Int64 = Int64/rotateRight(-1i64, -36678)"
    runTest(input, -1L)
  }

  test("rotateRight.08") {
    val input = "def r: Int64 = Int64/rotateRight(-1i64, 667)"
    runTest(input, -1L)
  }

  test("rotateRight.09") {
    val input = "def r: Int64 = Int64/rotateRight(-1i64, -11111)"
    runTest(input, -1L)
  }

  test("rotateRight.10") {
    val input = "def r: Int64 = Int64/rotateRight(-9223372036854775808i64, 3)"
    runTest(input, 1152921504606846976L)
  }

  test("rotateRight.11") {
    val input = "def r: Int64 = Int64/rotateRight(9223372036854775807i64, 2)"
    runTest(input, -2305843009213693953L)
  }

  test("highestOneBitPosition.01") {
    val input = "def r: Int32 = Int64/highestOneBitPosition(-1i64)"
    runTest(input, 63)
  }

  test("highestOneBitPosition.02") {
    val input = "def r: Int32 = Int64/highestOneBitPosition(-9223372036854775808i64)"
    runTest(input, 63)
  }

  test("highestOneBitPosition.03") {
    val input = "def r: Int32 = Int64/highestOneBitPosition(1i64)"
    runTest(input, 0)
  }

  test("highestOneBitPosition.04") {
    val input = "def r: Int32 = Int64/highestOneBitPosition(2i64)"
    runTest(input, 1)
  }

  test("highestOneBitPosition.05") {
    val input = "def r: Int32 = Int64/highestOneBitPosition(3i64)"
    runTest(input, 1)
  }

  test("highestOneBitPosition.06") {
    val input = "def r: Int32 = Int64/highestOneBitPosition(20i64)"
    runTest(input, 4)
  }

  test("highestOneBitPosition.07") {
    val input = "def r: Int32 = Int64/highestOneBitPosition(2i64 ** 30i64)"
    runTest(input, 30)
  }

  test("highestOneBitPosition.08") {
    val input = "def r: Int32 = Int64/highestOneBitPosition(9223372036854775807i64)"
    runTest(input, 62)
  }

  test("highestOneBitPosition.09") {
    val input = "def r: Int32 = Int64/highestOneBitPosition(0i64)"
    runTest(input, -1)
  }

  test("highestOneBitPosition.10") {
    val input = "def r: Int32 = Int64/highestOneBitPosition(2i64 ** 23i64 + 2i64 ** 60i64)"
    runTest(input, 60)
  }

  test("highestOneBitPosition.11") {
    val input = "def r: Int32 = Int64/highestOneBitPosition(2i64 ** 20i64 + 2i64 ** 59i64)"
    runTest(input, 59)
  }

  test("highestOneBitPosition.12") {
    val input = "def r: Int32 = Int64/highestOneBitPosition(-44556677i64)"
    runTest(input, 63)
  }

  test("highestOneBit.01") {
    val input = "def r: Int64 = Int64/highestOneBit(-1i64)"
    runTest(input, Long.MinValue)
  }

  test("highestOneBit.02") {
    val input = "def r: Int64 = Int64/highestOneBit(-9223372036854775808i64)"
    runTest(input, Long.MinValue)
  }

  test("highestOneBit.03") {
    val input = "def r: Int64 = Int64/highestOneBit(-778899i64)"
    runTest(input, Long.MinValue)
  }

  test("highestOneBit.04") {
    val input = "def r: Int64 = Int64/highestOneBit(2i64)"
    runTest(input, 2L)
  }

  test("highestOneBit.05") {
    val input = "def r: Int64 = Int64/highestOneBit(3i64)"
    runTest(input, 2L)
  }

  test("highestOneBit.06") {
    val input = "def r: Int64 = Int64/highestOneBit(20i64)"
    runTest(input, 16L)
  }

  test("highestOneBit.07") {
    val input = "def r: Int64 = Int64/highestOneBit(2i64 ** 57i64 + 2i64 ** 32i64)"
    runTest(input, 144115188075855872L)
  }

  test("highestOneBit.08") {
    val input = "def r: Int64 = Int64/highestOneBit(9223372036854775807i64)"
    runTest(input, 4611686018427387904L)
  }

  test("highestOneBit.09") {
    val input = "def r: Int64 = Int64/highestOneBit(0i64)"
    runTest(input, 0L)
  }

  test("highestOneBit.10") {
    val input = "def r: Int64 = Int64/highestOneBit(2i64 ** 44i64 + 2i64 ** 18i64)"
    runTest(input, 17592186044416L)
  }

  test("numberOfLeadingZeros.01") {
    val input = "def r: Int32 = Int64/numberOfLeadingZeros(0i64)"
    runTest(input, 64)
  }

  test("numberOfLeadingZeros.02") {
    val input = "def r: Int32 = Int64/numberOfLeadingZeros(-9223372036854775808i64)"
    runTest(input, 0)
  }

  test("numberOfLeadingZeros.03") {
    val input = "def r: Int32 = Int64/numberOfLeadingZeros(-1i64)"
    runTest(input, 0)
  }

  test("numberOfLeadingZeros.04") {
    val input = "def r: Int32 = Int64/numberOfLeadingZeros(9223372036854775807i64)"
    runTest(input, 1)
  }

  test("numberOfLeadingZeros.05") {
    val input = "def r: Int32 = Int64/numberOfLeadingZeros(2i64 ** 62i64)"
    runTest(input, 1)
  }

  test("numberOfLeadingZeros.06") {
    val input = "def r: Int32 = Int64/numberOfLeadingZeros(2i64 ** 61i64 + 2i64 ** 18i64)"
    runTest(input, 2)
  }

  test("numberOfLeadingZeros.07") {
    val input = "def r: Int32 = Int64/numberOfLeadingZeros(2i64 ** 20i64 + 2i64 ** 18i64)"
    runTest(input, 43)
  }

  test("numberOfLeadingZeros.08") {
    val input = "def r: Int32 = Int64/numberOfLeadingZeros(20i64)"
    runTest(input, 59)
  }

  test("numberOfLeadingZeros.09") {
    val input = "def r: Int32 = Int64/numberOfLeadingZeros(1i64)"
    runTest(input, 63)
  }

  test("lowestOneBitPosition.01") {
    val input = "def r: Int32 = Int64/lowestOneBitPosition(-1i64)"
    runTest(input, 0)
  }

  test("lowestOneBitPosition.02") {
    val input = "def r: Int32 = Int64/lowestOneBitPosition(-9223372036854775808i64)"
    runTest(input, 63)
  }

  test("lowestOneBitPosition.03") {
    val input = "def r: Int32 = Int64/lowestOneBitPosition(1i64)"
    runTest(input, 0)
  }

  test("lowestOneBitPosition.04") {
    val input = "def r: Int32 = Int64/lowestOneBitPosition(2i64)"
    runTest(input, 1)
  }

  test("lowestOneBitPosition.05") {
    val input = "def r: Int32 = Int64/lowestOneBitPosition(3i64)"
    runTest(input, 0)
  }

  test("lowestOneBitPosition.06") {
    val input = "def r: Int32 = Int64/lowestOneBitPosition(4i64)"
    runTest(input, 2)
  }

  test("lowestOneBitPosition.07") {
    val input = "def r: Int32 = Int64/lowestOneBitPosition(12i64)"
    runTest(input, 2)
  }

  test("lowestOneBitPosition.08") {
    val input = "def r: Int32 = Int64/lowestOneBitPosition(2i64 ** 30i64)"
    runTest(input, 30)
  }

  test("lowestOneBitPosition.09") {
    val input = "def r: Int32 = Int64/lowestOneBitPosition(2i64 ** 57i64 + 2i64 ** 18i64)"
    runTest(input, 18)
  }

  test("lowestOneBitPosition.10") {
    val input = "def r: Int32 = Int64/lowestOneBitPosition(9223372036854775807i64)"
    runTest(input, 0)
  }

  test("lowestOneBitPosition.11") {
    val input = "def r: Int32 = Int64/lowestOneBitPosition(0i64)"
    runTest(input, -1)
  }

  test("lowestOneBit.01") {
    val input = "def r: Int64 = Int64/lowestOneBit(-1i64)"
    runTest(input, 1L)
  }

  test("lowestOneBit.02") {
    val input = "def r: Int64 = Int64/lowestOneBit(-9223372036854775808i64)"
    runTest(input, Long.MinValue)
  }

  test("lowestOneBit.03") {
    val input = "def r: Int64 = Int64/lowestOneBit(1i64)"
    runTest(input, 1L)
  }

  test("lowestOneBit.04") {
    val input = "def r: Int64 = Int64/lowestOneBit(2i64)"
    runTest(input, 2L)
  }

  test("lowestOneBit.05") {
    val input = "def r: Int64 = Int64/lowestOneBit(6i64)"
    runTest(input, 2L)
  }

  test("lowestOneBit.06") {
    val input = "def r: Int64 = Int64/lowestOneBit(12i64)"
    runTest(input, 4L)
  }

  test("lowestOneBit.07") {
    val input = "def r: Int64 = Int64/lowestOneBit(2i64 ** 6i64 + 2i64 ** 18i64 + 2i64 ** 27i64)"
    runTest(input, 64L)
  }

  test("lowestOneBit.08") {
    val input = "def r: Int64 = Int64/lowestOneBit(2i64 ** 54i64)"
    runTest(input, 18014398509481984L)
  }

  test("lowestOneBit.09") {
    val input = "def r: Int64 = Int64/lowestOneBit(2i64 ** 62i64 + 2i64 ** 18i64)"
    runTest(input, 262144L)
  }

  test("lowestOneBit.10") {
    val input = "def r: Int64 = Int64/lowestOneBit(9223372036854775807i64)"
    runTest(input, 1L)
  }

  test("lowestOneBit.11") {
    val input = "def r: Int64 = Int64/lowestOneBit(0i64)"
    runTest(input, 0L)
  }

  test("numberOfTrailingZeros.01") {
    val input = "def r: Int32 = Int64/numberOfTrailingZeros(0i64)"
    runTest(input, 64)
  }

  test("numberOfTrailingZeros.02") {
    val input = "def r: Int32 = Int64/numberOfTrailingZeros(3i64)"
    runTest(input, 0)
  }

  test("numberOfTrailingZeros.03") {
    val input = "def r: Int32 = Int64/numberOfTrailingZeros(42i64)"
    runTest(input, 1)
  }

  test("numberOfTrailingZeros.04") {
    val input = "def r: Int32 = Int64/numberOfTrailingZeros(2i64 ** 16i64 + 2i64 ** 22i64)"
    runTest(input, 16)
  }

  test("numberOfTrailingZeros.05") {
    val input = "def r: Int32 = Int64/numberOfTrailingZeros(2i64 ** 57i64)"
    runTest(input, 57)
  }

  test("numberOfTrailingZeros.06") {
    val input = "def r: Int32 = Int64/numberOfTrailingZeros(-1i64)"
    runTest(input, 0)
  }

  test("numberOfTrailingZeros.07") {
    val input = "def r: Int32 = Int64/numberOfTrailingZeros(-9223372036854775808i64)"
    runTest(input, 63)
  }

  test("bit.01") {
    val input = "def r: Int32 = Int64/bit(-1i64, 5)"
    runTest(input, 1)
  }

  test("bit.02") {
    val input = "def r: Int32 = Int64/bit(-1000000i64, 31)"
    runTest(input, 1)
  }

  test("bit.03") {
    val input = "def r: Int32 = Int64/bit(2i64 ** 23i64, 23)"
    runTest(input, 1)
  }

  test("bit.04") {
    val input = "def r: Int32 = Int64/bit(2i64 ** 23i64, 22)"
    runTest(input, 0)
  }

  test("bit.05") {
    val input = "def r: Int32 = Int64/bit(2i64 ** 23i64, 24)"
    runTest(input, 0)
  }

  test("bit.06") {
    val input = "def r: Int32 = Int64/bit(-1i64 - 2i64 ** 61i64, 61)"
    runTest(input, 0)
  }

  test("bit.07") {
    val input = "def r: Int32 = Int64/bit(-1i64 - 2i64 ** 61i64, 60)"
    runTest(input, 1)
  }

  test("bit.08") {
    val input = "def r: Int32 = Int64/bit(0i64, 17)"
    runTest(input, 0)
  }
}