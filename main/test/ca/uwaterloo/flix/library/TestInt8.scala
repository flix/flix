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

class TestInt8 extends FunSuite {

  val options = Options.DefaultTest

  def runTest(input: String, output: Byte) {
    val flix = new Flix().setOptions(options).addPath("main/src/library/Int8.flix").addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  test("maxValue.01") {
    val input = "def r: Int8 = Int8.maxValue()"
    runTest(input, Byte.MaxValue)
  }

  test("minValue.01") {
    val input = "def r: Int8 = Int8.minValue()"
    runTest(input, Byte.MinValue)
  }

  test("size.01") {
    val input = "def r: Int32 = Int8.size()"
    runTest(input, 8:Byte)
  }

  test("min.01") {
    val input = "def r: Int8 = Int8.min(1i8, 2i8)"
    runTest(input, 1:Byte)
  }

  test("min.02") {
    val input = "def r: Int8 = Int8.min(2i8, -1i8)"
    runTest(input, -1:Byte)
  }

  test("min.03") {
    val input = "def r: Int8 = Int8.min(-33i8, -66i8)"
    runTest(input, -66:Byte)
  }

  test("max.01") {
    val input = "def r: Int8 = Int8.max(48i8, 49i8)"
    runTest(input, 49:Byte)
  }

  test("max.02") {
    val input = "def r: Int8 = Int8.max(4i8, -16i8)"
    runTest(input, 4:Byte)
  }

  test("max.03") {
    val input = "def r: Int8 = Int8.max(-34i8, -16i8)"
    runTest(input, -16:Byte)
  }

  test("abs.01") {
    val input = "def r: Int8 = Int8.abs(31i8)"
    runTest(input, 31:Byte)
  }

  test("abs.02") {
    val input = "def r: Int8 = Int8.abs(-31i8)"
    runTest(input, 31:Byte)
  }

  test("abs.03") {
    val input = "def r: Int8 = Int8.abs(0i8)"
    runTest(input, 0:Byte)
  }

  test("abs.04") {
    val input = "def r: Int8 = Int8.abs(127i8)"
    runTest(input, Byte.MaxValue)
  }

  test("abs.05") {
    val input = "def r: Int8 = Int8.abs(-127i8)"
    runTest(input, Byte.MaxValue)
  }

  test("abs.06") {
    val input = "def r: Int8 = Int8.abs(-128i8)"
    runTest(input, -1:Byte)
  }

  test("dist.01") {
    val input = "def r: Int8 = Int8.dist(31i8, -7i8)"
    runTest(input, 38:Byte)
  }

  test("dist.02") {
    val input = "def r: Int8 = Int8.dist(-44i8, -1i8)"
    runTest(input, 43:Byte)
  }

  test("dist.03") {
    val input = "def r: Int8 = Int8.dist(-2i8, -2i8)"
    runTest(input, 0:Byte)
  }

  test("dist.04") {
    val input = "def r: Int8 = Int8.dist(-128i8, -1i8)"
    runTest(input, Byte.MaxValue)
  }

  test("dist.05") {
    val input = "def r: Int8 = Int8.dist(-128i8, 0i8)"
    runTest(input, -1:Byte)
  }

  test("dist.06") {
    val input = "def r: Int8 = Int8.dist(-123i8, 4i8)"
    runTest(input, Byte.MaxValue)
  }

  test("dist.07") {
    val input = "def r: Int8 = Int8.dist(-123i8, 5i8)"
    runTest(input, -1:Byte)
  }

  test("dist.08") {
    val input = "def r: Int8 = Int8.dist(-26i8, 101i8)"
    runTest(input, Byte.MaxValue)
  }

  test("dist.09") {
    val input = "def r: Int8 = Int8.dist(-26i8, 102i8)"
    runTest(input, -1:Byte)
  }

  test("dist.10") {
    val input = "def r: Int8 = Int8.dist(127i8, 0i8)"
    runTest(input, Byte.MaxValue)
  }

  test("dist.11") {
    val input = "def r: Int8 = Int8.dist(127i8, 1i8)"
    runTest(input, 126:Byte)
  }

  test("compare.01") {
    val input = "def r: Int32 = Int8.compare(-1i8, 44i8)"
    runTest(input, -1:Byte)
  }

  test("compare.02") {
    val input = "def r: Int32 = Int8.compare(-1i8, -44i8)"
    runTest(input, 1:Byte)
  }

  test("compare.03") {
    val input = "def r: Int32 = Int8.compare(88i8, 88i8)"
    runTest(input, 0:Byte)
  }

  test("signum.01") {
    val input = "def r: Int32 = Int8.signum(-22i8)"
    runTest(input, -1:Byte)
  }

  test("signum.02") {
    val input = "def r: Int32 = Int8.signum(22i8)"
    runTest(input, 1:Byte)
  }

  test("signum.03") {
    val input = "def r: Int32 = Int8.signum(0i8)"
    runTest(input, 0:Byte)
  }

  test("highestOneBitPosition.01") {
    val input = "def r: Int32 = Int8.highestOneBitPosition(-1i8)"
    runTest(input, 7:Byte)
  }

  test("highestOneBitPosition.02") {
    val input = "def r: Int32 = Int8.highestOneBitPosition(-128i8)"
    runTest(input, 7:Byte)
  }

  test("highestOneBitPosition.03") {
    val input = "def r: Int32 = Int8.highestOneBitPosition(1i8)"
    runTest(input, 0:Byte)
  }

  test("highestOneBitPosition.04") {
    val input = "def r: Int32 = Int8.highestOneBitPosition(2i8)"
    runTest(input, 1:Byte)
  }

  test("highestOneBitPosition.05") {
    val input = "def r: Int32 = Int8.highestOneBitPosition(3i8)"
    runTest(input, 1:Byte)
  }

  test("highestOneBitPosition.06") {
    val input = "def r: Int32 = Int8.highestOneBitPosition(20i8)"
    runTest(input, 4:Byte)
  }

  test("highestOneBitPosition.07") {
    val input = "def r: Int32 = Int8.highestOneBitPosition(2i8 ** 6i8)"
    runTest(input, 6:Byte)
  }

  test("highestOneBitPosition.08") {
    val input = "def r: Int32 = Int8.highestOneBitPosition(127i8)"
    runTest(input, 6:Byte)
  }

  test("highestOneBitPosition.09") {
    val input = "def r: Int32 = Int8.highestOneBitPosition(0i8)"
    runTest(input, -1:Byte)
  }

  test("highestOneBitPosition.10") {
    val input = "def r: Int32 = Int8.highestOneBitPosition(2i8 ** 3i8 + 2i8 ** 5i8)"
    runTest(input, 5:Byte)
  }

  test("highestOneBitPosition.11") {
    val input = "def r: Int32 = Int8.highestOneBitPosition(2i8 ** 3i8 + 2i8 ** 4i8)"
    runTest(input, 4:Byte)
  }

  test("highestOneBitPosition.12") {
    val input = "def r: Int32 = Int8.highestOneBitPosition(-10i8)"
    runTest(input, 7:Byte)
  }

  test("highestOneBit.01") {
    val input = "def r: Int8 = Int8.highestOneBit(-1i8)"
    runTest(input, Byte.MinValue)
  }

  test("highestOneBit.02") {
    val input = "def r: Int8 = Int8.highestOneBit(-128i8)"
    runTest(input, Byte.MinValue)
  }

  test("highestOneBit.03") {
    val input = "def r: Int8 = Int8.highestOneBit(-77i8)"
    runTest(input, Byte.MinValue)
  }

  test("highestOneBit.04") {
    val input = "def r: Int8 = Int8.highestOneBit(2i8)"
    runTest(input, 2:Byte)
  }

  test("highestOneBit.05") {
    val input = "def r: Int8 = Int8.highestOneBit(3i8)"
    runTest(input, 2:Byte)
  }

  test("highestOneBit.06") {
    val input = "def r: Int8 = Int8.highestOneBit(20i8)"
    runTest(input, 16:Byte)
  }

  test("highestOneBit.07") {
    val input = "def r: Int8 = Int8.highestOneBit(2i8 ** 2i8 + 2i8 ** 5i8)"
    runTest(input, 32:Byte)
  }

  test("highestOneBit.08") {
    val input = "def r: Int8 = Int8.highestOneBit(127i8)"
    runTest(input, 64:Byte)
  }

  test("highestOneBit.09") {
    val input = "def r: Int8 = Int8.highestOneBit(0i8)"
    runTest(input, 0:Byte)
  }

  test("highestOneBit.10") {
    val input = "def r: Int8 = Int8.highestOneBit(2i8 ** 5i8 + 2i8 ** 6i8)"
    runTest(input, 64:Byte)
  }

  test("numberOfLeadingZeros.01") {
    val input = "def r: Int32 = Int8.numberOfLeadingZeros(0i8)"
    runTest(input, 8:Byte)
  }

  test("numberOfLeadingZeros.02") {
    val input = "def r: Int32 = Int8.numberOfLeadingZeros(-128i8)"
    runTest(input, 0:Byte)
  }

  test("numberOfLeadingZeros.03") {
    val input = "def r: Int32 = Int8.numberOfLeadingZeros(-1i8)"
    runTest(input, 0:Byte)
  }

  test("numberOfLeadingZeros.04") {
    val input = "def r: Int32 = Int8.numberOfLeadingZeros(127i8)"
    runTest(input, 1:Byte)
  }

  test("numberOfLeadingZeros.05") {
    val input = "def r: Int32 = Int8.numberOfLeadingZeros(2i8 ** 6i8)"
    runTest(input, 1:Byte)
  }

  test("numberOfLeadingZeros.06") {
    val input = "def r: Int32 = Int8.numberOfLeadingZeros(2i8 ** 3i8 + 2i8 ** 6i8)"
    runTest(input, 1:Byte)
  }

  test("numberOfLeadingZeros.07") {
    val input = "def r: Int32 = Int8.numberOfLeadingZeros(2i8 ** 4i8 + 2i8 ** 5i8)"
    runTest(input, 2:Byte)
  }

  test("numberOfLeadingZeros.08") {
    val input = "def r: Int32 = Int8.numberOfLeadingZeros(20i8)"
    runTest(input, 3:Byte)
  }

  test("numberOfLeadingZeros.09") {
    val input = "def r: Int32 = Int8.numberOfLeadingZeros(1i8)"
    runTest(input, 7:Byte)
  }

  test("lowestOneBitPosition.01") {
    val input = "def r: Int32 = Int8.lowestOneBitPosition(-1i8)"
    runTest(input, 0:Byte)
  }

  test("lowestOneBitPosition.02") {
    val input = "def r: Int32 = Int8.lowestOneBitPosition(-128i8)"
    runTest(input, 7:Byte)
  }

  test("lowestOneBitPosition.03") {
    val input = "def r: Int32 = Int8.lowestOneBitPosition(1i8)"
    runTest(input, 0:Byte)
  }

  test("lowestOneBitPosition.04") {
    val input = "def r: Int32 = Int8.lowestOneBitPosition(2i8)"
    runTest(input, 1:Byte)
  }

  test("lowestOneBitPosition.05") {
    val input = "def r: Int32 = Int8.lowestOneBitPosition(3i8)"
    runTest(input, 0:Byte)
  }

  test("lowestOneBitPosition.06") {
    val input = "def r: Int32 = Int8.lowestOneBitPosition(4i8)"
    runTest(input, 2:Byte)
  }

  test("lowestOneBitPosition.07") {
    val input = "def r: Int32 = Int8.lowestOneBitPosition(12i8)"
    runTest(input, 2:Byte)
  }

  test("lowestOneBitPosition.08") {
    val input = "def r: Int32 = Int8.lowestOneBitPosition(2i8 ** 4i8)"
    runTest(input, 4:Byte)
  }

  test("lowestOneBitPosition.09") {
    val input = "def r: Int32 = Int8.lowestOneBitPosition(2i8 ** 4i8 + 2i8 ** 6i8)"
    runTest(input, 4:Byte)
  }

  test("lowestOneBitPosition.10") {
    val input = "def r: Int32 = Int8.lowestOneBitPosition(127i8)"
    runTest(input, 0:Byte)
  }

  test("lowestOneBitPosition.11") {
    val input = "def r: Int32 = Int8.lowestOneBitPosition(0i8)"
    runTest(input, -1:Byte)
  }

  test("lowestOneBit.01") {
    val input = "def r: Int8 = Int8.lowestOneBit(-1i8)"
    runTest(input, 1:Byte)
  }

  test("lowestOneBit.02") {
    val input = "def r: Int8 = Int8.lowestOneBit(-128i8)"
    runTest(input, Byte.MinValue)
  }

  test("lowestOneBit.03") {
    val input = "def r: Int8 = Int8.lowestOneBit(1i8)"
    runTest(input, 1:Byte)
  }

  test("lowestOneBit.04") {
    val input = "def r: Int8 = Int8.lowestOneBit(2i8)"
    runTest(input, 2:Byte)
  }

  test("lowestOneBit.05") {
    val input = "def r: Int8 = Int8.lowestOneBit(6i8)"
    runTest(input, 2:Byte)
  }

  test("lowestOneBit.06") {
    val input = "def r: Int8 = Int8.lowestOneBit(12i8)"
    runTest(input, 4:Byte)
  }

  test("lowestOneBit.07") {
    val input = "def r: Int8 = Int8.lowestOneBit(2i8 ** 6i8)"
    runTest(input, 64:Byte)
  }

  test("lowestOneBit.08") {
    val input = "def r: Int8 = Int8.lowestOneBit(2i8 ** 5i8)"
    runTest(input, 32:Byte)
  }

  test("lowestOneBit.09") {
    val input = "def r: Int8 = Int8.lowestOneBit(2i8 ** 3i8 + 2i8 ** 5i8)"
    runTest(input, 8:Byte)
  }

  test("lowestOneBit.10") {
    val input = "def r: Int8 = Int8.lowestOneBit(127i8)"
    runTest(input, 1:Byte)
  }

  test("lowestOneBit.11") {
    val input = "def r: Int8 = Int8.lowestOneBit(0i8)"
    runTest(input, 0:Byte)
  }

  test("numberOfTrailingZeros.01") {
    val input = "def r: Int32 = Int8.numberOfTrailingZeros(0i8)"
    runTest(input, 8:Byte)
  }

  test("numberOfTrailingZeros.02") {
    val input = "def r: Int32 = Int8.numberOfTrailingZeros(3i8)"
    runTest(input, 0:Byte)
  }

  test("numberOfTrailingZeros.03") {
    val input = "def r: Int32 = Int8.numberOfTrailingZeros(42i8)"
    runTest(input, 1:Byte)
  }

  test("numberOfTrailingZeros.04") {
    val input = "def r: Int32 = Int8.numberOfTrailingZeros(2i8 ** 4i8 + 2i8 ** 6i8)"
    runTest(input, 4:Byte)
  }

  test("numberOfTrailingZeros.05") {
    val input = "def r: Int32 = Int8.numberOfTrailingZeros(2i8 ** 6i8)"
    runTest(input, 6:Byte)
  }

  test("numberOfTrailingZeros.06") {
    val input = "def r: Int32 = Int8.numberOfTrailingZeros(-1i8)"
    runTest(input, 0:Byte)
  }

  test("numberOfTrailingZeros.07") {
    val input = "def r: Int32 = Int8.numberOfTrailingZeros(-128i8)"
    runTest(input, 7:Byte)
  }

  test("bit.01") {
    val input = "def r: Int32 = Int8.bit(-1i8, 5)"
    runTest(input, 1:Byte)
  }

  test("bit.02") {
    val input = "def r: Int32 = Int8.bit(-100i8, 7)"
    runTest(input, 1:Byte)
  }

  test("bit.03") {
    val input = "def r: Int32 = Int8.bit(2i8 ** 5i8, 5)"
    runTest(input, 1:Byte)
  }

  test("bit.04") {
    val input = "def r: Int32 = Int8.bit(2i8 ** 5i8, 6)"
    runTest(input, 0:Byte)
  }

  test("bit.05") {
    val input = "def r: Int32 = Int8.bit(2i8 ** 5i8, 4)"
    runTest(input, 0:Byte)
  }

  test("bit.06") {
    val input = "def r: Int32 = Int8.bit(-1i8 - 2i8 ** 4i8, 4)"
    runTest(input, 0:Byte)
  }

  test("bit.07") {
    val input = "def r: Int32 = Int8.bit(-1i8 - 2i8 ** 4i8, 5)"
    runTest(input, 1:Byte)
  }

  test("bit.08") {
    val input = "def r: Int32 = Int8.bit(0i8, 7)"
    runTest(input, 0:Byte)
  }
}