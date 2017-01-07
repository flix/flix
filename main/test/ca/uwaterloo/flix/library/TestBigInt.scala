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
import ca.uwaterloo.flix.util.{Evaluation, Options}
import org.scalatest.FunSuite

class TestBigInt extends FunSuite {

  val options = Options.DefaultTest.copy(evaluation=Evaluation.Interpreted)

  def runTest(input: String, output: AnyRef) {
    val flix = new Flix().setOptions(options).addPath("main/src/library/BigInt.flix").addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  def runIntTest(input: String, output: Int) {
    val flix = new Flix().setOptions(options).addPath("main/src/library/BigInt.flix").addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  test("min.01") {
    val input = "def r: BigInt = BigInt/min(1234ii, 12345678939393939393939393932ii)"
    runTest(input, Value.mkBigInt(1234))
  }

  test("min.02") {
    val input = "def r: BigInt = BigInt/min(22737461919238461234728137461283412ii, -191283756ii)"
    runTest(input, Value.mkBigInt(-191283756))
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
    val input = "def r: BigInt = BigInt/max(4ii, -169879871293847921347918234912394789123ii)"
    runTest(input, Value.mkBigInt(4))
  }

  test("max.03") {
    val input = "def r: BigInt = BigInt/max(-3498457932459234592873452983453245ii, -16ii)"
    runTest(input, Value.mkBigInt(-16))
  }

  test("max.04") {
    val input = "def r: BigInt = BigInt/max(-34998734957235ii, 9223372036854775807ii)"
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
    val input = "def r: Int32 = BigInt/signum(-92233720368547758089ii)"
    runIntTest(input, -1)
  }

  test("gcd.01") {
    val input = "def r: BigInt = BigInt/gcd(9ii, 3ii)"
    runTest(input, Value.mkBigInt(3))
  }

  test("gcd.02") {
    val input = "def r: BigInt = BigInt/gcd(21ii, 15ii)"
    runTest(input, Value.mkBigInt(3))
  }

  test("gcd.03") {
    val input = "def r: BigInt = BigInt/gcd(9233ii, 0ii)"
    runTest(input, Value.mkBigInt(9233))
  }

  test("gcd.04") {
    val input = "def r: BigInt = BigInt/gcd(0ii, 9233ii)"
    runTest(input, Value.mkBigInt(9233))
  }

  test("gcd.05") {
    val input = "def r: BigInt = BigInt/gcd(9223ii, 33ii)"
    runTest(input, Value.mkBigInt(1))
  }

  test("gcd.06") {
    val input = "def r: BigInt = BigInt/gcd(32462531054272512000001ii, 578837438475345ii)"
    runTest(input, Value.mkBigInt(1))
  }

  test("gcd.07") {
    val input = "def r: BigInt = BigInt/gcd(32462531054272512000001ii, 578837982572398457234545ii)"
    runTest(input, Value.mkBigInt(1))
  }

  test("gcd.08") {
    val input = "def r: BigInt = BigInt/gcd(32462531054272512000001ii, 92371823432134ii)"
    runTest(input, Value.mkBigInt(1))
  }

  test("gcd.09") {
    val input = "def r: BigInt = BigInt/gcd(-9ii, -3ii)"
    runTest(input, Value.mkBigInt(3))
  }

  test("gcd.10") {
    val input = "def r: BigInt = BigInt/gcd(21ii, -15ii)"
    runTest(input, Value.mkBigInt(3))
  }

  test("gcd.11") {
    val input = "def r: BigInt = BigInt/gcd(-9233ii, 0ii)"
    runTest(input, Value.mkBigInt(9233))
  }

  test("gcd.12") {
    val input = "def r: BigInt = BigInt/gcd(0ii, -9233ii)"
    runTest(input, Value.mkBigInt(9233))
  }

  test("gcd.13") {
    val input = "def r: BigInt = BigInt/gcd(9223ii, -33ii)"
    runTest(input, Value.mkBigInt(1))
  }

  test("gcd.14") {
    val input = "def r: BigInt = BigInt/gcd(-32462531054272512000001ii, 578837438475345ii)"
    runTest(input, Value.mkBigInt(1))
  }

  test("gcd.15") {
    val input = "def r: BigInt = BigInt/gcd(-32462531054272512000001ii, 578837982572398457234545ii)"
    runTest(input, Value.mkBigInt(1))
  }

  test("gcd.16") {
    val input = "def r: BigInt = BigInt/gcd(32462531054272512000001ii, -92371823432134ii)"
    runTest(input, Value.mkBigInt(1))
  }

  test("bit.01") {
    val input = "def r: Int32 = BigInt/bit(1ii, 0)"
    runIntTest(input, 1)
  }

  test("bit.02") {
    val input = "def r: Int32 = BigInt/bit(1ii, 1)"
    runIntTest(input, 0)
  }

  test("bit.03") {
    val input = "def r: Int32 = BigInt/bit(8388608ii, 23)"
    runIntTest(input, 1)
  }

  test("bit.04") {
    val input = "def r: Int32 = BigInt/bit(8388608ii, 22)"
    runIntTest(input, 0)
  }

  test("bit.05") {
    val input = "def r: Int32 = BigInt/bit(8388608ii, 24)"
    runIntTest(input, 0)
  }

  test("bit.06") {
    val input = "def r: Int32 = BigInt/bit(9223372036854775808ii, 63)"
    runIntTest(input, 1)
  }

  test("bit.07") {
    val input = "def r: Int32 = BigInt/bit(9223372036854775808ii, 62)"
    runIntTest(input, 0)
  }

  test("bit.08") {
    val input = "def r: Int32 = BigInt/bit(9223372036854775808ii, 64)"
    runIntTest(input, 0)
  }

  test("bit.09") {
    val input = "def r: Int32 = BigInt/bit(154742504910672534362390528ii, 87)"
    runIntTest(input, 1)
  }

  test("bit.10") {
    val input = "def r: Int32 = BigInt/bit(154742504910672534362390528ii, 86)"
    runIntTest(input, 0)
  }

  test("bit.11") {
    val input = "def r: Int32 = BigInt/bit(154742504910672534362390528ii, 88)"
    runIntTest(input, 0)
  }

  test("bit.12") {
    val input = "def r: Int32 = BigInt/bit(0ii, 7)"
    runIntTest(input, 0)
  }

  test("bit.13") {
    val input = "def r: Int32 = BigInt/bit(-1ii, 17)"
    runIntTest(input, 1)
  }

  test("bit.14") {
    val input = "def r: Int32 = BigInt/bit(-17ii, 4)"
    runIntTest(input, 0)
  }

  test("bit.15") {
    val input = "def r: Int32 = BigInt/bit(-17ii, 3)"
    runIntTest(input, 1)
  }

  test("bit.16") {
    val input = "def r: Int32 = BigInt/bit(-17ii, 5)"
    runIntTest(input, 1)
  }

  test("bit.17") {
    val input = "def r: Int32 = BigInt/bit(-898274123413412341ii, 4)"
    runIntTest(input, 0)
  }

  test("bit.18") {
    val input = "def r: Int32 = BigInt/bit(-898274123413412341ii, 3)"
    runIntTest(input, 1)
  }

  test("bit.19") {
    val input = "def r: Int32 = BigInt/bit(-898274123413412341ii, 35)"
    runIntTest(input, 0)
  }

  test("bit.20") {
    val input = "def r: Int32 = BigInt/bit(-898274123413412341ii, 36)"
    runIntTest(input, 1)
  }

  test("bit.21") {
    val input = "def r: Int32 = BigInt/bit(-898274123413412341ii, 37)"
    runIntTest(input, 1)
  }

  test("bit.22") {
    val input = "def r: Int32 = BigInt/bit(-8982741234134123419879712341ii, 92)"
    runIntTest(input, 0)
  }

  test("bit.23") {
    val input = "def r: Int32 = BigInt/bit(-8982741234134123419879712341ii, 93)"
    runIntTest(input, 1)
  }

  test("bit.24") {
    val input = "def r: Int32 = BigInt/bit(-8982741234134123419879712341ii, 94)"
    runIntTest(input, 1)
  }

  test("bit.25") {
    val input = "def r: Int32 = BigInt/bit(-8982741234134123419879712341ii, 95)"
    runIntTest(input, 1)
  }

  test("bit.26") {
    val input = "def r: Int32 = BigInt/bit(-8982741234134123419879712341ii, 96)"
    runIntTest(input, 1)
  }

  test("setBit.01") {
    val input = "def r: BigInt = BigInt/setBit(0ii, 0)"
    runTest(input, Value.mkBigInt(1))
  }

  test("setBit.02") {
    val input = "def r: BigInt = BigInt/setBit(0ii, 1)"
    runTest(input, Value.mkBigInt(2))
  }

  test("setBit.03") {
    val input = "def r: BigInt = BigInt/setBit(0ii, 31)"
    runTest(input, Value.mkBigInt(2147483648L))
  }

  test("setBit.04") {
    val input = "def r: BigInt = BigInt/setBit(0ii, 32)"
    runTest(input, Value.mkBigInt(4294967296L))
  }

  test("setBit.05") {
    val input = "def r: BigInt = BigInt/setBit(2305843009213693952ii, 55)"
    runTest(input, Value.mkBigInt(2341871806232657920L))
  }

  test("setBit.06") {
    val input = "def r: BigInt = BigInt/setBit(2305843009213693952ii, 62)"
    runTest(input, Value.mkBigInt(6917529027641081856L))
  }

  test("setBit.07") {
    val input = "def r: BigInt = BigInt/setBit(-17ii, 4)"
    runTest(input, Value.mkBigInt(-1))
  }

  test("setBit.08") {
    val input = "def r: BigInt = BigInt/setBit(-1025ii, 10)"
    runTest(input, Value.mkBigInt(-1))
  }

  test("setBit.09") {
    val input = "def r: BigInt = BigInt/setBit(-17ii, 31)"
    runTest(input, Value.mkBigInt(-17))
  }

  test("setBit.10") {
    val input = "def r: BigInt = BigInt/setBit(-17ii, 32)"
    runTest(input, Value.mkBigInt(-17))
  }

  test("setBit.11") {
    val input = "def r: BigInt = BigInt/setBit(-17ii, 101)"
    runTest(input, Value.mkBigInt(-17))
  }

  test("clearBit.01") {
    val input = "def r: BigInt = BigInt/clearBit(1ii, 0)"
    runTest(input, Value.mkBigInt(0))
  }

  test("clearBit.02") {
    val input = "def r: BigInt = BigInt/clearBit(1ii, 1)"
    runTest(input, Value.mkBigInt(1))
  }

  test("clearBit.03") {
    val input = "def r: BigInt = BigInt/clearBit(2ii, 1)"
    runTest(input, Value.mkBigInt(0))
  }

  test("clearBit.04") {
    val input = "def r: BigInt = BigInt/clearBit(2ii, 2)"
    runTest(input, Value.mkBigInt(2))
  }

  test("clearBit.05") {
    val input = "def r: BigInt = BigInt/clearBit(0ii, 31)"
    runTest(input, Value.mkBigInt(0))
  }

  test("clearBit.06") {
    val input = "def r: BigInt = BigInt/clearBit(0ii, 32)"
    runTest(input, Value.mkBigInt(0))
  }

  test("clearBit.07") {
    val input = "def r: BigInt = BigInt/clearBit(2147483648ii, 31)"
    runTest(input, Value.mkBigInt(0))
  }

  test("clearBit.08") {
    val input = "def r: BigInt = BigInt/clearBit(618970019642690137449562112ii, 89)"
    runTest(input, Value.mkBigInt(0))
  }

  test("clearBit.09") {
    val input = "def r: BigInt = BigInt/clearBit(-1ii, 0)"
    runTest(input, Value.mkBigInt(-2))
  }

  test("clearBit.10") {
    val input = "def r: BigInt = BigInt/clearBit(-1ii, 1)"
    runTest(input, Value.mkBigInt(-3))
  }

  test("clearBit.11") {
    val input = "def r: BigInt = BigInt/clearBit(-1ii, 5)"
    runTest(input, Value.mkBigInt(-33))
  }

  test("clearBit.12") {
    val input = "def r: BigInt = BigInt/clearBit(-1ii, 31)"
    runTest(input, Value.mkBigInt(-2147483649L))
  }

  test("clearBit.13") {
    val input = "def r: BigInt = BigInt/clearBit(-1ii, 32)"
    runTest(input, Value.mkBigInt(-4294967297L))
  }

  test("flipBit.01") {
    val input = "def r: BigInt = BigInt/flipBit(1ii, 0)"
    runTest(input, Value.mkBigInt(0))
  }

  test("flipBit.02") {
    val input = "def r: BigInt = BigInt/flipBit(1ii, 1)"
    runTest(input, Value.mkBigInt(3))
  }

  test("flipBit.03") {
    val input = "def r: BigInt = BigInt/flipBit(2ii, 1)"
    runTest(input, Value.mkBigInt(0))
  }

  test("flipBit.04") {
    val input = "def r: BigInt = BigInt/flipBit(2ii, 2)"
    runTest(input, Value.mkBigInt(6))
  }

  test("flipBit.05") {
    val input = "def r: BigInt = BigInt/flipBit(0ii, 31)"
    runTest(input, Value.mkBigInt(2147483648L))
  }

  test("flipBit.06") {
    val input = "def r: BigInt = BigInt/flipBit(0ii, 32)"
    runTest(input, Value.mkBigInt(4294967296L))
  }

  test("flipBit.07") {
    val input = "def r: BigInt = BigInt/flipBit(2147483648ii, 31)"
    runTest(input, Value.mkBigInt(0))
  }

  test("flipBit.08") {
    val input = "def r: BigInt = BigInt/flipBit(618970019642690137449562112ii, 89)"
    runTest(input, Value.mkBigInt(0))
  }

  test("flipBit.09") {
    val input = "def r: BigInt = BigInt/flipBit(-1ii, 0)"
    runTest(input, Value.mkBigInt(-2))
  }

  test("flipBit.10") {
    val input = "def r: BigInt = BigInt/flipBit(-1ii, 1)"
    runTest(input, Value.mkBigInt(-3))
  }

  test("flipBit.11") {
    val input = "def r: BigInt = BigInt/flipBit(-1ii, 5)"
    runTest(input, Value.mkBigInt(-33))
  }

  test("flipBit.12") {
    val input = "def r: BigInt = BigInt/flipBit(-1ii, 31)"
    runTest(input, Value.mkBigInt(-2147483649L))
  }

  test("flipBit.13") {
    val input = "def r: BigInt = BigInt/flipBit(-1ii, 32)"
    runTest(input, Value.mkBigInt(-4294967297L))
  }
}