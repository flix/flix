package ca.uwaterloo.flix.library

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

/**
  * Created by liampalmer on 2016-09-18.
  */
class TestInt32 extends FunSuite {

  val options = Options.DefaultTest

  def runTest(input: String, output: Int) {
    val flix = new Flix().setOptions(options).addPath("main/src/library/Int32.flix").addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  test("maxValue.01") {
    val input =
      """def r: Int32 = Int32/maxValue()
        |
      """.stripMargin
    runTest(input, Int.MaxValue)
  }

  test("minValue.01") {
    val input =
      """def r: Int32 = Int32/minValue()
        |
      """.stripMargin
    runTest(input, Int.MinValue)
  }

  test("size.01") {
    val input =
      """def r: Int32 = Int32/size()
        |
      """.stripMargin
    runTest(input, 32)
  }

  test("min.01") {
    val input =
      """def r: Int32 = Int32/min(1, 2)
        |
      """.stripMargin
    runTest(input, 1)
  }

  test("min.02") {
    val input =
      """def r: Int32 = Int32/min(2, -1)
        |
      """.stripMargin
    runTest(input, -1)
  }

  test("min.03") {
    val input =
      """def r: Int32 = Int32/min(-33, -66)
        |
      """.stripMargin
    runTest(input, -66)
  }

  test("max.01") {
    val input =
      """def r: Int32 = Int32/max(48, 49)
        |
      """.stripMargin
    runTest(input, 49)
  }

  test("max.02") {
    val input =
      """def r: Int32 = Int32/max(4, -16)
        |
      """.stripMargin
    runTest(input, 4)
  }

  test("max.03") {
    val input =
      """def r: Int32 = Int32/max(-34, -16)
        |
      """.stripMargin
    runTest(input, -16)
  }

  test("abs.01") {
    val input =
      """def r: Int32 = Int32/abs(31)
        |
      """.stripMargin
    runTest(input, 31)
  }

  test("abs.02") {
    val input =
      """def r: Int32 = Int32/abs(-31)
        |
      """.stripMargin
    runTest(input, 31)
  }

  test("abs.03") {
    val input =
      """def r: Int32 = Int32/abs(0)
        |
      """.stripMargin
    runTest(input, 0)
  }

  test("dist.01") {
    val input =
      """def r: Int32 = Int32/dist(31, -7)
        |
      """.stripMargin
    runTest(input, 38)
  }

  test("dist.02") {
    val input =
      """def r: Int32 = Int32/dist(-44, -1)
        |
      """.stripMargin
    runTest(input, 43)
  }

  test("dist.03") {
    val input =
      """def r: Int32 = Int32/dist(-2, -2)
        |
      """.stripMargin
    runTest(input, 0)
  }

  test("compare.01") {
    val input =
      """def r: Int32 = Int32/compare(-1, 44)
        |
      """.stripMargin
    runTest(input, -1)
  }

  test("compare.02") {
    val input =
      """def r: Int32 = Int32/compare(-1, -44)
        |
      """.stripMargin
    runTest(input, 1)
  }

  test("compare.03") {
    val input =
      """def r: Int32 = Int32/compare(88, 88)
        |
      """.stripMargin
    runTest(input, 0)
  }

  test("signum.01") {
    val input =
      """def r: Int32 = Int32/signum(-22)
        |
      """.stripMargin
    runTest(input, -1)
  }

  test("signum.02") {
    val input =
      """def r: Int32 = Int32/signum(22)
        |
      """.stripMargin
    runTest(input, 1)
  }

  test("signum.03") {
    val input =
      """def r: Int32 = Int32/signum(0)
        |
      """.stripMargin
    runTest(input, 0)
  }

  test("logicalRightShift.01") {
    val input =
      """def r: Int32 = Int32/logicalRightShift(2233, 4)
        |
      """.stripMargin
    runTest(input, 2233 >>> 4)
  }

  test("logicalRightShift.02") {
    val input =
      """def r: Int32 = Int32/logicalRightShift(223366, 33)
        |
      """.stripMargin
    runTest(input, 223366 >>> 33)
  }

  test("logicalRightShift.03") {
    val input =
      """def r: Int32 = Int32/logicalRightShift(2233, -8)
        |
      """.stripMargin
    runTest(input, 2233 >>> -8)
  }

  test("logicalRightShift.04") {
    val input =
      """def r: Int32 = Int32/logicalRightShift(2233, 4)
        |
      """.stripMargin
    runTest(input, 2233 >>> 4)
  }

  test("logicalRightShift.05") {
    val input =
      """def r: Int32 = Int32/logicalRightShift(-1, 1)
        |
      """.stripMargin
    runTest(input, -1 >>> 1)
  }

  test("logicalRightShift.06") {
    val input =
      """def r: Int32 = Int32/logicalRightShift(-35, 4)
        |
      """.stripMargin
    runTest(input, -35 >>> 4)
  }

  test("logicalRightShift.07") {
    val input =
      """def r: Int32 = Int32/logicalRightShift(-2777, -2777)
        |
      """.stripMargin
    runTest(input, -2777 >>> -2777)
  }

  test("logicalRightShift.08") {
    val input =
      """def r: Int32 = Int32/logicalRightShift(-233, 64)
        |
      """.stripMargin
    runTest(input, -233 >>> 64)
  }

  test("logicalRightShift.09") {
    val input =
      """def r: Int32 = Int32/logicalRightShift(-27, 0)
        |
      """.stripMargin
    runTest(input, -27 >>> 0)
  }

  test("logicalRightShift.10") {
    val input =
      """def r: Int32 = Int32/logicalRightShift(2147483647, 1)
        |
      """.stripMargin
    runTest(input, 2147483647 >>> 1)
  }

  test("bitCount.01") {
    val input =
      """def r: Int32 = Int32/bitCount(19909)
        |
      """.stripMargin
    runTest(input, Integer.bitCount(19909))
  }

  test("bitCount.02") {
    val input =
      """def r: Int32 = Int32/bitCount(-1)
        |
      """.stripMargin
    runTest(input, Integer.bitCount(-1))
  }

  test("bitCount.03") {
    val input =
      """def r: Int32 = Int32/bitCount(-999888888)
        |
      """.stripMargin
    runTest(input, Integer.bitCount(-999888888))
  }

  test("bitCount.04") {
    val input =
      """def r: Int32 = Int32/bitCount(0)
        |
      """.stripMargin
    runTest(input, Integer.bitCount(0))
  }

  test("bitCount.05") {
    val input =
      """def r: Int32 = Int32/bitCount(2147483647)
        |
      """.stripMargin
    runTest(input, Integer.bitCount(2147483647))
  }

  test("bitCount.06") {
    val input =
      """def r: Int32 = Int32/bitCount(-2147483648)
        |
      """.stripMargin
    runTest(input, Integer.bitCount(-2147483648))
  }

  test("rotateLeft.01") {
    val input =
      """def r: Int32 = Int32/rotateLeft(-1698265258, 4)
        |
      """.stripMargin
    runTest(input, Integer.rotateLeft(-1698265258, 4))
  }

  test("rotateLeft.02") {
    val input =
      """def r: Int32 = Int32/rotateLeft(-1836677290, 5)
        |
      """.stripMargin
    runTest(input, Integer.rotateLeft(-1836677290, 5))
  }

  test("rotateLeft.03") {
    val input =
      """def r: Int32 = Int32/rotateLeft(1478293001, 24)
        |
      """.stripMargin
    runTest(input, Integer.rotateLeft(1478293001, 24))
  }

  test("rotateLeft.04") {
    val input =
      """def r: Int32 = Int32/rotateLeft(-12267711, -36)
        |
      """.stripMargin
    runTest(input, Integer.rotateLeft(-12267711, -36))
  }

  test("rotateRight.01") {
    val input =
      """def r: Int32 = Int32/rotateRight(-1698265258, 4)
        |
      """.stripMargin
    runTest(input, Integer.rotateRight(-1698265258, 4))
  }

  test("rotateRight.02") {
    val input =
      """def r: Int32 = Int32/rotateRight(-1836677290, 5)
        |
      """.stripMargin
    runTest(input, Integer.rotateRight(-1836677290, 5))
  }

  test("rotateRight.03") {
    val input =
      """def r: Int32 = Int32/rotateRight(1478293001, 24)
        |
      """.stripMargin
    runTest(input, Integer.rotateRight(1478293001, 24))
  }

  test("rotateRight.04") {
    val input =
      """def r: Int32 = Int32/rotateRight(-12267711, -36)
        |
      """.stripMargin
    runTest(input, Integer.rotateRight(-12267711, -36))
  }
}