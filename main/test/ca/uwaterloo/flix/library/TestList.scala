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

import java.lang.Boolean

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.runtime.Value
import ca.uwaterloo.flix.util.{Evaluation, Options}
import org.scalatest.FunSuite

class TestList extends FunSuite {

  val options = Options.DefaultTest.copy(core = false)

  def runTest(input: String, output: Int) {
    val flix = new Flix().setOptions(options).addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  def runBoolTest(input: String, output: Boolean) {
    val flix = new Flix().setOptions(options).addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  def runAnyTest(input: String, output: AnyRef) {
    val flix = new Flix().setOptions(options).addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  def mkList(xs: List[Int]): AnyRef = Value.mkList(xs.map(x => new Integer(x)))
  def mkBoolList(xs: List[Boolean]): AnyRef = Value.mkList(xs.map(x => new Boolean(x)))
  def mkBigIntList(xs: List[Int]): AnyRef = Value.mkList(xs.map(x => Value.mkBigInt(x)))
  def mkAnyList(xs: List[AnyRef]): AnyRef = Value.mkList(xs)
  def mkPermutationList(xs: List[Int]): AnyRef = Value.mkList(xs.permutations.toList.map(x => mkList(x)))
  def mkNil: AnyRef = Value.mkNil
  def mkNone: AnyRef = Value.mkNone()
  def mkSome(x: Int): AnyRef = Value.mkSome(new Integer(x))
  def mkSet(xs: List[Int]): AnyRef = Value.mkFlixSet(xs.map(x => new Integer(x)))
  def mkTuple(x: Int, y: Int): AnyRef = Array(new Integer(x), new Integer(y))
  def mkAnyTuple(x: AnyRef, y: AnyRef): AnyRef = Array(x, y)

  test("isEmpty.01") {
    val input = "def r: Bool = List.isEmpty(Nil)"
    runBoolTest(input, true)
  }

  test("isEmpty.02") {
    val input = "def r: Bool = List.isEmpty(1 :: Nil)"
    runBoolTest(input, false)
  }

  test("isEmpty.03") {
    val input = "def r: Bool = List.isEmpty(1:: 2 :: Nil)"
    runBoolTest(input, false)
  }

  test("headOpt.01") {
    val input = "def r: Option[Int32] = List.headOpt(Nil)"
    runAnyTest(input, mkNone)
  }

  test("headOpt.02") {
    val input = "def r: Option[Int32] = List.headOpt(1 :: Nil)"
    runAnyTest(input, mkSome(1))
  }

  test("headOpt.03") {
    val input = "def r: Option[Int32] = List.headOpt(2 :: 1 :: Nil)"
    runAnyTest(input, mkSome(2))
  }

  test("headOpt.04") {
    val input = "def r: Option[Int32] = List.headOpt(3 :: 2 :: 1 :: Nil)"
    runAnyTest(input, mkSome(3))
  }

  test("lastOpt.01") {
    val input = "def r: Option[Int32] = List.lastOpt(Nil)"
    runAnyTest(input, mkNone)
  }

  test("lastOpt.02") {
    val input = "def r: Option[Int32] = List.lastOpt(1 :: Nil)"
    runAnyTest(input, mkSome(1))
  }

  test("lastOpt.03") {
    val input = "def r: Option[Int32] = List.lastOpt(1 :: 2 :: Nil)"
    runAnyTest(input, mkSome(2))
  }

  test("lastOpt.04") {
    val input = "def r: Option[Int32] = List.lastOpt(1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkSome(3))
  }

  test("length.01") {
    val input = "def r: Int32 = List.length(Nil)"
    runTest(input, 0)
  }

  test("length.02") {
    val input = "def r: Int32 = List.length(1 :: Nil)"
    runTest(input, 1)
  }

  test("length.03") {
    val input = "def r: Int32 = List.length(1 :: 2 :: Nil)"
    runTest(input, 2)
  }

  test("length.04") {
    val input = "def r: Int32 = List.length(1 :: 2 :: 3 :: Nil)"
    runTest(input, 3)
  }

  test("append.01") {
    val input = "def r: List[Int32] = List.append(Nil, Nil)"
    runAnyTest(input, mkNil)
  }

  test("append.02") {
    val input = "def r: List[Int32] = List.append(Nil, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("append.03") {
    val input = "def r: List[Int32] = List.append(Nil, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("append.04") {
    val input = "def r: List[Int32] = List.append(1 :: 2 :: Nil, Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("append.05") {
    val input = "def r: List[Int32] = List.append(1 :: Nil, 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("append.06") {
    val input = "def r: List[Int32] = List.append(1 :: 2 :: Nil, 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3)))
  }

  test("append.07") {
    val input = "def r: List[Int32] = List.append(1 :: Nil, 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3)))
  }

  test("at.01") {
    val input = "def r: Int32 = List.at(0, 1 :: 2 :: 3 :: Nil)"
    runTest(input, 1)
  }

  test("at.02") {
    val input = "def r: Int32 = List.at(1, 1 :: 2 :: 3 :: Nil)"
    runTest(input, 2)
  }

  test("at.03") {
    val input = "def r: Int32 = List.at(2, 1 :: 2 :: 3 :: Nil)"
    runTest(input, 3)
  }

  test("remove.01") {
    val input = "def r: List[Int32] = List.remove(1, Nil)"
    runAnyTest(input, mkNil)
  }

  test("remove.02") {
    val input = "def r: List[Int32] = List.remove(1, 1 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("remove.03") {
    val input = "def r: List[Int32] = List.remove(2, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("remove.04") {
    val input = "def r: List[Int32] = List.remove(1, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(2)))
  }

  test("remove.05") {
    val input = "def r: List[Int32] = List.remove(2, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("remove.06") {
    val input = "def r: List[Int32] = List.remove(3, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("remove.07") {
    val input = "def r: List[Int32] = List.remove(1, 1 :: 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("remove.08") {
    val input = "def r: List[Int32] = List.remove(1, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(2, 3)))
  }

  test("remove.09") {
    val input = "def r: List[Int32] = List.remove(2, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 3)))
  }

  test("remove.10") {
    val input = "def r: List[Int32] = List.remove(3, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("remove.11") {
    val input = "def r: List[Int32] = List.remove(4, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3)))
  }

  test("remove.12") {
    val input = "def r: List[Int32] = List.remove(2, 1 :: 2 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("remove.13") {
    val input = "def r: List[Int32] = List.remove(2, 2 :: 2 :: 1 :: Nil)"
    runAnyTest(input, mkList(List(2, 1)))
  }

  test("remove.14") {
    val input = "def r: List[Int32] = List.remove(2, 2 :: 2 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(2, 2)))
  }

  test("removeIndex.01") {
    val input = "def r: List[Int32] = List.removeIndex(0, 1 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("removeIndex.02") {
    val input = "def r: List[Int32] = List.removeIndex(0, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(2)))
  }

  test("removeIndex.03") {
    val input = "def r: List[Int32] = List.removeIndex(1, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("removeIndex.04") {
    val input = "def r: List[Int32] = List.removeIndex(0, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(2, 3)))
  }

  test("removeIndex.05") {
    val input = "def r: List[Int32] = List.removeIndex(1, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 3)))
  }

  test("removeIndex.06") {
    val input = "def r: List[Int32] = List.removeIndex(2, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("removeIndex.07") {
    val input = "def r: List[Int32] = List.removeIndex(1, Nil)"
    runAnyTest(input, mkNil)
  }

  test("removeIndex.08") {
    val input = "def r: List[Int32] = List.removeIndex(-1, Nil)"
    runAnyTest(input, mkNil)
  }

  test("removeIndex.09") {
    val input = "def r: List[Int32] = List.removeIndex(2, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("removeIndex.10") {
    val input = "def r: List[Int32] = List.removeIndex(-1, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("memberOf.01") {
    val input = "def r: Bool = List.memberOf(0, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, false)
  }

  test("memberOf.02") {
    val input = "def r: Bool = List.memberOf(1, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, true)
  }

  test("memberOf.03") {
    val input = "def r: Bool = List.memberOf(2, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, true)
  }

  test("memberOf.04") {
    val input = "def r: Bool = List.memberOf(3, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, true)
  }

  test("memberOf.05") {
    val input = "def r: Bool = List.memberOf(4, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, false)
  }

  test("indexOf.01") {
    val input = "def r: Int32 = List.indexOf(1, Nil)"
    runTest(input, -1)
  }

  test("indexOf.02") {
    val input = "def r: Int32 = List.indexOf(1, 2 :: Nil)"
    runTest(input, -1)
  }

  test("indexOf.03") {
    val input = "def r: Int32 = List.indexOf(1, 2 :: 3 :: Nil)"
    runTest(input, -1)
  }

  test("indexOf.04") {
    val input = "def r: Int32 = List.indexOf(1, 2 :: 3 :: 4 :: Nil)"
    runTest(input, -1)
  }

  test("indexOf.05") {
    val input = "def r: Int32 = List.indexOf(1, 1 :: Nil)"
    runTest(input, 0)
  }

  test("indexOf.06") {
    val input = "def r: Int32 = List.indexOf(1, 1 :: 2 :: Nil)"
    runTest(input, 0)
  }

  test("indexOf.07") {
    val input = "def r: Int32 = List.indexOf(1, 2 :: 1 :: Nil)"
    runTest(input, 1)
  }

  test("indexOf.08") {
    val input = "def r: Int32 = List.indexOf(1, 1 :: 2 :: 3 :: Nil)"
    runTest(input, 0)
  }

  test("indexOf.09") {
    val input = "def r: Int32 = List.indexOf(2, 1 :: 2 :: 3 :: Nil)"
    runTest(input, 1)
  }

  test("indexOf.10") {
    val input = "def r: Int32 = List.indexOf(3, 1 :: 2 :: 3 :: Nil)"
    runTest(input, 2)
  }

  test("find.01") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = List.find(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNone)
  }

  test("find.02") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = List.find(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNone)
  }

  test("find.03") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = List.find(f, 1 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(2))
  }

  test("findLeft.01") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = List.findLeft(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNone)
  }

  test("findLeft.02") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = List.findLeft(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNone)
  }

  test("findLeft.03") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = List.findLeft(f, 1 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(2))
  }

  test("findRight.01") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = List.findRight(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNone)
  }

  test("findRight.02") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = List.findRight(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNone)
  }

  test("findRight.03") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = List.findRight(f, 1 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(2))
  }

  test("range.01") {
    val input = "def r: List[Int32] = List.range(4, 4)"
    runAnyTest(input, mkNil)
  }

  test("range.02") {
    val input = "def r: List[Int32] = List.range(4, 5)"
    runAnyTest(input, mkList(List(4)))
  }

  test("range.03") {
    val input = "def r: List[Int32] = List.range(4, 6)"
    runAnyTest(input, mkList(List(4, 5)))
  }

  test("range.04") {
    val input = "def r: List[Int32] = List.range(4, 7)"
    runAnyTest(input, mkList(List(4, 5, 6)))
  }

  test("repeat.01") {
    val input = "def r: List[Int32] = List.repeat(4, 0)"
    runAnyTest(input, mkNil)
  }

  test("repeat.02") {
    val input = "def r: List[Int32] = List.repeat(4, 1)"
    runAnyTest(input, mkList(List(4)))
  }

  test("repeat.03") {
    val input = "def r: List[Int32] = List.repeat(4, 2)"
    runAnyTest(input, mkList(List(4, 4)))
  }

  test("repeat.04") {
    val input = "def r: List[Int32] = List.repeat(4, 3)"
    runAnyTest(input, mkList(List(4, 4, 4)))
  }

  test("repeat.05") {
    val input = "def r: List[BigInt] = List.repeat(4ii, 0)"
    runAnyTest(input, mkNil)
  }

  test("repeat.06") {
    val input = "def r: List[BigInt] = List.repeat(4ii, 1)"
    runAnyTest(input, mkBigIntList(List(4)))
  }

  test("repeat.07") {
    val input = "def r: List[BigInt] = List.repeat(4ii, 2)"
    runAnyTest(input, mkBigIntList(List(4, 4)))
  }

  test("repeat.08") {
    val input = "def r: List[BigInt] = List.repeat(4ii, 3)"
    runAnyTest(input, mkBigIntList(List(4, 4, 4)))
  }

  test("scan.01") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List.scan(f, 1, Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("scan.02") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List.scan(f, 1, false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 3)))
  }

  test("scan.03") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List.scan(f, 1, false :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 3, 5)))
  }

  test("scan.04") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List.scan(f, 1, true :: false :: true :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 2, 4, 5)))
  }

  test("scan.05") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List.scan(f, 1, true :: false :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 2, 4, 6)))
  }

  test("scan.06") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List.scan(f, 1, Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("scan.07") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List.scan(f, 1, 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 4)))
  }

  test("scan.08") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List.scan(f, 1, 3 :: 8 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 4, 12)))
  }

  test("scan.09") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List.scan(f, 1, 3 :: 8 :: 15 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 4, 12, 27)))
  }

  test("scanLeft.01") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List.scanLeft(f, 1, Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("scanLeft.02") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List.scanLeft(f, 1, false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 3)))
  }

  test("scanLeft.03") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List.scanLeft(f, 1, false :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 3, 5)))
  }

  test("scanLeft.04") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List.scanLeft(f, 1, true :: false :: true :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 2, 4, 5)))
  }

  test("scanLeft.05") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List.scanLeft(f, 1, true :: false :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 2, 4, 6)))
  }

  test("scanLeft.06") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List.scanLeft(f, 1, Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("scanLeft.07") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List.scanLeft(f, 1, 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 4)))
  }

  test("scanLeft.08") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List.scanLeft(f, 1, 3 :: 8 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 4, 12)))
  }

  test("scanLeft.09") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List.scanLeft(f, 1, 3 :: 8 :: 15 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 4, 12, 27)))
  }

  test("scanRight.01") {
    val input =
      """def f(b: Bool, i: Int32): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List.scanRight(f, 1, Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("scanRight.02") {
    val input =
      """def f(b: Bool, i: Int32): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List.scanRight(f, 1, false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(3, 1)))
  }

  test("scanRight.03") {
    val input =
      """def f(b: Bool, i: Int32): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List.scanRight(f, 1, false :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(5, 3, 1)))
  }

  test("scanRight.04") {
    val input =
      """def f(b: Bool, i: Int32): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List.scanRight(f, 1, true :: false :: true :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(5, 4, 2, 1)))
  }

  test("scanRight.05") {
    val input =
      """def f(b: Bool, i: Int32): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List.scanRight(f, 1, true :: false :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(6, 5, 3, 1)))
  }

  test("scanRight.06") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List.scanRight(f, 1, Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("scanRight.07") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List.scanRight(f, 1, 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(4, 1)))
  }

  test("scanRight.08") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List.scanRight(f, 1, 3 :: 8 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(12, 9, 1)))
  }

  test("scanRight.09") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List.scanRight(f, 1, 3 :: 8 :: 15 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(27, 24, 16, 1)))
  }

  test("map.01") {
    val input =
      """def f(i: Int32): Bool = i == 2
        |def r: List[Bool] = List.map(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("map.02") {
    val input =
      """def f(i: Int32): Bool = i == 2
        |def r: List[Bool] = List.map(f, 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkBoolList(List(true)))
  }

  test("map.03") {
    val input =
      """def f(i: Int32): Bool = i == 2
        |def r: List[Bool] = List.map(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkBoolList(List(false)))
  }

  test("map.04") {
    val input =
      """def f(i: Int32): Bool = i == 2
        |def r: List[Bool] = List.map(f, 1 :: 2 :: 3 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkBoolList(List(false, true, false, true)))
  }

  test("map.05") {
    val input =
      """def f(i: Int32): Bool = i == 2
        |def r: List[Bool] = List.map(f, 2 :: 1 :: 2 :: 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkBoolList(List(true, false, true, false)))
  }

  test("mapWithIndex.01") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List.mapWithIndex(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("mapWithIndex.02") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List.mapWithIndex(f, 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2)))
  }

  test("mapWithIndex.03") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List.mapWithIndex(f, 5 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(5, 3)))
  }

  test("mapWithIndex.04") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List.mapWithIndex(f, 11 :: 5 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(11, 6, 4)))
  }

  test("flatMap.01") {
    val input =
      """def f(i: Int32): List[Int32] = List.repeat(i, i)
        |def r: List[Int32] = List.flatMap(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("flatMap.02") {
    val input =
      """def f(i: Int32): List[Int32] = List.repeat(i, i)
        |def r: List[Int32] = List.flatMap(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("flatMap.03") {
    val input =
      """def f(i: Int32): List[Int32] = List.repeat(i, i)
        |def r: List[Int32] = List.flatMap(f, 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(3, 3, 3)))
  }

  test("flatMap.04") {
    val input =
      """def f(i: Int32): List[Int32] = List.repeat(i, i)
        |def r: List[Int32] = List.flatMap(f, 2 :: 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2, 2, 1)))
  }

  test("flatMap.05") {
    val input =
      """def f(i: Int32): List[Int32] = List.repeat(i, i)
        |def r: List[Int32] = List.flatMap(f, 2 :: 1 :: 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2, 2, 1, 3, 3, 3)))
  }

  test("reverse.01") {
    val input = "def r: List[Int32] = List.reverse(Nil)"
    runAnyTest(input, mkNil)
  }

  test("reverse.02") {
    val input = "def r: List[Int32] = List.reverse(1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("reverse.03") {
    val input = "def r: List[Int32] = List.reverse(1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(2, 1)))
  }

  test("reverse.04") {
    val input = "def r: List[Int32] = List.reverse(1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(3, 2, 1)))
  }

  test("reverse.05") {
    val input = "def r: List[Bool] = List.reverse(Nil)"
    runAnyTest(input, mkNil)
  }

  test("reverse.06") {
    val input = "def r: List[Bool] = List.reverse(true :: Nil)"
    runAnyTest(input, mkBoolList(List(true)))
  }

  test("reverse.07") {
    val input = "def r: List[Bool] = List.reverse(true :: false :: Nil)"
    runAnyTest(input, mkBoolList(List(false, true)))
  }

  test("reverse.08") {
    val input = "def r: List[Bool] = List.reverse(true :: false :: false :: Nil)"
    runAnyTest(input, mkBoolList(List(false, false, true)))
  }

  test("rotateLeft.01") {
    val input = "def r: List[Int32] = List.rotateLeft(0, Nil)"
    runAnyTest(input, mkNil)
  }

  test("rotateLeft.02") {
    val input = "def r: List[Int32] = List.rotateLeft(0, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("rotateLeft.03") {
    val input = "def r: List[Int32] = List.rotateLeft(1, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("rotateLeft.04") {
    val input = "def r: List[Int32] = List.rotateLeft(0, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("rotateLeft.05") {
    val input = "def r: List[Int32] = List.rotateLeft(1, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(2, 1)))
  }

  test("rotateLeft.06") {
    val input = "def r: List[Int32] = List.rotateLeft(2, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("rotateLeft.07") {
    val input = "def r: List[Int32] = List.rotateLeft(0, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3)))
  }

  test("rotateLeft.08") {
    val input = "def r: List[Int32] = List.rotateLeft(1, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(2, 3, 1)))
  }

  test("rotateLeft.09") {
    val input = "def r: List[Int32] = List.rotateLeft(2, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(3, 1, 2)))
  }

  test("rotateLeft.10") {
    val input = "def r: List[Int32] = List.rotateLeft(3, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3)))
  }

  test("rotateRight.01") {
    val input = "def r: List[Int32] = List.rotateRight(0, Nil)"
    runAnyTest(input, mkNil)
  }

  test("rotateRight.02") {
    val input = "def r: List[Int32] = List.rotateRight(1, Nil)"
    runAnyTest(input, mkNil)
  }

  test("rotateRight.03") {
    val input = "def r: List[Int32] = List.rotateRight(10, Nil)"
    runAnyTest(input, mkNil)
  }

  test("rotateRight.04") {
    val input = "def r: List[Int32] = List.rotateRight(-5, Nil)"
    runAnyTest(input, mkNil)
  }

  test("rotateRight.05") {
    val input = "def r: List[Int32] = List.rotateRight(0, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("rotateRight.06") {
    val input = "def r: List[Int32] = List.rotateRight(1, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("rotateRight.07") {
    val input = "def r: List[Int32] = List.rotateRight(6, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("rotateRight.08") {
    val input = "def r: List[Int32] = List.rotateRight(-9, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("rotateRight.09") {
    val input = "def r: List[Int32] = List.rotateRight(0, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("rotateRight.10") {
    val input = "def r: List[Int32] = List.rotateRight(1, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(2, 1)))
  }

  test("rotateRight.11") {
    val input = "def r: List[Int32] = List.rotateRight(2, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("rotateRight.12") {
    val input = "def r: List[Int32] = List.rotateRight(33, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(2, 1)))
  }

  test("rotateRight.13") {
    val input = "def r: List[Int32] = List.rotateRight(68, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("rotateRight.14") {
    val input = "def r: List[Int32] = List.rotateRight(-87, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(2, 1)))
  }

  test("rotateRight.15") {
    val input = "def r: List[Int32] = List.rotateRight(-100, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("rotateRight.16") {
    val input = "def r: List[Int32] = List.rotateRight(0, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3)))
  }

  test("rotateRight.17") {
    val input = "def r: List[Int32] = List.rotateRight(1, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(3, 1, 2)))
  }

  test("rotateRight.18") {
    val input = "def r: List[Int32] = List.rotateRight(2, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(2, 3, 1)))
  }

  test("rotateRight.19") {
    val input = "def r: List[Int32] = List.rotateRight(3, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3)))
  }

  test("rotateRight.20") {
    val input = "def r: List[Int32] = List.rotateRight(40, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(3, 1, 2)))
  }

  test("rotateRight.21") {
    val input = "def r: List[Int32] = List.rotateRight(50, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(2, 3, 1)))
  }

  test("rotateRight.22") {
    val input = "def r: List[Int32] = List.rotateRight(60, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3)))
  }

  test("rotateRight.23") {
    val input = "def r: List[Int32] = List.rotateRight(-49, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(2, 3, 1)))
  }

  test("rotateRight.24") {
    val input = "def r: List[Int32] = List.rotateRight(-50, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(3, 1, 2)))
  }

  test("rotateRight.25") {
    val input = "def r: List[Int32] = List.rotateRight(-60, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3)))
  }

  test("rotateRight.26") {
    val input = "def r: List[Int32] = List.rotateRight(-1, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(2, 3, 1)))
  }

  test("replace.01") {
    val input = "def r: List[Int32] = List.replace(0, 2, Nil)"
    runAnyTest(input, mkNil)
  }

  test("replace.02") {
    val input = "def r: List[Int32] = List.replace(0, 2, 1 :: Nil)"
    runAnyTest(input, mkList(List(2)))
  }

  test("replace.03") {
    val input = "def r: List[Int32] = List.replace(1, 2, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("replace.04") {
    val input = "def r: List[Int32] = List.replace(-1, 2, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("replace.05") {
    val input = "def r: List[Int32] = List.replace(0, 3, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(3, 2)))
  }

  test("replace.06") {
    val input = "def r: List[Int32] = List.replace(1, 3, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 3)))
  }

  test("replace.07") {
    val input = "def r: List[Int32] = List.replace(2, 3, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("replace.08") {
    val input = "def r: List[Int32] = List.replace(3, 3, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("replace.09") {
    val input = "def r: List[Int32] = List.replace(-1, 3, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("replace.10") {
    val input = "def r: List[Int32] = List.replace(-5, 3, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("replace.11") {
    val input = "def r: List[Int32] = List.replace(0, 4, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(4, 2, 3)))
  }

  test("replace.12") {
    val input = "def r: List[Int32] = List.replace(1, 4, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 4, 3)))
  }

  test("replace.13") {
    val input = "def r: List[Int32] = List.replace(2, 4, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 4)))
  }

  test("replace.14") {
    val input = "def r: List[Int32] = List.replace(3, 4, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3)))
  }

  test("patch.01") {
    val input = "def r: List[Int32] = List.patch(0, 0, Nil, Nil)"
    runAnyTest(input, mkNil)
  }

  test("patch.02") {
    val input = "def r: List[Int32] = List.patch(0, 2, 1 :: 2 :: Nil, Nil)"
    runAnyTest(input, mkNil)
  }

  test("patch.03") {
    val input = "def r: List[Int32] = List.patch(0, 2, Nil, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("patch.04") {
    val input = "def r: List[Int32] = List.patch(-3, 3, 1 :: 2 :: 4 :: Nil, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("patch.05") {
    val input = "def r: List[Int32] = List.patch(2, 3, 1 :: 2 :: 4 :: Nil, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("patch.06") {
    val input = "def r: List[Int32] = List.patch(0, 0, Nil, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("patch.07") {
    val input = "def r: List[Int32] = List.patch(1, 0, 2 :: Nil, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("patch.08") {
    val input = "def r: List[Int32] = List.patch(0, 1, 2 :: Nil, 1 :: Nil)"
    runAnyTest(input, mkList(List(2)))
  }

  test("patch.09") {
    val input = "def r: List[Int32] = List.patch(0, 2, 2 :: 4 :: Nil, 1 :: Nil)"
    runAnyTest(input, mkList(List(2)))
  }

  test("patch.10") {
    val input = "def r: List[Int32] = List.patch(-1, 2, 2 :: 4 :: Nil, 1 :: Nil)"
    runAnyTest(input, mkList(List(4)))
  }

  test("patch.11") {
    val input = "def r: List[Int32] = List.patch(-1, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(4, 2)))
  }

  test("patch.12") {
    val input = "def r: List[Int32] = List.patch(1, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 3)))
  }

  test("patch.13") {
    val input = "def r: List[Int32] = List.patch(-2, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("patch.14") {
    val input = "def r: List[Int32] = List.patch(2, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("patch.15") {
    val input = "def r: List[Int32] = List.patch(1, 1, 3 :: Nil, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 3)))
  }

  test("patch.16") {
    val input = "def r: List[Int32] = List.patch(0, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(3, 4)))
  }

  test("patch.17") {
    val input = "def r: List[Int32] = List.patch(0, 1, 4 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(4, 2, 3)))
  }

  test("patch.18") {
    val input = "def r: List[Int32] = List.patch(1, 1, 4 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 4, 3)))
  }

  test("patch.19") {
    val input = "def r: List[Int32] = List.patch(2, 1, 4 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 4)))
  }

  test("patch.20") {
    val input = "def r: List[Int32] = List.patch(0, 2, 4 :: 5 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(4, 5, 3)))
  }

  test("patch.21") {
    val input = "def r: List[Int32] = List.patch(1, 2, 4 :: 5 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 4, 5)))
  }

  test("patch.22") {
    val input = "def r: List[Int32] = List.patch(0, 2, 4 :: 5 :: 6 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(4, 5, 3)))
  }

  test("patch.23") {
    val input = "def r: List[Int32] = List.patch(0, 3, 4 :: 5 :: 6 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(4, 5, 6)))
  }

  test("patch.24") {
    val input = "def r: List[Int32] = List.patch(2, 4, 14 :: 15 :: 16 :: 17 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 14, 15, 16, 17, 7)))
  }

  test("patch.25") {
    val input = "def r: List[Int32] = List.patch(-2, 4, 14 :: 15 :: 16 :: 17 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil)"
    runAnyTest(input, mkList(List(16, 17, 3, 4, 5, 6, 7)))
  }

  test("patch.26") {
    val input = "def r: List[Int32] = List.patch(4, 5, 14 :: 15 :: 16 :: 17 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3, 4, 14, 15, 16)))
  }

  test("patch.27") {
    val input = "def r: List[Int32] = List.patch(4, 2, 14 :: 15 :: 16 :: 17 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3, 4, 14, 15, 7)))
  }

  test("patch.28") {
    val input = """def r: List[Int32] = List.patch(-1, 10, -1 :: -2 :: -3 :: -4 :: -5 :: -6 :: -7 :: -8 :: Nil,
                  |                                1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil)
                """.stripMargin
    runAnyTest(input, mkList(List(-2, -3, -4, -5, -6, -7, -8)))
  }

  test("permutations.01") {
    val input = "def r: List[List[Int32]] = List.permutations(Nil)"
    runAnyTest(input, mkPermutationList(List()))
  }

  test("permutations.02") {
    val input = "def r: List[List[Int32]] = List.permutations(1 :: Nil)"
    runAnyTest(input, mkPermutationList(List(1)))
  }

  test("permutations.03") {
    val input = "def r: List[List[Int32]] = List.permutations(1 :: 2 :: Nil)"
    runAnyTest(input, mkPermutationList(List(1, 2)))
  }

  test("permutations.04") {
    val input = "def r: List[List[Int32]] = List.permutations(1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkPermutationList(List(1, 2, 3)))
  }

  test("permutations.05") {
    val input = "def r: List[List[Int32]] = List.permutations(1 :: 2 :: 3 :: 4 :: Nil)"
    runAnyTest(input, mkPermutationList(List(1, 2, 3, 4)))
  }

  test("permutations.06") {
    val input = "def r: List[List[Int32]] = List.permutations(5 :: 4 :: 3 :: 2 :: 1 :: Nil)"
    runAnyTest(input, mkPermutationList(List(5, 4, 3, 2, 1)))
  }

  test("permutations.07") {
    val input = "def r: List[List[Int32]] = List.permutations(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)"
    runAnyTest(input, mkPermutationList(List(5, 4, 3, 2, 1, 0)))
  }

  test("subsequences.01") {
    val input = "def r: List[List[Int32]] = List.subsequences(Nil)"
    runAnyTest(input, mkAnyList(List(mkNil)))
  }

  test("subsequences.02") {
    val input = "def r: List[List[Int32]] = List.subsequences(1 :: Nil)"
    runAnyTest(input, mkAnyList(List(mkList(List(1)), mkNil)))
  }

  test("subsequences.03") {
    val input = "def r: List[List[Int32]] = List.subsequences(1 :: 2 :: Nil)"
    runAnyTest(input, mkAnyList(List(mkList(List(1, 2)), mkList(List(1)), mkList(List(2)), mkNil)))
  }

  test("subsequences.04") {
    val input = "def r: List[List[Int32]] = List.subsequences(1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkAnyList(List(mkList(List(1, 2, 3)), mkList(List(1, 2)), mkList(List(1, 3)),
                                     mkList(List(1)), mkList(List(2, 3)), mkList(List(2)),
                                     mkList(List(3)), mkNil)))
  }

  test("intersperse.01") {
    val input = "def r: List[Int32] = List.intersperse(11, Nil)"
    runAnyTest(input, mkNil)
  }

  test("intersperse.02") {
    val input = "def r: List[Int32] = List.intersperse(11, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("intersperse.03") {
    val input = "def r: List[Int32] = List.intersperse(11, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 11, 2)))
  }

  test("intersperse.04") {
    val input = "def r: List[Int32] = List.intersperse(11, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 11, 2, 11, 3)))
  }

  test("intersperse.05") {
    val input = "def r: List[Int32] = List.intersperse(11, 1 :: 2 :: 3 :: 4 :: Nil)"
    runAnyTest(input, mkList(List(1, 11, 2, 11, 3, 11, 4)))
  }

  test("intersperse.06") {
    val input = "def r: List[Int32] = List.intersperse(11, 1 :: 2 :: 3 :: 4 :: 5 :: Nil)"
    runAnyTest(input, mkList(List(1, 11, 2, 11, 3, 11, 4, 11, 5)))
  }

  test("transpose.01") {
    val input = "def r: List[List[Int32]] = List.transpose(Nil)"
    runAnyTest(input, mkNil)
  }

  test("transpose.02") {
    val input = "def r: List[List[Int32]] = List.transpose(Nil :: Nil)"
    runAnyTest(input, mkAnyList(List(mkNil)))
  }

  test("transpose.03") {
    val input = "def r: List[List[Int32]] = List.transpose(Nil :: Nil :: Nil)"
    runAnyTest(input, mkAnyList(List(mkNil, mkNil)))
  }

  test("transpose.04") {
    val input = "def r: List[List[Int32]] = List.transpose(Nil :: Nil :: Nil :: Nil)"
    runAnyTest(input, mkAnyList(List(mkNil, mkNil, mkNil)))
  }

  test("transpose.05") {
    val input = "def r: List[List[Int32]] = List.transpose((1 :: Nil) :: Nil)"
    runAnyTest(input, mkAnyList(List(mkList(List(1)))))
  }

  test("transpose.06") {
    val input = "def r: List[List[Int32]] = List.transpose((1 :: 2 :: Nil) :: Nil)"
    runAnyTest(input, mkAnyList(List(mkList(List(1)), mkList(List(2)))))
  }

  test("transpose.07") {
    val input = "def r: List[List[Int32]] = List.transpose((1 :: 2 :: 3 :: Nil) :: Nil)"
    runAnyTest(input, mkAnyList(List(mkList(List(1)), mkList(List(2)), mkList(List(3)))))
  }

  test("transpose.08") {
    val input = "def r: List[List[Int32]] = List.transpose((1 :: 2 :: 3 :: 4 :: Nil) :: Nil)"
    runAnyTest(input, mkAnyList(List(mkList(List(1)), mkList(List(2)), mkList(List(3)), mkList(List(4)))))
  }

  test("transpose.09") {
    val input = "def r: List[List[Int32]] = List.transpose((1 :: Nil) :: (2 :: Nil) :: Nil)"
    runAnyTest(input, mkAnyList(List(mkList(List(1, 2)))))
  }

  test("transpose.10") {
    val input = "def r: List[List[Int32]] = List.transpose((1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: Nil)"
    runAnyTest(input, mkAnyList(List(mkList(List(1, 2, 3)))))
  }

  test("transpose.11") {
    val input = "def r: List[List[Int32]] = List.transpose((1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: (4 :: Nil) :: Nil)"
    runAnyTest(input, mkAnyList(List(mkList(List(1, 2, 3, 4)))))
  }

  test("transpose.12") {
    val input = "def r: List[List[Int32]] = List.transpose((1 :: 2 :: Nil) :: (3 :: 4 :: Nil) :: Nil)"
    runAnyTest(input, mkAnyList(List(mkList(List(1, 3)), mkList(List(2, 4)))))
  }

  test("transpose.13") {
    val input = "def r: List[List[Int32]] = List.transpose((1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: Nil)"
    runAnyTest(input, mkAnyList(List(mkList(List(1, 4)), mkList(List(2, 5)), mkList(List(3, 6)))))
  }

  test("transpose.14") {
    val input = "def r: List[List[Int32]] = List.transpose((1 :: 2 :: 3 :: 4 :: Nil) :: (5 :: 6 :: 7 :: 8 :: Nil) :: Nil)"
    runAnyTest(input, mkAnyList(List(mkList(List(1, 5)), mkList(List(2, 6)), mkList(List(3, 7)), mkList(List(4, 8)))))
  }

  test("transpose.15") {
    val input = "def r: List[List[Int32]] = List.transpose((1 :: 2 :: 3 :: 4 :: 5 :: Nil) :: (6 :: 7 :: 8 :: 9 :: 10 :: Nil) :: Nil)"
    runAnyTest(input, mkAnyList(List(mkList(List(1, 6)), mkList(List(2, 7)), mkList(List(3, 8)), mkList(List(4, 9)),
                                     mkList(List(5, 10)))))
  }

  test("transpose.16") {
    val input = "def r: List[List[Int32]] = List.transpose((1 :: 2 :: Nil) :: (3 :: 4 :: Nil) :: (5 :: 6 :: Nil) :: Nil)"
    runAnyTest(input, mkAnyList(List(mkList(List(1, 3, 5)), mkList(List(2, 4, 6)))))
  }

  test("transpose.17") {
    val input = """def r: List[List[Int32]] = List.transpose((1 :: 2 :: Nil) :: (3 :: 4 :: Nil) :: (5 :: 6 :: Nil) ::
                  |                                          (7 :: 8 :: Nil) :: Nil)
                """.stripMargin
    runAnyTest(input, mkAnyList(List(mkList(List(1, 3, 5, 7)), mkList(List(2, 4, 6, 8)))))
  }

  test("transpose.18") {
    val input = """def r: List[List[Int32]] = List.transpose((1 :: 2 :: Nil) :: (3 :: 4 :: Nil) :: (5 :: 6 :: Nil) ::
                  |                                          (7 :: 8 :: Nil) :: (9 :: 10 :: Nil) :: Nil)
                """.stripMargin
    runAnyTest(input, mkAnyList(List(mkList(List(1, 3, 5, 7, 9)), mkList(List(2, 4, 6, 8, 10)))))
  }

  test("transpose.19") {
    val input = """def r: List[List[Int32]] = List.transpose((1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) ::
                  |                                          (7 :: 8 :: 9 :: Nil) :: Nil)
                """.stripMargin
    runAnyTest(input, mkAnyList(List(mkList(List(1, 4, 7)), mkList(List(2, 5, 8)), mkList(List(3, 6, 9)))))
  }

  test("transpose.20") {
    val input = """def r: List[List[Int32]] = List.transpose((1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: Nil) ::
                  |                                          (7 :: 8 :: 9 :: Nil) :: Nil)
                """.stripMargin
    runAnyTest(input, mkAnyList(List(mkList(List(1, 2, 3)), mkList(List(4, 5)), mkList(List(7, 8, 9)))))
  }

  test("transpose.21") {
    val input = """def r: List[List[Int32]] = List.transpose((1 :: 2 :: 3 :: Nil) :: Nil ::
                  |                                          (7 :: 8 :: 9 :: Nil) :: Nil)
                """.stripMargin
    runAnyTest(input, mkAnyList(List(mkList(List(1, 2, 3)), mkNil, mkList(List(7, 8, 9)))))
  }

  test("transpose.22") {
    val input = """def r: List[List[Int32]] = List.transpose((1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) ::
                  |                                          (7 :: 8 :: 9 :: 10 :: Nil) :: Nil)
                """.stripMargin
    runAnyTest(input, mkAnyList(List(mkList(List(1, 2, 3)), mkList(List(4, 5, 6)), mkList(List(7, 8, 9, 10)))))
  }

  test("isPrefixOf.01") {
    val input = "def r: Bool = List.isPrefixOf(Nil, Nil)"
    runBoolTest(input, true)
  }

  test("isPrefixOf.02") {
    val input = "def r: Bool = List.isPrefixOf(Nil, 1 :: Nil)"
    runBoolTest(input, true)
  }

  test("isPrefixOf.03") {
    val input = "def r: Bool = List.isPrefixOf(1 :: Nil, 1 :: Nil)"
    runBoolTest(input, true)
  }

  test("isPrefixOf.04") {
    val input = "def r: Bool = List.isPrefixOf(2 :: Nil, 1 :: Nil)"
    runBoolTest(input, false)
  }

  test("isPrefixOf.05") {
    val input = "def r: Bool = List.isPrefixOf(1 :: 2 :: Nil, 1 :: Nil)"
    runBoolTest(input, false)
  }

  test("isPrefixOf.06") {
    val input = "def r: Bool = List.isPrefixOf(1 :: Nil, 1 :: 2 :: Nil)"
    runBoolTest(input, true)
  }

  test("isPrefixOf.07") {
    val input = "def r: Bool = List.isPrefixOf(1 :: 2 :: Nil, 1 :: 2 :: Nil)"
    runBoolTest(input, true)
  }

  test("isPrefixOf.08") {
    val input = "def r: Bool = List.isPrefixOf(2 :: Nil, 1 :: 2 :: Nil)"
    runBoolTest(input, false)
  }

  test("isPrefixOf.09") {
    val input = "def r: Bool = List.isPrefixOf(1 :: 3 :: Nil, 1 :: 2 :: Nil)"
    runBoolTest(input, false)
  }

  test("isPrefixOf.10") {
    val input = "def r: Bool = List.isPrefixOf(1 :: 2 :: 3 :: Nil, 1 :: 2 :: Nil)"
    runBoolTest(input, false)
  }

  test("isPrefixOf.11") {
    val input = "def r: Bool = List.isPrefixOf(1 :: 2 :: 3 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, true)
  }

  test("isPrefixOf.12") {
    val input = "def r: Bool = List.isPrefixOf(1 :: 2 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, true)
  }

  test("isPrefixOf.13") {
    val input = "def r: Bool = List.isPrefixOf(1 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, true)
  }

  test("isPrefixOf.14") {
    val input = "def r: Bool = List.isPrefixOf(2 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, false)
  }

  test("isPrefixOf.15") {
    val input = "def r: Bool = List.isPrefixOf(1 :: 3 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, false)
  }

  test("isPrefixOf.16") {
    val input = "def r: Bool = List.isPrefixOf(1 :: 2 :: 4 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, false)
  }

  test("isInfixOf.01") {
    val input = "def r: Bool = List.isInfixOf(Nil, Nil)"
    runBoolTest(input, true)
  }

  test("isInfixOf.02") {
    val input = "def r: Bool = List.isInfixOf(1 :: Nil, Nil)"
    runBoolTest(input, false)
  }

  test("isInfixOf.03") {
    val input = "def r: Bool = List.isInfixOf(Nil, 1 :: Nil)"
    runBoolTest(input, true)
  }

  test("isInfixOf.04") {
    val input = "def r: Bool = List.isInfixOf(1 :: Nil, 1 :: Nil)"
    runBoolTest(input, true)
  }

  test("isInfixOf.05") {
    val input = "def r: Bool = List.isInfixOf(2 :: Nil, 1 :: Nil)"
    runBoolTest(input, false)
  }

  test("isInfixOf.06") {
    val input = "def r: Bool = List.isInfixOf(1 :: 2 :: Nil, 1 :: Nil)"
    runBoolTest(input, false)
  }

  test("isInfixOf.07") {
    val input = "def r: Bool = List.isInfixOf(Nil, 1 :: 2 :: Nil)"
    runBoolTest(input, true)
  }

  test("isInfixOf.08") {
    val input = "def r: Bool = List.isInfixOf(1 :: Nil, 1 :: 2 :: Nil)"
    runBoolTest(input, true)
  }

  test("isInfixOf.09") {
    val input = "def r: Bool = List.isInfixOf(2 :: Nil, 1 :: 2 :: Nil)"
    runBoolTest(input, true)
  }

  test("isInfixOf.10") {
    val input = "def r: Bool = List.isInfixOf(1 :: 2 :: Nil, 1 :: 2 :: Nil)"
    runBoolTest(input, true)
  }

  test("isInfixOf.11") {
    val input = "def r: Bool = List.isInfixOf(3 :: Nil, 1 :: 2 :: Nil)"
    runBoolTest(input, false)
  }

  test("isInfixOf.12") {
    val input = "def r: Bool = List.isInfixOf(1 :: 3 :: Nil, 1 :: 2 :: Nil)"
    runBoolTest(input, false)
  }

  test("isInfixOf.13") {
    val input = "def r: Bool = List.isInfixOf(1 :: 2 :: 3 :: Nil, 1 :: 2 :: Nil)"
    runBoolTest(input, false)
  }

  test("isInfixOf.14") {
    val input = "def r: Bool = List.isInfixOf(1 :: 2 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, true)
  }

  test("isInfixOf.15") {
    val input = "def r: Bool = List.isInfixOf(2 :: 3 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, true)
  }

  test("isInfixOf.16") {
    val input = "def r: Bool = List.isInfixOf(1 :: 2 :: 3 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, true)
  }

  test("isInfixOf.17") {
    val input = "def r: Bool = List.isInfixOf(1 :: 2 :: 4 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, false)
  }

  test("isInfixOf.18") {
    val input = "def r: Bool = List.isInfixOf(1 :: 4 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, false)
  }

  test("isInfixOf.19") {
    val input = "def r: Bool = List.isInfixOf(1 :: 3 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, false)
  }

  test("isInfixOf.20") {
    val input = "def r: Bool = List.isInfixOf(1 :: 1 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, false)
  }

  test("isSuffixOf.01") {
    val input = "def r: Bool = List.isSuffixOf(Nil, Nil)"
    runBoolTest(input, true)
  }

  test("isSuffixOf.02") {
    val input = "def r: Bool = List.isSuffixOf(Nil, 1 :: Nil)"
    runBoolTest(input, true)
  }

  test("isSuffixOf.03") {
    val input = "def r: Bool = List.isSuffixOf(1 :: Nil, 1 :: Nil)"
    runBoolTest(input, true)
  }

  test("isSuffixOf.04") {
    val input = "def r: Bool = List.isSuffixOf(2 :: Nil, 1 :: Nil)"
    runBoolTest(input, false)
  }

  test("isSuffixOf.05") {
    val input = "def r: Bool = List.isSuffixOf(2 :: 1 :: Nil, 1 :: Nil)"
    runBoolTest(input, false)
  }

  test("isSuffixOf.06") {
    val input = "def r: Bool = List.isSuffixOf(1 :: Nil, 2 :: 1 :: Nil)"
    runBoolTest(input, true)
  }

  test("isSuffixOf.07") {
    val input = "def r: Bool = List.isSuffixOf(2 :: 1 :: Nil, 2 :: 1 :: Nil)"
    runBoolTest(input, true)
  }

  test("isSuffixOf.08") {
    val input = "def r: Bool = List.isSuffixOf(2 :: Nil, 2 :: 1 :: Nil)"
    runBoolTest(input, false)
  }

  test("isSuffixOf.09") {
    val input = "def r: Bool = List.isSuffixOf(3 :: 1 :: Nil, 2 :: 1 :: Nil)"
    runBoolTest(input, false)
  }

  test("isSuffixOf.10") {
    val input = "def r: Bool = List.isSuffixOf(3 :: 2 :: 1 :: Nil, 2 :: 1 :: Nil)"
    runBoolTest(input, false)
  }

  test("isSuffixOf.11") {
    val input = "def r: Bool = List.isSuffixOf(3 :: 2 :: 1 :: Nil, 3 :: 2 :: 1 :: Nil)"
    runBoolTest(input, true)
  }

  test("isSuffixOf.12") {
    val input = "def r: Bool = List.isSuffixOf(2 :: 1 :: Nil, 3 :: 2 :: 1 :: Nil)"
    runBoolTest(input, true)
  }

  test("isSuffixOf.13") {
    val input = "def r: Bool = List.isSuffixOf(3 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, true)
  }

  test("isSuffixOf.14") {
    val input = "def r: Bool = List.isSuffixOf(2 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, false)
  }

  test("isSuffixOf.15") {
    val input = "def r: Bool = List.isSuffixOf(1 :: 3 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, false)
  }

  test("isSuffixOf.16") {
    val input = "def r: Bool = List.isSuffixOf(2 :: 2 :: 43 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, false)
  }

  test("fold.01") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Int32 = List.fold(f, 1, Nil)
      """.stripMargin
    runTest(input, 1)
  }

  test("fold.02") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Int32 = List.fold(f, 1, 2 :: Nil)
      """.stripMargin
    runTest(input, 3)
  }

  test("fold.03") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Int32 = List.fold(f, 1, 2 :: 3 :: Nil)
      """.stripMargin
    runTest(input, 6)
  }

  test("fold.04") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Int32 = List.fold(f, 1, 2 :: 3 :: 4 :: Nil)
      """.stripMargin
    runTest(input, 10)
  }

  test("fold.05") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i*2
        |def r: Int32 = List.fold(f, 1, Nil)
      """.stripMargin
    runTest(input, 1)
  }

  test("fold.06") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i*2
        |def r: Int32 = List.fold(f, 1, true :: Nil)
      """.stripMargin
    runTest(input, 2)
  }

  test("fold.07") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i*2
        |def r: Int32 = List.fold(f, 1, true :: true :: Nil)
      """.stripMargin
    runTest(input, 3)
  }

  test("fold.08") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i*2
        |def r: Int32 = List.fold(f, 1, true :: true :: false :: Nil)
      """.stripMargin
    runTest(input, 6)
  }

  test("fold.09") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i*2
        |def r: Int32 = List.fold(f, 1, true :: true :: false :: false :: Nil)
      """.stripMargin
    runTest(input, 12)
  }

  test("foldLeft.01") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Int32 = List.foldLeft(f, 1, Nil)
      """.stripMargin
    runTest(input, 1)
  }

  test("foldLeft.02") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Int32 = List.foldLeft(f, 1, 2 :: Nil)
      """.stripMargin
    runTest(input, 3)
  }

  test("foldLeft.03") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Int32 = List.foldLeft(f, 1, 2 :: 3 :: Nil)
      """.stripMargin
    runTest(input, 6)
  }

  test("foldLeft.04") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Int32 = List.foldLeft(f, 1, 2 :: 3 :: 4 :: Nil)
      """.stripMargin
    runTest(input, 10)
  }

  test("foldLeft.05") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i*2
        |def r: Int32 = List.foldLeft(f, 1, Nil)
      """.stripMargin
    runTest(input, 1)
  }

  test("foldLeft.06") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i*2
        |def r: Int32 = List.foldLeft(f, 1, true :: Nil)
      """.stripMargin
    runTest(input, 2)
  }

  test("foldLeft.07") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i*2
        |def r: Int32 = List.foldLeft(f, 1, true :: true :: Nil)
      """.stripMargin
    runTest(input, 3)
  }

  test("foldLeft.08") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i*2
        |def r: Int32 = List.foldLeft(f, 1, true :: true :: false :: Nil)
      """.stripMargin
    runTest(input, 6)
  }

  test("foldLeft.09") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i*2
        |def r: Int32 = List.foldLeft(f, 1, true :: true :: false :: false :: Nil)
      """.stripMargin
    runTest(input, 12)
  }

  test("foldRight.01") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Int32 = List.foldRight(f, 1, Nil)
      """.stripMargin
    runTest(input, 1)
  }

  test("foldRight.02") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Int32 = List.foldRight(f, 1, 2 :: Nil)
      """.stripMargin
    runTest(input, 3)
  }

  test("foldRight.03") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Int32 = List.foldRight(f, 1, 2 :: 3 :: Nil)
      """.stripMargin
    runTest(input, 6)
  }

  test("foldRight.04") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Int32 = List.foldRight(f, 1, 2 :: 3 :: 4 :: Nil)
      """.stripMargin
    runTest(input, 10)
  }

  test("foldRight.05") {
    val input =
      """def f(b: Bool, i: Int32): Int32 = if (b) i+1 else i*2
        |def r: Int32 = List.foldRight(f, 1, Nil)
      """.stripMargin
    runTest(input, 1)
  }

  test("foldRight.06") {
    val input =
      """def f(b: Bool, i: Int32): Int32 = if (b) i+1 else i*2
        |def r: Int32 = List.foldRight(f, 1, true :: Nil)
      """.stripMargin
    runTest(input, 2)
  }

  test("foldRight.07") {
    val input =
      """def f(b: Bool, i: Int32): Int32 = if (b) i+1 else i*2
        |def r: Int32 = List.foldRight(f, 1, true :: true :: Nil)
      """.stripMargin
    runTest(input, 3)
  }

  test("foldRight.08") {
    val input =
      """def f(b: Bool, i: Int32): Int32 = if (b) i+1 else i*2
        |def r: Int32 = List.foldRight(f, 1, true :: true :: false :: Nil)
      """.stripMargin
    runTest(input, 4)
  }

  test("foldRight.09") {
    val input =
      """def f(b: Bool, i: Int32): Int32 = if (b) i+1 else i*2
        |def r: Int32 = List.foldRight(f, 1, true :: true :: false :: false :: Nil)
      """.stripMargin
    runTest(input, 6)
  }

  test("reduceOpt.01") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Option[Int32] = List.reduceOpt(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNone)
  }

  test("reduceOpt.02") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Option[Int32] = List.reduceOpt(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(1))
  }

  test("reduceOpt.03") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Option[Int32] = List.reduceOpt(f, 1 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(3))
  }

  test("reduceOpt.04") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Option[Int32] = List.reduceOpt(f, 1 :: 2 :: 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(6))
  }

  test("reduceOpt.05") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Option[Int32] = List.reduceOpt(f, 1 :: 2 :: 3 :: 4 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(10))
  }

  test("reduceLeftOpt.01") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Option[Int32] = List.reduceLeftOpt(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNone)
  }

  test("reduceLeftOpt.02") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Option[Int32] = List.reduceLeftOpt(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(1))
  }

  test("reduceLeftOpt.03") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Option[Int32] = List.reduceLeftOpt(f, 1 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(3))
  }

  test("reduceLeftOpt.04") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Option[Int32] = List.reduceLeftOpt(f, 1 :: 2 :: 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(6))
  }

  test("reduceLeftOpt.05") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Option[Int32] = List.reduceLeftOpt(f, 1 :: 2 :: 3 :: 4 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(10))
  }

  test("reduceRightOpt.01") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Option[Int32] = List.reduceRightOpt(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNone)
  }

  test("reduceRightOpt.02") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Option[Int32] = List.reduceRightOpt(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(1))
  }

  test("reduceRightOpt.03") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Option[Int32] = List.reduceRightOpt(f, 1 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(3))
  }

  test("reduceRightOpt.04") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Option[Int32] = List.reduceRightOpt(f, 1 :: 2 :: 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(6))
  }

  test("reduceRightOpt.05") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: Option[Int32] = List.reduceRightOpt(f, 1 :: 2 :: 3 :: 4 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(10))
  }

  test("count.01") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Int32 = List.count(f, Nil)
      """.stripMargin
    runTest(input, 0)
  }

  test("count.02") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Int32 = List.count(f, 2 :: Nil)
      """.stripMargin
    runTest(input, 0)
  }

  test("count.03") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Int32 = List.count(f, 2 :: 5 :: Nil)
      """.stripMargin
    runTest(input, 0)
  }

  test("count.04") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Int32 = List.count(f, 3 :: Nil)
      """.stripMargin
    runTest(input, 1)
  }

  test("count.05") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Int32 = List.count(f, 3 :: 4 :: 2 :: Nil)
      """.stripMargin
    runTest(input, 1)
  }

  test("count.06") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Int32 = List.count(f, 1 :: 3 :: 4 :: 2 :: Nil)
      """.stripMargin
    runTest(input, 1)
  }

  test("count.07") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Int32 = List.count(f, 3 :: 3 :: Nil)
      """.stripMargin
    runTest(input, 2)
  }

  test("count.08") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Int32 = List.count(f, 1 :: 6 :: 3 :: 3 :: Nil)
      """.stripMargin
    runTest(input, 2)
  }

  test("count.09") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Int32 = List.count(f, 3 :: 3 :: 8 :: 88 :: Nil)
      """.stripMargin
    runTest(input, 2)
  }

  test("count.10") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Int32 = List.count(f, 5 :: 2 :: 3 :: 1 :: 67 :: 3 :: 0 :: -6 :: -3 :: Nil)
      """.stripMargin
    runTest(input, 2)
  }

  test("count.11") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Int32 = List.count(f, 3 :: 2 :: 3 :: 1 :: 67 :: 3 :: 0 :: -6 :: -3 :: 3 :: 3 :: Nil)
      """.stripMargin
    runTest(input, 5)
  }

  test("concat.01") {
    val input = "def r: List[Int32] = List.concat(Nil)"
    runAnyTest(input, mkNil)
  }

  test("concat.02") {
    val input = "def r: List[Int32] = List.concat(Nil :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("concat.03") {
    val input = "def r: List[Int32] = List.concat((1 :: Nil) :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("concat.04") {
    val input = "def r: List[Int32] = List.concat((1 :: 2 :: Nil) :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("concat.05") {
    val input = "def r: List[Int32] = List.concat(Nil :: Nil :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("concat.06") {
    val input = "def r: List[Int32] = List.concat((1 :: Nil) :: Nil :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("concat.07") {
    val input = "def r: List[Int32] = List.concat(Nil :: (1 :: Nil) :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("concat.08") {
    val input = "def r: List[Int32] = List.concat((1 :: Nil) :: (2 :: Nil) :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("concat.09") {
    val input = "def r: List[Int32] = List.concat((1 :: 2 :: Nil) :: (3 :: Nil) :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3)))
  }

  test("concat.10") {
    val input = "def r: List[Int32] = List.concat((1 :: 2 :: Nil) :: (3 :: 4 :: Nil) :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3, 4)))
  }

  test("concat.11") {
    val input = "def r: List[Int32] = List.concat((1 :: 2 :: Nil) :: (3 :: 4 :: 5 :: Nil) :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3, 4, 5)))
  }

  test("concat.12") {
    val input = "def r: List[Int32] = List.concat(Nil :: (1 :: Nil) :: Nil :: (2 :: 3 :: Nil) :: (7 :: 8 :: 11 :: Nil) :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3, 7, 8, 11)))
  }

  test("exists.01") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Bool = List.exists(f, Nil)
      """.stripMargin
    runBoolTest(input, false)
  }

  test("exists.02") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Bool = List.exists(f, 1 :: Nil)
      """.stripMargin
    runBoolTest(input, false)
  }

  test("exists.03") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Bool = List.exists(f, 3 :: Nil)
      """.stripMargin
    runBoolTest(input, true)
  }

  test("exists.04") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Bool = List.exists(f, 1 :: 33 :: Nil)
      """.stripMargin
    runBoolTest(input, false)
  }

  test("exists.05") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Bool = List.exists(f, 1 :: 3 :: Nil)
      """.stripMargin
    runBoolTest(input, true)
  }

  test("exists.06") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Bool = List.exists(f, 3 :: 33 :: Nil)
      """.stripMargin
    runBoolTest(input, true)
  }

  test("exists.07") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Bool = List.exists(f, 3 :: 3 :: Nil)
      """.stripMargin
    runBoolTest(input, true)
  }

  test("exists.08") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Bool = List.exists(f, 1 :: 4 :: 8 :: 3 :: 2 :: 99 :: Nil)
      """.stripMargin
    runBoolTest(input, true)
  }

  test("exists.09") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Bool = List.exists(f, 3 :: 1 :: 2 :: 99 :: 22 :: 11 :: Nil)
      """.stripMargin
    runBoolTest(input, true)
  }

  test("exists.10") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Bool = List.exists(f, 1 :: 31 :: 99 :: 21 :: 14 :: 89 :: Nil)
      """.stripMargin
    runBoolTest(input, false)
  }

  test("forall.01") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Bool = List.forall(f, Nil)
      """.stripMargin
    runBoolTest(input, true)
  }

  test("forall.02") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Bool = List.forall(f, 1 :: Nil)
      """.stripMargin
    runBoolTest(input, false)
  }

  test("forall.03") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Bool = List.forall(f, 3 :: Nil)
      """.stripMargin
    runBoolTest(input, true)
  }

  test("forall.04") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Bool = List.forall(f, 1 :: 2 :: Nil)
      """.stripMargin
    runBoolTest(input, false)
  }

  test("forall.05") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Bool = List.forall(f, 1 :: 3 :: Nil)
      """.stripMargin
    runBoolTest(input, false)
  }

  test("forall.06") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Bool = List.forall(f, 3 :: 2 :: Nil)
      """.stripMargin
    runBoolTest(input, false)
  }

  test("forall.07") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Bool = List.forall(f, 3 :: 3 :: Nil)
      """.stripMargin
    runBoolTest(input, true)
  }

  test("forall.08") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Bool = List.forall(f, 3 :: 3 :: 3 :: Nil)
      """.stripMargin
    runBoolTest(input, true)
  }

  test("forall.09") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Bool = List.forall(f, 3 :: 3 :: 1 :: Nil)
      """.stripMargin
    runBoolTest(input, false)
  }

  test("forall.10") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Bool = List.forall(f, 33 :: 3 :: 3 :: Nil)
      """.stripMargin
    runBoolTest(input, false)
  }

  test("forall.11") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Bool = List.forall(f, 3 :: 3 :: 3 :: 3 :: 3 :: 3 :: 3 :: Nil)
      """.stripMargin
    runBoolTest(input, true)
  }

  test("forall.12") {
    val input =
      """def f(i: Int32): Bool = i == 3
        |def r: Bool = List.forall(f, 3 :: 3 :: 3 :: 3 :: 3 :: 3 :: 0 :: Nil)
      """.stripMargin
    runBoolTest(input, false)
  }

  test("and.01") {
    val input = "def r: Bool = List.and(Nil)"
    runBoolTest(input, true)
  }

  test("and.02") {
    val input = "def r: Bool = List.and(true :: Nil)"
    runBoolTest(input, true)
  }

  test("and.03") {
    val input = "def r: Bool = List.and(false :: Nil)"
    runBoolTest(input, false)
  }

  test("and.04") {
    val input = "def r: Bool = List.and(true :: true :: Nil)"
    runBoolTest(input, true)
  }

  test("and.05") {
    val input = "def r: Bool = List.and(true :: false :: Nil)"
    runBoolTest(input, false)
  }

  test("and.06") {
    val input = "def r: Bool = List.and(false :: true :: Nil)"
    runBoolTest(input, false)
  }

  test("and.07") {
    val input = "def r: Bool = List.and(false :: false :: Nil)"
    runBoolTest(input, false)
  }

  test("and.08") {
    val input = "def r: Bool = List.and(true :: true :: true :: Nil)"
    runBoolTest(input, true)
  }

  test("and.09") {
    val input = "def r: Bool = List.and(true :: true :: false :: Nil)"
    runBoolTest(input, false)
  }

  test("and.10") {
    val input = "def r: Bool = List.and(false :: true :: true :: Nil)"
    runBoolTest(input, false)
  }

  test("and.11") {
    val input = "def r: Bool = List.and(true :: true :: true :: true :: true :: Nil)"
    runBoolTest(input, true)
  }

  test("and.12") {
    val input = "def r: Bool = List.and(true :: true :: true :: true :: false :: Nil)"
    runBoolTest(input, false)
  }

  test("or.01") {
    val input = "def r: Bool = List.or(Nil)"
    runBoolTest(input, false)
  }

  test("or.02") {
    val input = "def r: Bool = List.or(true :: Nil)"
    runBoolTest(input, true)
  }

  test("or.03") {
    val input = "def r: Bool = List.or(false :: Nil)"
    runBoolTest(input, false)
  }

  test("or.04") {
    val input = "def r: Bool = List.or(true :: true :: Nil)"
    runBoolTest(input, true)
  }

  test("or.05") {
    val input = "def r: Bool = List.or(true :: false :: Nil)"
    runBoolTest(input, true)
  }

  test("or.06") {
    val input = "def r: Bool = List.or(false :: true :: Nil)"
    runBoolTest(input, true)
  }

  test("or.07") {
    val input = "def r: Bool = List.or(false :: false :: Nil)"
    runBoolTest(input, false)
  }

  test("or.08") {
    val input = "def r: Bool = List.or(true :: true :: true :: Nil)"
    runBoolTest(input, true)
  }

  test("or.09") {
    val input = "def r: Bool = List.or(true :: false :: false :: Nil)"
    runBoolTest(input, true)
  }

  test("or.10") {
    val input = "def r: Bool = List.or(false :: false :: true :: Nil)"
    runBoolTest(input, true)
  }

  test("or.11") {
    val input = "def r: Bool = List.or(false :: false :: false :: Nil)"
    runBoolTest(input, false)
  }

  test("or.12") {
    val input = "def r: Bool = List.or(true :: false :: true :: true :: true :: Nil)"
    runBoolTest(input, true)
  }

  test("or.13") {
    val input = "def r: Bool = List.or(true :: true :: true :: true :: false :: Nil)"
    runBoolTest(input, true)
  }

  test("or.14") {
    val input = "def r: Bool = List.or(false :: false :: false :: false :: false :: false :: Nil)"
    runBoolTest(input, false)
  }

  test("filter.01") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.filter(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("filter.02") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.filter(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("filter.03") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.filter(f, 4 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(4)))
  }

  test("filter.04") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.filter(f, 1 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("filter.05") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.filter(f, 1 :: 6 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(6)))
  }

  test("filter.06") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.filter(f, 9 :: 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(9)))
  }

  test("filter.07") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.filter(f, 90 :: 6 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(90, 6)))
  }

  test("filter.08") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.filter(f, 90 :: 1 :: 6 :: 2 :: -8 :: 11 :: 1000 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(90, 6, 11, 1000)))
  }

  test("filter.09") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.filter(f, -10 :: -11 :: 4 :: 4 :: 3 :: 9 :: 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(4, 4, 9)))
  }

  test("slice.01") {
    val input = "def r: List[Int32] = List.slice(0, 0, Nil)"
    runAnyTest(input, mkNil)
  }

  test("slice.02") {
    val input = "def r: List[Int32] = List.slice(1, 2, Nil)"
    runAnyTest(input, mkNil)
  }

  test("slice.03") {
    val input = "def r: List[Int32] = List.slice(0, 1, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("slice.04") {
    val input = "def r: List[Int32] = List.slice(-1, 2, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("slice.05") {
    val input = "def r: List[Int32] = List.slice(1, 2, 1 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("slice.06") {
    val input = "def r: List[Int32] = List.slice(0, 1, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("slice.07") {
    val input = "def r: List[Int32] = List.slice(1, 2, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(2)))
  }

  test("slice.08") {
    val input = "def r: List[Int32] = List.slice(0, 2, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("slice.09") {
    val input = "def r: List[Int32] = List.slice(0, 2, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("slice.10") {
    val input = "def r: List[Int32] = List.slice(2, 4, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil)"
    runAnyTest(input, mkList(List(3, 4)))
  }

  test("slice.11") {
    val input = "def r: List[Int32] = List.slice(7, 11, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil)"
    runAnyTest(input, mkList(List(8)))
  }

  test("slice.12") {
    val input = "def r: List[Int32] = List.slice(1, 7, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil)"
    runAnyTest(input, mkList(List(2, 3, 4, 5, 6, 7)))
  }

  test("partition.01") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.partition(f, Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkNil, mkNil))
  }

  test("partition.02") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.partition(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkNil, mkList(List(1))))
  }

  test("partition.03") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.partition(f, 4 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(4)), mkNil))
  }

  test("partition.04") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.partition(f, 1 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkNil, mkList(List(1, 2))))
  }

  test("partition.05") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.partition(f, 1 :: 6 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(6)), mkList(List(1))))
  }

  test("partition.06") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.partition(f, 5 :: 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(5)), mkList(List(3))))
  }

  test("partition.07") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.partition(f, 99 :: 6 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(99, 6)), mkNil))
  }

  test("partition.08") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.partition(f, 99 :: 6 :: 4 :: 1 :: -99 :: 99 :: 88 :: 5 :: 1 :: 0 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(99, 6, 4, 99, 88, 5)), mkList(List(1, -99, 1, 0))))
  }

  test("partition.09") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.partition(f, 1 :: 11 :: 24 :: -1 :: 3 :: 14 :: 5 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(11, 24, 14, 5)), mkList(List(1, -1, 3))))
  }

  test("span.01") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkNil, mkNil))
  }

  test("span.02") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkNil, mkList(List(1))))
  }

  test("span.03") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, 4 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(4)), mkNil))
  }

  test("span.04") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, 1 :: 4 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkNil, mkList(List(1, 4))))
  }

  test("span.05") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, 4 :: 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(4)), mkList(List(1))))
  }

  test("span.06") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, 4 :: 8 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(4, 8)), mkNil))
  }

  test("span.07") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, 1 :: -1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkNil, mkList(List(1, -1))))
  }

  test("span.08") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, -1 :: 11 :: 88 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkNil, mkList(List(-1, 11, 88))))
  }

  test("span.09") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, 4 :: -1 :: 88 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(4)), mkList(List(-1, 88))))
  }

  test("span.10") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, 4 :: 9 :: -2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(4, 9)), mkList(List(-2))))
  }

  test("span.11") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, 4 :: 9 :: 9 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(4, 9, 9)), mkNil))
  }

  test("span.12") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, 8 :: 11 :: 89 :: -1 :: 34 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(8, 11, 89)), mkList(List(-1, 34))))
  }

  test("span.13") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, 3 :: 8 :: 11 :: 89 :: -1 :: 34 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkNil, mkList(List(3, 8, 11, 89, -1, 34))))
  }

  test("drop.01") {
    val input = "def r: List[Int32] = List.drop(0, Nil)"
    runAnyTest(input, mkNil)
  }

  test("drop.02") {
    val input = "def r: List[Int32] = List.drop(-1, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("drop.03") {
    val input = "def r: List[Int32] = List.drop(0, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("drop.04") {
    val input = "def r: List[Int32] = List.drop(1, 1 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("drop.05") {
    val input = "def r: List[Int32] = List.drop(2, 1 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("drop.06") {
    val input = "def r: List[Int32] = List.drop(0, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("drop.07") {
    val input = "def r: List[Int32] = List.drop(1, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(2)))
  }

  test("drop.08") {
    val input = "def r: List[Int32] = List.drop(2, 1 :: 2 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("drop.09") {
    val input = "def r: List[Int32] = List.drop(3, 1 :: 2 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("drop.10") {
    val input = "def r: List[Int32] = List.drop(3, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil)"
    runAnyTest(input, mkList(List(4, 5, 6, 7)))
  }

  test("drop.11") {
    val input = "def r: List[Int32] = List.drop(6, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil)"
    runAnyTest(input, mkList(List(7)))
  }

  test("drop.12") {
    val input = "def r: List[Int32] = List.drop(99, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("dropWhile.01") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.dropWhile(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("dropWhile.02") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.dropWhile(f, 5 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("dropWhile.03") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.dropWhile(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("dropWhile.04") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.dropWhile(f, 1 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("dropWhile.05") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.dropWhile(f, 1 :: 6 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 6)))
  }

  test("dropWhile.06") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.dropWhile(f, 6 :: 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("dropWhile.07") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.dropWhile(f, 6 :: 11 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("dropWhile.08") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.dropWhile(f, 1 :: 2 :: 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 2, 3)))
  }

  test("dropWhile.09") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.dropWhile(f, 6 :: 2 :: 5 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2, 5)))
  }

  test("dropWhile.10") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.dropWhile(f, 6 :: 12 :: 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(3)))
  }

  test("dropWhile.11") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.dropWhile(f, 6 :: 12 :: 32 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("take.01") {
    val input = "def r: List[Int32] = List.take(0, Nil)"
    runAnyTest(input, mkNil)
  }

  test("take.02") {
    val input = "def r: List[Int32] = List.take(-1, 1 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("take.03") {
    val input = "def r: List[Int32] = List.take(0, 1 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("take.04") {
    val input = "def r: List[Int32] = List.take(1, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("take.05") {
    val input = "def r: List[Int32] = List.take(2, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("take.06") {
    val input = "def r: List[Int32] = List.take(-1, 1 :: 2 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("take.07") {
    val input = "def r: List[Int32] = List.take(0, 1 :: 2 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("take.08") {
    val input = "def r: List[Int32] = List.take(1, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("take.09") {
    val input = "def r: List[Int32] = List.take(2, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("take.10") {
    val input = "def r: List[Int32] = List.take(3, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("take.11") {
    val input = "def r: List[Int32] = List.take(3, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3)))
  }

  test("take.12") {
    val input = "def r: List[Int32] = List.take(6, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3, 4, 5, 6)))
  }

  test("take.13") {
    val input = "def r: List[Int32] = List.take(99, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3, 4, 5, 6, 7)))
  }

  test("takeWhile.01") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.takeWhile(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("takeWhile.02") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.takeWhile(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("takeWhile.03") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.takeWhile(f, 4 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(4)))
  }

  test("takeWhile.04") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.takeWhile(f, 1 :: 4 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("takeWhile.05") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.takeWhile(f, 4 :: -4 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(4)))
  }

  test("takeWhile.06") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.takeWhile(f, 8 :: 4 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(8, 4)))
  }

  test("takeWhile.07") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.takeWhile(f, 4 :: 1 :: 8 :: 9 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(4)))
  }

  test("takeWhile.08") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.takeWhile(f, 4 :: 11 :: 8 :: -9 :: 7 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(4, 11, 8)))
  }

  test("takeWhile.09") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.takeWhile(f, 3 :: 11 :: 8 :: -9 :: 7 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("groupBy.01") {
    val input =
      """def f(a: Int32, b: Int32): Bool = a > 3 || b > 8
        |def r: List[List[Int32]] = List.groupBy(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("groupBy.02") {
    val input =
      """def f(a: Int32, b: Int32): Bool = a > 3 || b > 8
        |def r: List[List[Int32]] = List.groupBy(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyList(List(mkList(List(1)))))
  }

  test("groupBy.03") {
    val input =
      """def f(a: Int32, b: Int32): Bool = a > 3 || b > 8
        |def r: List[List[Int32]] = List.groupBy(f, 1 :: 4 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyList(List(mkList(List(1)), mkList(List(4)))))
  }

  test("groupBy.04") {
    val input =
      """def f(a: Int32, b: Int32): Bool = a > 3 || b > 8
        |def r: List[List[Int32]] = List.groupBy(f, 1 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyList(List(mkList(List(1)), mkList(List(2)))))
  }

  test("groupBy.05") {
    val input =
      """def f(a: Int32, b: Int32): Bool = a > 3 || b > 8
        |def r: List[List[Int32]] = List.groupBy(f, 1 :: 9 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyList(List(mkList(List(1, 9)))))
  }

  test("groupBy.06") {
    val input =
      """def f(a: Int32, b: Int32): Bool = a > 3 || b > 8
        |def r: List[List[Int32]] = List.groupBy(f, 1 :: 4 :: 7 :: 6 :: 9 :: 2 :: 4 :: 4 :: 8 :: 16 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyList(List(mkList(List(1, 9, 16)), mkList(List(4, 7, 6, 4, 4, 8)), mkList(List(2)))))
  }

  test("groupBy.07") {
    val input =
      """def f(a: Int32, b: Int32): Bool = a > -6 || a*b >= 0
        |def r: List[List[Int32]] = List.groupBy(f, -1 :: -11 :: 4 :: -11 :: 0 :: 8 :: 2 :: 1 :: -3 :: -24 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyList(List(mkList(List(-1, -11, -11, 0, -3, -24)), mkList(List(4, 8, 2, 1)))))
  }

  test("groupBy.08") {
    val input =
      """def f(a: Int32, b: Int32): Bool = a < 0 || (a > 10 || (b > 10 || a == b))
        |def r: List[List[Int32]] = List.groupBy(f, -5 :: 6 :: 11 :: 8 :: 8 :: -11 :: -1 :: 0 :: 4 :: -1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyList(List(mkList(List(-5, 11, -11, -1, -1)), mkList(List(6)),
                                     mkList(List(8, 8)), mkList(List(0)), mkList(List(4)))))
  }

  test("zip.01") {
    val input = "def r: List[(Int32, Int32)] = List.zip(Nil, Nil)"
    runAnyTest(input, mkNil)
  }

  test("zip.02") {
    val input = "def r: List[(Int32, Int32)] = List.zip(1 :: Nil, Nil)"
    runAnyTest(input, mkNil)
  }

  test("zip.03") {
    val input = "def r: List[(Int32, Int32)] = List.zip(Nil, 2 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("zip.04") {
    val input = "def r: List[(Int32, Int32)] = List.zip(1 :: Nil, 2 :: Nil)"
    runAnyTest(input, mkAnyList(List(mkTuple(1, 2))))
  }

  test("zip.05") {
    val input = "def r: List[(Int32, Int32)] = List.zip(1 :: 3 :: Nil, 2 :: 4 :: Nil)"
    runAnyTest(input, mkAnyList(List(mkTuple(1, 2), mkTuple(3, 4))))
  }

  test("zip.06") {
    val input = "def r: List[(Int32, Int32)] = List.zip(1 :: 3 :: 5 :: Nil, 2 :: 4 :: 6 :: Nil)"
    runAnyTest(input, mkAnyList(List(mkTuple(1, 2), mkTuple(3, 4), mkTuple(5, 6))))
  }

  test("zip.07") {
    val input = "def r: List[(Int32, Int32)] = List.zip(1 :: 3 :: 5 :: 7 :: Nil, 2 :: 4 :: 6 :: Nil)"
    runAnyTest(input, mkAnyList(List(mkTuple(1, 2), mkTuple(3, 4), mkTuple(5, 6))))
  }

  test("zip.08") {
    val input = "def r: List[(Int32, Int32)] = List.zip(1 :: 3 :: 5 :: Nil, 2 :: 4 :: 6 :: 8 :: Nil)"
    runAnyTest(input, mkAnyList(List(mkTuple(1, 2), mkTuple(3, 4), mkTuple(5, 6))))
  }

  test("zipWith.01") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.zipWith(f, Nil, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("zipWith.02") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.zipWith(f, 1 :: Nil, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("zipWith.03") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.zipWith(f, Nil, true :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("zipWith.04") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.zipWith(f, 1 :: Nil, true :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2)))
  }

  test("zipWith.05") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.zipWith(f, 1 :: Nil, false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("zipWith.06") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.zipWith(f, 1 :: 2 :: Nil, true :: true :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2, 3)))
  }

  test("zipWith.07") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.zipWith(f, 1 :: 2 :: Nil, true :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2, 2)))
  }

  test("zipWith.08") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.zipWith(f, 1 :: 2 :: Nil, false :: true :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 3)))
  }

  test("zipWith.09") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.zipWith(f, 1 :: 2 :: Nil, false :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("zipWith.10") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.zipWith(f, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil, false :: true :: true :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 3, 4, 4)))
  }

  test("unzip.01") {
    val input = "def r: (List[Int32], List[Bool]) = List.unzip(Nil)"
    runAnyTest(input, mkAnyTuple(mkNil, mkNil))
  }

  test("unzip.02") {
    val input = "def r: (List[Int32], List[Bool]) = List.unzip((1, true) :: Nil)"
    runAnyTest(input, mkAnyTuple(mkList(List(1)), mkBoolList(List(true))))
  }

  test("unzip.03") {
    val input = "def r: (List[Int32], List[Bool]) = List.unzip((1, true) :: (2, false) :: Nil)"
    runAnyTest(input, mkAnyTuple(mkList(List(1, 2)), mkBoolList(List(true, false))))
  }

  test("unzip.04") {
    val input = "def r: (List[Int32], List[Bool]) = List.unzip((1, true) :: (2, false) :: (3, false) :: Nil)"
    runAnyTest(input, mkAnyTuple(mkList(List(1, 2, 3)), mkBoolList(List(true, false, false))))
  }

  test("unzip.05") {
    val input = "def r: (List[BigInt], List[Bool]) = List.unzip((1ii, true) :: (2ii, false) :: (3ii, false) :: Nil)"
    runAnyTest(input, mkAnyTuple(mkBigIntList(List(1, 2, 3)), mkBoolList(List(true, false, false))))
  }

  test("map2.01") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.map2(f, Nil, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("map2.02") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.map2(f, 1 :: Nil, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("map2.03") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.map2(f, Nil, true :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("map2.04") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.map2(f, 1 :: Nil, true :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2)))
  }

  test("map2.05") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.map2(f, 1 :: Nil, false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("map2.06") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.map2(f, 1 :: 2 :: Nil, true :: true :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2, 3)))
  }

  test("map2.07") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.map2(f, 1 :: 2 :: Nil, true :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2, 2)))
  }

  test("map2.08") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.map2(f, 1 :: 2 :: Nil, false :: true :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 3)))
  }

  test("map2.09") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.map2(f, 1 :: 2 :: Nil, false :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("map2.10") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.map2(f, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil, false :: true :: true :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 3, 4, 4)))
  }

  test("flatMap2.01") {
    val input =
      """def f(a: Int32, b: Bool): List[Int32] = if (b) List.repeat(a, a) else List.repeat(a+1, a+1)
        |def r: List[Int32] = List.flatMap2(f, Nil, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("flatMap2.02") {
    val input =
      """def f(a: Int32, b: Bool): List[Int32] = if (b) List.repeat(a, a) else List.repeat(a+1, a+1)
        |def r: List[Int32] = List.flatMap2(f, 1 :: Nil, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("flatMap2.03") {
    val input =
      """def f(a: Int32, b: Bool): List[Int32] = if (b) List.repeat(a, a) else List.repeat(a+1, a+1)
        |def r: List[Int32] = List.flatMap2(f, Nil, true :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("flatMap2.04") {
    val input =
      """def f(a: Int32, b: Bool): List[Int32] = if (b) List.repeat(a, a) else List.repeat(a+1, a+1)
        |def r: List[Int32] = List.flatMap2(f, 1 :: Nil, true :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("flatMap2.05") {
    val input =
      """def f(a: Int32, b: Bool): List[Int32] = if (b) List.repeat(a, a) else List.repeat(a+1, a+1)
        |def r: List[Int32] = List.flatMap2(f, 1 :: 2 :: Nil, true :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 3, 3, 3)))
  }

  test("flatMap2.06") {
    val input =
      """def f(a: Int32, b: Bool): List[Int32] = if (b) List.repeat(a, a) else List.repeat(a+1, a+1)
        |def r: List[Int32] = List.flatMap2(f, 1 :: 2 :: 2 :: Nil, true :: false :: true :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 3, 3, 3, 2, 2)))
  }

  test("flatMap2.07") {
    val input =
      """def f(a: Int32, b: Bool): List[Int32] = if (b) List.repeat(a, a) else List.repeat(a+1, a+1)
        |def r: List[Int32] = List.flatMap2(f, 1 :: 2 :: 2 :: 4 :: Nil, true :: false :: true :: false :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 3, 3, 3, 2, 2, 5, 5, 5, 5, 5)))
  }

  test("fold2.01") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.fold2(f, 4, Nil, Nil)
      """.stripMargin
    runTest(input, 4)
  }

  test("fold2.02") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.fold2(f, 4, 1 :: Nil, Nil)
      """.stripMargin
    runTest(input, 4)
  }

  test("fold2.03") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.fold2(f, 4, Nil, true :: Nil)
      """.stripMargin
    runTest(input, 4)
  }

  test("fold2.04") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.fold2(f, 4, 1 :: Nil, true :: Nil)
      """.stripMargin
    runTest(input, 5)
  }

  test("fold2.05") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.fold2(f, 4, 2 :: Nil, false :: Nil)
      """.stripMargin
    runTest(input, 8)
  }

  test("fold2.06") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.fold2(f, 4, 2 :: 7 :: Nil, false :: true :: Nil)
      """.stripMargin
    runTest(input, 15)
  }

  test("fold2.07") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.fold2(f, 4, 2 :: 7 :: 4 :: Nil, false :: true :: false :: Nil)
      """.stripMargin
    runTest(input, 60)
  }

  test("fold2.08") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.fold2(f, 4, 2 :: 7 :: 4 :: -9 :: Nil, false :: true :: false :: false :: Nil)
      """.stripMargin
    runTest(input, -540)
  }

  test("foldLeft2.01") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldLeft2(f, 4, Nil, Nil)
      """.stripMargin
    runTest(input, 4)
  }

  test("foldLeft2.02") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldLeft2(f, 4, 1 :: Nil, Nil)
      """.stripMargin
    runTest(input, 4)
  }

  test("foldLeft2.03") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldLeft2(f, 4, Nil, true :: Nil)
      """.stripMargin
    runTest(input, 4)
  }

  test("foldLeft2.04") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldLeft2(f, 4, 1 :: Nil, true :: Nil)
      """.stripMargin
    runTest(input, 5)
  }

  test("foldLeft2.05") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldLeft2(f, 4, 2 :: Nil, false :: Nil)
      """.stripMargin
    runTest(input, 8)
  }

  test("foldLeft2.06") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldLeft2(f, 4, 2 :: 7 :: Nil, false :: true :: Nil)
      """.stripMargin
    runTest(input, 15)
  }

  test("foldLeft2.07") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldLeft2(f, 4, 2 :: 7 :: 4 :: Nil, false :: true :: false :: Nil)
      """.stripMargin
    runTest(input, 60)
  }

  test("foldLeft2.08") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldLeft2(f, 4, 2 :: 7 :: 4 :: -9 :: Nil, false :: true :: false :: false :: Nil)
      """.stripMargin
    runTest(input, -540)
  }

  test("foldRight2.01") {
    val input =
      """def f(a: Int32, b: Bool, c: Int32): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldRight2(f, 4, Nil, Nil)
      """.stripMargin
    runTest(input, 4)
  }

  test("foldRight2.02") {
    val input =
      """def f(a: Int32, b: Bool, c: Int32): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldRight2(f, 4, 1 :: Nil, Nil)
      """.stripMargin
    runTest(input, 4)
  }

  test("foldRight2.03") {
    val input =
      """def f(a: Int32, b: Bool, c: Int32): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldRight2(f, 4, Nil, true :: Nil)
      """.stripMargin
    runTest(input, 4)
  }

  test("foldRight2.04") {
    val input =
      """def f(a: Int32, b: Bool, c: Int32): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldRight2(f, 4, 1 :: Nil, true :: Nil)
      """.stripMargin
    runTest(input, 5)
  }

  test("foldRight2.05") {
    val input =
      """def f(a: Int32, b: Bool, c: Int32): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldRight2(f, 4, 2 :: Nil, false :: Nil)
      """.stripMargin
    runTest(input, 8)
  }

  test("foldRight2.06") {
    val input =
      """def f(a: Int32, b: Bool, c: Int32): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldRight2(f, 4, 2 :: 7 :: Nil, false :: true :: Nil)
      """.stripMargin
    runTest(input, 22)
  }

  test("foldRight2.07") {
    val input =
      """def f(a: Int32, b: Bool, c: Int32): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldRight2(f, 4, 5 :: 88 :: 2 :: 7 :: 4 :: Nil, false :: true :: false :: Nil)
      """.stripMargin
    runTest(input, 46)
  }

  test("foldRight2.08") {
    val input =
      """def f(a: Int32, b: Bool, c: Int32): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldRight2(f, 4, 2 :: 7 :: 4 :: -9 :: Nil, false :: false :: true :: false :: false :: Nil)
      """.stripMargin
    runTest(input, -274)
  }

  test("foldRight2.09") {
    val input =
      """def f(a: Int32, b: Bool, c: Int32): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldRight2(f, 4, 1 :: 2 :: 7 :: 4 :: -9 :: Nil, true :: false :: true :: false :: false :: Nil)
      """.stripMargin
    runTest(input, -273)
  }

  test("concatMap.01") {
    val input =
      """def f(i: Int32): List[Int32] = List.repeat(i, i)
        |def r: List[Int32] = List.concatMap(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("concatMap.02") {
    val input =
      """def f(i: Int32): List[Int32] = List.repeat(i, i)
        |def r: List[Int32] = List.concatMap(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("concatMap.03") {
    val input =
      """def f(i: Int32): List[Int32] = List.repeat(i, i)
        |def r: List[Int32] = List.concatMap(f, 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(3, 3, 3)))
  }

  test("concatMap.04") {
    val input =
      """def f(i: Int32): List[Int32] = List.repeat(i, i)
        |def r: List[Int32] = List.concatMap(f, 2 :: 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2, 2, 1)))
  }

  test("concatMap.05") {
    val input =
      """def f(i: Int32): List[Int32] = List.repeat(i, i)
        |def r: List[Int32] = List.concatMap(f, 2 :: 1 :: 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2, 2, 1, 3, 3, 3)))
  }

  test("concatMap.06") {
    val input =
      """def f(i: Int32): List[Int32] = List.repeat(i, i)
        |def r: List[Int32] = List.concatMap(f, 2 :: 1 :: 3 :: 4 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2, 2, 1, 3, 3, 3, 4, 4, 4, 4)))
  }

  test("filterMap.01") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: List[Int32] = List.filterMap(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("filterMap.02") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: List[Int32] = List.filterMap(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("filterMap.03") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: List[Int32] = List.filterMap(f, 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("filterMap.04") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: List[Int32] = List.filterMap(f, 1 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("filterMap.05") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: List[Int32] = List.filterMap(f, 4 :: 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2)))
  }

  test("filterMap.06") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: List[Int32] = List.filterMap(f, -9 :: 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("filterMap.07") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: List[Int32] = List.filterMap(f, -8 :: 44 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(-4, 22)))
  }

  test("filterMap.08") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: List[Int32] = List.filterMap(f, -8 :: 44 :: 11 :: 0 :: 4 :: 87 :: 1 :: 4 :: 3 :: -18 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(-4, 22, 0, 2, 2, -9)))
  }

  test("findMap.01") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: Option[Int32] = List.findMap(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNone)
  }

  test("findMap.02") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: Option[Int32] = List.findMap(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNone)
  }

  test("findMap.03") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: Option[Int32] = List.findMap(f, 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(1))
  }

  test("findMap.04") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: Option[Int32] = List.findMap(f, 1 :: 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNone)
  }

  test("findMap.05") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: Option[Int32] = List.findMap(f, 12 :: 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(6))
  }

  test("findMap.06") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: Option[Int32] = List.findMap(f, 11 :: 38 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(19))
  }

  test("findMap.07") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: Option[Int32] = List.findMap(f, 112 :: 38 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(56))
  }

  test("findMap.08") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: Option[Int32] = List.findMap(f, 1 :: 3 :: 5 :: 7 :: 87 :: 112 :: 38 :: 37 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(56))
  }

  test("findMap.09") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: Option[Int32] = List.findMap(f, 12 :: 3 :: 5 :: 7 :: 87 :: 112 :: 38 :: 37 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(6))
  }

  test("toSet.01") {
    val input = "def r: Set[Int32] = List.toSet(Nil)"
    runAnyTest(input, mkSet(List()))
  }

  test("toSet.02") {
    val input = "def r: Set[Int32] = List.toSet(1 :: Nil)"
    runAnyTest(input, mkSet(List(1)))
  }

  test("toSet.03") {
    val input = "def r: Set[Int32] = List.toSet(1 :: 2 :: Nil)"
    runAnyTest(input, mkSet(List(1, 2)))
  }

  test("toSet.04") {
    val input = "def r: Set[Int32] = List.toSet(2 :: 1 :: Nil)"
    runAnyTest(input, mkSet(List(2, 1)))
  }

  test("toSet.05") {
    val input = "def r: Set[Int32] = List.toSet(2 :: 2 :: Nil)"
    runAnyTest(input, mkSet(List(2)))
  }

  test("toSet.06") {
    val input = "def r: Set[Int32] = List.toSet(1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkSet(List(1, 2, 3)))
  }

  test("toSet.07") {
    val input = "def r: Set[Int32] = List.toSet(2 :: 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkSet(List(1, 2, 3)))
  }

  test("toSet.08") {
    val input = "def r: Set[Int32] = List.toSet(11 :: 9 :: -1 :: 3 :: 2 :: 1 :: 2 :: 3 :: 11 :: Nil)"
    runAnyTest(input, mkSet(List(9, -1, 1, 2, 3, 11)))
  }

  test("toSet.09") {
    val input = "def r: Set[Int32] = List.toSet(4 :: 11 :: 9 :: 3 :: 4 :: 1 :: 9 :: -9 :: Nil)"
    runAnyTest(input, mkSet(List(11, 3, 4, 1, 9, -9)))
  }
}
