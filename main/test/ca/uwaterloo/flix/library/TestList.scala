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

  val options = Options.DefaultTest.copy(evaluation=Evaluation.Interpreted)

  def runTest(input: String, output: Int) {
    val flix = new Flix().setOptions(options).addPath("main/src/library/List.flix").addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  def runBoolTest(input: String, output: Boolean) {
    val flix = new Flix().setOptions(options).addPath("main/src/library/List.flix").addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  def runAnyTest(input: String, output: AnyRef) {
    val flix = new Flix().setOptions(options).addPath("main/src/library/List.flix").addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  def mkList(xs: List[Int]): AnyRef = Value.mkList(xs.map(x => new Integer(x)))
  def mkBoolList(xs: List[Boolean]): AnyRef = Value.mkList(xs.map(x => new Boolean(x)))
  def mkBigIntList(xs: List[Int]): AnyRef = Value.mkList(xs.map(x => Value.mkBigInt(x)))
  def mkNil: AnyRef = Value.mkNil

  test("null.01") {
    val input = "def r: Bool = List/null(Nil)"
    runBoolTest(input, true)
  }

  test("null.02") {
    val input = "def r: Bool = List/null(1 :: Nil)"
    runBoolTest(input, false)
  }

  test("null.03") {
    val input = "def r: Bool = List/null(1:: 2 :: Nil)"
    runBoolTest(input, false)
  }

  test("tail.01") {
    val input = "def r: List[Int32] = List/tail(1 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("tail.02") {
    val input = "def r: List[Int32] = List/tail(1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(2)))
  }

  test("tail.03") {
    val input = "def r: List[Int32] = List/tail(1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(2, 3)))
  }

  test("init.01") {
    val input = "def r: List[Int32] = List/init(1 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("init.02") {
    val input = "def r: List[Int32] = List/init(1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("init.03") {
    val input = "def r: List[Int32] = List/init(1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("last.01") {
    val input = "def r: Int32 = List/last(1 :: Nil)"
    runTest(input, 1)
  }

  test("last.02") {
    val input = "def r: Int32 = List/last(1 :: 2 :: Nil)"
    runTest(input, 2)
  }

  test("last.03") {
    val input = "def r: Int32 = List/last(1 :: 2 :: 3 :: Nil)"
    runTest(input, 3)
  }

  test("prefix.01") {
    val input = "def r: List[Int32] = List/prefix(0, Nil)"
    runAnyTest(input, mkNil)
  }

  test("prefix.02") {
    val input = "def r: List[Int32] = List/prefix(0, 1 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("prefix.03") {
    val input = "def r: List[Int32] = List/prefix(1, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("prefix.04") {
    val input = "def r: List[Int32] = List/prefix(0, 1 :: 2 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("prefix.05") {
    val input = "def r: List[Int32] = List/prefix(1, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("prefix.06") {
    val input = "def r: List[Int32] = List/prefix(2, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("prefix.07") {
    val input = "def r: List[Int32] = List/prefix(0, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("prefix.08") {
    val input = "def r: List[Int32] = List/prefix(1, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("prefix.09") {
    val input = "def r: List[Int32] = List/prefix(2, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("prefix.10") {
    val input = "def r: List[Int32] = List/prefix(3, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3)))
  }

  test("suffix.01") {
    val input = "def r: List[Int32] = List/suffix(0, Nil)"
    runAnyTest(input, mkNil)
  }

  test("suffix.02") {
    val input = "def r: List[Int32] = List/suffix(0, 1 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("suffix.03") {
    val input = "def r: List[Int32] = List/suffix(1, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("suffix.04") {
    val input = "def r: List[Int32] = List/suffix(0, 1 :: 2 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("suffix.05") {
    val input = "def r: List[Int32] = List/suffix(1, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(2)))
  }

  test("suffix.06") {
    val input = "def r: List[Int32] = List/suffix(2, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("suffix.07") {
    val input = "def r: List[Int32] = List/suffix(0, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("suffix.08") {
    val input = "def r: List[Int32] = List/suffix(1, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(3)))
  }

  test("suffix.09") {
    val input = "def r: List[Int32] = List/suffix(2, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(2, 3)))
  }

  test("suffix.10") {
    val input = "def r: List[Int32] = List/suffix(3, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3)))
  }

  test("length.01") {
    val input = "def r: Int32 = List/length(Nil)"
    runTest(input, 0)
  }

  test("length.02") {
    val input = "def r: Int32 = List/length(1 :: Nil)"
    runTest(input, 1)
  }

  test("length.03") {
    val input = "def r: Int32 = List/length(1 :: 2 :: Nil)"
    runTest(input, 2)
  }

  test("length.04") {
    val input = "def r: Int32 = List/length(1 :: 2 :: 3 :: Nil)"
    runTest(input, 3)
  }

  test("append.01") {
    val input = "def r: List[Int32] = List/append(Nil, Nil)"
    runAnyTest(input, mkNil)
  }

  test("append.02") {
    val input = "def r: List[Int32] = List/append(Nil, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("append.03") {
    val input = "def r: List[Int32] = List/append(Nil, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("append.04") {
    val input = "def r: List[Int32] = List/append(1 :: 2 :: Nil, Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("append.05") {
    val input = "def r: List[Int32] = List/append(1 :: Nil, 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("append.06") {
    val input = "def r: List[Int32] = List/append(1 :: 2 :: Nil, 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3)))
  }

  test("append.07") {
    val input = "def r: List[Int32] = List/append(1 :: Nil, 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3)))
  }

  test("at.01") {
    val input = "def r: Int32 = List/at(0, 1 :: 2 :: 3 :: Nil)"
    runTest(input, 1)
  }

  test("at.02") {
    val input = "def r: Int32 = List/at(1, 1 :: 2 :: 3 :: Nil)"
    runTest(input, 2)
  }

  test("at.03") {
    val input = "def r: Int32 = List/at(2, 1 :: 2 :: 3 :: Nil)"
    runTest(input, 3)
  }

  test("memberOf.01") {
    val input = "def r: Bool = List/memberOf(0, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, false)
  }

  test("memberOf.02") {
    val input = "def r: Bool = List/memberOf(1, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, true)
  }

  test("memberOf.03") {
    val input = "def r: Bool = List/memberOf(2, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, true)
  }

  test("memberOf.04") {
    val input = "def r: Bool = List/memberOf(3, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, true)
  }

  test("memberOf.05") {
    val input = "def r: Bool = List/memberOf(4, 1 :: 2 :: 3 :: Nil)"
    runBoolTest(input, false)
  }

  test("indexOf.01") {
    val input = "def r: Int32 = List/indexOf(1, 1 :: 2 :: 3 :: Nil)"
    runTest(input, 0)
  }

  test("indexOf.02") {
    val input = "def r: Int32 = List/indexOf(2, 1 :: 2 :: 3 :: Nil)"
    runTest(input, 1)
  }

  test("indexOf.03") {
    val input = "def r: Int32 = List/indexOf(3, 1 :: 2 :: 3 :: Nil)"
    runTest(input, 2)
  }

  test("find.01") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = List/find(f, Nil)
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("find.02") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = List/find(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("find.03") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = List/find(f, 1 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, Value.mkSome(new Integer(2)))
  }

  test("findLeft.01") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = List/findLeft(f, Nil)
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("findLeft.02") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = List/findLeft(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("findLeft.03") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = List/findLeft(f, 1 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, Value.mkSome(new Integer(2)))
  }

  test("findRight.01") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = List/findRight(f, Nil)
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("findRight.02") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = List/findRight(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("findRight.03") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = List/findRight(f, 1 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, Value.mkSome(new Integer(2)))
  }

  test("range.01") {
    val input = "def r: List[Int32] = List/range(4, 4)"
    runAnyTest(input, mkNil)
  }

  test("range.02") {
    val input = "def r: List[Int32] = List/range(4, 5)"
    runAnyTest(input, mkList(List(4)))
  }

  test("range.03") {
    val input = "def r: List[Int32] = List/range(4, 6)"
    runAnyTest(input, mkList(List(4, 5)))
  }

  test("range.04") {
    val input = "def r: List[Int32] = List/range(4, 7)"
    runAnyTest(input, mkList(List(4, 5, 6)))
  }

  test("repeat.01") {
    val input = "def r: List[Int32] = List/repeat(4, 0)"
    runAnyTest(input, mkNil)
  }

  test("repeat.02") {
    val input = "def r: List[Int32] = List/repeat(4, 1)"
    runAnyTest(input, mkList(List(4)))
  }

  test("repeat.03") {
    val input = "def r: List[Int32] = List/repeat(4, 2)"
    runAnyTest(input, mkList(List(4, 4)))
  }

  test("repeat.04") {
    val input = "def r: List[Int32] = List/repeat(4, 3)"
    runAnyTest(input, mkList(List(4, 4, 4)))
  }

  test("repeat.05") {
    val input = "def r: List[BigInt] = List/repeat(4ii, 0)"
    runAnyTest(input, mkNil)
  }

  test("repeat.06") {
    val input = "def r: List[BigInt] = List/repeat(4ii, 1)"
    runAnyTest(input, mkBigIntList(List(4)))
  }

  test("repeat.07") {
    val input = "def r: List[BigInt] = List/repeat(4ii, 2)"
    runAnyTest(input, mkBigIntList(List(4, 4)))
  }

  test("repeat.08") {
    val input = "def r: List[BigInt] = List/repeat(4ii, 3)"
    runAnyTest(input, mkBigIntList(List(4, 4, 4)))
  }

  test("scan.01") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List/scan(f, 1, Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("scan.02") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List/scan(f, 1, false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 3)))
  }

  test("scan.03") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List/scan(f, 1, false :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 3, 5)))
  }

  test("scan.04") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List/scan(f, 1, true :: false :: true :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 2, 4, 5)))
  }

  test("scan.05") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List/scan(f, 1, true :: false :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 2, 4, 6)))
  }

  test("scan.06") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List/scan(f, 1, Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("scan.07") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List/scan(f, 1, 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 4)))
  }

  test("scan.08") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List/scan(f, 1, 3 :: 8 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 4, 12)))
  }

  test("scan.09") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List/scan(f, 1, 3 :: 8 :: 15 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 4, 12, 27)))
  }

  test("scanLeft.01") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List/scanLeft(f, 1, Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("scanLeft.02") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List/scanLeft(f, 1, false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 3)))
  }

  test("scanLeft.03") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List/scanLeft(f, 1, false :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 3, 5)))
  }

  test("scanLeft.04") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List/scanLeft(f, 1, true :: false :: true :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 2, 4, 5)))
  }

  test("scanLeft.05") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List/scanLeft(f, 1, true :: false :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 2, 4, 6)))
  }

  test("scanLeft.06") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List/scanLeft(f, 1, Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("scanLeft.07") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List/scanLeft(f, 1, 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 4)))
  }

  test("scanLeft.08") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List/scanLeft(f, 1, 3 :: 8 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 4, 12)))
  }

  test("scanLeft.09") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List/scanLeft(f, 1, 3 :: 8 :: 15 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 4, 12, 27)))
  }

  test("scanRight.01") {
    val input =
      """def f(b: Bool, i: Int32): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List/scanRight(f, 1, Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("scanRight.02") {
    val input =
      """def f(b: Bool, i: Int32): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List/scanRight(f, 1, false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(3, 1)))
  }

  test("scanRight.03") {
    val input =
      """def f(b: Bool, i: Int32): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List/scanRight(f, 1, false :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(5, 3, 1)))
  }

  test("scanRight.04") {
    val input =
      """def f(b: Bool, i: Int32): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List/scanRight(f, 1, true :: false :: true :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(5, 4, 2, 1)))
  }

  test("scanRight.05") {
    val input =
      """def f(b: Bool, i: Int32): Int32 = if (b) i+1 else i+2
        |def r: List[Int32] = List/scanRight(f, 1, true :: false :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(6, 5, 3, 1)))
  }

  test("scanRight.06") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List/scanRight(f, 1, Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("scanRight.07") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List/scanRight(f, 1, 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(4, 1)))
  }

  test("scanRight.08") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List/scanRight(f, 1, 3 :: 8 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(12, 9, 1)))
  }

  test("scanRight.09") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List/scanRight(f, 1, 3 :: 8 :: 15 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(27, 24, 16, 1)))
  }

  test("map.01") {
    val input =
      """def f(i: Int32): Bool = i == 2
        |def r: List[Bool] = List/map(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("map.02") {
    val input =
      """def f(i: Int32): Bool = i == 2
        |def r: List[Bool] = List/map(f, 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkBoolList(List(true)))
  }

  test("map.03") {
    val input =
      """def f(i: Int32): Bool = i == 2
        |def r: List[Bool] = List/map(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkBoolList(List(false)))
  }

  test("map.04") {
    val input =
      """def f(i: Int32): Bool = i == 2
        |def r: List[Bool] = List/map(f, 1 :: 2 :: 3 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkBoolList(List(false, true, false, true)))
  }

  test("map.05") {
    val input =
      """def f(i: Int32): Bool = i == 2
        |def r: List[Bool] = List/map(f, 2 :: 1 :: 2 :: 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkBoolList(List(true, false, true, false)))
  }

  test("mapWithIndex.01") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List/mapWithIndex(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("mapWithIndex.02") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List/mapWithIndex(f, 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2)))
  }

  test("mapWithIndex.03") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List/mapWithIndex(f, 5 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(5, 3)))
  }

  test("mapWithIndex.04") {
    val input =
      """def f(i1: Int32, i2: Int32): Int32 = i1 + i2
        |def r: List[Int32] = List/mapWithIndex(f, 11 :: 5 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(11, 6, 4)))
  }

  test("flatMap.01") {
    val input =
      """def f(i: Int32): List[Int32] = List/repeat(i, i)
        |def r: List[Int32] = List/flatMap(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("flatMap.02") {
    val input =
      """def f(i: Int32): List[Int32] = List/repeat(i, i)
        |def r: List[Int32] = List/flatMap(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("flatMap.03") {
    val input =
      """def f(i: Int32): List[Int32] = List/repeat(i, i)
        |def r: List[Int32] = List/flatMap(f, 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(3, 3, 3)))
  }

  test("flatMap.04") {
    val input =
      """def f(i: Int32): List[Int32] = List/repeat(i, i)
        |def r: List[Int32] = List/flatMap(f, 2 :: 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2, 2, 1)))
  }

  test("flatMap.05") {
    val input =
      """def f(i: Int32): List[Int32] = List/repeat(i, i)
        |def r: List[Int32] = List/flatMap(f, 2 :: 1 :: 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2, 2, 1, 3, 3, 3)))
  }

  test("concatMap.01") {
    val input =
      """def f(i: Int32): List[Int32] = List/repeat(i, i)
        |def r: List[Int32] = List/concatMap(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("concatMap.02") {
    val input =
      """def f(i: Int32): List[Int32] = List/repeat(i, i)
        |def r: List[Int32] = List/concatMap(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("concatMap.03") {
    val input =
      """def f(i: Int32): List[Int32] = List/repeat(i, i)
        |def r: List[Int32] = List/concatMap(f, 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(3, 3, 3)))
  }

  test("concatMap.04") {
    val input =
      """def f(i: Int32): List[Int32] = List/repeat(i, i)
        |def r: List[Int32] = List/concatMap(f, 2 :: 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2, 2, 1)))
  }

  test("concatMap.05") {
    val input =
      """def f(i: Int32): List[Int32] = List/repeat(i, i)
        |def r: List[Int32] = List/concatMap(f, 2 :: 1 :: 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2, 2, 1, 3, 3, 3)))
  }

  test("reverse.01") {
    val input = "def r: List[Int32] = List/reverse(Nil)"
    runAnyTest(input, mkNil)
  }

  test("reverse.02") {
    val input = "def r: List[Int32] = List/reverse(1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("reverse.03") {
    val input = "def r: List[Int32] = List/reverse(1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(2, 1)))
  }

  test("reverse.04") {
    val input = "def r: List[Int32] = List/reverse(1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(3, 2, 1)))
  }

  test("reverse.05") {
    val input = "def r: List[Bool] = List/reverse(Nil)"
    runAnyTest(input, mkNil)
  }

  test("reverse.06") {
    val input = "def r: List[Bool] = List/reverse(true :: Nil)"
    runAnyTest(input, mkBoolList(List(true)))
  }

  test("reverse.07") {
    val input = "def r: List[Bool] = List/reverse(true :: false :: Nil)"
    runAnyTest(input, mkBoolList(List(false, true)))
  }

  test("reverse.08") {
    val input = "def r: List[Bool] = List/reverse(true :: false :: false :: Nil)"
    runAnyTest(input, mkBoolList(List(false, false, true)))
  }

  test("rotateLeft.01") {
    val input = "def r: List[Int32] = List/rotateLeft(0, Nil)"
    runAnyTest(input, mkNil)
  }

  test("rotateLeft.02") {
    val input = "def r: List[Int32] = List/rotateLeft(0, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("rotateLeft.03") {
    val input = "def r: List[Int32] = List/rotateLeft(1, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("rotateLeft.04") {
    val input = "def r: List[Int32] = List/rotateLeft(0, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("rotateLeft.05") {
    val input = "def r: List[Int32] = List/rotateLeft(1, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(2, 1)))
  }

  test("rotateLeft.06") {
    val input = "def r: List[Int32] = List/rotateLeft(2, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("rotateLeft.07") {
    val input = "def r: List[Int32] = List/rotateLeft(0, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3)))
  }

  test("rotateLeft.08") {
    val input = "def r: List[Int32] = List/rotateLeft(1, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(2, 3, 1)))
  }

  test("rotateLeft.09") {
    val input = "def r: List[Int32] = List/rotateLeft(2, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(3, 1, 2)))
  }

  test("rotateLeft.10") {
    val input = "def r: List[Int32] = List/rotateLeft(3, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3)))
  }

  test("rotateRight.01") {
    val input = "def r: List[Int32] = List/rotateRight(0, Nil)"
    runAnyTest(input, mkNil)
  }

  test("rotateRight.02") {
    val input = "def r: List[Int32] = List/rotateRight(0, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("rotateRight.03") {
    val input = "def r: List[Int32] = List/rotateRight(1, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("rotateRight.04") {
    val input = "def r: List[Int32] = List/rotateRight(0, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("rotateRight.05") {
    val input = "def r: List[Int32] = List/rotateRight(1, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(2, 1)))
  }

  test("rotateRight.06") {
    val input = "def r: List[Int32] = List/rotateRight(2, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("rotateRight.07") {
    val input = "def r: List[Int32] = List/rotateRight(0, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3)))
  }

  test("rotateRight.08") {
    val input = "def r: List[Int32] = List/rotateRight(1, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(3, 1, 2)))
  }

  test("rotateRight.09") {
    val input = "def r: List[Int32] = List/rotateRight(2, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(2, 3, 1)))
  }

  test("rotateRight.10") {
    val input = "def r: List[Int32] = List/rotateRight(3, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3)))
  }

  test("replace.01") {
    val input = "def r: List[Int32] = List/replace(0, 2, 1 :: Nil)"
    runAnyTest(input, mkList(List(2)))
  }

  test("replace.02") {
    val input = "def r: List[Int32] = List/replace(0, 3, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(3, 2)))
  }

  test("replace.03") {
    val input = "def r: List[Int32] = List/replace(1, 3, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 3)))
  }

  test("replace.04") {
    val input = "def r: List[Int32] = List/replace(0, 4, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(4, 2, 3)))
  }

  test("replace.05") {
    val input = "def r: List[Int32] = List/replace(1, 4, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 4, 3)))
  }

  test("replace.06") {
    val input = "def r: List[Int32] = List/replace(2, 4, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 4)))
  }

  test("patch.01") {
    val input = "def r: List[Int32] = List/patch(0, 0, Nil, Nil)"
    runAnyTest(input, mkNil)
  }

  test("patch.02") {
    val input = "def r: List[Int32] = List/patch(0, 0, Nil, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("patch.03") {
    val input = "def r: List[Int32] = List/patch(1, 0, 2 :: Nil, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("patch.04") {
    val input = "def r: List[Int32] = List/patch(0, 1, 3 :: Nil, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(3, 2)))
  }

  test("patch.05") {
    val input = "def r: List[Int32] = List/patch(1, 1, 3 :: Nil, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 3)))
  }

  test("patch.06") {
    val input = "def r: List[Int32] = List/patch(0, 1, 4 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(4, 2, 3)))
  }

  test("patch.07") {
    val input = "def r: List[Int32] = List/patch(1, 1, 4 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 4, 3)))
  }

  test("patch.08") {
    val input = "def r: List[Int32] = List/patch(2, 1, 4 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 4)))
  }

  test("patch.09") {
    val input = "def r: List[Int32] = List/patch(0, 2, 4 :: 5 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(4, 5, 3)))
  }

  test("patch.10") {
    val input = "def r: List[Int32] = List/patch(1, 2, 4 :: 5 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(1, 4, 5)))
  }

  test("patch.11") {
    val input = "def r: List[Int32] = List/patch(0, 2, 4 :: 5 :: 6 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(4, 5, 3)))
  }

  test("patch.13") {
    val input = "def r: List[Int32] = List/patch(0, 3, 4 :: 5 :: 6 :: Nil, 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkList(List(4, 5, 6)))
  }

  test("patch.14") {
    val input = "def r: List[Int32] = List/patch(2, 4, 14 :: 15 :: 16 :: 17 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 14, 15, 16, 17, 7)))
  }
}