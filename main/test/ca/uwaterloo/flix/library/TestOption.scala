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

import java.lang.Boolean

class TestOption extends FunSuite {

  val options = Options.DefaultTest.copy(evaluation=Evaluation.Interpreted)

  def runTest(input: String, output: Int) {
    val flix = new Flix().setOptions(options).addPath("main/src/library/Option.flix").addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  def runBoolTest(input: String, output: Boolean) {
    val flix = new Flix().setOptions(options).addPath("main/src/library/Option.flix").addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  def runAnyTest(input: String, output: AnyRef) {
    val flix = new Flix().setOptions(options).addPath("main/src/library/Option.flix").addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  test("null.01") {
    val input = "def r: Bool = null(None)"
    runBoolTest(input, true)
  }

  test("null.02") {
    val input = "def r: Bool = null(Some(32))"
    runBoolTest(input, false)
  }

  test("null.03") {
    val input = "def r: Bool = null(Some(888ii))"
    runBoolTest(input, false)
  }

  test("get.01") {
    val input = "def r: Int32 = get(Some(32))"
    runTest(input, 32)
  }

  test("get.02") {
    val input = "def r: BigInt = get(Some(32ii))"
    runAnyTest(input, Value.mkBigInt(32))
  }

  test("getWithDefault.01") {
    val input = "def r: Int32 = getWithDefault(None, 32)"
    runTest(input, 32)
  }

  test("getWithDefault.02") {
    val input = "def r: Int32 = getWithDefault(Some(30), 32)"
    runTest(input, 30)
  }

  test("getWithDefault.03") {
    val input = "def r: BigInt = getWithDefault(None, 32ii)"
    runAnyTest(input, Value.mkBigInt(32))
  }

  test("getWithDefault.04") {
    val input = "def r: BigInt = getWithDefault(Some(30ii), 32ii)"
    runAnyTest(input, Value.mkBigInt(30))
  }

  test("exists.01") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Bool = exists(f, None)
      """.stripMargin
    runBoolTest(input, false)
  }

  test("exists.02") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Bool = exists(f, Some(1))
      """.stripMargin
    runBoolTest(input, false)
  }

  test("exists.03") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Bool = exists(f, Some(2))
      """.stripMargin
    runBoolTest(input, true)
  }

  test("forall.01") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Bool = forall(f, None)
      """.stripMargin
    runBoolTest(input, true)
  }

  test("forall.02") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Bool = forall(f, Some(1))
      """.stripMargin
    runBoolTest(input, false)
  }

  test("forall.03") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Bool = forall(f, Some(2))
      """.stripMargin
    runBoolTest(input, true)
  }

  test("filter.01") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = filter(f, None)
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("filter.02") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = filter(f, Some(3))
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("filter.03") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = filter(f, Some(2))
      """.stripMargin
    runAnyTest(input, Value.mkSome(new Integer(2)))
  }

  test("map.01") {
    val input =
      """def f(i: Int32): Int32 = 2*i
        |def r: Option[Int32] = map(f, None)
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("map.02") {
    val input =
      """def f(i: Int32): Int32 = 2*i
        |def r: Option[Int32] = map(f, Some(2))
      """.stripMargin
    runAnyTest(input, Value.mkSome(new Integer(4)))
  }

  test("map.03") {
    val input =
      """def f(i: Int32): Bool = true
        |def r: Option[Bool] = map(f, Some(2))
      """.stripMargin
    runAnyTest(input, Value.mkSome(new Boolean(true)))
  }

  test("map2.01") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) 2*i else 3*i
        |def r: Option[Int32] = map2(f, None, Some(true))
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("map2.02") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) 2*i else 3*i
        |def r: Option[Int32] = map2(f, Some(1), None)
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("map2.03") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) 2*i else 3*i
        |def r: Option[Int32] = map2(f, Some(1), Some(true))
      """.stripMargin
    runAnyTest(input, Value.mkSome(new Integer(2)))
  }

  test("map2.04") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) 2*i else 3*i
        |def r: Option[Int32] = map2(f, Some(1), Some(false))
      """.stripMargin
    runAnyTest(input, Value.mkSome(new Integer(3)))
  }

  test("flatMap.01") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i == 0) None else Some(2*i)
        |def r: Option[Int32] = flatMap(f, None)
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("flatMap.02") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i == 0) None else Some(2*i)
        |def r: Option[Int32] = flatMap(f, Some(0))
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("flatMap.03") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i == 0) None else Some(2*i)
        |def r: Option[Int32] = flatMap(f, Some(2))
      """.stripMargin
    runAnyTest(input, Value.mkSome(new Integer(4)))
  }

  test("flatMap2.01") {
    val input =
      """def f(i: Int32, b: Bool): Option[Int32] = if (b) Some(i) else None
        |def r: Option[Int32] = flatMap2(f, None, Some(true))
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("flatMap2.02") {
    val input =
      """def f(i: Int32, b: Bool): Option[Int32] = if (b) Some(i) else None
        |def r: Option[Int32] = flatMap2(f, Some(1), None)
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("flatMap2.03") {
    val input =
      """def f(i: Int32, b: Bool): Option[Int32] = if (b) Some(i) else None
        |def r: Option[Int32] = flatMap2(f, Some(5), Some(false))
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("flatMap2.04") {
    val input =
      """def f(i: Int32, b: Bool): Option[Int32] = if (b) Some(i) else None
        |def r: Option[Int32] = flatMap2(f, Some(5), Some(true))
      """.stripMargin
    runAnyTest(input, Value.mkSome(new Integer(5)))
  }

  test("toList.01") {
    val input = "def r: List[Int32] = toList(None)"
    runAnyTest(input, Value.mkList(List()))
  }

  test("toList.02") {
    val input = "def r: List[Int32] = toList(Some(11))"
    runAnyTest(input, Value.mkList(List(new Integer(11))))
  }

  test("toSet.01") {
    val input = "def r: Set[Int32] = toSet(None)"
    runAnyTest(input, Set())
  }

  test("toSet.02") {
    val input = "def r: Set[Int32] = toSet(Some(4))"
    runAnyTest(input, Set(4))
  }

  test("withDefault.01") {
    val input = "def r: Option[Int32] = withDefault(None, None)"
    runAnyTest(input, Value.mkNone())
  }

  test("withDefault.02") {
    val input = "def r: Option[Int32] = withDefault(None, Some(4))"
    runAnyTest(input, Value.mkSome(new Integer(4)))
  }

  test("withDefault.03") {
    val input = "def r: Option[Int32] = withDefault(Some(2), Some(4))"
    runAnyTest(input, Value.mkSome(new Integer(2)))
  }

  test("toResult.01") {
    val input = "def r: Result[Int32, Bool] = toResult(None, false)"
    runAnyTest(input, Value.mkErr(new Boolean(false)))
  }

  test("toResult.02") {
    val input = "def r: Result[Int32, Bool] = toResult(Some(5), false)"
    runAnyTest(input, Value.mkOk(new Integer(5)))
  }
}