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
    val v1 = output
    val v2 = flix.solve().get.getConstant("r")
    assert(Value.equal(v1, v2), s"v1 = $v1, v2 = $v2")
  }

  test("null.01") {
    val input = "def r: Bool = Option.null(None)"
    runBoolTest(input, true)
  }

  test("null.02") {
    val input = "def r: Bool = Option.null(Some(32))"
    runBoolTest(input, false)
  }

  test("null.03") {
    val input = "def r: Bool = Option.null(Some(888ii))"
    runBoolTest(input, false)
  }

  test("get.01") {
    val input = "def r: Int32 = Option.get(Some(32))"
    runTest(input, 32)
  }

  test("get.02") {
    val input = "def r: BigInt = Option.get(Some(32ii))"
    runAnyTest(input, Value.mkBigInt(32))
  }

  test("getWithDefault.01") {
    val input = "def r: Int32 = Option.getWithDefault(None, 32)"
    runTest(input, 32)
  }

  test("getWithDefault.02") {
    val input = "def r: Int32 = Option.getWithDefault(Some(30), 32)"
    runTest(input, 30)
  }

  test("getWithDefault.03") {
    val input = "def r: BigInt = Option.getWithDefault(None, 32ii)"
    runAnyTest(input, Value.mkBigInt(32))
  }

  test("getWithDefault.04") {
    val input = "def r: BigInt = Option.getWithDefault(Some(30ii), 32ii)"
    runAnyTest(input, Value.mkBigInt(30))
  }

  test("exists.01") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Bool = Option.exists(f, None)
      """.stripMargin
    runBoolTest(input, false)
  }

  test("exists.02") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Bool = Option.exists(f, Some(1))
      """.stripMargin
    runBoolTest(input, false)
  }

  test("exists.03") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Bool = Option.exists(f, Some(2))
      """.stripMargin
    runBoolTest(input, true)
  }

  test("forall.01") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Bool = Option.forall(f, None)
      """.stripMargin
    runBoolTest(input, true)
  }

  test("forall.02") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Bool = Option.forall(f, Some(1))
      """.stripMargin
    runBoolTest(input, false)
  }

  test("forall.03") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Bool = Option.forall(f, Some(2))
      """.stripMargin
    runBoolTest(input, true)
  }

  test("filter.01") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = Option.filter(f, None)
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("filter.02") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = Option.filter(f, Some(3))
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("filter.03") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = Option.filter(f, Some(2))
      """.stripMargin
    runAnyTest(input, Value.mkSome(new Integer(2)))
  }

  test("map.01") {
    val input =
      """def f(i: Int32): Int32 = 2*i
        |def r: Option[Int32] = Option.map(f, None)
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("map.02") {
    val input =
      """def f(i: Int32): Int32 = 2*i
        |def r: Option[Int32] = Option.map(f, Some(2))
      """.stripMargin
    runAnyTest(input, Value.mkSome(new Integer(4)))
  }

  test("map.03") {
    val input =
      """def f(i: Int32): Bool = true
        |def r: Option[Bool] = Option.map(f, Some(2))
      """.stripMargin
    runAnyTest(input, Value.mkSome(new Boolean(true)))
  }

  test("map2.01") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) 2*i else 3*i
        |def r: Option[Int32] = Option.map2(f, None, Some(true))
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("map2.02") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) 2*i else 3*i
        |def r: Option[Int32] = Option.map2(f, Some(1), None)
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("map2.03") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) 2*i else 3*i
        |def r: Option[Int32] = Option.map2(f, Some(1), Some(true))
      """.stripMargin
    runAnyTest(input, Value.mkSome(new Integer(2)))
  }

  test("map2.04") {
    val input =
      """def f(i: Int32, b: Bool): Int32 = if (b) 2*i else 3*i
        |def r: Option[Int32] = Option.map2(f, Some(1), Some(false))
      """.stripMargin
    runAnyTest(input, Value.mkSome(new Integer(3)))
  }

  test("flatMap.01") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i == 0) None else Some(2*i)
        |def r: Option[Int32] = Option.flatMap(f, None)
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("flatMap.02") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i == 0) None else Some(2*i)
        |def r: Option[Int32] = Option.flatMap(f, Some(0))
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("flatMap.03") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i == 0) None else Some(2*i)
        |def r: Option[Int32] = Option.flatMap(f, Some(2))
      """.stripMargin
    runAnyTest(input, Value.mkSome(new Integer(4)))
  }

  test("flatMap2.01") {
    val input =
      """def f(i: Int32, b: Bool): Option[Int32] = if (b) Some(i) else None
        |def r: Option[Int32] = Option.flatMap2(f, None, Some(true))
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("flatMap2.02") {
    val input =
      """def f(i: Int32, b: Bool): Option[Int32] = if (b) Some(i) else None
        |def r: Option[Int32] = Option.flatMap2(f, Some(1), None)
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("flatMap2.03") {
    val input =
      """def f(i: Int32, b: Bool): Option[Int32] = if (b) Some(i) else None
        |def r: Option[Int32] = Option.flatMap2(f, Some(5), Some(false))
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("flatMap2.04") {
    val input =
      """def f(i: Int32, b: Bool): Option[Int32] = if (b) Some(i) else None
        |def r: Option[Int32] = Option.flatMap2(f, Some(5), Some(true))
      """.stripMargin
    runAnyTest(input, Value.mkSome(new Integer(5)))
  }

  test("toList.01") {
    val input = "def r: List[Int32] = Option.toList(None)"
    runAnyTest(input, Value.mkNil)
  }

  test("toList.02") {
    val input = "def r: List[Int32] = Option.toList(Some(11))"
    runAnyTest(input, Value.mkList(List(new Integer(11))))
  }

  // TODO
  ignore("toSet.01") {
    val input = "def r: Set[Int32] = Option.toSet(None)"
    runAnyTest(input, Set())
  }

  // TODO
  ignore("toSet.02") {
    val input = "def r: Set[Int32] = Option.toSet(Some(4))"
    runAnyTest(input, Set(4))
  }

  test("withDefault.01") {
    val input = "def r: Option[Int32] = Option.withDefault(None, None)"
    runAnyTest(input, Value.mkNone())
  }

  test("withDefault.02") {
    val input = "def r: Option[Int32] = Option.withDefault(None, Some(4))"
    runAnyTest(input, Value.mkSome(new Integer(4)))
  }

  test("withDefault.03") {
    val input = "def r: Option[Int32] = Option.withDefault(Some(2), Some(4))"
    runAnyTest(input, Value.mkSome(new Integer(2)))
  }

  test("toOk.01") {
    val input = "def r: Result[Bool, Int32] = Option.toOk(None, 3)"
    runAnyTest(input, Value.mkErr(new Integer(3)))
  }

  test("toOk.02") {
    val input = "def r: Result[Bool, Int32] = Option.toOk(Some(true), 3)"
    runAnyTest(input, Value.mkOk(new Boolean(true)))
  }

  test("toErr.01") {
    val input = "def r: Result[Bool, Int32] = Option.toErr(None, true)"
    runAnyTest(input, Value.mkOk(new Boolean(true)))
  }

  test("toErr.02") {
    val input = "def r: Result[Bool, Int32] = Option.toErr(Some(2), true)"
    runAnyTest(input, Value.mkErr(new Integer(2)))
  }

  test("count.01") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Int32 = Option.count(f, None)
      """.stripMargin
    runTest(input, 0)
  }

  test("count.02") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Int32 = Option.count(f, Some(1))
      """.stripMargin
    runTest(input, 0)
  }

  test("count.03") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Int32 = Option.count(f, Some(2))
      """.stripMargin
    runTest(input, 1)
  }

  test("find.01") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = Option.find(f, None)
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("find.02") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = Option.find(f, Some(1))
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("find.03") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = Option.find(f, Some(2))
      """.stripMargin
    runAnyTest(input, Value.mkSome(new Integer(2)))
  }

  test("flatten.01") {
    val input = "def r: Option[Int32] = Option.flatten(None)"
    runAnyTest(input, Value.mkNone())
  }

  test("flatten.02") {
    val input = "def r: Option[Int32] = Option.flatten(Some(None))"
    runAnyTest(input, Value.mkNone())
  }

  test("flatten.03") {
    val input = "def r: Option[Int32] = Option.flatten(Some(Some(1)))"
    runAnyTest(input, Value.mkSome(new Integer(1)))
  }

  test("foldLeft.01") {
    val input =
      """def f(b: Bool, i: Int32): Bool = if (i == 2 && b) true else false
        |def r: Bool = Option.foldLeft(f, false, None)
      """.stripMargin
    runBoolTest(input, false)
  }

  test("foldLeft.02") {
    val input =
      """def f(b: Bool, i: Int32): Bool = if (i == 2 && b) true else false
        |def r: Bool = Option.foldLeft(f, false, Some(1))
      """.stripMargin
    runBoolTest(input, false)
  }

  test("foldLeft.03") {
    val input =
      """def f(b: Bool, i: Int32): Bool = if (i == 2 && b) true else false
        |def r: Bool = Option.foldLeft(f, false, Some(2))
      """.stripMargin
    runBoolTest(input, false)
  }

  test("foldLeft.04") {
    val input =
      """def f(b: Bool, i: Int32): Bool = if (i == 2 && b) true else false
        |def r: Bool = Option.foldLeft(f, true, Some(1))
      """.stripMargin
    runBoolTest(input, false)
  }

  test("foldLeft.05") {
    val input =
      """def f(b: Bool, i: Int32): Bool = if (i == 2 && b) true else false
        |def r: Bool = Option.foldLeft(f, true, Some(2))
      """.stripMargin
    runBoolTest(input, true)
  }

  test("foldRight.01") {
    val input =
      """def f(i: Int32, b: Bool): Bool = if (i == 2 && b) true else false
        |def r: Bool = Option.foldRight(f, None, false)
      """.stripMargin
    runBoolTest(input, false)
  }

  test("foldRight.02") {
    val input =
      """def f(i: Int32, b: Bool): Bool = if (i == 2 && b) true else false
        |def r: Bool = Option.foldRight(f, Some(1), false)
      """.stripMargin
    runBoolTest(input, false)
  }

  test("foldRight.03") {
    val input =
      """def f(i: Int32, b: Bool): Bool = if (i == 2 && b) true else false
        |def r: Bool = Option.foldRight(f, Some(2), false)
      """.stripMargin
    runBoolTest(input, false)
  }

  test("foldRight.04") {
    val input =
      """def f(i: Int32, b: Bool): Bool = if (i == 2 && b) true else false
        |def r: Bool = Option.foldRight(f, Some(1), true)
      """.stripMargin
    runBoolTest(input, false)
  }

  test("foldRight.05") {
    val input =
      """def f(i: Int32, b: Bool): Bool = if (i == 2 && b) true else false
        |def r: Bool = Option.foldRight(f, Some(2), true)
      """.stripMargin
    runBoolTest(input, true)
  }

  test("isNone.01") {
    val input = "def r: Bool = Option.isNone(None)"
    runBoolTest(input, true)
  }

  test("isNone.02") {
    val input = "def r: Bool = Option.isNone(Some(1))"
    runBoolTest(input, false)
  }

  test("isSome.01") {
    val input = "def r: Bool = Option.isSome(None)"
    runBoolTest(input, false)
  }

  test("isSome.02") {
    val input = "def r: Bool = Option.isSome(Some(1))"
    runBoolTest(input, true)
  }

  test("zip.01") {
    val input = "def r: Option[(Int32, Bool)] = Option.zip(None, None)"
    runAnyTest(input, Value.mkNone())
  }

  test("zip.02") {
    val input = "def r: Option[(Int32, Bool)] = Option.zip(None, Some(true))"
    runAnyTest(input, Value.mkNone())
  }

  test("zip.03") {
    val input = "def r: Option[(Int32, Bool)] = Option.zip(Some(1), None)"
    runAnyTest(input, Value.mkNone())
  }

  test("zip.04") {
    val input = "def r: Option[(Int32, Int32)] = Option.zip(Some(1), Some(2))"
    runAnyTest(input, Value.mkSome(Array(new Integer(1), new Integer(2))))
  }

  test("unzip.01") {
    val input = "def r: (Option[Int32], Option[Bool]) = Option.unzip(None)"
    runAnyTest(input, Array(Value.mkNone(), Value.mkNone()))
  }

  test("unzip.02") {
    val input = "def r: (Option[Int32], Option[Bool]) = Option.unzip(Some((1, true)))"
    runAnyTest(input, Array(Value.mkSome(new Integer(1)), Value.mkSome(new Boolean(true))))
  }
}