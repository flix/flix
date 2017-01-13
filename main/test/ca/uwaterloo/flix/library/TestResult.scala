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

class TestResult extends FunSuite {

  val options = Options.DefaultTest

  def runTest(input: String, output: Int) {
    val flix = new Flix().setOptions(options).addPath("main/src/library/Result.flix").addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  def runBoolTest(input: String, output: Boolean) {
    val flix = new Flix().setOptions(options).addPath("main/src/library/Result.flix").addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  def runAnyTest(input: String, output: AnyRef) {
    val flix = new Flix().setOptions(options).addPath("main/src/library/Result.flix").addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  test("isOk.01") {
    val input = "def r: Bool = Result.isOk(Ok(1))"
    runBoolTest(input, true)
  }

  test("isOk.02") {
    val input = "def r: Bool = Result.isOk(Err(0))"
    runBoolTest(input, false)
  }

  test("isErr.01") {
    val input = "def r: Bool = Result.isErr(Ok(1))"
    runBoolTest(input, false)
  }

  test("isErr.02") {
    val input = "def r: Bool = Result.isErr(Err(0))"
    runBoolTest(input, true)
  }

  test("get.01") {
    val input = "def r: Int32 = Result.get(Ok(1))"
    runTest(input, 1)
  }

  test("getWithDefault.01") {
    val input = "def r: Int32 = Result.getWithDefault(Ok(1), 2)"
    runTest(input, 1)
  }

  test("getWithDefault.02") {
    val input = "def r: Int32 = Result.getWithDefault(Err(1), 2)"
    runTest(input, 2)
  }

  test("withDefault.01") {
    val input = "def r: Result[Int32, BigInt] = Result.withDefault(Ok(1), Ok(2))"
    runAnyTest(input, Value.mkOk(new Integer(1)))
  }

  test("withDefault.02") {
    val input = "def r: Result[Int32, BigInt] = Result.withDefault(Ok(1), Err(2ii))"
    runAnyTest(input, Value.mkOk(new Integer(1)))
  }

  test("withDefault.03") {
    val input = "def r: Result[Int32, BigInt] = Result.withDefault(Err(true), Ok(2))"
    runAnyTest(input, Value.mkOk(new Integer(2)))
  }

  test("withDefault.04") {
    val input = "def r: Result[Int32, BigInt] = Result.withDefault(Err(true), Err(2ii))"
    runAnyTest(input, Value.mkErr(Value.mkBigInt(2)))
  }

  test("exists.01") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Bool = Result.exists(f, Err(false))
      """.stripMargin
    runBoolTest(input, false)
  }

  test("exists.02") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Bool = Result.exists(f, Ok(1))
      """.stripMargin
    runBoolTest(input, false)
  }

  test("exists.03") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Bool = Result.exists(f, Ok(2))
      """.stripMargin
    runBoolTest(input, true)
  }

  test("forall.01") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Bool = Result.forall(f, Err(false))
      """.stripMargin
    runBoolTest(input, true)
  }

  test("forall.02") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Bool = Result.forall(f, Ok(1))
      """.stripMargin
    runBoolTest(input, false)
  }

  test("forall.03") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Bool = Result.forall(f, Ok(2))
      """.stripMargin
    runBoolTest(input, true)
  }

  test("map.01") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Result[Bool, Int32] = Result.map(f, Err(0))
      """.stripMargin
    runAnyTest(input, Value.mkErr(new Integer(0)))
  }

  test("map.02") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Result[Bool, Int32] = Result.map(f, Ok(1))
      """.stripMargin
    runAnyTest(input, Value.mkOk(new Boolean(false)))
  }

  test("map.03") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Result[Bool, Int32] = Result.map(f, Ok(2))
      """.stripMargin
    runAnyTest(input, Value.mkOk(new Boolean(true)))
  }

  test("flatMap.01") {
    val input =
      """def f(i: Int32): Result[Bool, Bool] = if (i > 0) Ok(true) else Err(false)
        |def r: Result[Bool, Bool] = Result.flatMap(f, Err(false))
      """.stripMargin
    runAnyTest(input, Value.mkErr(new Boolean(false)))
  }

  test("flatMap.02") {
    val input =
      """def f(i: Int32): Result[Bool, Bool] = if (i > 0) Ok(true) else Err(false)
        |def r: Result[Bool, Bool] = Result.flatMap(f, Ok(0))
      """.stripMargin
    runAnyTest(input, Value.mkErr(new Boolean(false)))
  }

  test("flatMap.03") {
    val input =
      """def f(i: Int32): Result[Bool, Bool] = if (i > 0) Ok(true) else Err(false)
        |def r: Result[Bool, Bool] = Result.flatMap(f, Ok(1))
      """.stripMargin
    runAnyTest(input, Value.mkOk(new Boolean(true)))
  }

  test("and.01") {
    val input = "def r: Result[Int32, Int32] = Result.and(Ok(1), Ok(2))"
    runAnyTest(input, Value.mkOk(new Integer(2)))
  }

  test("and.02") {
    val input = "def r: Result[Int32, Int32] = Result.and(Ok(1), Err(2))"
    runAnyTest(input, Value.mkErr(new Integer(2)))
  }

  test("and.03") {
    val input = "def r: Result[Int32, Int32] = Result.and(Err(1), Ok(2))"
    runAnyTest(input, Value.mkErr(new Integer(1)))
  }

  test("and.04") {
    val input = "def r: Result[Int32, Int32] = Result.and(Err(1), Err(2))"
    runAnyTest(input, Value.mkErr(new Integer(1)))
  }

  test("or.01") {
    val input = "def r: Result[Int32, Int32] = Result.or(Ok(1), Ok(2))"
    runAnyTest(input, Value.mkOk(new Integer(1)))
  }

  test("or.02") {
    val input = "def r: Result[Int32, Int32] = Result.or(Ok(1), Err(2))"
    runAnyTest(input, Value.mkOk(new Integer(1)))
  }

  test("or.03") {
    val input = "def r: Result[Int32, Int32] = Result.or(Err(1), Ok(2))"
    runAnyTest(input, Value.mkOk(new Integer(2)))
  }

  test("or.04") {
    val input = "def r: Result[Int32, Int32] = Result.or(Err(1), Err(2))"
    runAnyTest(input, Value.mkErr(new Integer(2)))
  }

  test("count.01") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Int32 = Result.count(f, Err(2))
      """.stripMargin
    runTest(input, 0)
  }

  test("count.02") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Int32 = Result.count(f, Ok(1))
      """.stripMargin
    runTest(input, 0)
  }

  test("count.03") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Int32 = Result.count(f, Ok(2))
      """.stripMargin
    runTest(input, 1)
  }

  test("find.01") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = Result.find(f, Err(true))
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("find.02") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = Result.find(f, Ok(1))
      """.stripMargin
    runAnyTest(input, Value.mkNone())
  }

  test("find.03") {
    val input =
      """def f(i: Int32): Bool = if (i == 2) true else false
        |def r: Option[Int32] = Result.find(f, Ok(2))
      """.stripMargin
    runAnyTest(input, Value.mkSome(new Integer(2)))
  }

  test("foldLeft.01") {
    val input =
      """def f(b: Bool, i: Int32): Bool = if (i == 2 && b) true else false
        |def r: Bool = Result.foldLeft(f, false, Err(0))
      """.stripMargin
    runBoolTest(input, false)
  }

  test("foldLeft.02") {
    val input =
      """def f(b: Bool, i: Int32): Bool = if (i == 2 && b) true else false
        |def r: Bool = Result.foldLeft(f, false, Ok(1))
      """.stripMargin
    runBoolTest(input, false)
  }

  test("foldLeft.03") {
    val input =
      """def f(b: Bool, i: Int32): Bool = if (i == 2 && b) true else false
        |def r: Bool = Result.foldLeft(f, true, Ok(1))
      """.stripMargin
    runBoolTest(input, false)
  }

  test("foldLeft.04") {
    val input =
      """def f(b: Bool, i: Int32): Bool = if (i == 2 && b) true else false
        |def r: Bool = Result.foldLeft(f, false, Ok(2))
      """.stripMargin
    runBoolTest(input, false)
  }

  test("foldLeft.05") {
    val input =
      """def f(b: Bool, i: Int32): Bool = if (i == 2 && b) true else false
        |def r: Bool = Result.foldLeft(f, true, Ok(2))
      """.stripMargin
    runBoolTest(input, true)
  }

  test("foldRight.01") {
    val input =
      """def f(i: Int32, b: Bool): Bool = if (i == 2 && b) true else false
        |def r: Bool = Result.foldRight(f, Err(0), false)
      """.stripMargin
    runBoolTest(input, false)
  }

  test("foldRight.02") {
    val input =
      """def f(i: Int32, b: Bool): Bool = if (i == 2 && b) true else false
        |def r: Bool = Result.foldRight(f, Ok(1), false)
      """.stripMargin
    runBoolTest(input, false)
  }

  test("foldRight.03") {
    val input =
      """def f(i: Int32, b: Bool): Bool = if (i == 2 && b) true else false
        |def r: Bool = Result.foldRight(f, Ok(1), true)
      """.stripMargin
    runBoolTest(input, false)
  }

  test("foldRight.04") {
    val input =
      """def f(i: Int32, b: Bool): Bool = if (i == 2 && b) true else false
        |def r: Bool = Result.foldRight(f, Ok(2), false)
      """.stripMargin
    runBoolTest(input, false)
  }

  test("foldRight.05") {
    val input =
      """def f(i: Int32, b: Bool): Bool = if (i == 2 && b) true else false
        |def r: Bool = Result.foldRight(f, Ok(2), true)
      """.stripMargin
    runBoolTest(input, true)
  }

  test("toList.01") {
    val input = "def r: List[Int32] = Result.toList(Err(0))"
    runAnyTest(input, Value.mkNil)
  }

  test("toList.02") {
    val input = "def r: List[Int32] = Result.toList(Ok(11))"
    runAnyTest(input, Value.mkList(List(new Integer(11))))
  }

  // TODO
  ignore("toSet.01") {
    val input = "def r: Set[Int32] = Result.toSet(Err(0))"
    runAnyTest(input, Set())
  }

  // TODO
  ignore("toSet.02") {
    val input = "def r: Set[Int32] = Result.toSet(Ok(4))"
    runAnyTest(input, Set(4))
  }

  test("toOption.01") {
    val input = "def r: Option[Int32] = Result.toOption(Err(0))"
    runAnyTest(input, Value.mkNone())
  }

  test("toOption.02") {
    val input = "def r: Option[Int32] = Result.toOption(Ok(5))"
    runAnyTest(input, Value.mkSome(new Integer(5)))
  }
}