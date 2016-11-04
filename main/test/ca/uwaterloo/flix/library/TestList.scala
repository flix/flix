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
import ca.uwaterloo.flix.util.{Evaluation, Options}
import org.scalatest.FunSuite

class TestList extends FunSuite {

  val options = Options.DefaultTest.copy(evaluation=Evaluation.Interpreted)

  def runTest(input: String, output: Int) {
    val flix = new Flix().setOptions(options).addPath("main/src/library/List.flix").addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  def runAnyTest(input: String, output: AnyRef) {
    val flix = new Flix().setOptions(options).addPath("main/src/library/List.flix").addStr(input)
    assertResult(output)(flix.solve().get.getConstant("r"))
  }

  test("size.01") {
    val input = "def r: Int32 = List/size(Nil)"
    runTest(input, 0)
  }

  test("size.02") {
    val input = "def r: Int32 = List/size(1 :: Nil)"
    runTest(input, 1)
  }

  test("size.03") {
    val input = "def r: Int32 = List/size(1 :: 2 :: Nil)"
    runTest(input, 2)
  }

  test("head.01") {
    val input = "def r: Int32 = List/head(1 :: 2 :: 3 :: Nil)"
    runTest(input, 1)
  }

  test("get.01") {
    val input = "def r: Int32 = List/get(1 :: 2 :: 3 :: Nil, 0)"
    runTest(input, 1)
  }

  test("get.02") {
    val input = "def r: Int32 = List/get(1 :: 2 :: 3 :: Nil, 1)"
    runTest(input, 2)
  }

  test("get.03") {
    val input = "def r: Int32 = List/get(1 :: 2 :: 3 :: Nil, 2)"
    runTest(input, 3)
  }

  test("indexOf.01") {
    val input = "def r: Int32 = List/indexOf(1 :: 2 :: 3 :: Nil, 4)"
    runTest(input, -1)
  }

  test("indexOf.02") {
    val input = "def r: Int32 = List/indexOf(Nil, 1)"
    runTest(input, -1)
  }

  test("indexOf.03") {
    val input = "def r: Int32 = List/indexOf(1 :: 2 :: 3 :: 4 :: Nil, 1)"
    runTest(input, 0)
  }

  test("indexOf.04") {
    val input = "def r: Int32 = List/indexOf(1 :: 2 :: 3 :: 4 :: Nil, 2)"
    runTest(input, 1)
  }

  test("indexOf.05") {
    val input = "def r: Int32 = List/indexOf(1 :: 2 :: 3 :: 4 :: Nil, 3)"
    runTest(input, 2)
  }

  test("indexOf.06") {
    val input = "def r: Int32 = List/indexOf(1 :: 2 :: 3 :: 4 :: Nil, 4)"
    runTest(input, 3)
  }
/*
  test("head.02") {
    val input = "def r: Int32 = List/head(Nil)"
    runAnyTest(input, Nil)
  }

  test("tail.01") {
    val input = "def r: List[Int] = List/tail(1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, 2 :: 3 :: Nil)
  }
*/
}