/*
 * Copyright 2016 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase

// import ca.uwaterloo.flix.api.Flix
//import ca.uwaterloo.flix.language.ast.AstStats
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestOptimizer extends FunSuite {

  val opts = Options.DefaultTest.copy(optimize = true)
/*
  test("ConstantFold.Plus.01") {
    val input = "def f: Int = 1 + 2 + 3"
    val s = statsOf(input)
    val v = valueOf(input)
    assertResult(0)(s.plusExpressions)
    assertResult(6)(v)
  }

  test("RedundantBranching.01") {
    val input =
      s"""
         |def f: Int =
         |  let x = 2;
         |    if (x > 1)
         |      (if (x > 0) 5 else 2)
         |    else
         |      5
       """.stripMargin
    val s = statsOf(input)
    val v = valueOf(input)
    assertResult(1)(s.ifThenElseExpressions)
    assertResult(5)(v)
  }

  test("RedundantBranching.02") {
    val input =
      s"""
         |def f: Int =
         |  let x = 2;
         |    if (x > 1)
         |      (if (x > 0) 2 else 5)
         |    else
         |      5
       """.stripMargin
    val s = statsOf(input)
    val v = valueOf(input)
    assertResult(1)(s.ifThenElseExpressions)
    assertResult(2)(v)
  }

  test("RedundantBranching.03") {
    val input =
      s"""
         |def f: Int =
         |  let x = 2;
         |    if (x > 1)
         |      5
         |    else
         |      (if (x > 0) 5 else 2)
       """.stripMargin
    val s = statsOf(input)
    val v = valueOf(input)
    assertResult(1)(s.ifThenElseExpressions)
    assertResult(5)(v)
  }

  test("RedundantBranching.04") {
    val input =
      s"""
         |def f: Int =
         |  let x = 2;
         |    if (x > 1)
         |      5
         |    else
         |      (if (x > 0) 2 else 5)
       """.stripMargin
    val s = statsOf(input)
    val v = valueOf(input)
    assertResult(1)(s.ifThenElseExpressions)
    assertResult(5)(v)
  }
/*
  test("SingleValuedEnum.01") {
    val input =
      s"""
         |enum Val { case Val(Int) }
         |
         |def f: Int =
         |  let x = Val.Val(3);
         |  match x with {
         |    case Val(i) => i
         |    case _ => 4
         |  }
       """.stripMargin
    val s = statsOf(input)
    val v = valueOf(input)
    assertResult(0)(s.isExpressions)
    assertResult(1)(s.letExpressions)
    assertResult(3)(v)
  }
*/
  private def statsOf(input: String): AstStats = new Flix().addStr(input).setOptions(opts).astStats().get

  private def valueOf(input: String): AnyRef = new Flix().setOptions(opts).addStr(input).solve().get.getConstant("f")
*/
}
