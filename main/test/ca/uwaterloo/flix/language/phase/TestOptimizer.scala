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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.AstStats
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestOptimizer extends FunSuite {

  val opts = Options.DefaultTest.copy(optimize = true)

  // TODO: Add two types of test cases. One type to check that the behaviour is preserved,
  // and another to test that the expected transformations take place.

  test("ConstantFold.Plus.01") {
    val input = "def f: Int = 1 + 2 + 3"
    val s = statsOf(input)
    assertResult(2)(s.plusExpressions) // TODO: Change to zero for when the optimizer is enabled.
  }

  test("ConstantFold.And.01") {
    val input = "def f: Bool = true && false"
    val s = statsOf(input)
    assertResult(1)(s.logicalAndExpressions) // TODO: Change to zero for when the optimizer is enabled.
  }

  private def statsOf(input: String): AstStats = new Flix().addStr(input).setOptions(opts).astStats().get

}
