/*
 * Copyright 2022 Magnus Madsen
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

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestRegions extends AnyFunSuite with TestUtils {

  test("RegionVarEscapes.01") {
    val input =
      """
        |pub enum Option[t] {
        |    case None,
        |    case Some(t)
        |}
        |
        |pub def f(): Unit \ IO =
        |    let m = ref None @ Static;
        |    region r {
        |        let x = ref 123 @ r;
        |        m := Some(x);
        |        ()
        |    }
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.RegionVarEscapes](result)
  }

  test("RegionVarEscapes.02") {
    val input =
      """
        |pub enum Option[t] {
        |    case None,
        |    case Some(t)
        |}
        |
        |pub def f(): Unit \ IO =
        |    let m = ref None @ Static;
        |    region r {
        |        let x = ref 123 @ r;
        |        m := Some(_ -> x);
        |        ()
        |    }
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.RegionVarEscapes](result)
  }

}
