/*
 * Copyright 2025 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestMissingSyms extends AnyFunSuite with TestUtils {

  test("MissingSyms.01") {
    val input =
      """
        |pub def f(): Unit = ()
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val result = MissingSyms.run(root)
    assert(result.isEmpty)
  }

  test("MissingSyms.02") {
    val input =
      """
        |pub def f(): Unit = ()
        |pub def g(): Unit = ()
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val result = MissingSyms.run(root)
    assert(result.isEmpty)
  }

  test("MissingSyms.03") {
    val input =
      """
        |pub def f(): Unit = ()
        |pub def g(): Unit = ()
        |pub def h(): Int32 = 42
        |""".stripMargin

    val (Some(root), _) = check(input, Options.TestWithLibNix)
    val result = MissingSyms.run(root)
    assert(result.isEmpty)
  }

}
