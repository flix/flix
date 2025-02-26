/*
 * Copyright 2025 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestTypeSimplifier extends AnyFunSuite with TestUtils {

  test("RunWithSimplifier") {
    val input = "def main(): Bool = true"
    val result = compile(input, Options.TestWithLibAll.copy(xsolveWithUserFacingSimplifier = true))
    expectSuccess(result)
  }

}
