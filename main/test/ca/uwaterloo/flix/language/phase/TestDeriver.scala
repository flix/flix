/*
 * Copyright 2023 Sam Ezeh
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
import ca.uwaterloo.flix.language.errors.DerivationError
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestDeriver extends AnyFunSuite with TestUtils {
  test("DerivationError.EmptyEnum.Eq") {
    val compiled = compile("enum E with Eq", Options.TestWithLibMin)
    expectError[DerivationError.IllegalDerivationForEmptyEnum](compiled)
  }
  test("DerivationError.EmptyEnum.Order") {
    val compiled = compile("enum E with Eq, Order", Options.TestWithLibMin)
    expectError[DerivationError.IllegalDerivationForEmptyEnum](compiled)
  }

  test("DerivationError.EmptyEnum.ToString") {
    val compiled = compile("enum E with ToString", Options.TestWithLibMin)
    expectError[DerivationError.IllegalDerivationForEmptyEnum](compiled)
  }
  test("DerivationError.EmptyEnum.Hash") {
    val compiled = compile("enum E with Hash", Options.TestWithLibMin)
    expectError[DerivationError.IllegalDerivationForEmptyEnum](compiled)
  }
}

