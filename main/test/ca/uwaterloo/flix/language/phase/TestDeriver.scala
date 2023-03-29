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

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.DerivationError
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestDeriver extends FunSuite with TestUtils {

  test("DerivationError") {
    val inputs = List(
      "pub enum E with Eq, Order",
      "pub enum E with ToString",
      "pub enum E with Hash",
    )
    inputs
      .map { compile(_, Options.TestWithLibNix) }
      .foreach(expectError[DerivationError.IllegalDerivationForEmptyEnum])
  }
}

