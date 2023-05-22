/*
 * Copyright 2022 Matthew Lutze
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
package flix.experimental

import ca.uwaterloo.flix.util.{FlixSuite, Options}

/**
  * Suite of tests for which nonstandard testing options are required.
  *
  * Intended for work-in-progress features for which maintaining the standard options (e.g. redundancy checking)
  * would be a burden.
  *
  * Tests should be promoted out of this suite and into [[flix.CompilerSuite]]
  * as soon as they run without special options.
  */
class ExperimentalSuite extends FlixSuite(incremental = true) {
  implicit val options: Options = Options.TestWithLibAll

  mkTest("main/test/flix/experimental/Test.Dec.AssocType.flix")
}
