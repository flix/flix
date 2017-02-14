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

package ca.uwaterloo.flix.library

import ca.uwaterloo.flix.util.FlixTest
import org.scalatest.{ParallelTestExecution, Suites}

class LibrarySuite extends Suites(
  //new TestBigInt, // TODO: Disabled pending flix test
  //new TestFloat32,  // TODO: Disabled pending flix test
  //new TestFloat64,  // TODO: Disabled pending flix test
  //new TestInt8, // TODO: Disabled pending flix test
  //new TestInt16,  // TODO: Disabled pending flix test
  new FlixTest("TestPrelude", "main/test/ca/uwaterloo/flix/library/TestPrelude.flix"),
  new FlixTest("TestMap", "main/test/ca/uwaterloo/flix/library/TestMap.flix"),
  new FlixTest("TestSet", "main/test/ca/uwaterloo/flix/library/TestSet.flix")
) with ParallelTestExecution {
  /* left empty */
}
