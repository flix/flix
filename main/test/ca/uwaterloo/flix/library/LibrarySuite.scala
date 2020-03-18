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

import ca.uwaterloo.flix.util.{FlixTest, Options}
import org.scalatest.{ParallelTestExecution, Suites}

class LibrarySuite extends Suites(
  new FlixTest("TestPrelude", "main/test/ca/uwaterloo/flix/library/TestPrelude.flix")(Options.TestWithLibrary),
  new FlixTest("TestArray", "main/test/ca/uwaterloo/flix/library/TestArray.flix")(Options.TestWithLibrary),
  new FlixTest("TestChannel", "main/test/ca/uwaterloo/flix/library/TestChannel.flix")(Options.TestWithLibrary),
  new FlixTest("TestInt8", "main/test/ca/uwaterloo/flix/library/TestInt8.flix")(Options.TestWithLibrary),
  new FlixTest("TestInt16", "main/test/ca/uwaterloo/flix/library/TestInt16.flix")(Options.TestWithLibrary),
  new FlixTest("TestInt32", "main/test/ca/uwaterloo/flix/library/TestInt32.flix")(Options.TestWithLibrary),
  new FlixTest("TestInt64", "main/test/ca/uwaterloo/flix/library/TestInt64.flix")(Options.TestWithLibrary),
  new FlixTest("TestBigInt", "main/test/ca/uwaterloo/flix/library/TestBigInt.flix")(Options.TestWithLibrary),
  new FlixTest("TestFloat32", "main/test/ca/uwaterloo/flix/library/TestFloat32.flix")(Options.TestWithLibrary),
  new FlixTest("TestFloat64", "main/test/ca/uwaterloo/flix/library/TestFloat64.flix")(Options.TestWithLibrary),
  new FlixTest("TestOption", "main/test/ca/uwaterloo/flix/library/TestOption.flix")(Options.TestWithLibrary),
  new FlixTest("TestPath", "main/test/ca/uwaterloo/flix/library/TestPath.flix")(Options.TestWithLibrary),
  new FlixTest("TestRandom", "main/test/ca/uwaterloo/flix/library/TestRandom.flix")(Options.TestWithLibrary),
  new FlixTest("TestResult", "main/test/ca/uwaterloo/flix/library/TestResult.flix")(Options.TestWithLibrary),
  new FlixTest("TestList", "main/test/ca/uwaterloo/flix/library/TestList.flix")(Options.TestWithLibrary),
  new FlixTest("TestSet", "main/test/ca/uwaterloo/flix/library/TestSet.flix")(Options.TestWithLibrary),
  new FlixTest("TestMap", "main/test/ca/uwaterloo/flix/library/TestMap.flix")(Options.TestWithLibrary),
  new FlixTest("TestNel", "main/test/ca/uwaterloo/flix/library/TestNel.flix")(Options.TestWithLibrary),
  new FlixTest("TestStr", "main/test/ca/uwaterloo/flix/library/TestStr.flix")(Options.TestWithLibrary),
  new FlixTest("TestStringBuilder", "main/test/ca/uwaterloo/flix/library/TestStringBuilder.flix")(Options.TestWithLibrary),
  new FlixTest("TestTimer", "main/test/ca/uwaterloo/flix/library/TestTimer.flix")(Options.TestWithLibrary),
  new FlixTest("TestValidation", "main/test/ca/uwaterloo/flix/library/TestValidation.flix")(Options.TestWithLibrary)
) with ParallelTestExecution {
  /* left empty */
}
