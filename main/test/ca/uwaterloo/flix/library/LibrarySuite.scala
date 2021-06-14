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
import org.scalatest.Suites

class LibrarySuite extends Suites(
  new FlixTest("TestPrelude", "main/test/ca/uwaterloo/flix/library/TestPrelude.flix")(Options.TestWithLibAll),
  new FlixTest("TestArray", "main/test/ca/uwaterloo/flix/library/TestArray.flix")(Options.TestWithLibAll),
  new FlixTest("TestBool", "main/test/ca/uwaterloo/flix/library/TestBool.flix")(Options.TestWithLibAll),
  new FlixTest("TestChar", "main/test/ca/uwaterloo/flix/library/TestChar.flix")(Options.TestWithLibAll),
  new FlixTest("TestChoice", "main/test/ca/uwaterloo/flix/library/TestChoice.flix")(Options.TestWithLibAll),
  new FlixTest("TestChannel", "main/test/ca/uwaterloo/flix/library/TestChannel.flix")(Options.TestWithLibAll),
  new FlixTest("TestInt8", "main/test/ca/uwaterloo/flix/library/TestInt8.flix")(Options.TestWithLibAll),
  new FlixTest("TestInt16", "main/test/ca/uwaterloo/flix/library/TestInt16.flix")(Options.TestWithLibAll),
  new FlixTest("TestInt32", "main/test/ca/uwaterloo/flix/library/TestInt32.flix")(Options.TestWithLibAll),
  new FlixTest("TestInt64", "main/test/ca/uwaterloo/flix/library/TestInt64.flix")(Options.TestWithLibAll),
  new FlixTest("TestBigInt", "main/test/ca/uwaterloo/flix/library/TestBigInt.flix")(Options.TestWithLibAll),
  new FlixTest("TestFloat32", "main/test/ca/uwaterloo/flix/library/TestFloat32.flix")(Options.TestWithLibAll),
  new FlixTest("TestFloat64", "main/test/ca/uwaterloo/flix/library/TestFloat64.flix")(Options.TestWithLibAll),
  new FlixTest("TestOption", "main/test/ca/uwaterloo/flix/library/TestOption.flix")(Options.TestWithLibAll),
  new FlixTest("TestRandom", "main/test/ca/uwaterloo/flix/library/TestRandom.flix")(Options.TestWithLibAll),
  new FlixTest("TestResult", "main/test/ca/uwaterloo/flix/library/TestResult.flix")(Options.TestWithLibAll),
  new FlixTest("TestList", "main/test/ca/uwaterloo/flix/library/TestList.flix")(Options.TestWithLibAll),
  new FlixTest("TestSet", "main/test/ca/uwaterloo/flix/library/TestSet.flix")(Options.TestWithLibAll),
  new FlixTest("TestMap", "main/test/ca/uwaterloo/flix/library/TestMap.flix")(Options.TestWithLibAll),
  new FlixTest("TestNel", "main/test/ca/uwaterloo/flix/library/TestNel.flix")(Options.TestWithLibAll),
  new FlixTest("TestString", "main/test/ca/uwaterloo/flix/library/TestString.flix")(Options.TestWithLibAll),
  new FlixTest("TestStringBuilder", "main/test/ca/uwaterloo/flix/library/TestStringBuilder.flix")(Options.TestWithLibAll),
  new FlixTest("TestTimer", "main/test/ca/uwaterloo/flix/library/TestTimer.flix")(Options.TestWithLibAll),
  new FlixTest("TestValidation", "main/test/ca/uwaterloo/flix/library/TestValidation.flix")(Options.TestWithLibAll),
  new FlixTest("TestMutList", "main/test/ca/uwaterloo/flix/library/TestMutList.flix")(Options.TestWithLibAll),
  new FlixTest("TestMutSet", "main/test/ca/uwaterloo/flix/library/TestMutSet.flix")(Options.TestWithLibAll),
  new FlixTest("TestMutMap", "main/test/ca/uwaterloo/flix/library/TestMutMap.flix")(Options.TestWithLibAll),
  new FlixTest("TestToString", "main/test/ca/uwaterloo/flix/library/TestToString.flix")(Options.TestWithLibAll),
  new FlixTest("TestFromString", "main/test/ca/uwaterloo/flix/library/TestFromString.flix")(Options.TestWithLibAll),
  new FlixTest("TestHash", "main/test/ca/uwaterloo/flix/library/TestHash.flix")(Options.TestWithLibAll),
  new FlixTest("TestLazyList", "main/test/ca/uwaterloo/flix/library/TestLazyList.flix")(Options.TestWithLibAll),
  new FlixTest("TestMonoid", "main/test/ca/uwaterloo/flix/library/TestMonoid.flix")(Options.TestWithLibAll),
  new FlixTest("TestLowerBound", "main/test/ca/uwaterloo/flix/library/TestLowerBound.flix")(Options.TestWithLibAll),
  new FlixTest("TestUpperBound", "main/test/ca/uwaterloo/flix/library/TestUpperBound.flix")(Options.TestWithLibAll),
  new FlixTest("TestGetOpt", "main/test/ca/uwaterloo/flix/library/TestGetOpt.flix")(Options.TestWithLibAll),

  new FlixTest("Core/Io/TestFile", "main/test/ca/uwaterloo/flix/library/Core/Io/TestFile.flix")(Options.TestWithLibAll),
  new FlixTest("Core/Io/TestInputStream", "main/test/ca/uwaterloo/flix/library/Core/Io/TestInputStream.flix")(Options.TestWithLibAll),
  new FlixTest("Core/Io/TestOutputStream", "main/test/ca/uwaterloo/flix/library/Core/Io/TestOutputStream.flix")(Options.TestWithLibAll),
)
