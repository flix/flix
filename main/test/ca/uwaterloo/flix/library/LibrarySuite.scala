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
  new FlixTest(
    "LibrarySuite",
    List(
      "main/test/ca/uwaterloo/flix/library/TestPrelude.flix",
      "main/test/ca/uwaterloo/flix/library/TestArray.flix",
      "main/test/ca/uwaterloo/flix/library/TestBool.flix",
      "main/test/ca/uwaterloo/flix/library/TestBoxable.flix",
      "main/test/ca/uwaterloo/flix/library/TestChar.flix",
      "main/test/ca/uwaterloo/flix/library/TestChoice.flix",
      "main/test/ca/uwaterloo/flix/library/TestDemandList.flix",
      "main/test/ca/uwaterloo/flix/library/TestChannel.flix",
      "main/test/ca/uwaterloo/flix/library/TestEnvironment.flix",
      "main/test/ca/uwaterloo/flix/library/TestInt8.flix",
      "main/test/ca/uwaterloo/flix/library/TestInt16.flix",
      "main/test/ca/uwaterloo/flix/library/TestInt32.flix",
      "main/test/ca/uwaterloo/flix/library/TestInt64.flix",
      "main/test/ca/uwaterloo/flix/library/TestIterator.flix",
      "main/test/ca/uwaterloo/flix/library/TestBigInt.flix",
      "main/test/ca/uwaterloo/flix/library/TestFloat32.flix",
      "main/test/ca/uwaterloo/flix/library/TestFloat64.flix",
      "main/test/ca/uwaterloo/flix/library/TestObject.flix",
      "main/test/ca/uwaterloo/flix/library/TestOption.flix",
      "main/test/ca/uwaterloo/flix/library/TestRandom.flix",
      "main/test/ca/uwaterloo/flix/library/TestResult.flix",
      "main/test/ca/uwaterloo/flix/library/TestLazyList.flix",
      "main/test/ca/uwaterloo/flix/library/TestList.flix",
      "main/test/ca/uwaterloo/flix/library/TestSet.flix",
      "main/test/ca/uwaterloo/flix/library/TestStream.flix",
      "main/test/ca/uwaterloo/flix/library/TestMap.flix",
      "main/test/ca/uwaterloo/flix/library/TestNel.flix",
      "main/test/ca/uwaterloo/flix/library/TestString.flix",
      "main/test/ca/uwaterloo/flix/library/TestStringBuilder.flix",
      "main/test/ca/uwaterloo/flix/library/TestTimer.flix",
      "main/test/ca/uwaterloo/flix/library/TestValidation.flix",
      "main/test/ca/uwaterloo/flix/library/TestMutList.flix",
      "main/test/ca/uwaterloo/flix/library/TestMutSet.flix",
      "main/test/ca/uwaterloo/flix/library/TestMutMap.flix",
      "main/test/ca/uwaterloo/flix/library/TestToString.flix",
      "main/test/ca/uwaterloo/flix/library/TestFromString.flix",
      "main/test/ca/uwaterloo/flix/library/TestHash.flix",
      "main/test/ca/uwaterloo/flix/library/TestMonoid.flix",
      "main/test/ca/uwaterloo/flix/library/TestLowerBound.flix",
      "main/test/ca/uwaterloo/flix/library/TestUpperBound.flix",
      "main/test/ca/uwaterloo/flix/library/TestGetOpt.flix",
    ),
    Options.TestWithLibAll
  )
)
