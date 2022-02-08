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

import ca.uwaterloo.flix.util.{FlixSuite, Options}

class LibrarySuite extends FlixSuite {

  private val Path = "main/test/ca/uwaterloo/flix/library/"
  private implicit val TestOptions: Options = Options.TestWithLibAll

  // TODO: Sort alphabetically.

  mkTest(Path + "TestPrelude.flix")
  mkTest(Path + "TestArray.flix")
  mkTest(Path + "TestBool.flix")
  mkTest(Path + "TestBoxable.flix")
  mkTest(Path + "TestChain.flix")
  mkTest(Path + "TestChar.flix")
  mkTest(Path + "TestChoice.flix")
  mkTest(Path + "TestCommutativeSemiGroup.flix")
  mkTest(Path + "TestChannel.flix")
  mkTest(Path + "TestDemandList.flix")
  mkTest(Path + "TestEnvironment.flix")
  mkTest(Path + "TestInt8.flix")
  mkTest(Path + "TestInt16.flix")
  mkTest(Path + "TestInt32.flix")
  mkTest(Path + "TestInt64.flix")
  mkTest(Path + "TestIterator.flix")
  mkTest(Path + "TestBigInt.flix")
  mkTest(Path + "TestFile.flix")
  mkTest(Path + "TestFloat32.flix")
  mkTest(Path + "TestFloat64.flix")
  mkTest(Path + "TestObject.flix")
  mkTest(Path + "TestRandom.flix")
  mkTest(Path + "TestOption.flix")
  mkTest(Path + "TestRedBlackTree.flix")
  mkTest(Path + "TestResult.flix")
  mkTest(Path + "TestLazyList.flix")
  mkTest(Path + "TestList.flix")
  mkTest(Path + "TestSet.flix")
  mkTest(Path + "TestMap.flix")
  mkTest(Path + "TestDelayMap.flix")
  mkTest(Path + "TestString.flix")
  mkTest(Path + "TestNel.flix")
  mkTest(Path + "TestStringBuilder.flix")
  mkTest(Path + "TestTimer.flix")
  mkTest(Path + "TestValidation.flix")
  mkTest(Path + "TestMutDeque.flix")
  mkTest(Path + "TestMutList.flix")
  mkTest(Path + "TestMutSet.flix")
  mkTest(Path + "TestMutMap.flix")
  mkTest(Path + "TestToString.flix")
  mkTest(Path + "TestFromString.flix")
  mkTest(Path + "TestHash.flix")
  mkTest(Path + "TestMonoid.flix")
  mkTest(Path + "TestLowerBound.flix")
  mkTest(Path + "TestUpperBound.flix")
  mkTest(Path + "TestGetOpt.flix")
  mkTest(Path + "TestSemiGroup.flix")
  mkTest(Path + "TestTraversable.flix")
  mkTest(Path + "TestFunctor.flix")
  mkTest(Path + "TestApplicative.flix")
  mkTest(Path + "TestMonad.flix")
  mkTest(Path + "TestFoldable.flix")
  mkTest(Path + "TestReducible.flix")
  mkTest(Path + "TestIdentity.flix")

}
