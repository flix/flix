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

  private val Dir = "main/test/ca/uwaterloo/flix/library/"
  private implicit val TestOptions: Options = Options.TestWithLibAll

  // TODO: Sort alphabetically.
  // TODO: Ensure every file in Dir is present here.

  mkTest(Dir + "TestPrelude.flix")
  mkTest(Dir + "TestArray.flix")
  mkTest(Dir + "TestBool.flix")
  mkTest(Dir + "TestBoxable.flix")
  mkTest(Dir + "TestChain.flix")
  mkTest(Dir + "TestChar.flix")
  mkTest(Dir + "TestChoice.flix")
  mkTest(Dir + "TestCommutativeSemiGroup.flix")
  mkTest(Dir + "TestChannel.flix")
  mkTest(Dir + "TestDemandList.flix")
  mkTest(Dir + "TestEnvironment.flix")
  mkTest(Dir + "TestInt8.flix")
  mkTest(Dir + "TestInt16.flix")
  mkTest(Dir + "TestInt32.flix")
  mkTest(Dir + "TestInt64.flix")
  mkTest(Dir + "TestIterator.flix")
  mkTest(Dir + "TestBigInt.flix")
  mkTest(Dir + "TestFile.flix")
  mkTest(Dir + "TestFloat32.flix")
  mkTest(Dir + "TestFloat64.flix")
  mkTest(Dir + "TestObject.flix")
  mkTest(Dir + "TestRandom.flix")
  mkTest(Dir + "TestOption.flix")
  mkTest(Dir + "TestRedBlackTree.flix")
  mkTest(Dir + "TestResult.flix")
  mkTest(Dir + "TestLazyList.flix")
  mkTest(Dir + "TestList.flix")
  mkTest(Dir + "TestSet.flix")
  mkTest(Dir + "TestMap.flix")
  mkTest(Dir + "TestDelayMap.flix")
  mkTest(Dir + "TestString.flix")
  mkTest(Dir + "TestNel.flix")
  mkTest(Dir + "TestStringBuilder.flix")
  mkTest(Dir + "TestTimer.flix")
  mkTest(Dir + "TestValidation.flix")
  mkTest(Dir + "TestMutDeque.flix")
  mkTest(Dir + "TestMutList.flix")
  mkTest(Dir + "TestMutSet.flix")
  mkTest(Dir + "TestMutMap.flix")
  mkTest(Dir + "TestToString.flix")
  mkTest(Dir + "TestFromString.flix")
  mkTest(Dir + "TestHash.flix")
  mkTest(Dir + "TestMonoid.flix")
  mkTest(Dir + "TestLowerBound.flix")
  mkTest(Dir + "TestUpperBound.flix")
  mkTest(Dir + "TestGetOpt.flix")
  mkTest(Dir + "TestSemiGroup.flix")
  mkTest(Dir + "TestTraversable.flix")
  mkTest(Dir + "TestFunctor.flix")
  mkTest(Dir + "TestApplicative.flix")
  mkTest(Dir + "TestMonad.flix")
  mkTest(Dir + "TestFoldable.flix")
  mkTest(Dir + "TestReducible.flix")
  mkTest(Dir + "TestIdentity.flix")

}
