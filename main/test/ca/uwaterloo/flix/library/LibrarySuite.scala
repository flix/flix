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

  private val Prefix = "main/test/ca/uwaterloo/flix/library/"
  private implicit val Opts: Options = Options.TestWithLibAll

  // TODO: Sort alphabetically.

  addTest(Prefix + "TestPrelude.flix")
  addTest(Prefix + "TestArray.flix")
  addTest(Prefix + "TestBool.flix")
  addTest(Prefix + "TestBoxable.flix")
  addTest(Prefix + "TestChain.flix")
  addTest(Prefix + "TestChar.flix")
  addTest(Prefix + "TestChoice.flix")
  addTest(Prefix + "TestCommutativeSemiGroup.flix")
  addTest(Prefix + "TestChannel.flix")
  addTest(Prefix + "TestDemandList.flix")
  addTest(Prefix + "TestEnvironment.flix")
  addTest(Prefix + "TestInt8.flix")
  addTest(Prefix + "TestInt16.flix")
  addTest(Prefix + "TestInt32.flix")
  addTest(Prefix + "TestInt64.flix")
  addTest(Prefix + "TestIterator.flix")
  addTest(Prefix + "TestBigInt.flix")
  addTest(Prefix + "TestFile.flix")
  addTest(Prefix + "TestFloat32.flix")
  addTest(Prefix + "TestFloat64.flix")
  addTest(Prefix + "TestObject.flix")
  addTest(Prefix + "TestRandom.flix")
  addTest(Prefix + "TestOption.flix")
  addTest(Prefix + "TestRedBlackTree.flix")
  addTest(Prefix + "TestResult.flix")
  addTest(Prefix + "TestLazyList.flix")
  addTest(Prefix + "TestList.flix")
  addTest(Prefix + "TestSet.flix")
  addTest(Prefix + "TestMap.flix")
  addTest(Prefix + "TestDelayMap.flix")
  addTest(Prefix + "TestString.flix")
  addTest(Prefix + "TestNel.flix")
  addTest(Prefix + "TestStringBuilder.flix")
  addTest(Prefix + "TestTimer.flix")
  addTest(Prefix + "TestValidation.flix")
  addTest(Prefix + "TestMutDeque.flix")
  addTest(Prefix + "TestMutList.flix")
  addTest(Prefix + "TestMutSet.flix")
  addTest(Prefix + "TestMutMap.flix")
  addTest(Prefix + "TestToString.flix")
  addTest(Prefix + "TestFromString.flix")
  addTest(Prefix + "TestHash.flix")
  addTest(Prefix + "TestMonoid.flix")
  addTest(Prefix + "TestLowerBound.flix")
  addTest(Prefix + "TestUpperBound.flix")
  addTest(Prefix + "TestGetOpt.flix")
  addTest(Prefix + "TestSemiGroup.flix")
  addTest(Prefix + "TestTraversable.flix")
  addTest(Prefix + "TestFunctor.flix")
  addTest(Prefix + "TestApplicative.flix")
  addTest(Prefix + "TestMonad.flix")
  addTest(Prefix + "TestFoldable.flix")
  addTest(Prefix + "TestReducible.flix")
  addTest(Prefix + "TestIdentity.flix")

}
