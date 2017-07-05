/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.language.feature

import ca.uwaterloo.flix.util.FlixTest
import org.scalatest.{ParallelTestExecution, Suites}

class FeatureSuite extends Suites(
  new FlixTest("TestConstraint", "main/test/ca/uwaterloo/flix/language/feature/TestConstraint.flix"),
  new FlixTest("TestEquality", "main/test/ca/uwaterloo/flix/language/feature/TestEquality.flix"),
  new FlixTest("Test.Expression.Ascribe", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Ascribe.flix"),
  new FlixTest("Test.Expression.Cast", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Cast.flix"),
  new FlixTest("Test.Expression.Char", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Char.flix"),
  new FlixTest("Test.Expression.Float32", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Float32.flix"),
  new FlixTest("Test.Expression.Tuple", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Tuple.flix"),
  new FlixTest("TestNativeCall", "main/test/ca/uwaterloo/flix/language/feature/TestNativeCall.flix"),
  new FlixTest("TestPatternMatch", "main/test/ca/uwaterloo/flix/language/feature/TestPatternMatch.flix")
) with ParallelTestExecution {
  /* left empty */
}
