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
  new FlixTest("Test.Expression.Float32", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Float32.flix"),
  new FlixTest("Test.Expression.Tuple", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Tuple.flix"),
  new FlixTest("TestNativeCall", "main/test/ca/uwaterloo/flix/language/feature/TestNativeCall.flix"),
  new FlixTest("TestPatternMatch", "main/test/ca/uwaterloo/flix/language/feature/TestPatternMatch.flix"),
  new FlixTest("Test.Expression.Float", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Float.flix"),
  new FlixTest("Test.Expression.Float64", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Float64.flix"),
  new FlixTest("Test.Expression.Int", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Int.flix"),
  new FlixTest("Test.Expression.Int8", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Int8.flix"),
  new FlixTest("Test.Expression.Int16", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Int16.flix"),
  new FlixTest("Test.Expression.Int32", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Int32.flix"),
  new FlixTest("Test.Expression.Int64", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Int64.flix"),
  new FlixTest("Test.Expression.BigInt", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.BigInt.flix"),
  new FlixTest("Test.Expression.Unit", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Unit.flix"),
  new FlixTest("Test.Expression.Bool", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Bool.flix"),
  new FlixTest("Test.Expression.Char", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Char.flix"),
  new FlixTest("Test.Expression.Str", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Str.flix"),
  new FlixTest("Test.Expression.Ref", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Ref.flix"),
  new FlixTest("Test.Expression.Lambda", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Lambda.flix"),
  new FlixTest("Test.Expression.Unary", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Unary.flix")
) with ParallelTestExecution {
  /* left empty */
}
