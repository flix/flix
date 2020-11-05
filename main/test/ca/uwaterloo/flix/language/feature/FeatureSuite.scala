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

import ca.uwaterloo.flix.util.{FlixTest, Options}
import org.scalatest.Suites

class FeatureSuite extends Suites(

  new FlixTest("Test.Expression.Tuple", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Tuple.flix")(Options.TestWithLibrary),

  new FlixTest("Test.Expression.Let", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Let.flix")(Options.TestWithLibrary),

  //
  // Pattern Match.
  //
  new FlixTest("Test.Expression.Match", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Match.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Expression.Match.Array", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Match.Array.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Expression.Match.Array.TailSpread", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Match.Array.TailSpread.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Expression.Match.Array.HeadSpread", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Match.Array.HeadSpread.flix")(Options.TestWithLibrary),

  //
  // Equality
  //
  new FlixTest("Test.Equality", "main/test/ca/uwaterloo/flix/language/feature/Test.Equality.flix")(Options.TestWithLibrary),

  //
  // Unused
  //
  new FlixTest("Test.UnusedTypeVar", "main/test/ca/uwaterloo/flix/language/feature/Test.UnusedTypeVar.flix")(Options.TestWithLibrary),

)
