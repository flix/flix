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

// TODO: Sort
class FeatureSuite extends Suites(

  new FlixTest("TestCurrying", "main/test/ca/uwaterloo/flix/language/feature/TestCurrying.flix")(Options.TestWithLibrary),

  new FlixTest("Test.Pattern.List", "main/test/ca/uwaterloo/flix/language/feature/Test.Pattern.List.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Expression.LambdaMatch", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.LambdaMatch.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Expression.Binary.Arithmetic", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Binary.Arithmetic.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Expression.Binary.Comparison", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Binary.Comparison.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Expression.Binary.Bitwise", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Binary.Bitwise.flix")(Options.TestWithLibrary),

  new FlixTest("Test.Expression.Float", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Float.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Expression.Float32", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Float32.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Expression.Float64", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Float64.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Expression.Int", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Int.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Expression.Int8", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Int8.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Expression.Int16", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Int16.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Expression.Int32", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Int32.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Expression.Int64", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Int64.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Expression.BigInt", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.BigInt.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Expression.Tuple", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Tuple.flix")(Options.TestWithLibrary),

  new FlixTest("Test.Expression.Unary", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Unary.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Expression.Let", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Let.flix")(Options.TestWithLibrary),
  new FlixTest("TestTailCallElimination", "main/test/ca/uwaterloo/flix/language/feature/TestTailCallElimination.flix")(Options.TestWithLibrary),

  new FlixTest("TestUncurry", "main/test/ca/uwaterloo/flix/language/feature/TestUncurry.flix")(Options.TestWithLibrary),

  //
  // Declarations.
  //
  new FlixTest("Test.Decl.Def", "main/test/ca/uwaterloo/flix/language/feature/Test.Decl.Def.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Decl.OpaqueType", "main/test/ca/uwaterloo/flix/language/feature/Test.Decl.OpaqueType.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Decl.TypeAlias", "main/test/ca/uwaterloo/flix/language/feature/Test.Decl.TypeAlias.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Decl.Class", "main/test/ca/uwaterloo/flix/language/feature/Test.Decl.Class.flix")(Options.TestWithLibrary),

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
  new FlixTest("Test.Equality.Closure", "main/test/ca/uwaterloo/flix/language/feature/Test.Equality.Closure.flix")(Options.TestWithLibrary),

  //
  // Unused
  //
  new FlixTest("Test.UnusedVarSym", "main/test/ca/uwaterloo/flix/language/feature/Test.UnusedVarSym.flix")(Options.TestWithLibrary),
  new FlixTest("Test.UnusedTypeVar", "main/test/ca/uwaterloo/flix/language/feature/Test.UnusedTypeVar.flix")(Options.TestWithLibrary),

)
