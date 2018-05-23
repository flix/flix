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

// TODO: Sort
class FeatureSuite extends Suites(
  new FlixTest("TestConstraint", "main/test/ca/uwaterloo/flix/language/feature/TestConstraint.flix"),
  new FlixTest("TestEquality", "main/test/ca/uwaterloo/flix/language/feature/TestEquality.flix"),
  new FlixTest("Test.Expression.Ascribe", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Ascribe.flix"),
  new FlixTest("Test.Expression.Binary.Arithmetic", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Binary.Arithmetic.flix"),
  new FlixTest("Test.Expression.Binary.Comparison", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Binary.Comparison.flix"),
  new FlixTest("Test.Expression.Binary.Logic", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Binary.Logic.flix"),
  new FlixTest("Test.Expression.Block", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Block.flix"),
  new FlixTest("Test.Expression.Binary.Bitwise", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Binary.Bitwise.flix"),
  new FlixTest("Test.Expression.Cast", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Cast.flix"),
  new FlixTest("Test.Expression.Unit", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Unit.flix"),
  new FlixTest("Test.Expression.Bool", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Bool.flix"),
  new FlixTest("Test.Expression.Char", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Char.flix"),
  new FlixTest("Test.Expression.Float", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Float.flix"),
  new FlixTest("Test.Expression.Float32", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Float32.flix"),
  new FlixTest("Test.Expression.Float64", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Float64.flix"),
  //new FlixTest("Test.Expression.Hole", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Hole.flix"), // TODO
  new FlixTest("Test.Expression.Int", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Int.flix"),
  new FlixTest("Test.Expression.Int8", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Int8.flix"),
  new FlixTest("Test.Expression.Int16", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Int16.flix"),
  new FlixTest("Test.Expression.Int32", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Int32.flix"),
  new FlixTest("Test.Expression.Int64", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Int64.flix"),
  new FlixTest("Test.Expression.BigInt", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.BigInt.flix"),
  new FlixTest("Test.Expression.Tuple", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Tuple.flix"),
  new FlixTest("Test.Expression.Str", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Str.flix"),
  new FlixTest("Test.Expression.Def", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Def.flix"),
  new FlixTest("TestReferences", "main/test/ca/uwaterloo/flix/language/feature/TestReferences.flix"),
  //new FlixTest("TestNativeCall", "main/test/ca/uwaterloo/flix/language/feature/TestNativeCall.flix"), // TODO
  new FlixTest("TestPatternMatch", "main/test/ca/uwaterloo/flix/language/feature/TestPatternMatch.flix"),
  new FlixTest("Test.Expression.Unary", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Unary.flix"),
  new FlixTest("Test.Expression.IfThenElse", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.IfThenElse.flix"),
  new FlixTest("Test.Expression.Infix", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Infix.flix"),
  new FlixTest("Test.Expression.Let", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Let.flix"),
  new FlixTest("Test.Expression.Tag", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Tag.flix"),
  new FlixTest("Test.Expression.Tag.Lambda", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Tag.Lambda.flix"),
  new FlixTest("Test.Expression.Switch", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Switch.flix"),
  new FlixTest("Test.Expression.Match", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Match.flix"),
  new FlixTest("Test.Expression.Postfix", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Postfix.flix"),
  new FlixTest("Test.Optimization.CompactEnum", "main/test/ca/uwaterloo/flix/language/feature/Test.Optimization.CompactEnum.flix"),
  new FlixTest("Test.Optimization.SingleCaseEnum", "main/test/ca/uwaterloo/flix/language/feature/Test.Optimization.SingleCaseEnum.flix"),
  new FlixTest("TestTailCallElimination", "main/test/ca/uwaterloo/flix/language/feature/TestTailCallElimination.flix"),
  new FlixTest("TestUncurry", "main/test/ca/uwaterloo/flix/language/feature/TestUncurry.flix"),
  new FlixTest("TestUniformFunctionCallSyntax", "main/test/ca/uwaterloo/flix/language/feature/TestUniformFunctionCallSyntax.flix"),
  new FlixTest("TestStratifiedNegation", "main/test/ca/uwaterloo/flix/language/feature/TestStratifiedNegation.flix"),
  new FlixTest("TestEffects", "main/test/ca/uwaterloo/flix/language/feature/TestEffects.flix"),
  new FlixTest("Test.Decl.Class", "main/test/ca/uwaterloo/flix/language/feature/Test.Decl.Class.flix"),
  new FlixTest("Test.Decl.Disallow", "main/test/ca/uwaterloo/flix/language/feature/Test.Decl.Disallow.flix"),
  new FlixTest("Test.Decl.Impl", "main/test/ca/uwaterloo/flix/language/feature/Test.Decl.Impl.flix"),
  new FlixTest("Test.Expression.ArrayLit", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.ArrayLit.flix"),
  new FlixTest("Test.Expression.ArrayNew", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.ArrayNew.flix"),
  new FlixTest("Test.Expression.ArrayLoad", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.ArrayLoad.flix"),
  new FlixTest("Test.Expression.ArrayStore", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.ArrayStore.flix"),
  new FlixTest("Test.Expression.ArrayLength", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.ArrayLength.flix"),
  new FlixTest("Test.Expression.ArraySlice", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.ArraySlice.flix"),
  new FlixTest("Test.Expression.ArraySliceNoEndIndex", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.ArraySliceNoEndIndex.flix"),
  new FlixTest("Test.Expression.ArraySliceNoStartIndex", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.ArraySliceNoStartIndex.flix"),
  new FlixTest("Test.Expression.ArraySliceNoIndexes", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.ArraySliceNoIndexes.flix"),
  new FlixTest("Test.Expression.VectorLit", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.VectorLit.flix"),
  new FlixTest("Test.Expression.VectorNew", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.VectorNew.flix"),
  new FlixTest("Test.Expression.VectorLoad", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.VectorLoad.flix"),
  new FlixTest("Test.Expression.VectorStore", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.VectorStore.flix"),
  new FlixTest("Test.Expression.VectorLength", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.VectorLength.flix"),
  new FlixTest("Test.Expression.VectorSlice", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.VectorSlice.flix"),
  new FlixTest("Test.Expression.VectorSliceNoIndexes", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.VectorSliceNoIndexes.flix"),
  new FlixTest("Test.Expression.VectorSliceNoEnd", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.VectorSliceNoEnd.flix"),
  new FlixTest("Test.Expression.VectorSliceNoStart", "main/test/ca/uwaterloo/flix/language/feature/Test.Expression.VectorSliceNoStart.flix"),
) with ParallelTestExecution {
  /* left empty */
}
