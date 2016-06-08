/*
 * Copyright 2015-2016 Magnus Madsen
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

package ca.uwaterloo.flix.language.ast

import org.scalatest.FunSuite

class TestTypedAst extends FunSuite {

  val SL = SourceLocation.Unknown

  test("Pattern.Bound") {
    val x = ident("x")
    val y = ident("y")

    val pat = TypedAst.Pattern.Tuple(List(
      TypedAst.Pattern.Var(x, Type.Bool, SL),
      TypedAst.Pattern.Var(y, Type.Int32, SL)
    ), Type.Tuple(List(Type.Bool, Type.Int32)), SL)

    assertResult(Map(
      "x" -> Type.Bool,
      "y" -> Type.Int32
    ))(pat.freeVars)
  }

  def ident(s: String): Name.Ident = Name.Ident(SourcePosition.Unknown, s, SourcePosition.Unknown)

}
