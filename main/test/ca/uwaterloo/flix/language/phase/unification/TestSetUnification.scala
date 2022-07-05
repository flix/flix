/*
 *  Copyright 2016 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Ast, Kind, Name, RigidityEnv, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.phase.unification.InferMonad.seqM
import ca.uwaterloo.flix.util.Result
import org.scalatest.FunSuite

class TestSetUnification extends FunSuite with TestUtils {

  implicit val flix: Flix = new Flix()

  val loc: SourceLocation = SourceLocation.Unknown

  test("Test.SetUnification.01") {
    // ∅ ≐ ∅
    val tpe1 = Type.Empty
    val tpe2 = Type.Empty
    assertUnifies(tpe1, tpe2, RigidityEnv.empty)
  }

  test("Test.SetUnification.02") {
    // ∅ ≐ 𝓤ᶜ
    val tpe1 = Type.Empty
    val tpe2 = Type.mkComplement(Type.All, loc)
    assertUnifies(tpe1, tpe2, RigidityEnv.empty)
  }

  test("Test.SetUnification.03") {
    // ∅ ≐ ∅ ∪ ∅
    val tpe1 = Type.Empty
    val tpe2 = Type.mkUnion(Type.Empty, Type.Empty, loc)
    assertUnifies(tpe1, tpe2, RigidityEnv.empty)
  }

  test("Test.SetUnification.04") {
    // e ≐ e ∪ e
    val tpe1 = Type.KindedVar(new Symbol.KindedTypeVarSym(1, Ast.VarText.SourceText("e"), Kind.Effect, false, loc), loc)
    val tpe2 = Type.mkUnion(tpe1, tpe1, loc)
    assertUnifies(tpe1, tpe2, RigidityEnv.empty)
  }

  test("Test.SetUnification.05") {
    // e! ≐ e! ∪ e!
    val sym = new Symbol.KindedTypeVarSym(1, Ast.VarText.SourceText("e"), Kind.Effect, false, loc)
    val tpe1 = Type.KindedVar(sym, loc)
    val tpe2 = Type.mkUnion(tpe1, tpe1, loc)
    val renv = RigidityEnv.empty.markRigid(sym)
    assertUnifies(tpe1, tpe2, renv)
  }

  test("Test.SetUnification.06") {
    // e ≐ f
    val tpe1 = Type.KindedVar(new Symbol.KindedTypeVarSym(1, Ast.VarText.SourceText("e"), Kind.Effect, false, loc), loc)
    val tpe2 = Type.KindedVar(new Symbol.KindedTypeVarSym(1, Ast.VarText.SourceText("f"), Kind.Effect, false, loc), loc)
    assertUnifies(tpe1, tpe2, RigidityEnv.empty)
  }

  test("Test.SetUnification.07") {
    // e! ≐ f!
    val sym1 = new Symbol.KindedTypeVarSym(1, Ast.VarText.SourceText("e"), Kind.Effect, false, loc)
    val sym2 = new Symbol.KindedTypeVarSym(2, Ast.VarText.SourceText("f"), Kind.Effect, false, loc)
    val tpe1 = Type.KindedVar(sym1, loc)
    val tpe2 = Type.KindedVar(sym2, loc)
    val renv = RigidityEnv.empty.markRigid(sym1).markRigid(sym2)
    assertUnifies(tpe1, tpe2, renv)
  }

  private def assertUnifies(tpe1: Type, tpe2: Type, renv: RigidityEnv): Unit = {
    assert(isOk(SetUnification.unify(tpe1, tpe2, renv)))
  }

  private def isOk[T, E](r: Result[T, E]) = r match {
    case Result.Ok(_) => true
    case Result.Err(_) => false
  }

}
