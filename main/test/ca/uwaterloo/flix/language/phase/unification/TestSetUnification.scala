/*
 *  Copyright 2022 Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.{Ast, Kind, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.Result
import org.scalatest.funsuite.AnyFunSuite

class TestSetUnification extends AnyFunSuite with TestUtils {

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
    val tpe1 = Type.Var(mkTypeVarSym("e"), loc)
    val tpe2 = Type.mkUnion(tpe1, tpe1, loc)
    assertUnifies(tpe1, tpe2, RigidityEnv.empty)
  }

  test("Test.SetUnification.05") {
    // e! ≐ e! ∪ e!
    val sym = mkTypeVarSym("e")
    val tpe1 = Type.Var(sym, loc)
    val tpe2 = Type.mkUnion(tpe1, tpe1, loc)
    val renv = RigidityEnv.empty.markRigid(sym)
    assertUnifies(tpe1, tpe2, renv)
  }

  test("Test.SetUnification.06") {
    // e ≐ f
    val tpe1 = Type.Var(mkTypeVarSym("e"), loc)
    val tpe2 = Type.Var(mkTypeVarSym("f"), loc)
    assertUnifies(tpe1, tpe2, RigidityEnv.empty)
  }

  test("Test.SetUnification.07") {
    // ((E ∪ (e ∩ F)) ∩ Fᶜ) ∪ ((F ∪ (f ∩ E)) ∩ Eᶜ) ≐ E ∪ F
    val effE = Type.Cst(TypeConstructor.Effect(mkEffectSym("E")), loc)
    val effF = Type.Cst(TypeConstructor.Effect(mkEffectSym("F")), loc)
    val varE = Type.Var(mkTypeVarSym("e"), loc)
    val varF = Type.Var(mkTypeVarSym("f"), loc)

    val tpe1 = Type.mkUnion(
      Type.mkIntersection(
        Type.mkUnion(
          effE,
          Type.mkIntersection(
            varE,
            effF,
            loc
          ),
          loc
        ),
        Type.mkComplement(effF, loc),
        loc
      ),
      Type.mkIntersection(
        Type.mkUnion(
          effF,
          Type.mkIntersection(
            varF,
            effE,
            loc
          ),
          loc
        ),
        Type.mkComplement(effE, loc),
        loc
      ),
      loc
    )

    val tpe2 = Type.mkUnion(effE, effF, loc)

    assertUnifies(tpe1, tpe2, RigidityEnv.empty)
  }

  test("Test.SetUnification.08") {
    // ((E ∪ (e ∩ F)) ∩ Fᶜ) ≐ E
    val effE = Type.Cst(TypeConstructor.Effect(mkEffectSym("E")), loc)
    val effF = Type.Cst(TypeConstructor.Effect(mkEffectSym("F")), loc)
    val varE = Type.Var(mkTypeVarSym("e"), loc)

    val tpe1 = Type.mkIntersection(
      Type.mkUnion(
        effE,
        Type.mkIntersection(
          varE,
          effF,
          loc
        ),
        loc
      ),
      Type.mkComplement(effF, loc),
      loc
    )

    val tpe2 = effE

    assertUnifies(tpe1, tpe2, RigidityEnv.empty)
  }

  test("Test.SetUnification.09") {
    // E ∩ Fᶜ ≐ E
    val effE = Type.Cst(TypeConstructor.Effect(mkEffectSym("E")), loc)
    val effF = Type.Cst(TypeConstructor.Effect(mkEffectSym("F")), loc)

    val tpe1 = Type.mkIntersection(
      effE,
      Type.mkComplement(effF, loc),
      loc
    )

    val tpe2 = effE

    assertUnifies(tpe1, tpe2, RigidityEnv.empty)
  }

  test("Test.SetUnification.10") {
    // e! ∪ f! ≐ f! ∪ e!
    val symE = mkTypeVarSym("e")
    val symF = mkTypeVarSym("f")
    val e = Type.Var(symE, loc)
    val f = Type.Var(symF, loc)
    val tpe1 = Type.mkUnion(e, f, loc)
    val tpe2 = Type.mkUnion(f, e, loc)
    val renv = RigidityEnv.empty.markRigid(symE).markRigid(symF)
    assertUnifies(tpe1, tpe2, renv)
  }

  test("Test.SetUnification.11") {
    // e! ∪ f! ≐ e! ∪ f!
    val symE = mkTypeVarSym("e")
    val symF = mkTypeVarSym("f")
    val e = Type.Var(symE, loc)
    val f = Type.Var(symF, loc)
    val tpe1 = Type.mkUnion(e, f, loc)
    val tpe2 = Type.mkUnion(e, f, loc)
    val renv = RigidityEnv.empty.markRigid(symE).markRigid(symF)
    assertUnifies(tpe1, tpe2, renv)
  }

  test("Test.SetUnification.12") {
    // e ≐ f!
    val symE = mkTypeVarSym("e")
    val symF = mkTypeVarSym("f")
    val tpe1 = Type.Var(symE, loc)
    val tpe2 = Type.Var(symF, loc)
    val renv = RigidityEnv.empty.markRigid(symF)
    assertUnifies(tpe1, tpe2, renv)
  }

  test("Test.SetUnification.13") {
    // e ∪ f ≐ g! ∪ h!
    val symE = mkTypeVarSym("e")
    val symF = mkTypeVarSym("f")
    val symG = mkTypeVarSym("g")
    val symH = mkTypeVarSym("h")

    val tpe1 = Type.mkUnion(
      Type.Var(symE, loc),
      Type.Var(symF, loc),
      loc
    )
    val tpe2 = Type.mkUnion(
      Type.Var(symG, loc),
      Type.Var(symH, loc),
      loc
    )
    val renv = RigidityEnv.empty.markRigid(symG).markRigid(symH)
    assertUnifies(tpe1, tpe2, renv)
  }

  test("Test.SetUnification.14") {
    // (e ∪ (f! ∩ g!)) ∩ (g! ∪ f!) ≐ f!
    val symE = mkTypeVarSym("e")
    val symF = mkTypeVarSym("f")
    val symG = mkTypeVarSym("g")

    val e = Type.Var(symE, loc)
    val f = Type.Var(symF, loc)
    val g = Type.Var(symG, loc)

    val tpe1 = Type.mkIntersection(
      Type.mkUnion(
        e,
        Type.mkIntersection(f, g, loc)
        ,loc
      ),
      Type.mkUnion(g, f, loc),
      loc
    )
    val tpe2 = f

    val renv = RigidityEnv.empty.markRigid(symF).markRigid(symG)
    assertUnifies(tpe1, tpe2, renv)
  }

  test("Test.SetUnification.15") {
    // e - (Print ∪ Throw) ≐ Time
    val print = Type.Cst(TypeConstructor.Effect(mkEffectSym("Print")), loc)
    val time = Type.Cst(TypeConstructor.Effect(mkEffectSym("Time")), loc)
    val thro = Type.Cst(TypeConstructor.Effect(mkEffectSym("Throw")), loc)

    val tpe1 = Type.mkDifference(Type.Var(mkTypeVarSym("e"), loc), Type.mkUnion(print, thro, loc), loc)
    val tpe2 = time
    assertUnifies(tpe1, tpe2, RigidityEnv.empty)
  }

  test("Test.SetUnification.16") {
    // e! ∪ ∅ ≐ e!
    val sym = mkTypeVarSym("e")
    // build the union rather than going through smart constructors
    val varE = Type.Var(sym, loc)
    val tpe1 = Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, loc), varE, loc), Type.Empty, loc)
    val tpe2 = varE
    val renv = RigidityEnv.empty.markRigid(sym)
    assertUnifies(tpe1, tpe2, renv)
  }

  test("Test.SetUnification.Fail.01") {
    // e! ≐ f!
    val sym1 = mkTypeVarSym("e")
    val sym2 = mkTypeVarSym("f")
    val tpe1 = Type.Var(sym1, loc)
    val tpe2 = Type.Var(sym2, loc)
    val renv = RigidityEnv.empty.markRigid(sym1).markRigid(sym2)
    assertDoesNotUnify(tpe1, tpe2, renv)
  }

  test("Test.SetUnification.Fail.02") {
    // E ≐ F
    val sym1 = mkEffectSym("E")
    val sym2 = mkEffectSym("F")
    val tpe1 = Type.Cst(TypeConstructor.Effect(sym1), loc)
    val tpe2 = Type.Cst(TypeConstructor.Effect(sym2), loc)
    assertDoesNotUnify(tpe1, tpe2, RigidityEnv.empty)
  }


  private def mkTypeVarSym(name: String): Symbol.KindedTypeVarSym = {
    Symbol.freshKindedTypeVarSym(Ast.VarText.SourceText(name), Kind.Effect, isRegion = false, loc)
  }

  private def mkEffectSym(name: String): Symbol.EffectSym = new Symbol.EffectSym(Nil, name, loc)

  private def assertUnifies(tpe1: Type, tpe2: Type, renv: RigidityEnv): Unit = {
    assert(isOk(SetUnification.unify(tpe1, tpe2, renv)))
  }

  private def assertDoesNotUnify(tpe1: Type, tpe2: Type, renv: RigidityEnv): Unit = {
    assert(!isOk(SetUnification.unify(tpe1, tpe2, renv)))
  }

  private def isOk[T, E](r: Result[T, E]) = r match {
    case Result.Ok(_) => true
    case Result.Err(_) => false
  }

}
