/*
 *  Copyright 2023 Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.{Ast, Kind, Name, RigidityEnv, SourceLocation, SourcePosition, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.Result
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.SortedSet

class TestCaseSetUnification extends AnyFunSuite with TestUtils {

  private implicit val flix: Flix = new Flix()

  private val loc: SourceLocation = SourceLocation.Unknown

  private def mkIdent(name: String): Name.Ident = Name.Ident(name, SourceLocation.Unknown)

  private val E = Symbol.mkRestrictableEnumSym(
    Name.RootNS,
    mkIdent("E"),
    List("C1", "C2", "C3").map(mkIdent)
  )
  private val C1 = Symbol.mkRestrictableCaseSym(E, mkIdent("C1"))
  private val C2 = Symbol.mkRestrictableCaseSym(E, mkIdent("C2"))
  private val C3 = Symbol.mkRestrictableCaseSym(E, mkIdent("C3"))

  private val Expr = Symbol.mkRestrictableEnumSym(
    Name.RootNS,
    mkIdent("Expr"),
    List("And", "Xor", "Or", "Var", "Not", "Cst").map(mkIdent)
  )
  private val And = Symbol.mkRestrictableCaseSym(Expr, mkIdent("And"))
  private val Xor = Symbol.mkRestrictableCaseSym(Expr, mkIdent("Xor"))
  private val Or = Symbol.mkRestrictableCaseSym(Expr, mkIdent("Or"))
  private val Var = Symbol.mkRestrictableCaseSym(Expr, mkIdent("Var"))
  private val Not = Symbol.mkRestrictableCaseSym(Expr, mkIdent("Not"))
  private val Cst = Symbol.mkRestrictableCaseSym(Expr, mkIdent("Cst"))

  test("Test.CaseSetUnification.01") {
    // ∅ ≐ ∅
    val tpe1 = Type.Cst(TypeConstructor.CaseSet(SortedSet.empty, E), loc)
    val tpe2 = Type.Cst(TypeConstructor.CaseSet(SortedSet.empty, E), loc)
    assertUnifies(tpe1, tpe2, RigidityEnv.empty, E)
  }

  test("Test.CaseSetUnification.02") {
    // C1 ∪ C2 ∪ C3 ≐ ∅ᶜ
    val caseC1 = Type.Cst(TypeConstructor.CaseSet(SortedSet(C1), E), loc)
    val caseC2 = Type.Cst(TypeConstructor.CaseSet(SortedSet(C2), E), loc)
    val caseC3 = Type.Cst(TypeConstructor.CaseSet(SortedSet(C3), E), loc)

    val tpe1 = Type.mkCaseUnion(
      Type.mkCaseUnion(
        caseC1,
        caseC2,
        E,
        loc
      ),
      caseC3,
      E,
      loc
    )

    val tpe2 = Type.mkCaseComplement(Type.Cst(TypeConstructor.CaseSet(SortedSet.empty, E), loc), E, loc)

    val renv = RigidityEnv.empty
    assertUnifies(tpe1, tpe2, renv, E)
  }

  test("Test.CaseSetUnification.03") {
    // ∅ ≐ ∅ ∪ ∅
    val tpe1 = Type.Cst(TypeConstructor.CaseSet(SortedSet.empty, E), loc)
    val tpe2 = Type.mkCaseUnion(Type.Cst(TypeConstructor.CaseSet(SortedSet.empty, E), loc), Type.Cst(TypeConstructor.CaseSet(SortedSet.empty, E), loc), E, loc)
    assertUnifies(tpe1, tpe2, RigidityEnv.empty, E)
  }

  test("Test.CaseSetUnification.04") {
    // e ≐ e ∪ e
    val tpe1 = Type.Var(mkTypeVarSym("e", E), loc)
    val tpe2 = Type.mkCaseUnion(tpe1, tpe1, E, loc)
    assertUnifies(tpe1, tpe2, RigidityEnv.empty, E)
  }

  test("Test.CaseSetUnification.05") {
    // e! ≐ e! ∪ e!
    val sym = mkTypeVarSym("e", E)
    val tpe1 = Type.Var(sym, loc)
    val tpe2 = Type.mkCaseUnion(tpe1, tpe1, E, loc)
    val renv = RigidityEnv.empty.markRigid(sym)
    assertUnifies(tpe1, tpe2, renv, E)
  }

  test("Test.CaseSetUnification.06") {
    // e ≐ f
    val tpe1 = Type.Var(mkTypeVarSym("e", E), loc)
    val tpe2 = Type.Var(mkTypeVarSym("f", E), loc)
    assertUnifies(tpe1, tpe2, RigidityEnv.empty, E)
  }

  test("Test.CaseSetUnification.07") {
    // ((C1 ∪ (e ∩ C2)) ∩ C2ᶜ) ∪ ((C2 ∪ (f ∩ C1)) ∩ C1ᶜ) ≐ C1 ∪ C2
    val caseC1 = Type.Cst(TypeConstructor.CaseSet(SortedSet(C1), E), loc)
    val caseC2 = Type.Cst(TypeConstructor.CaseSet(SortedSet(C2), E), loc)
    val varE = Type.Var(mkTypeVarSym("e", E), loc)
    val varF = Type.Var(mkTypeVarSym("f", E), loc)

    val tpe1 = Type.mkCaseUnion(
      Type.mkCaseIntersection(
        Type.mkCaseUnion(
          caseC1,
          Type.mkCaseIntersection(
            varE,
            caseC2,
            E,
            loc
          ),
          E,
          loc
        ),
        Type.mkCaseComplement(caseC2, E, loc),
        E,
        loc
      ),
      Type.mkCaseIntersection(
        Type.mkCaseUnion(
          caseC2,
          Type.mkCaseIntersection(
            varF,
            caseC1,
            E,
            loc
          ),
          E,
          loc
        ),
        Type.mkCaseComplement(caseC1, E, loc),
        E,
        loc
      ),
      E,
      loc
    )

    val tpe2 = Type.mkCaseUnion(caseC1, caseC2, E, loc)

    assertUnifies(tpe1, tpe2, RigidityEnv.empty, E)
  }

  test("Test.CaseSetUnification.08") {
    // ((C1 ∪ (e ∩ C2)) ∩ C2ᶜ) ≐ C1
    val caseC1 = Type.Cst(TypeConstructor.CaseSet(SortedSet(C1), E), loc)
    val caseC2 = Type.Cst(TypeConstructor.CaseSet(SortedSet(C2), E), loc)
    val varE = Type.Var(mkTypeVarSym("e", E), loc)

    val tpe1 = Type.mkCaseIntersection(
      Type.mkCaseUnion(
        caseC1,
        Type.mkCaseIntersection(
          varE,
          caseC2,
          E,
          loc
        ),
        E,
        loc
      ),
      Type.mkCaseComplement(caseC2, E, loc),
      E,
      loc
    )

    val tpe2 = caseC1

    assertUnifies(tpe1, tpe2, RigidityEnv.empty, E)
  }

  test("Test.CaseSetUnification.09") {
    // C1 ∩ C2ᶜ ≐ C1
    val caseC1 = Type.Cst(TypeConstructor.CaseSet(SortedSet(C1), E), loc)
    val caseC2 = Type.Cst(TypeConstructor.CaseSet(SortedSet(C2), E), loc)

    val tpe1 = Type.mkCaseIntersection(
      caseC1,
      Type.mkCaseComplement(caseC2, E, loc),
      E,
      loc
    )

    val tpe2 = caseC1

    assertUnifies(tpe1, tpe2, RigidityEnv.empty, E)
  }

  test("Test.CaseSetUnification.10") {
    // e! ∪ f! ≐ f! ∪ e!
    val symE = mkTypeVarSym("e", E)
    val symF = mkTypeVarSym("f", E)
    val e = Type.Var(symE, loc)
    val f = Type.Var(symF, loc)
    val tpe1 = Type.mkCaseUnion(e, f, E, loc)
    val tpe2 = Type.mkCaseUnion(f, e, E, loc)
    val renv = RigidityEnv.empty.markRigid(symE).markRigid(symF)
    assertUnifies(tpe1, tpe2, renv, E)
  }

  test("Test.CaseSetUnification.11") {
    // e! ∪ f! ≐ e! ∪ f!
    val symE = mkTypeVarSym("e", E)
    val symF = mkTypeVarSym("f", E)
    val e = Type.Var(symE, loc)
    val f = Type.Var(symF, loc)
    val tpe1 = Type.mkCaseUnion(e, f, E, loc)
    val tpe2 = Type.mkCaseUnion(e, f, E, loc)
    val renv = RigidityEnv.empty.markRigid(symE).markRigid(symF)
    assertUnifies(tpe1, tpe2, renv, E)
  }

  test("Test.CaseSetUnification.12") {
    // e ≐ f!
    val symE = mkTypeVarSym("e", E)
    val symF = mkTypeVarSym("f", E)
    val tpe1 = Type.Var(symE, loc)
    val tpe2 = Type.Var(symF, loc)
    val renv = RigidityEnv.empty.markRigid(symF)
    assertUnifies(tpe1, tpe2, renv, E)
  }

  test("Test.CaseSetUnification.13") {
    // e ∪ f ≐ g! ∪ h!
    val symE = mkTypeVarSym("e", E)
    val symF = mkTypeVarSym("f", E)
    val symG = mkTypeVarSym("g", E)
    val symH = mkTypeVarSym("h", E)

    val tpe1 = Type.mkCaseUnion(
      Type.Var(symE, loc),
      Type.Var(symF, loc),
      E,
      loc
    )
    val tpe2 = Type.mkCaseUnion(
      Type.Var(symG, loc),
      Type.Var(symH, loc),
      E,
      loc
    )
    val renv = RigidityEnv.empty.markRigid(symG).markRigid(symH)
    assertUnifies(tpe1, tpe2, renv, E)
  }

  test("Test.CaseSetUnification.14") {
    // (e ∪ (f! ∩ g!)) ∩ (g! ∪ f!) ≐ f!
    val symE = mkTypeVarSym("e", E)
    val symF = mkTypeVarSym("f", E)
    val symG = mkTypeVarSym("g", E)

    val e = Type.Var(symE, loc)
    val f = Type.Var(symF, loc)
    val g = Type.Var(symG, loc)

    val tpe1 = Type.mkCaseIntersection(
      Type.mkCaseUnion(
        e,
        Type.mkCaseIntersection(f, g, E, loc),
        E,
        loc
      ),
      Type.mkCaseUnion(g, f, E, loc),
      E,
      loc
    )
    val tpe2 = f

    val renv = RigidityEnv.empty.markRigid(symF).markRigid(symG)
    assertUnifies(tpe1, tpe2, renv, E)
  }

  test("Test.CaseSetUnification.15") {
    // e - (C1 ∪ C2) ≐ C3
    val caseC1 = Type.Cst(TypeConstructor.CaseSet(SortedSet(C1), E), loc)
    val caseC2 = Type.Cst(TypeConstructor.CaseSet(SortedSet(C2), E), loc)
    val caseC3 = Type.Cst(TypeConstructor.CaseSet(SortedSet(C3), E), loc)

    val tpe1 = Type.mkCaseDifference(
      Type.Var(mkTypeVarSym("e", E), loc),
      Type.mkCaseUnion(caseC1, caseC2, E, loc),
      E,
      loc
    )
    val tpe2 = caseC3
    assertUnifies(tpe1, tpe2, RigidityEnv.empty, E)
  }

  test("Test.CaseSetUnification.16") {
    // e! ∪ ∅ ≐ e!
    val sym = mkTypeVarSym("e", E)
    // build the union rather than going through smart constructors
    val varE = Type.Var(sym, loc)
    val tpe1 = Type.Apply(Type.Apply(Type.Cst(TypeConstructor.CaseUnion(E), loc), varE, loc), Type.Cst(TypeConstructor.CaseSet(SortedSet.empty, E), loc), loc)
    val tpe2 = varE
    val renv = RigidityEnv.empty.markRigid(sym)
    assertUnifies(tpe1, tpe2, renv, E)
  }

  test("Test.CaseSetUnification.17") {
    val varS1 = mkTypeVarSym("s1", Expr)
    val varS2 = mkTypeVarSym("s2", Expr)

    val caseAnd = Type.Cst(TypeConstructor.CaseSet(SortedSet(And), Expr), loc)
    val caseXor = Type.Cst(TypeConstructor.CaseSet(SortedSet(Xor), Expr), loc)
    val caseOr = Type.Cst(TypeConstructor.CaseSet(SortedSet(Or), Expr), loc)

    val tpe1 = Type.mkCaseIntersection(
      Type.Var(varS1, loc),
      Type.mkCaseUnion(
        caseAnd,
        caseXor,
        Expr,
        loc
      ),
      Expr,
      loc
    )

    val tpe2 = Type.mkCaseIntersection(
      Type.Var(varS2, loc),
      Type.mkCaseUnion(
        caseOr,
        caseXor,
        Expr,
        loc
      ),
      Expr,
      loc
    )

    val renv = RigidityEnv.empty
    assertUnifies(tpe1, tpe2, renv, Expr)
  }

  test("Test.CaseSetUnification.18") {
    val varS1 = mkTypeVarSym("s1", Expr)
    val varS2 = mkTypeVarSym("s2", Expr)

    val caseAnd = Type.Cst(TypeConstructor.CaseSet(SortedSet(And), Expr), loc)
    val caseXor = Type.Cst(TypeConstructor.CaseSet(SortedSet(Xor), Expr), loc)

    val tpe1 = Type.mkCaseUnion(
      Type.Var(varS1, loc),
      caseXor,
      Expr,
      loc
    )

    val tpe2 = Type.mkCaseIntersection(
      Type.Var(varS2, loc),
      Type.mkCaseUnion(
        caseXor,
        caseAnd,
        Expr,
        loc
      ),
      Expr,
      loc
    )

    val renv = RigidityEnv.empty
    assertUnifies(tpe1, tpe2, renv, Expr)
  }

  test("Test.CaseSetUnification.Fail.01") {
    // e! ≐ f!
    val sym1 = mkTypeVarSym("e", E)
    val sym2 = mkTypeVarSym("f", E)
    val tpe1 = Type.Var(sym1, loc)
    val tpe2 = Type.Var(sym2, loc)
    val renv = RigidityEnv.empty.markRigid(sym1).markRigid(sym2)
    assertDoesNotUnify(tpe1, tpe2, renv, E)
  }

  test("Test.CaseSetUnification.Fail.02") {
    // C1 ≐ C2
    val tpe1 = Type.Cst(TypeConstructor.CaseSet(SortedSet(C1), E), loc)
    val tpe2 = Type.Cst(TypeConstructor.CaseSet(SortedSet(C2), E), loc)
    assertDoesNotUnify(tpe1, tpe2, RigidityEnv.empty, E)
  }


  private def mkTypeVarSym(name: String, enumSym: Symbol.RestrictableEnumSym): Symbol.KindedTypeVarSym = {
    Symbol.freshKindedTypeVarSym(Ast.VarText.SourceText(name), Kind.CaseSet(enumSym), isRegion = false, loc)
  }

  private def assertUnifies(tpe1: Type, tpe2: Type, renv: RigidityEnv, enumSym: Symbol.RestrictableEnumSym): Unit = {
    assert(isOk(CaseSetUnification.unify(tpe1, tpe2, renv, enumSym.universe, enumSym)))
  }

  private def assertDoesNotUnify(tpe1: Type, tpe2: Type, renv: RigidityEnv, enumSym: Symbol.RestrictableEnumSym): Unit = {
    assert(!isOk(CaseSetUnification.unify(tpe1, tpe2, renv, enumSym.universe, enumSym)))
  }

  private def isOk[T, E](r: Result[T, E]) = r match {
    case Result.Ok(_) => true
    case Result.Err(_) => false
  }

}
