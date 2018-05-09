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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.phase.Unification._
import org.scalatest.FunSuite

class TestUnification extends FunSuite {

  val SL = SourceLocation.Unknown

  /////////////////////////////////////////////////////////////////////////////
  // Substitutions                                                           //
  /////////////////////////////////////////////////////////////////////////////

  test("Substitution.Empty.01") {
    val tpe = Type.Unit
    val subst = Unification.Substitution.empty
    assertResult(Type.Unit)(subst(tpe))
  }

  test("Substitution.Empty.02") {
    val tpe = Type.Bool
    val subst = Unification.Substitution.empty
    assertResult(Type.Bool)(subst(tpe))
  }

  test("Substitution.Empty.03") {
    val tpe = Type.Var(1, Kind.Star)
    val subst = Unification.Substitution.empty
    assertResult(Type.Var(1, Kind.Star))(subst(tpe))
  }

  test("Substitution.Empty.04") {
    val tpe = Type.mkArrow(Type.Bool, Type.Unit)
    val subst = Unification.Substitution.empty
    assertResult(Type.mkArrow(Type.Bool, Type.Unit))(subst(tpe))
  }

  test("Substitution.Singleton.01") {
    val tpe = Type.Var(1, Kind.Star)
    val subst = Unification.Substitution.singleton(Type.Var(2, Kind.Star), Type.Bool)
    assertResult(Type.Var(1, Kind.Star))(subst(tpe))
  }

  test("Substitution.Singleton.02") {
    val tpe = Type.Var(1, Kind.Star)
    val subst = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    assertResult(Type.Bool)(subst(tpe))
  }

  test("Substitution.Singleton.05") {
    val tpe = Type.Var(1, Kind.Star)
    val subst = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Var(2, Kind.Star))
    assertResult(Type.Var(2, Kind.Star))(subst(tpe))
  }

  test("Substitution.++.01") {
    val subst1 = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    val subst2 = Unification.Substitution.singleton(Type.Var(2, Kind.Star), Type.Char)

    val tpe = Type.Var(1, Kind.Star)
    assertResult(Type.Bool)((subst1 ++ subst2) (tpe))
  }

  test("Substitution.++.02") {
    val subst1 = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    val subst2 = Unification.Substitution.singleton(Type.Var(2, Kind.Star), Type.Char)

    val tpe = Type.Var(2, Kind.Star)
    assertResult(Type.Char)((subst1 ++ subst2) (tpe))
  }

  test("Substitution.++.03") {
    val subst1 = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    val subst2 = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Char)

    val tpe = Type.Var(1, Kind.Star)
    assertResult(Type.Bool)((subst1 ++ subst2) (tpe))
  }

  test("Substitution.++.04") {
    val subst1 = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    val subst2 = Unification.Substitution.singleton(Type.Var(2, Kind.Star), Type.Char)

    val tpe = Type.mkArrow(Type.Var(1, Kind.Star), Type.Var(2, Kind.Star))
    assertResult(Type.mkArrow(Type.Bool, Type.Char))((subst1 ++ subst2) (tpe))
  }

  test("Substitution.@@.01") {
    val subst1 = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    val subst2 = Unification.Substitution.singleton(Type.Var(2, Kind.Star), Type.Char)

    val tpe = Type.mkArrow(Type.Var(1, Kind.Star), Type.Var(2, Kind.Star))
    assertResult(Type.mkArrow(Type.Bool, Type.Char))((subst2 @@ subst1) (tpe))
  }

  test("Substitution.@@.02") {
    val subst1 = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    val subst2 = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Char)

    val tpe = Type.Var(1, Kind.Star)
    assertResult(Type.Bool)((subst2 @@ subst1) (tpe))
  }

  test("Substitution.@@.03") {
    val subst1 = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Var(2, Kind.Star))
    val subst2 = Unification.Substitution.singleton(Type.Var(2, Kind.Star), Type.Bool)

    val tpe = Type.Var(1, Kind.Star)
    assertResult(Type.Bool)((subst2 @@ subst1) (tpe))
  }

  test("Substitution.@@.04") {
    val subst1 = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Var(2, Kind.Star))
    val subst2 = Unification.Substitution.singleton(Type.Var(2, Kind.Star), Type.Var(3, Kind.Star))
    val subst3 = Unification.Substitution.singleton(Type.Var(3, Kind.Star), Type.Bool)

    val tpe = Type.Var(1, Kind.Star)
    assertResult(Type.Bool)((subst3 @@ (subst2 @@ subst1)) (tpe))
  }

  test("Unify.Var.01") {
    val result = Unification.unify(Type.Var(1, Kind.Star), Type.Unit)
    assert(result.isOk)
  }

  test("Unify.Var.02") {
    val result = Unification.unify(Type.Unit, Type.Var(1, Kind.Star))
    assert(result.isOk)
  }

  test("Unify.Var.03") {
    val result = Unification.unify(Type.Var(1, Kind.Star), Type.Var(1, Kind.Star))
    assert(result.isOk)
  }

  test("Unify.Var.04") {
    val result = Unification.unify(Type.Var(1, Kind.Star), Type.Var(2, Kind.Star))
    assert(result.isOk)
  }

  test("Unify.Unit") {
    val result = Unification.unify(Type.Unit, Type.Unit)
    assert(result.isOk)
  }

  test("Unify.Bool") {
    val result = Unification.unify(Type.Bool, Type.Bool)
    assert(result.isOk)
  }

  test("Unify.Char") {
    val result = Unification.unify(Type.Char, Type.Char)
    assert(result.isOk)
  }

  test("Unify.Float32") {
    val result = Unification.unify(Type.Float32, Type.Float32)
    assert(result.isOk)
  }

  test("Unify.Float64") {
    val result = Unification.unify(Type.Float64, Type.Float64)
    assert(result.isOk)
  }

  test("Unify.Int8") {
    val result = Unification.unify(Type.Int8, Type.Int8)
    assert(result.isOk)
  }

  test("Unify.Int16") {
    val result = Unification.unify(Type.Int16, Type.Int16)
    assert(result.isOk)
  }

  test("Unify.Int32") {
    val result = Unification.unify(Type.Int32, Type.Int32)
    assert(result.isOk)
  }

  test("Unify.Int64") {
    val result = Unification.unify(Type.Int64, Type.Int64)
    assert(result.isOk)
  }

  test("Unify.BigInt") {
    val result = Unification.unify(Type.BigInt, Type.BigInt)
    assert(result.isOk)
  }

  test("Unify.Str") {
    val result = Unification.unify(Type.Str, Type.Str)
    assert(result.isOk)
  }

  test("Unify.Native") {
    val result = Unification.unify(Type.Native, Type.Native)
    assert(result.isOk)
  }

  test("Unify.Arrow") {
    val result = Unification.unify(Type.Arrow(3), Type.Arrow(3))
    assert(result.isOk)
  }

  test("Unify.FTuple") {
    val result = Unification.unify(Type.Tuple(42), Type.Tuple(42))
    assert(result.isOk)
  }

  test("Unify.Zero") {
    val result = Unification.unify(Type.Zero, Type.Zero)
    assert(result.isOk)
  }

  test("Unify.ZeroSucc.01") {
    val result = Unification.unify(Type.Zero, Type.Succ(0, Type.Zero))
    assert(result.isOk)
  }

  test("Unify.ZeroSucc.02") {
    val result = Unification.unify(Type.Succ(0, Type.Zero), Type.Zero)
    assert(result.isOk)
  }

  test("Unify.Succ.01") {
    val freshVar = Type.Var(1, Kind.Star)
    val result = Unification.unify(Type.Succ(1, freshVar), Type.Succ(1, freshVar))
    assert(result.isOk)
  }

  test("Unify.Succ.02") {
    val freshVar = Type.Var(1, Kind.Star)
    val result = Unification.unify(Type.Succ(1, freshVar), Type.Succ(2, freshVar))
    assert(result.isOk)
  }

  test("Unify.Succ.03") {
    val freshVar = Type.Var(1, Kind.Star)
    val freshVar2 = Type.Var(1, Kind.Star)
    val result = Unification.unify(Type.Succ(1, freshVar), Type.Succ(2, freshVar2))
    assert(result.isOk)
  }

  test("Unify.Succ.04") {
    val freshVar = Type.Var(1, Kind.Star)
    val result = Unification.unify(Type.Succ(1, freshVar), Type.Succ(2, Type.Zero))
    assert(result.isOk)
  }

  test("Unify.Succ.05") {
    val freshVar = Type.Var(1, Kind.Star)
    val tpe1 = Type.mkVector(Type.Bool, Type.Succ(7, Type.Zero))
    val tpe2 = Type.mkVector(Type.Bool, Type.Succ(5, freshVar))
    val result = Unification.unify(tpe1, tpe2)
    assert(result.isOk)
  }

  test("Unify.Succ.06") {
    val freshVar = Type.Var(1, Kind.Star)
    val freshVar2 = Type.Var(1, Kind.Star)
    val result = Unification.unify(Type.Succ(1, freshVar), Type.Succ(2, Type.Zero))
    assert(result.isOk)
  }

  test("Unify.Enum.01") {
    val sym = Symbol.mkEnumSym("Color")
    val result = Unification.unify(Type.Enum(sym, Kind.Star), Type.Enum(sym, Kind.Star))
    assert(result.isOk)
  }

  test("Unify.01") {
    val tpe1 = Type.Var(1, Kind.Star)
    val tpe2 = Type.Bool
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(Type.Bool)(result(tpe1))
  }

  test("Unify.02") {
    val tpe1 = Type.Bool
    val tpe2 = Type.Var(1, Kind.Star)
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(Type.Bool)(result(tpe2))
  }

  test("Unify.03") {
    val A = Type.Var(1, Kind.Star)
    val tpe1 = Type.mkArrow(Type.Bool, Type.Char)
    val tpe2 = Type.mkArrow(Type.Bool, A)
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(Type.Char)(result(A))
  }

  test("Unify.04") {
    val A = Type.Var(1, Kind.Star)
    val tpe1 = Type.mkArrow(Type.Bool, Type.Char)
    val tpe2 = Type.mkArrow(Type.Bool, A)
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(Type.Char)(result(A))
  }

  test("Unify.05") {
    val A = Type.Var(1, Kind.Star)
    val tpe1 = Type.mkArrow(Type.Bool, Type.Char)
    val tpe2 = A
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(tpe1)(result(A))
  }

  test("Unify.06") {
    val A = Type.Var(1, Kind.Star)
    val tpe1 = A
    val tpe2 = Type.mkArrow(Type.Bool, Type.Char)
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(tpe2)(result(A))
  }

  test("Unify.07") {
    val A = Type.Var(1, Kind.Star)
    val tpe1 = Type.mkArrow(A, Type.Bool)
    val tpe2 = Type.mkArrow(Type.Bool, A)
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(Type.Bool)(result(A))
  }

  test("Unify.08") {
    val A = Type.Var(1, Kind.Star)
    val B = Type.Var(2, Kind.Star)
    val tpe1 = Type.mkArrow(A, B)
    val tpe2 = Type.mkArrow(Type.Bool, Type.Char)
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(Type.Bool)(result(A))
    assertResult(Type.Char)(result(B))
  }

  test("Unify.09") {
    val A = Type.Var(1, Kind.Star)
    val B = Type.Var(2, Kind.Star)
    val tpe1 = Type.mkArrow(Type.Bool, Type.Char)
    val tpe2 = Type.mkArrow(A, B)
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(Type.Bool)(result(A))
    assertResult(Type.Char)(result(B))
  }

  test("Unify.10") {
    val A = Type.Var(1, Kind.Star)
    val B = Type.Var(2, Kind.Star)
    val tpe1 = Type.mkArrow(A, Type.Char)
    val tpe2 = Type.mkArrow(Type.Bool, B)
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(Type.Bool)(result(A))
    assertResult(Type.Char)(result(B))
  }

  test("Unify.11") {
    val A = Type.Var(1, Kind.Star)
    val B = Type.Var(2, Kind.Star)
    val C = Type.Var(3, Kind.Star)
    val tpe1 = Type.mkArrow(A, B)
    val tpe2 = Type.mkArrow(C, Type.Bool)
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(Type.Bool)(result(B))
    assertResult(C)(result(A))
  }

  test("liftM.01") {
    val subst0 = Substitution.empty
    val result = Unification.liftM(Type.Bool, subst0).run(subst0)
    val (_, tpe) = result.get
    assertResult(Type.Bool)(tpe)
  }

  test("liftM.02") {
    val tpe1 = Type.Var(1, Kind.Star)
    val tpe2 = Type.Bool
    val subst0 = Substitution.singleton(tpe1, tpe2)
    val result = Unification.liftM(tpe1, subst0).run(subst0)
    val (subst, _) = result.get
    assertResult(Type.Bool)(subst.m(tpe1))
  }

  test("unifyM.01") {
    val subst0 = Substitution.empty
    val result = Unification.unifyM(Type.Bool, Type.Bool, SL).run(subst0)
    assert(result.isOk)
  }

  test("unifyM.02") {
    val subst0 = Substitution.empty
    val result = Unification.unifyM(Type.Bool, Type.Char, SL).run(subst0)
    assert(result.isErr)
  }

  test("unifyM.03") {
    val tpe1 = Type.Var(1, Kind.Star)
    val tpe2 = Type.Bool
    val subst0 = Substitution.empty
    val result = Unification.unifyM(tpe1, tpe2, SL).run(subst0)
    val (subst, tpe) = result.get
    assertResult(Type.Bool)(subst(tpe1))
    assertResult(Type.Bool)(subst(tpe2))
    assertResult(Type.Bool)(tpe)
  }

  test("seqM.01") {
    val subst0 = Substitution.empty
    val res1 = Unification.unifyM(Type.Bool, Type.Bool, SL)
    val res2 = Unification.unifyM(Type.Char, Type.Char, SL)
    val result = seqM(List(res1, res2)).run(subst0)
    assert(result.isOk)
  }


  test("seqM.02") {
    val subst0 = Substitution.empty
    val res1 = Unification.unifyM(Type.Bool, Type.Char, SL)
    val res2 = Unification.unifyM(Type.Bool, Type.Char, SL)
    val result = seqM(List(res1, res2)).run(subst0)
    assert(result.isErr)
  }

  test("seqM.03") {
    val subst0 = Substitution.empty
    val res1 = Unification.unifyM(Type.Var(1, Kind.Star), Type.Bool, SL)
    val res2 = Unification.unifyM(Type.Var(2, Kind.Star), Type.Char, SL)
    val res3 = Unification.unifyM(Type.Var(3, Kind.Star), Type.mkTuple(List(Type.Var(1, Kind.Star), Type.Var(2, Kind.Star))), SL)
    val result = seqM(List(res1, res2, res3)).run(subst0)
    val (subst, tpe) = result.get
    assertResult(Type.Bool)(subst.m(Type.Var(1, Kind.Star)))
    assertResult(Type.Char)(subst.m(Type.Var(2, Kind.Star)))
    assertResult(Type.mkTuple(List(Type.Bool, Type.Char)))(subst.m(Type.Var(3, Kind.Star)))
  }

}
