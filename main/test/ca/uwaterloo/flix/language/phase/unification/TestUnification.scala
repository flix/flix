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
import ca.uwaterloo.flix.language.ast.{Kind, Name, Rigidity, SourceLocation, Type}
import ca.uwaterloo.flix.language.phase.unification.InferMonad.seqM
import ca.uwaterloo.flix.util.Result
import org.scalatest.FunSuite

class TestUnification extends FunSuite with TestUtils {

  implicit val flix: Flix = new Flix()

  val loc: SourceLocation = SourceLocation.Unknown

  /////////////////////////////////////////////////////////////////////////////
  // Substitutions                                                           //
  /////////////////////////////////////////////////////////////////////////////

  test("Substitution.Empty.01") {
    val tpe = Type.Unit
    val subst = Substitution.empty
    assertResult(Type.Unit)(subst(tpe))
  }

  test("Substitution.Empty.02") {
    val tpe = Type.Bool
    val subst = Substitution.empty
    assertResult(Type.Bool)(subst(tpe))
  }

  test("Substitution.Empty.03") {
    val tpe = Type.Var(1, Kind.Star)
    val subst = Substitution.empty
    assertResult(Type.Var(1, Kind.Star))(subst(tpe))
  }

  test("Substitution.Empty.04") {
    val tpe = Type.mkPureArrow(Type.Bool, Type.Unit)
    val subst = Substitution.empty
    assertResult(Type.mkPureArrow(Type.Bool, Type.Unit))(subst(tpe))
  }

  test("Substitution.Singleton.01") {
    val tpe = Type.Var(1, Kind.Star)
    val subst = Substitution.singleton(Type.Var(2, Kind.Star), Type.Bool)
    assertResult(Type.Var(1, Kind.Star))(subst(tpe))
  }

  test("Substitution.Singleton.02") {
    val tpe = Type.Var(1, Kind.Star)
    val subst = Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    assertResult(Type.Bool)(subst(tpe))
  }

  test("Substitution.Singleton.05") {
    val tpe = Type.Var(1, Kind.Star)
    val subst = Substitution.singleton(Type.Var(1, Kind.Star), Type.Var(2, Kind.Star))
    assertResult(Type.Var(2, Kind.Star))(subst(tpe))
  }

  test("Substitution.++.01") {
    val subst1 = Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    val subst2 = Substitution.singleton(Type.Var(2, Kind.Star), Type.Char)

    val tpe = Type.Var(1, Kind.Star)
    assertResult(Type.Bool)((subst1 ++ subst2) (tpe))
  }

  test("Substitution.++.02") {
    val subst1 = Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    val subst2 = Substitution.singleton(Type.Var(2, Kind.Star), Type.Char)

    val tpe = Type.Var(2, Kind.Star)
    assertResult(Type.Char)((subst1 ++ subst2) (tpe))
  }

  test("Substitution.++.03") {
    val subst1 = Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    val subst2 = Substitution.singleton(Type.Var(1, Kind.Star), Type.Char)

    val tpe = Type.Var(1, Kind.Star)
    assertResult(Type.Bool)((subst1 ++ subst2) (tpe))
  }

  test("Substitution.++.04") {
    val subst1 = Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    val subst2 = Substitution.singleton(Type.Var(2, Kind.Star), Type.Char)

    val tpe = Type.mkPureArrow(Type.Var(1, Kind.Star), Type.Var(2, Kind.Star))
    assertResult(Type.mkPureArrow(Type.Bool, Type.Char))((subst1 ++ subst2) (tpe))
  }

  test("Substitution.@@.01") {
    val subst1 = Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    val subst2 = Substitution.singleton(Type.Var(2, Kind.Star), Type.Char)

    val tpe = Type.mkPureArrow(Type.Var(1, Kind.Star), Type.Var(2, Kind.Star))
    assertResult(Type.mkPureArrow(Type.Bool, Type.Char))((subst2 @@ subst1) (tpe))
  }

  test("Substitution.@@.02") {
    val subst1 = Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    val subst2 = Substitution.singleton(Type.Var(1, Kind.Star), Type.Char)

    val tpe = Type.Var(1, Kind.Star)
    assertResult(Type.Bool)((subst2 @@ subst1) (tpe))
  }

  test("Substitution.@@.03") {
    val subst1 = Substitution.singleton(Type.Var(1, Kind.Star), Type.Var(2, Kind.Star))
    val subst2 = Substitution.singleton(Type.Var(2, Kind.Star), Type.Bool)

    val tpe = Type.Var(1, Kind.Star)
    assertResult(Type.Bool)((subst2 @@ subst1) (tpe))
  }

  test("Substitution.@@.04") {
    val subst1 = Substitution.singleton(Type.Var(1, Kind.Star), Type.Var(2, Kind.Star))
    val subst2 = Substitution.singleton(Type.Var(2, Kind.Star), Type.Var(3, Kind.Star))
    val subst3 = Substitution.singleton(Type.Var(3, Kind.Star), Type.Bool)

    val tpe = Type.Var(1, Kind.Star)
    assertResult(Type.Bool)((subst3 @@ (subst2 @@ subst1)) (tpe))
  }

  test("Unify.Var.01") {
    val result = Unification.unifyTypes(Type.Var(1, Kind.Star), Type.Unit)
    assert(isOk(result))
  }

  test("Unify.Var.02") {
    val result = Unification.unifyTypes(Type.Unit, Type.Var(1, Kind.Star))
    assert(isOk(result))
  }

  test("Unify.Var.03") {
    val result = Unification.unifyTypes(Type.Var(1, Kind.Star), Type.Var(1, Kind.Star))
    assert(isOk(result))
  }

  test("Unify.Var.04") {
    val result = Unification.unifyTypes(Type.Var(1, Kind.Star), Type.Var(2, Kind.Star))
    assert(isOk(result))
  }

  test("Unify.Unit") {
    val result = Unification.unifyTypes(Type.Unit, Type.Unit)
    assert(isOk(result))
  }

  test("Unify.Bool") {
    val result = Unification.unifyTypes(Type.Bool, Type.Bool)
    assert(isOk(result))
  }

  test("Unify.Char") {
    val result = Unification.unifyTypes(Type.Char, Type.Char)
    assert(isOk(result))
  }

  test("Unify.Float32") {
    val result = Unification.unifyTypes(Type.Float32, Type.Float32)
    assert(isOk(result))
  }

  test("Unify.Float64") {
    val result = Unification.unifyTypes(Type.Float64, Type.Float64)
    assert(isOk(result))
  }

  test("Unify.Int8") {
    val result = Unification.unifyTypes(Type.Int8, Type.Int8)
    assert(isOk(result))
  }

  test("Unify.Int16") {
    val result = Unification.unifyTypes(Type.Int16, Type.Int16)
    assert(isOk(result))
  }

  test("Unify.Int32") {
    val result = Unification.unifyTypes(Type.Int32, Type.Int32)
    assert(isOk(result))
  }

  test("Unify.Int64") {
    val result = Unification.unifyTypes(Type.Int64, Type.Int64)
    assert(isOk(result))
  }

  test("Unify.BigInt") {
    val result = Unification.unifyTypes(Type.BigInt, Type.BigInt)
    assert(isOk(result))
  }

  test("Unify.Str") {
    val result = Unification.unifyTypes(Type.Str, Type.Str)
    assert(isOk(result))
  }

  test("Unify.01") {
    val tpe1 = Type.Var(1, Kind.Star)
    val tpe2 = Type.Bool
    val result = Unification.unifyTypes(tpe1, tpe2).get
    assertResult(Type.Bool)(result(tpe1))
  }

  test("Unify.02") {
    val tpe1 = Type.Bool
    val tpe2 = Type.Var(1, Kind.Star)
    val result = Unification.unifyTypes(tpe1, tpe2).get
    assertResult(Type.Bool)(result(tpe2))
  }

  test("Unify.03") {
    val A = Type.Var(1, Kind.Star)
    val tpe1 = Type.mkPureArrow(Type.Bool, Type.Char)
    val tpe2 = Type.mkPureArrow(Type.Bool, A)
    val result = Unification.unifyTypes(tpe1, tpe2).get
    assertResult(Type.Char)(result(A))
  }

  test("Unify.04") {
    val A = Type.Var(1, Kind.Star)
    val tpe1 = Type.mkPureArrow(Type.Bool, Type.Char)
    val tpe2 = Type.mkPureArrow(Type.Bool, A)
    val result = Unification.unifyTypes(tpe1, tpe2).get
    assertResult(Type.Char)(result(A))
  }

  test("Unify.05") {
    val A = Type.Var(1, Kind.Star)
    val tpe1 = Type.mkPureArrow(Type.Bool, Type.Char)
    val tpe2 = A
    val result = Unification.unifyTypes(tpe1, tpe2).get
    assertResult(tpe1)(result(A))
  }

  test("Unify.06") {
    val A = Type.Var(1, Kind.Star)
    val tpe1 = A
    val tpe2 = Type.mkPureArrow(Type.Bool, Type.Char)
    val result = Unification.unifyTypes(tpe1, tpe2).get
    assertResult(tpe2)(result(A))
  }

  test("Unify.07") {
    val A = Type.Var(1, Kind.Star)
    val tpe1 = Type.mkPureArrow(A, Type.Bool)
    val tpe2 = Type.mkPureArrow(Type.Bool, A)
    val result = Unification.unifyTypes(tpe1, tpe2).get
    assertResult(Type.Bool)(result(A))
  }

  test("Unify.08") {
    val A = Type.Var(1, Kind.Star)
    val B = Type.Var(2, Kind.Star)
    val tpe1 = Type.mkPureArrow(A, B)
    val tpe2 = Type.mkPureArrow(Type.Bool, Type.Char)
    val result = Unification.unifyTypes(tpe1, tpe2).get
    assertResult(Type.Bool)(result(A))
    assertResult(Type.Char)(result(B))
  }

  test("Unify.09") {
    val A = Type.Var(1, Kind.Star)
    val B = Type.Var(2, Kind.Star)
    val tpe1 = Type.mkPureArrow(Type.Bool, Type.Char)
    val tpe2 = Type.mkPureArrow(A, B)
    val result = Unification.unifyTypes(tpe1, tpe2).get
    assertResult(Type.Bool)(result(A))
    assertResult(Type.Char)(result(B))
  }

  test("Unify.10") {
    val A = Type.Var(1, Kind.Star)
    val B = Type.Var(2, Kind.Star)
    val tpe1 = Type.mkPureArrow(A, Type.Char)
    val tpe2 = Type.mkPureArrow(Type.Bool, B)
    val result = Unification.unifyTypes(tpe1, tpe2).get
    assertResult(Type.Bool)(result(A))
    assertResult(Type.Char)(result(B))
  }

  test("Unify.11") {
    val A = Type.Var(1, Kind.Star)
    val B = Type.Var(2, Kind.Star)
    val C = Type.Var(3, Kind.Star)
    val tpe1 = Type.mkPureArrow(A, B)
    val tpe2 = Type.mkPureArrow(C, Type.Bool)
    val result = Unification.unifyTypes(tpe1, tpe2).get
    assertResult(Type.Bool)(result(B))
    assertResult(C)(result(A))
  }

  test("Unify.12") {
    val tpe1 = Type.Var(1, Kind.Record)
    val field = Type.Bool
    val label = Name.Field("x", loc)
    val tpe2 = Type.mkRecordExtend(label, field, tpe1)
    val result = Unification.unifyTypes(tpe1, tpe2)
    assert(!isOk(result))
  }

  test("Unify.13") {
    val tpe1 = Type.Var(1, Kind.Schema)
    val field = Type.mkRelation(List(Type.Bool))
    val label = Name.Pred("X", loc)
    val tpe2 = Type.mkSchemaExtend(label, field, tpe1)
    val result = Unification.unifyTypes(tpe1, tpe2)
    assert(!isOk(result))
  }

  test("Unify.14") {
    val tpe1 = Type.Var(1, Kind.Schema)
    val tpe2 = Type.Var(2, Kind.Record)
    val result = Unification.unifyTypes(tpe1, tpe2)
    assert(!isOk(result))
  }

  test("Unify.15") {
    val tpe1 = Type.Var(1, Kind.Star, Rigidity.Rigid)
    val tpe2 = Type.Var(2, Kind.Record, Rigidity.Flexible)
    val result = Unification.unifyTypes(tpe1, tpe2)
    assert(!isOk(result))
  }

  test("Unify.16") {
    val tpe1 = Type.Var(1, Kind.Star, Rigidity.Flexible)
    val tpe2 = Type.Var(2, Kind.Record, Rigidity.Rigid)
    val result = Unification.unifyTypes(tpe1, tpe2)
    assert(isOk(result))
  }

  test("Unify.17") {
    val tpe1 = Type.Var(1, Kind.Star, Rigidity.Rigid)
    val tpe2 = Type.Var(2, Kind.Star, Rigidity.Rigid)
    val result = Unification.unifyTypes(tpe1, tpe2)
    assert(!isOk(result))
  }

  test("unifyM.01") {
    val subst0 = Substitution.empty
    val result = Unification.unifyTypeM(Type.Bool, Type.Bool, loc).run(subst0)
    assert(isOk(result))
  }

  test("unifyM.02") {
    val subst0 = Substitution.empty
    val result = Unification.unifyTypeM(Type.Bool, Type.Char, loc).run(subst0)
    assert(!isOk(result))
  }

  test("unifyM.03") {
    val tpe1 = Type.Var(1, Kind.Star)
    val tpe2 = Type.Bool
    val subst0 = Substitution.empty
    val result = Unification.unifyTypeM(tpe1, tpe2, loc).run(subst0)
    val (subst, tpe) = result.get
    assertResult(Type.Bool)(subst(tpe1))
    assertResult(Type.Bool)(subst(tpe2))
    assertResult(Type.Bool)(tpe)
  }

  test("seqM.01") {
    val subst0 = Substitution.empty
    val res1 = Unification.unifyTypeM(Type.Bool, Type.Bool, loc)
    val res2 = Unification.unifyTypeM(Type.Char, Type.Char, loc)
    val result = seqM(List(res1, res2)).run(subst0)
    assert(isOk(result))
  }

  test("seqM.02") {
    val subst0 = Substitution.empty
    val res1 = Unification.unifyTypeM(Type.Bool, Type.Char, loc)
    val res2 = Unification.unifyTypeM(Type.Bool, Type.Char, loc)
    val result = seqM(List(res1, res2)).run(subst0)
    assert(!isOk(result))
  }

  test("seqM.03") {
    val subst0 = Substitution.empty
    val res1 = Unification.unifyTypeM(Type.Var(1, Kind.Star), Type.Bool, loc)
    val res2 = Unification.unifyTypeM(Type.Var(2, Kind.Star), Type.Char, loc)
    val res3 = Unification.unifyTypeM(Type.Var(3, Kind.Star), Type.mkTuple(List(Type.Var(1, Kind.Star), Type.Var(2, Kind.Star))), loc)
    val result = seqM(List(res1, res2, res3)).run(subst0)
    val (subst, tpe) = result.get
    assertResult(Type.Bool)(subst.m(Type.Var(1, Kind.Star)))
    assertResult(Type.Char)(subst.m(Type.Var(2, Kind.Star)))
    assertResult(Type.mkTuple(List(Type.Bool, Type.Char)))(subst.m(Type.Var(3, Kind.Star)))
  }

  private def isOk[T, E](r: Result[T, E]) = r match {
    case Result.Ok(_) => true
    case Result.Err(_) => false
  }

}
