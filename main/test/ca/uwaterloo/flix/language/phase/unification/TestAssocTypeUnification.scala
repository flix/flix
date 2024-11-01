/*
 * Copyright 2023 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.{EqualityConstraint, Scope}
import ca.uwaterloo.flix.language.ast.{Ast, Kind, Name, RigidityEnv, SourceLocation, SourcePosition, Symbol, Type}
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.Ok
import ca.uwaterloo.flix.util.collection.ListMap
import org.scalatest.funsuite.AnyFunSuite

class TestAssocTypeUnification extends AnyFunSuite with TestUtils {

  private implicit val flix: Flix = new Flix()
  private implicit val scope: Scope = Scope.Top
  private val loc: SourceLocation = SourceLocation.Unknown
  private val eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef] = ListMap.empty
  private val CollSym: Symbol.TraitSym = Symbol.mkTraitSym("Coll")
  private val ElemSym: Symbol.AssocTypeSym = Symbol.mkAssocTypeSym(CollSym, Name.Ident("Elem", SourceLocation.Unknown))
  private val ElemCst: Ast.AssocTypeConstructor = Ast.AssocTypeConstructor(ElemSym, loc)

  test("TestUnifyTypes.01") {
    val tpe1 = Type.AssocType(ElemCst, Type.Str, Kind.Star, loc)
    val tpe2 = Type.Char
    val renv = RigidityEnv.empty
    val result = Unification.unifyTypes(tpe1, tpe2, renv, eqEnv)

    val expectedSubst = Substitution.empty
    val expectedEconstrs = List(EqualityConstraint(ElemCst, Type.Str, Type.Char, loc))
    val expectedResult: Result[(Substitution, List[EqualityConstraint]), ?] = Ok((expectedSubst, expectedEconstrs))

    assert(result == expectedResult)
  }

  test("TestUnifiesWith.01") {
    val tpe1 = Type.AssocType(ElemCst, Type.Str, Kind.Star, loc)
    val tpe2 = Type.Char
    val renv = RigidityEnv.empty
    val eqEnv = ListMap.empty[Symbol.AssocTypeSym, Ast.AssocTypeDef]
    val result = Unification.unifiesWith(tpe1, tpe2, renv, eqEnv)

    val expectedResult = false

    assert(result == expectedResult)
  }

  test("TestUnifiesWith.02") {
    val tpe1 = Type.AssocType(ElemCst, Type.Str, Kind.Star, loc)
    val tpe2 = Type.Int32
    val renv = RigidityEnv.empty
    val eqEnv = ListMap.singleton(ElemSym, Ast.AssocTypeDef(Type.Str, Type.Char))
    val result = Unification.unifiesWith(tpe1, tpe2, renv, eqEnv)

    val expectedResult = false

    assert(result == expectedResult)
  }
}
