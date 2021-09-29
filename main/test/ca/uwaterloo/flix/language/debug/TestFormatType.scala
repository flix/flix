/*
 * Copyright 2020 Matthew Lutze
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

package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.ast.{Kind, Name, Rigidity, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.vt.{TerminalContext, VirtualString}
import org.scalatest.FunSuite

class TestFormatType extends FunSuite with TestUtils {

  val loc: SourceLocation = SourceLocation.Unknown

  test("FormatType.WellFormedType.Record.External.01") {
    val tpe = Type.mkRecord(Type.mkRecordRowExtend(Name.Field("x", loc), Type.Int32, Type.mkRecordRowExtend(Name.Field("y", loc), Type.Str, Type.RecordRowEmpty, loc), loc), loc)

    val expected = "{ x :: Int32, y :: String }"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Record.External.02") {
    val rest = Type.KindedVar(0, Kind.RecordRow, loc, Rigidity.Rigid)
    val tpe = Type.mkRecord(Type.mkRecordRowExtend(Name.Field("x", loc), Type.Int32, rest, loc), loc)

    val expected = "{ x :: Int32 | '0 }"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatType.WellFormedType.RecordRow.External.01") {
    val tpe = Type.mkRecordRowExtend(Name.Field("x", loc), Type.Int32, Type.mkRecordRowExtend(Name.Field("y", loc), Type.Str, Type.RecordRowEmpty, loc), loc)

    val expected = "( x :: Int32, y :: String )"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.RecordRow.External.02") {
    val rest = Type.KindedVar(0, Kind.RecordRow, loc, Rigidity.Rigid)
    val tpe = Type.mkRecordRowExtend(Name.Field("x", loc), Type.Int32, rest, loc)

    val expected = "( x :: Int32 | '0 )"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Arrow.External.01") {
    val paramType = Type.KindedVar(0, Kind.Star, loc, Rigidity.Rigid)
    val tpe = Type.mkArrowWithEffect(paramType, Type.Pure, paramType, loc)

    val expected = "'0 -> '0"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Arrow.External.02") {
    val paramType = Type.KindedVar(0, Kind.Star, loc, Rigidity.Rigid)
    val returnType = Type.KindedVar(1, Kind.Star, loc, Rigidity.Rigid)
    val effectType = Type.KindedVar(2, Kind.Bool, loc, Rigidity.Rigid)
    val tpe = Type.mkArrowWithEffect(paramType, effectType, returnType, loc)

    val expected = "'0 -> '1 & '2"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Arrow.External.03") {
    val paramType = Type.KindedVar(0, Kind.Star, loc, Rigidity.Rigid)
    val returnType = Type.KindedVar(1, Kind.Star, loc, Rigidity.Rigid)
    val tpe = Type.mkArrowWithEffect(paramType, Type.Impure, returnType, loc)

    val expected = "'0 ~> '1"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Arrow.External.04") {
    val tpe = Type.mkImpureUncurriedArrow(Type.Int8 :: Type.Int16 :: Nil, Type.Int32, loc)

    val expected = "Int8 -> Int16 ~> Int32"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Arrow.External.05") {
    val eff = Type.mkAnd(Type.KindedVar(1, Kind.Bool, loc, Rigidity.Flexible), Type.KindedVar(2, Kind.Bool, loc, Rigidity.Flexible), loc)
    val tpe = Type.mkArrowWithEffect(Type.BigInt, eff, Type.Bool, loc)

    val expected = "BigInt -> Bool & ('1 ∧ '2)"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Schema.External.01") {
    val relationType = Type.mkRelation(Type.Int32 :: Type.Str :: Nil, loc)
    val tpe = Type.mkSchema(Type.mkSchemaRowExtend(Name.Pred("S", loc), relationType, Type.SchemaRowEmpty, loc), loc)

    val expected = "#{ S(Int32, String) }"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Schema.External.02") {
    val latticeType1 = Type.mkLattice(List(Type.Str), loc)
    val latticeType2 = Type.mkLattice(List(Type.Int32, Type.Str), loc)
    val restType = Type.KindedVar(5, Kind.SchemaRow, loc, Rigidity.Flexible)
    val tpe = Type.mkSchema(Type.mkSchemaRowExtend(Name.Pred("A", loc), latticeType1, Type.mkSchemaRowExtend(Name.Pred("B", loc), latticeType2, restType, loc), loc), loc)

    val expected = "#{ A<>(String), B<>(Int32, String) | '5 }"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.SchemaRow.External.01") {
    val relationType = Type.mkRelation(Type.Int32 :: Type.Str :: Nil, loc)
    val tpe = Type.mkSchemaRowExtend(Name.Pred("S", loc), relationType, Type.SchemaRowEmpty, loc)

    val expected = "#( S(Int32, String) )"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.SchemaRow.External.02") {
    val latticeType1 = Type.mkLattice(List(Type.Str), loc)
    val latticeType2 = Type.mkLattice(List(Type.Int32, Type.Str), loc)
    val restType = Type.KindedVar(5, Kind.SchemaRow, loc, Rigidity.Flexible)
    val tpe = Type.mkSchemaRowExtend(Name.Pred("A", loc), latticeType1, Type.mkSchemaRowExtend(Name.Pred("B", loc), latticeType2, restType, loc), loc)

    val expected = "#( A<>(String), B<>(Int32, String) | '5 )"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Enum.External.07") {
    val tvar1 = Type.KindedVar(1, Kind.Star, loc, Rigidity.Flexible)
    val tvar2 = Type.KindedVar(2, Kind.Star, loc, Rigidity.Flexible)
    val tvar3 = Type.KindedVar(3, Kind.Star, loc, Rigidity.Flexible)
    val tpe = Type.mkEnum(Symbol.mkEnumSym("Triplet"), List(tvar1, tvar2, tvar3), loc)

    val expected = "Triplet['1, '2, '3]"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatType.WellFormedType.Record.Internal.01") {
    val tpe = Type.mkRecord(Type.mkRecordRowExtend(Name.Field("x", loc), Type.Int32, Type.mkRecordRowExtend(Name.Field("y", loc), Type.Str, Type.RecordRowEmpty, loc), loc), loc)

    val expected = "{ x :: Int32, y :: String }"
    val actual = FormatType.formatType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Record.Internal.02") {
    val rest = Type.KindedVar(0, Kind.RecordRow, loc, Rigidity.Rigid)
    val tpe = Type.mkRecord(Type.mkRecordRowExtend(Name.Field("x", loc), Type.Int32, rest, loc), loc)

    val expected = "{ x :: Int32 | '0 }"
    val actual = FormatType.formatType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Arrow.Internal.01") {
    val paramType = Type.KindedVar(0, Kind.Star, loc, Rigidity.Rigid)
    val tpe = Type.mkArrowWithEffect(paramType, Type.Pure, paramType, loc)

    val expected = "'0 -> '0"
    val actual = FormatType.formatType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Arrow.Internal.02") {
    val paramType = Type.KindedVar(0, Kind.Star, loc, Rigidity.Rigid)
    val returnType = Type.KindedVar(1, Kind.Star, loc, Rigidity.Rigid)
    val effectType = Type.KindedVar(2, Kind.Bool, loc, Rigidity.Rigid)
    val tpe = Type.mkArrowWithEffect(paramType, effectType, returnType, loc)

    val expected = "'0 -> '1 & ''2"
    val actual = FormatType.formatType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Schema.Internal.01") {
    val relationType = Type.mkRelation(List(Type.Int32, Type.Str), loc)
    val tpe = Type.mkSchema(Type.mkSchemaRowExtend(Name.Pred("S", loc), relationType, Type.SchemaRowEmpty, loc), loc)

    val expected = "#{ S(Int32, String) }"
    val actual = FormatType.formatType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Schema.Internal.02") {
    val latticeType1 = Type.mkLattice(List(Type.Str), loc)
    val latticeType2 = Type.mkLattice(List(Type.Int32, Type.Str), loc)
    val restType = Type.KindedVar(5, Kind.SchemaRow, loc, Rigidity.Flexible)
    val tpe = Type.mkSchema(Type.mkSchemaRowExtend(Name.Pred("A", loc), latticeType1, Type.mkSchemaRowExtend(Name.Pred("B", loc), latticeType2, restType, loc), loc), loc)

    val expected = "#{ A<>(String), B<>(Int32, String) | '5 }"
    val actual = FormatType.formatType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Enum.Internal.07") {
    val tvar1 = Type.KindedVar(1, Kind.Star, loc, Rigidity.Flexible)
    val tvar2 = Type.KindedVar(2, Kind.Star, loc, Rigidity.Flexible)
    val tvar3 = Type.KindedVar(3, Kind.Star, loc, Rigidity.Flexible)
    val tpe = Type.mkEnum(Symbol.mkEnumSym("Triplet"), List(tvar1, tvar2, tvar3), loc)

    val expected = "Triplet['1, '2, '3]"
    val actual = FormatType.formatType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Tuple.External.01") {
    val tpe = Type.mkApply(Type.Cst(TypeConstructor.Tuple(2), loc), List(Type.Str), loc)

    val expected = "(String, ???)"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Effect.External.01") {
    val tpe = Type.Not

    val expected = "¬???"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Effect.External.03") {
    val tpe = Type.And

    val expected = "??? ∧ ???"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Effect.External.04") {
    val tpe = Type.Apply(Type.And, Type.Pure, loc)

    val expected = "(true) ∧ ???"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Effect.External.06") {
    val tpe = Type.Or

    val expected = "??? ∨ ???"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Effect.External.07") {
    val tpe = Type.Apply(Type.Or, Type.Pure, loc)

    val expected = "(true) ∨ ???"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Arrow.External.01") {
    val tpe = Type.Apply(Type.Cst(TypeConstructor.Arrow(2), loc), Type.Pure, loc)

    val expected = "??? -> ???"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Arrow.External.02") {
    val tpe = Type.mkApply(Type.Cst(TypeConstructor.Arrow(3), loc), List(Type.Impure, Type.Str), loc)

    val expected = "String -> ??? ~> ???"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatTypeDiff.Tuple.01") {
    val tpe1 = Type.mkTuple(List(Type.Int32, Type.Int32, Type.Int32), loc)
    val tpe2 = Type.mkTuple(List(Type.Int32, Type.Bool, Type.Int32), loc)

    val diff = TypeDiff.diff(tpe1, tpe2)
    val expected = "(..., Int32, ...)"
    val actual = FormatType.formatTypeDiff(diff, VirtualString.Text)(Audience.External).fmt(TerminalContext.NoTerminal)

    assert(actual == expected)
  }

  test("FormatTypeDiff.Arrow.01") {
    val tpe1 = Type.mkArrowWithEffect(Type.Int32, Type.Pure, Type.Int32, loc)
    val tpe2 = Type.mkArrowWithEffect(Type.Int32, Type.Pure, Type.Bool, loc)

    val diff = TypeDiff.diff(tpe1, tpe2)
    val expected = "... -> Int32"
    val actual = FormatType.formatTypeDiff(diff, VirtualString.Text)(Audience.External).fmt(TerminalContext.NoTerminal)

    assert(actual == expected)
  }

  test("FormatTypeDiff.Enum.01") {
    val map = Type.mkEnum(Symbol.mkEnumSym("Map"), Kind.Star ->: Kind.Star ->: Kind.Star, loc)
    val tpe1 = Type.mkApply(map, List(Type.Int32, Type.Bool), loc)
    val tpe2 = Type.mkApply(map, List(Type.Int32, Type.Str), loc)

    val diff = TypeDiff.diff(tpe1, tpe2)
    val expected = "...[..., Bool]"
    val actual = FormatType.formatTypeDiff(diff, VirtualString.Text)(Audience.External).fmt(TerminalContext.NoTerminal)

    assert(actual == expected)
  }
}
