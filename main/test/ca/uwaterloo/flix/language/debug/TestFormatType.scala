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
    val tpe = Type.mkRecordExtend(Name.Field("x", loc), Type.Int32, Type.mkRecordExtend(Name.Field("y", loc), Type.Str, Type.RecordEmpty))

    val expected = "{ x: Int32, y: String }"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  ignore("FormatWellFormedType.Record.External.02") {
    val rest = Type.Var(0, Kind.Record, Rigidity.Rigid)
    val tpe = Type.mkRecordExtend(Name.Field("x", loc), Type.Int32, rest)

    val expected = "{ x: Int32 | 'a }"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  ignore("FormatWellFormedType.Arrow.External.01") {
    val paramType = Type.Var(0, Kind.Star, Rigidity.Rigid)
    val tpe = Type.mkArrowWithEffect(paramType, Type.Pure, paramType)

    val expected = "'a -> 'a"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  ignore("FormatWellFormedType.Arrow.External.02") {
    val paramType = Type.Var(0, Kind.Star, Rigidity.Rigid)
    val returnType = Type.Var(1, Kind.Star, Rigidity.Rigid)
    val effectType = Type.Var(2, Kind.Bool, Rigidity.Rigid)
    val tpe = Type.mkArrowWithEffect(paramType, effectType, returnType)

    val expected = "'a -> 'b & 'c"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  ignore("FormatWellFormedType.Arrow.External.03") {
    val paramType = Type.Var(0, Kind.Star, Rigidity.Rigid)
    val returnType = Type.Var(1, Kind.Star, Rigidity.Rigid)
    val tpe = Type.mkArrowWithEffect(paramType, Type.Impure, returnType)

    val expected = "'a ~> 'b"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Arrow.External.04") {
    val tpe = Type.mkImpureUncurriedArrow(Type.Int8 :: Type.Int16 :: Nil, Type.Int32)

    val expected = "Int8 -> Int16 ~> Int32"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  ignore("FormatWellFormedType.Arrow.External.05") {
    val eff = Type.mkAnd(Type.Var(1, Kind.Bool, Rigidity.Flexible), Type.Var(2, Kind.Bool, Rigidity.Flexible))
    val tpe = Type.mkArrowWithEffect(Type.BigInt, eff, Type.Bool)

    val expected = "BigInt -> Bool & ('a ∧ 'b)"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Schema.External.01") {
    val relationType = Type.mkRelation(Type.Int32 :: Type.Str :: Nil)
    val tpe = Type.mkSchemaExtend(Name.Pred("S", loc), relationType, Type.SchemaEmpty)

    val expected = "#{ S(Int32, String) }"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  ignore("FormatWellFormedType.Schema.External.02") {
    val latticeType1 = Type.mkLattice(List(Type.Str))
    val latticeType2 = Type.mkLattice(List(Type.Int32, Type.Str))
    val restType = Type.Var(5, Kind.Schema, Rigidity.Flexible)
    val tpe = Type.mkSchemaExtend(Name.Pred("A", loc), latticeType1, Type.mkSchemaExtend(Name.Pred("B", loc), latticeType2, restType))

    val expected = "#{ A<>(String), B<>(Int32, String) | 'a }"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  ignore("FormatWellFormedType.Enum.External.07") {
    val tvar1 = Type.Var(1, Kind.Star, Rigidity.Flexible)
    val tvar2 = Type.Var(2, Kind.Star, Rigidity.Flexible)
    val tvar3 = Type.Var(3, Kind.Star, Rigidity.Flexible)
    val tpe = Type.mkEnum(Symbol.mkEnumSym("Triplet"), List(tvar1, tvar2, tvar3))

    val expected = "Triplet['a, 'b, 'c]"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatType.WellFormedType.Record.Internal.01") {
    val tpe = Type.mkRecordExtend(Name.Field("x", loc), Type.Int32, Type.mkRecordExtend(Name.Field("y", loc), Type.Str, Type.RecordEmpty))

    val expected = "{ x: Int32, y: String }"
    val actual = FormatType.formatType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Record.Internal.02") {
    val rest = Type.Var(0, Kind.Record, Rigidity.Rigid)
    val tpe = Type.mkRecordExtend(Name.Field("x", loc), Type.Int32, rest)

    val expected = "{ x: Int32 | '0 }"
    val actual = FormatType.formatType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Arrow.Internal.01") {
    val paramType = Type.Var(0, Kind.Star, Rigidity.Rigid)
    val tpe = Type.mkArrowWithEffect(paramType, Type.Pure, paramType)

    val expected = "'0 -> '0"
    val actual = FormatType.formatType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Arrow.Internal.02") {
    val paramType = Type.Var(0, Kind.Star, Rigidity.Rigid)
    val returnType = Type.Var(1, Kind.Star, Rigidity.Rigid)
    val effectType = Type.Var(2, Kind.Bool, Rigidity.Rigid)
    val tpe = Type.mkArrowWithEffect(paramType, effectType, returnType)

    val expected = "'0 -> '1 & ''2"
    val actual = FormatType.formatType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Schema.Internal.01") {
    val relationType = Type.mkRelation(List(Type.Int32, Type.Str))
    val tpe = Type.mkSchemaExtend(Name.Pred("S", loc), relationType, Type.SchemaEmpty)

    val expected = "#{ S(Int32, String) }"
    val actual = FormatType.formatType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Schema.Internal.02") {
    val latticeType1 = Type.mkLattice(List(Type.Str))
    val latticeType2 = Type.mkLattice(List(Type.Int32, Type.Str))
    val restType = Type.Var(5, Kind.Schema, Rigidity.Flexible)
    val tpe = Type.mkSchemaExtend(Name.Pred("A", loc), latticeType1, Type.mkSchemaExtend(Name.Pred("B", loc), latticeType2, restType))

    val expected = "#{ A<>(String), B<>(Int32, String) | '5 }"
    val actual = FormatType.formatType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Enum.Internal.07") {
    val tvar1 = Type.Var(1, Kind.Star, Rigidity.Flexible)
    val tvar2 = Type.Var(2, Kind.Star, Rigidity.Flexible)
    val tvar3 = Type.Var(3, Kind.Star, Rigidity.Flexible)
    val tpe = Type.mkEnum(Symbol.mkEnumSym("Triplet"), List(tvar1, tvar2, tvar3))

    val expected = "Triplet['1, '2, '3]"
    val actual = FormatType.formatType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Record.External.01") {
    val tpe = Type.Cst(TypeConstructor.RecordExtend(Name.Field("x", loc)), loc)

    val expected = "{ x: ??? }"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Record.External.02") {
    val tpe = Type.Apply(Type.Cst(TypeConstructor.RecordExtend(Name.Field("x", loc)), loc), Type.Int32)

    val expected = "{ x: Int32 | ??? }"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Schema.External.01") {
    val tpe = Type.Cst(TypeConstructor.SchemaExtend(Name.Pred("X", loc)), loc)

    val expected = "#{ X?(???) }"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Schema.External.02") {
    val tpe = Type.Apply(Type.Cst(TypeConstructor.SchemaExtend(Name.Pred("X", loc)), loc), Type.Int32)

    val expected = "#{ X?(Int32) | ??? }"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Tuple.External.01") {
    val tpe = Type.mkApply(Type.Cst(TypeConstructor.Tuple(2), loc), List(Type.Str))

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
    val tpe = Type.Apply(Type.And, Type.Pure)

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
    val tpe = Type.Apply(Type.Or, Type.Pure)

    val expected = "(true) ∨ ???"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Arrow.External.01") {
    val tpe = Type.Apply(Type.Cst(TypeConstructor.Arrow(2), loc), Type.Pure)

    val expected = "??? -> ???"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Arrow.External.02") {
    val tpe = Type.mkApply(Type.Cst(TypeConstructor.Arrow(3), loc), List(Type.Impure, Type.Str))

    val expected = "String -> ??? ~> ???"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatTypeDiff.Tuple.01") {
    val tpe1 = Type.mkTuple(List(Type.Int32, Type.Int32, Type.Int32))
    val tpe2 = Type.mkTuple(List(Type.Int32, Type.Bool, Type.Int32))

    val diff = TypeDiff.diff(tpe1, tpe2)
    val expected = "(..., Int32, ...)"
    val actual = FormatType.formatTypeDiff(diff, VirtualString.Text)(Audience.External).fmt(TerminalContext.NoTerminal)

    assert(actual == expected)
  }

  test("FormatTypeDiff.Arrow.01") {
    val tpe1 = Type.mkArrowWithEffect(Type.Int32, Type.Pure, Type.Int32)
    val tpe2 = Type.mkArrowWithEffect(Type.Int32, Type.Pure, Type.Bool)

    val diff = TypeDiff.diff(tpe1, tpe2)
    val expected = "... -> Int32"
    val actual = FormatType.formatTypeDiff(diff, VirtualString.Text)(Audience.External).fmt(TerminalContext.NoTerminal)

    assert(actual == expected)
  }

  test("FormatTypeDiff.Enum.01") {
    val map = Type.mkEnum(Symbol.mkEnumSym("Map"), Kind.Star ->: Kind.Star ->: Kind.Star, loc)
    val tpe1 = Type.mkApply(map, List(Type.Int32, Type.Bool))
    val tpe2 = Type.mkApply(map, List(Type.Int32, Type.Str))

    val diff = TypeDiff.diff(tpe1, tpe2)
    val expected = "...[..., Bool]"
    val actual = FormatType.formatTypeDiff(diff, VirtualString.Text)(Audience.External).fmt(TerminalContext.NoTerminal)

    assert(actual == expected)
  }
}
