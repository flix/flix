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
import ca.uwaterloo.flix.language.ast.{Kind, Name, Rigidity, SourceLocation, SourcePosition, Symbol, Type, TypeConstructor}
import org.scalatest.FunSuite

class TestFormatType extends FunSuite with TestUtils {

  val loc: SourceLocation = SourceLocation.Unknown

  test("FormatType.Type.Record.External.01") {
    val tpe = Type.mkRecord(Type.mkRecordRowExtend(Name.Field("x", loc), Type.Int32, Type.mkRecordRowExtend(Name.Field("y", loc), Type.Str, Type.RecordRowEmpty, loc), loc), loc)

    val expected = "{ x :: Int32, y :: String }"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatType.Record.External.02") {
    val rest = Type.KindedVar(0, Kind.RecordRow, loc, Rigidity.Rigid)
    val tpe = Type.mkRecord(Type.mkRecordRowExtend(Name.Field("x", loc), Type.Int32, rest, loc), loc)

    val expected = "{ x :: Int32 | r0! }"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatType.Type.RecordRow.External.01") {
    val tpe = Type.mkRecordRowExtend(Name.Field("x", loc), Type.Int32, Type.mkRecordRowExtend(Name.Field("y", loc), Type.Str, Type.RecordRowEmpty, loc), loc)

    val expected = "( x :: Int32, y :: String )"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatType.RecordRow.External.02") {
    val rest = Type.KindedVar(0, Kind.RecordRow, loc, Rigidity.Rigid)
    val tpe = Type.mkRecordRowExtend(Name.Field("x", loc), Type.Int32, rest, loc)

    val expected = "( x :: Int32 | r0! )"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatType.RecordRow.External.03") {
    val name = Name.mkQName("MyEmptyRecordRow")
    val ident = name.ident
    val sym = Symbol.mkTypeAliasSym(name.namespace, ident)
    val alias = Type.Alias(Type.AliasConstructor(sym, loc), Nil, Type.RecordRowEmpty, loc)

    val tpe = Type.mkRecordRowExtend(Name.Field("x", loc), Type.Int32, alias, loc)

    val expected = "( x :: Int32 | MyEmptyRecordRow )"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatType.Record.External.04") {
    val name = Name.mkQName("MyEmptyRecordRow")
    val ident = name.ident
    val sym = Symbol.mkTypeAliasSym(name.namespace, ident)
    val alias = Type.Alias(Type.AliasConstructor(sym, loc), Nil, Type.RecordRowEmpty, loc)

    val tpe = Type.mkRecord(alias, loc)

    val expected = "{ MyEmptyRecordRow }"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatType.Arrow.External.01") {
    val paramType = Type.KindedVar(0, Kind.Star, loc, Rigidity.Rigid)
    val tpe = Type.mkArrowWithEffect(paramType, Type.Pure, paramType, loc)

    val expected = "t0! -> t0!"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatType.Arrow.External.02") {
    val paramType = Type.KindedVar(0, Kind.Star, loc, Rigidity.Rigid)
    val returnType = Type.KindedVar(1, Kind.Star, loc, Rigidity.Rigid)
    val effectType = Type.KindedVar(2, Kind.Bool, loc, Rigidity.Rigid)
    val tpe = Type.mkArrowWithEffect(paramType, effectType, returnType, loc)

    val expected = "t0! ->{b2!} t1!"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatType.Arrow.External.03") {
    val paramType = Type.KindedVar(0, Kind.Star, loc, Rigidity.Rigid)
    val returnType = Type.KindedVar(1, Kind.Star, loc, Rigidity.Rigid)
    val tpe = Type.mkArrowWithEffect(paramType, Type.Impure, returnType, loc)

    val expected = "t0! ->{Impure} t1!"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatType.Arrow.External.04") {
    val tpe = Type.mkImpureUncurriedArrow(Type.Int8 :: Type.Int16 :: Nil, Type.Int32, loc)

    val expected = "Int8 -> (Int16 ->{Impure} Int32)"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatType.Arrow.External.05") {
    val eff = Type.mkAnd(Type.KindedVar(1, Kind.Bool, loc, Rigidity.Flexible), Type.KindedVar(2, Kind.Bool, loc, Rigidity.Flexible), loc)
    val tpe = Type.mkArrowWithEffect(Type.BigInt, eff, Type.Bool, loc)

    val expected = "BigInt ->{b1 and b2} Bool"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatType.Arrow.External.06") {
    val arg = Type.mkTuple(List(Type.Bool, Type.Bool), loc)
    val tpe = Type.mkPureArrow(arg, Type.Str, loc)

    val expected = "((Bool, Bool)) -> String"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatType.Schema.External.01") {
    val relationType = Type.mkRelation(Type.Int32 :: Type.Str :: Nil, loc)
    val tpe = Type.mkSchema(Type.mkSchemaRowExtend(Name.Pred("S", loc), relationType, Type.SchemaRowEmpty, loc), loc)

    val expected = "#{ S(Int32, String) }"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatType.Schema.External.02") {
    val latticeType1 = Type.mkLattice(List(Type.Str, Type.Bool), loc)
    val latticeType2 = Type.mkLattice(List(Type.Int32, Type.Str), loc)
    val restType = Type.KindedVar(5, Kind.SchemaRow, loc, Rigidity.Flexible)
    val tpe = Type.mkSchema(Type.mkSchemaRowExtend(Name.Pred("A", loc), latticeType1, Type.mkSchemaRowExtend(Name.Pred("B", loc), latticeType2, restType, loc), loc), loc)

    val expected = "#{ A(String; Bool), B(Int32; String) | s5 }"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatType.SchemaRow.External.01") {
    val relationType = Type.mkRelation(Type.Int32 :: Type.Str :: Nil, loc)
    val tpe = Type.mkSchemaRowExtend(Name.Pred("S", loc), relationType, Type.SchemaRowEmpty, loc)

    val expected = "#( S(Int32, String) )"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatType.SchemaRow.External.02") {
    val latticeType1 = Type.mkLattice(List(Type.Str, Type.Bool), loc)
    val latticeType2 = Type.mkLattice(List(Type.Int32, Type.Str), loc)
    val restType = Type.KindedVar(5, Kind.SchemaRow, loc, Rigidity.Flexible)
    val tpe = Type.mkSchemaRowExtend(Name.Pred("A", loc), latticeType1, Type.mkSchemaRowExtend(Name.Pred("B", loc), latticeType2, restType, loc), loc)

    val expected = "#( A(String; Bool), B(Int32; String) | s5 )"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatType.Enum.External.07") {
    val tvar1 = Type.KindedVar(1, Kind.Star, loc, Rigidity.Flexible)
    val tvar2 = Type.KindedVar(2, Kind.Star, loc, Rigidity.Flexible)
    val tvar3 = Type.KindedVar(3, Kind.Star, loc, Rigidity.Flexible)
    val tpe = Type.mkEnum(Symbol.mkEnumSym("Triplet"), List(tvar1, tvar2, tvar3), loc)

    val expected = "Triplet[t1, t2, t3]"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatType.Type.Record.Internal.01") {
    val tpe = Type.mkRecord(Type.mkRecordRowExtend(Name.Field("x", loc), Type.Int32, Type.mkRecordRowExtend(Name.Field("y", loc), Type.Str, Type.RecordRowEmpty, loc), loc), loc)

    val expected = "{ x :: Int32, y :: String }"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatType.Record.Internal.02") {
    val rest = Type.KindedVar(0, Kind.RecordRow, loc, Rigidity.Rigid)
    val tpe = Type.mkRecord(Type.mkRecordRowExtend(Name.Field("x", loc), Type.Int32, rest, loc), loc)

    val expected = "{ x :: Int32 | r0! }"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatType.Arrow.Internal.01") {
    val paramType = Type.KindedVar(0, Kind.Star, loc, Rigidity.Rigid)
    val tpe = Type.mkArrowWithEffect(paramType, Type.Pure, paramType, loc)

    val expected = "t0! -> t0!"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatType.Arrow.Internal.02") {
    val paramType = Type.KindedVar(0, Kind.Star, loc, Rigidity.Rigid)
    val returnType = Type.KindedVar(1, Kind.Star, loc, Rigidity.Rigid)
    val effectType = Type.KindedVar(2, Kind.Bool, loc, Rigidity.Rigid)
    val tpe = Type.mkArrowWithEffect(paramType, effectType, returnType, loc)

    val expected = "t0! ->{b2!} t1!"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatType.Schema.Internal.01") {
    val relationType = Type.mkRelation(List(Type.Int32, Type.Str), loc)
    val tpe = Type.mkSchema(Type.mkSchemaRowExtend(Name.Pred("S", loc), relationType, Type.SchemaRowEmpty, loc), loc)

    val expected = "#{ S(Int32, String) }"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatType.Schema.Internal.02") {
    val latticeType1 = Type.mkLattice(List(Type.Str, Type.Bool), loc)
    val latticeType2 = Type.mkLattice(List(Type.Int32, Type.Str), loc)
    val restType = Type.KindedVar(5, Kind.SchemaRow, loc, Rigidity.Flexible)
    val tpe = Type.mkSchema(Type.mkSchemaRowExtend(Name.Pred("A", loc), latticeType1, Type.mkSchemaRowExtend(Name.Pred("B", loc), latticeType2, restType, loc), loc), loc)

    val expected = "#{ A(String; Bool), B(Int32; String) | s5 }"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatType.Enum.Internal.07") {
    val tvar1 = Type.KindedVar(1, Kind.Star, loc, Rigidity.Flexible)
    val tvar2 = Type.KindedVar(2, Kind.Star, loc, Rigidity.Flexible)
    val tvar3 = Type.KindedVar(3, Kind.Star, loc, Rigidity.Flexible)
    val tpe = Type.mkEnum(Symbol.mkEnumSym("Triplet"), List(tvar1, tvar2, tvar3), loc)

    val expected = "Triplet[t1, t2, t3]"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatType.Boolean.External.01") {
    val tvar1 = Type.KindedVar(1, Kind.Bool, loc, Rigidity.Flexible, Some("a"))
    val tvar2 = Type.KindedVar(2, Kind.Bool, loc, Rigidity.Flexible, Some("b"))
    val tvar3 = Type.KindedVar(3, Kind.Bool, loc, Rigidity.Flexible, Some("c"))
    val tpe = Type.mkAnd(List(tvar1, tvar2, tvar3), loc)

    val expected = "a and b and c"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatPartialType.Tuple.External.01") {
    val tpe = Type.mkApply(Type.Cst(TypeConstructor.Tuple(2), loc), List(Type.Str), loc)

    val expected = "(String, ?)"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatPartialType.Boolean.External.01") {
    val tpe = Type.Not

    val expected = "not ?"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatPartialType.Boolean.External.03") {
    val tpe = Type.And

    val expected = "? and ?"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatPartialType.Boolean.External.04") {
    val tpe = Type.Apply(Type.And, Type.Pure, loc)

    val expected = "true and ?"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatPartialType.Boolean.External.06") {
    val tpe = Type.Or

    val expected = "? or ?"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatPartialType.Boolean.External.07") {
    val tpe = Type.Apply(Type.Or, Type.Pure, loc)

    val expected = "true or ?"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatPartialType.Arrow.External.01") {
    val tpe = Type.Apply(Type.Cst(TypeConstructor.Arrow(2), loc), Type.Pure, loc)

    val expected = "? -> ?"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatPartialType.Arrow.External.02") {
    val tpe = Type.mkApply(Type.Cst(TypeConstructor.Arrow(3), loc), List(Type.Impure, Type.Str), loc)

    val expected = "String -> (? ->{Impure} ?)"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatPartialType.Arrow.External.03") {
    val tpe = Type.Cst(TypeConstructor.Arrow(4), loc)

    val expected = "? -> (? -> (? ->{?} ?))"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatType.Alias.External.01") {
    val name = Name.mkQName("MyType")
    val ident = name.ident
    val sym = Symbol.mkTypeAliasSym(name.namespace, ident)
    val tpe = Type.Alias(Type.AliasConstructor(sym, loc), Nil, Type.Int32, loc)

    val expected = "MyType"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatType.Tag.External.01") {
    val sym = Symbol.mkEnumSym("MyType")
    val tag = Name.mkTag(Name.Ident(SourcePosition.Unknown, "MyTag", SourcePosition.Unknown))
    val tpe = Type.mkApply(
      Type.Cst(TypeConstructor.Tag(sym, tag), loc),
      List(
        Type.mkTuple(List(Type.Int32, Type.Bool), loc),
        Type.mkEnum(sym, Nil, loc)
      ),
      loc
    )

    val expected = "((Int32, Bool)) -> MyType"
    val actual = FormatType.formatWellKindedType(tpe)(Audience.External)

    assert(actual == expected)
  }
}
