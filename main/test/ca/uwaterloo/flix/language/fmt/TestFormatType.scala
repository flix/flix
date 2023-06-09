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

package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.ast.{Ast, Kind, Name, SourceLocation, Symbol, Type, TypeConstructor}
import org.scalatest.funsuite.AnyFunSuite

class TestFormatType extends AnyFunSuite with TestUtils {

  private val loc = SourceLocation.Unknown
  private val standardFormat = FormatOptions(
    ignorePur = false,
    ignoreEff = false,
    FormatOptions.VarName.NameBased
  )

  test("FormatType.Type.Record.External.01") {
    val tpe = Type.mkRecord(Type.mkRecordRowExtend(Name.Field("x", loc), Type.Int32, Type.mkRecordRowExtend(Name.Field("y", loc), Type.Str, Type.RecordRowEmpty, loc), loc), loc)

    val expected = "{ x = Int32, y = String }"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatType.Record.External.02") {
    val rest = Type.Var(new Symbol.KindedTypeVarSym(0, Ast.VarText.Absent, Kind.RecordRow, isRegion = true, loc), loc)
    val tpe = Type.mkRecord(Type.mkRecordRowExtend(Name.Field("x", loc), Type.Int32, rest, loc), loc)

    val expected = "{ x = Int32 | r0! }"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatType.Type.RecordRow.External.01") {
    val tpe = Type.mkRecordRowExtend(Name.Field("x", loc), Type.Int32, Type.mkRecordRowExtend(Name.Field("y", loc), Type.Str, Type.RecordRowEmpty, loc), loc)

    val expected = "( x = Int32, y = String )"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatType.RecordRow.External.02") {
    val rest = Type.Var(new Symbol.KindedTypeVarSym(0, Ast.VarText.Absent, Kind.RecordRow, isRegion = true, loc), loc)
    val tpe = Type.mkRecordRowExtend(Name.Field("x", loc), Type.Int32, rest, loc)

    val expected = "( x = Int32 | r0! )"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatType.RecordRow.External.03") {
    val name = Name.mkQName("MyEmptyRecordRow")
    val ident = name.ident
    val sym = Symbol.mkTypeAliasSym(name.namespace, ident)
    val alias = Type.Alias(Ast.AliasConstructor(sym, loc), Nil, Type.RecordRowEmpty, loc)

    val tpe = Type.mkRecordRowExtend(Name.Field("x", loc), Type.Int32, alias, loc)

    val expected = "( x = Int32 | MyEmptyRecordRow )"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatType.Record.External.04") {
    val name = Name.mkQName("MyEmptyRecordRow")
    val ident = name.ident
    val sym = Symbol.mkTypeAliasSym(name.namespace, ident)
    val alias = Type.Alias(Ast.AliasConstructor(sym, loc), Nil, Type.RecordRowEmpty, loc)

    val tpe = Type.mkRecord(alias, loc)

    val expected = "{ MyEmptyRecordRow }"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatType.Arrow.External.01") {
    val paramType = Type.Var(new Symbol.KindedTypeVarSym(0, Ast.VarText.Absent, Kind.Star, isRegion = true, loc), loc)
    val tpe = Type.mkArrowWithEffect(paramType, Type.Pure, paramType, loc)

    val expected = "t0! -> t0!"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatType.Arrow.External.02") {
    val paramType = Type.Var(new Symbol.KindedTypeVarSym(0, Ast.VarText.Absent, Kind.Star, isRegion = true, loc), loc)
    val returnType = Type.Var(new Symbol.KindedTypeVarSym(1, Ast.VarText.Absent, Kind.Star, isRegion = true, loc), loc)
    val effectType = Type.Var(new Symbol.KindedTypeVarSym(2, Ast.VarText.Absent, Kind.Eff, isRegion = true, loc), loc)
    val tpe = Type.mkArrowWithEffect(paramType, effectType, returnType, loc)

    val expected = raw"t0! -> t1! \ e2!"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatType.Arrow.External.03") {
    val paramType = Type.Var(new Symbol.KindedTypeVarSym(0, Ast.VarText.Absent, Kind.Star, isRegion = true, loc), loc)
    val returnType = Type.Var(new Symbol.KindedTypeVarSym(1, Ast.VarText.Absent, Kind.Star, isRegion = true, loc), loc)
    val tpe = Type.mkArrowWithEffect(paramType, Type.Impure, returnType, loc)

    val expected = raw"t0! -> t1! \ IO"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatType.Arrow.External.04") {
    val tpe = Type.mkImpureUncurriedArrow(Type.Int8 :: Type.Int16 :: Nil, Type.Int32, loc)

    val expected = raw"Int8 -> (Int16 -> Int32 \ IO)"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatType.Arrow.External.05") {
    val eff = Type.mkUnion(Type.Var(new Symbol.KindedTypeVarSym(1, Ast.VarText.Absent, Kind.Eff, isRegion = false, loc), loc), Type.Var(new Symbol.KindedTypeVarSym(2, Ast.VarText.Absent, Kind.Eff, isRegion = false, loc), loc), loc)
    val tpe = Type.mkArrowWithEffect(Type.BigInt, eff, Type.Bool, loc)

    val expected = raw"BigInt -> Bool \ e1 + e2"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatType.Arrow.External.06") {
    val arg = Type.mkTuple(List(Type.Bool, Type.Bool), loc)
    val tpe = Type.mkPureArrow(arg, Type.Str, loc)

    val expected = "((Bool, Bool)) -> String"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatType.Schema.External.01") {
    val relationType = Type.mkRelation(Type.Int32 :: Type.Str :: Nil, loc)
    val tpe = Type.mkSchema(Type.mkSchemaRowExtend(Name.Pred("S", loc), relationType, Type.SchemaRowEmpty, loc), loc)

    val expected = "#{ S(Int32, String) }"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatType.Schema.External.02") {
    val latticeType1 = Type.mkLattice(List(Type.Str, Type.Bool), loc)
    val latticeType2 = Type.mkLattice(List(Type.Int32, Type.Str), loc)
    val restType = Type.Var(new Symbol.KindedTypeVarSym(5, Ast.VarText.Absent, Kind.SchemaRow, isRegion = false, loc), loc)
    val tpe = Type.mkSchema(Type.mkSchemaRowExtend(Name.Pred("A", loc), latticeType1, Type.mkSchemaRowExtend(Name.Pred("B", loc), latticeType2, restType, loc), loc), loc)

    val expected = "#{ A(String; Bool), B(Int32; String) | s5 }"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatType.SchemaRow.External.01") {
    val relationType = Type.mkRelation(Type.Int32 :: Type.Str :: Nil, loc)
    val tpe = Type.mkSchemaRowExtend(Name.Pred("S", loc), relationType, Type.SchemaRowEmpty, loc)

    val expected = "#( S(Int32, String) )"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatType.SchemaRow.External.02") {
    val latticeType1 = Type.mkLattice(List(Type.Str, Type.Bool), loc)
    val latticeType2 = Type.mkLattice(List(Type.Int32, Type.Str), loc)
    val restType = Type.Var(new Symbol.KindedTypeVarSym(5, Ast.VarText.Absent, Kind.SchemaRow, isRegion = false, loc), loc)
    val tpe = Type.mkSchemaRowExtend(Name.Pred("A", loc), latticeType1, Type.mkSchemaRowExtend(Name.Pred("B", loc), latticeType2, restType, loc), loc)

    val expected = "#( A(String; Bool), B(Int32; String) | s5 )"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatType.SchemaRow.External.03") {
    val name = Name.mkQName("MyEmptySchemaRow")
    val ident = name.ident
    val sym = Symbol.mkTypeAliasSym(name.namespace, ident)
    val alias = Type.Alias(Ast.AliasConstructor(sym, loc), Nil, Type.RecordRowEmpty, loc)

    val latticeType1 = Type.mkLattice(List(Type.Str, Type.Bool), loc)

    val tpe = Type.mkSchemaRowExtend(Name.Pred("X", loc), latticeType1, alias, loc)

    val expected = "#( X(String; Bool) | MyEmptySchemaRow )"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatType.SchemaRow.External.04") {
    val name = Name.mkQName("X")
    val ident = name.ident
    val sym = Symbol.mkTypeAliasSym(name.namespace, ident)
    val latticeType1 = Type.mkLattice(List(Type.Str, Type.Bool), loc)
    val alias = Type.Alias(Ast.AliasConstructor(sym, loc), Nil, latticeType1, loc)


    val tpe = Type.mkSchemaRowExtend(Name.Pred("X", loc), alias, Type.SchemaRowEmpty, loc)

    val expected = "#( X(String; Bool) )"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatType.Enum.External.07") {
    val tvar1 = Type.Var(new Symbol.KindedTypeVarSym(1, Ast.VarText.Absent, Kind.Star, isRegion = false, loc), loc)
    val tvar2 = Type.Var(new Symbol.KindedTypeVarSym(2, Ast.VarText.Absent, Kind.Star, isRegion = false, loc), loc)
    val tvar3 = Type.Var(new Symbol.KindedTypeVarSym(3, Ast.VarText.Absent, Kind.Star, isRegion = false, loc), loc)
    val tpe = Type.mkEnum(Symbol.mkEnumSym("Triplet"), List(tvar1, tvar2, tvar3), loc)

    val expected = "Triplet[t1, t2, t3]"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatType.Type.Record.Internal.01") {
    val tpe = Type.mkRecord(Type.mkRecordRowExtend(Name.Field("x", loc), Type.Int32, Type.mkRecordRowExtend(Name.Field("y", loc), Type.Str, Type.RecordRowEmpty, loc), loc), loc)

    val expected = "{ x = Int32, y = String }"
    val actual = FormatType.formatTypeWithOptions(tpe, FormatOptions.Internal)

    assert(actual == expected)
  }

  test("FormatType.Record.Internal.02") {
    val rest = Type.Var(new Symbol.KindedTypeVarSym(0, Ast.VarText.Absent, Kind.RecordRow, isRegion = true, loc), loc)
    val tpe = Type.mkRecord(Type.mkRecordRowExtend(Name.Field("x", loc), Type.Int32, rest, loc), loc)

    val expected = "{ x = Int32 | r0! }"
    val actual = FormatType.formatTypeWithOptions(tpe, FormatOptions.Internal)

    assert(actual == expected)
  }

  test("FormatType.Arrow.Internal.01") {
    val paramType = Type.Var(new Symbol.KindedTypeVarSym(0, Ast.VarText.Absent, Kind.Star, isRegion = true, loc), loc)
    val tpe = Type.mkArrowWithEffect(paramType, Type.Pure, paramType, loc)

    val expected = "t0! -> t0!"
    val actual = FormatType.formatTypeWithOptions(tpe, FormatOptions.Internal)

    assert(actual == expected)
  }

  test("FormatType.Arrow.Internal.02") {
    val paramType = Type.Var(new Symbol.KindedTypeVarSym(0, Ast.VarText.Absent, Kind.Star, isRegion = true, loc), loc)
    val returnType = Type.Var(new Symbol.KindedTypeVarSym(1, Ast.VarText.Absent, Kind.Star, isRegion = true, loc), loc)
    val effectType = Type.Var(new Symbol.KindedTypeVarSym(2, Ast.VarText.Absent, Kind.Eff, isRegion = true, loc), loc)
    val tpe = Type.mkArrowWithEffect(paramType, effectType, returnType, loc)

    val expected = raw"t0! -> t1! \ e2!"
    val actual = FormatType.formatTypeWithOptions(tpe, FormatOptions.Internal)

    assert(actual == expected)
  }

  test("FormatType.Schema.Internal.01") {
    val relationType = Type.mkRelation(List(Type.Int32, Type.Str), loc)
    val tpe = Type.mkSchema(Type.mkSchemaRowExtend(Name.Pred("S", loc), relationType, Type.SchemaRowEmpty, loc), loc)

    val expected = "#{ S(Int32, String) }"
    val actual = FormatType.formatTypeWithOptions(tpe, FormatOptions.Internal)

    assert(actual == expected)
  }

  test("FormatType.Schema.Internal.02") {
    val latticeType1 = Type.mkLattice(List(Type.Str, Type.Bool), loc)
    val latticeType2 = Type.mkLattice(List(Type.Int32, Type.Str), loc)
    val restType = Type.Var(new Symbol.KindedTypeVarSym(5, Ast.VarText.Absent, Kind.SchemaRow, isRegion = false, loc), loc)
    val tpe = Type.mkSchema(Type.mkSchemaRowExtend(Name.Pred("A", loc), latticeType1, Type.mkSchemaRowExtend(Name.Pred("B", loc), latticeType2, restType, loc), loc), loc)

    val expected = "#{ A(String; Bool), B(Int32; String) | s5 }"
    val actual = FormatType.formatTypeWithOptions(tpe, FormatOptions.Internal)

    assert(actual == expected)
  }

  test("FormatType.Enum.Internal.07") {
    val tvar1 = Type.Var(new Symbol.KindedTypeVarSym(1, Ast.VarText.Absent, Kind.Star, isRegion = false, loc), loc)
    val tvar2 = Type.Var(new Symbol.KindedTypeVarSym(2, Ast.VarText.Absent, Kind.Star, isRegion = false, loc), loc)
    val tvar3 = Type.Var(new Symbol.KindedTypeVarSym(3, Ast.VarText.Absent, Kind.Star, isRegion = false, loc), loc)
    val tpe = Type.mkEnum(Symbol.mkEnumSym("Triplet"), List(tvar1, tvar2, tvar3), loc)

    val expected = "Triplet[t1, t2, t3]"
    val actual = FormatType.formatTypeWithOptions(tpe, FormatOptions.Internal)

    assert(actual == expected)
  }

  test("FormatType.Boolean.External.01") {
    val tvar1 = Type.Var(new Symbol.KindedTypeVarSym(1, Ast.VarText.SourceText("a"), Kind.Eff, isRegion = false, loc), loc)
    val tvar2 = Type.Var(new Symbol.KindedTypeVarSym(2, Ast.VarText.SourceText("b"), Kind.Eff, isRegion = false, loc), loc)
    val tvar3 = Type.Var(new Symbol.KindedTypeVarSym(3, Ast.VarText.SourceText("c"), Kind.Eff, isRegion = false, loc), loc)
    val tpe = Type.mkUnion(List(tvar1, tvar2, tvar3), loc)

    val expected = "a + b + c"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatPartialType.Tuple.External.01") {
    val tpe = Type.mkApply(Type.Cst(TypeConstructor.Tuple(2), loc), List(Type.Str), loc)

    val expected = "(String, ?)"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatPartialType.Boolean.External.01") {
    val tpe = Type.Complement

    val expected = "~?"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatPartialType.Boolean.External.03") {
    val tpe = Type.Union

    val expected = "? + ?"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatPartialType.Boolean.External.04") {
    val tpe = Type.Apply(Type.Union, Type.Pure, loc)

    val expected = "Pure + ?"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatPartialType.Boolean.External.06") {
    val tpe = Type.Intersection

    val expected = "? & ?"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatPartialType.Boolean.External.07") {
    val tpe = Type.Apply(Type.Intersection, Type.Pure, loc)

    val expected = "Pure & ?"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatPartialType.Arrow.External.01") {
    val tpe = Type.mkApply(Type.Cst(TypeConstructor.Arrow(2), loc), List(Type.Pure), loc)

    val expected = raw"? -> ?"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatPartialType.Arrow.External.02") {
    val tpe = Type.mkApply(Type.Cst(TypeConstructor.Arrow(3), loc), List(Type.Impure, Type.Str), loc)

    val expected = raw"String -> (? -> ? \ IO)"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatPartialType.Arrow.External.03") {
    val tpe = Type.Cst(TypeConstructor.Arrow(4), loc)

    val expected = raw"? -> (? -> (? -> ? \ ?))"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatType.Alias.External.01") {
    val name = Name.mkQName("MyType")
    val ident = name.ident
    val sym = Symbol.mkTypeAliasSym(name.namespace, ident)
    val tpe = Type.Alias(Ast.AliasConstructor(sym, loc), Nil, Type.Int32, loc)

    val expected = "MyType"
    val actual = FormatType.formatTypeWithOptions(tpe, standardFormat)

    assert(actual == expected)
  }

  test("FormatType.Var.External.01") {
    val m = Type.Var(new Symbol.KindedTypeVarSym(1, Ast.VarText.SourceText("m"), Kind.Star ->: Kind.Star, isRegion = false, loc), loc)
    val a = Type.Var(new Symbol.KindedTypeVarSym(2, Ast.VarText.SourceText("a"), Kind.Star, isRegion = false, loc), loc)

    val ma = Type.mkApply(m, List(a), loc)

    val expected = "m[a]"
    val actual = FormatType.formatTypeWithOptions(ma, standardFormat)

    assert(actual == expected)
  }
}
