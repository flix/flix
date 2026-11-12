/*
 * Copyright 2020 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.ast.shared.SymUse.TypeAliasSymUse
import ca.uwaterloo.flix.language.ast.shared.{RegionScope, VarText}
import ca.uwaterloo.flix.language.ast.{Kind, Name, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.collection.Nel
import org.scalatest.funsuite.AnyFunSuite

class TestFormatType extends AnyFunSuite with TestUtils {

  private val scope = RegionScope.Top
  private val loc = SourceLocation.Unknown
  private val standardFormat = FormatOptions(
    FormatOptions.VarNames.BySource
  )

  test("FormatType.Type.Record.External.01") {
    val tpe = Type.mkRecord(Type.mkRecordRowExtend(Name.Label("x", loc), Type.Int32, Type.mkRecordRowExtend(Name.Label("y", loc), Type.Str, Type.RecordRowEmpty, loc), loc), loc)

    val expected = "{ x = Int32, y = String }"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Record.External.02") {
    val rest = Type.Var(new Symbol.KindedTypeVarSym(0, VarText.Absent, Kind.RecordRow, scope, loc), loc)
    val tpe = Type.mkRecord(Type.mkRecordRowExtend(Name.Label("x", loc), Type.Int32, rest, loc), loc)

    val expected = "{ x = Int32 | r0 }"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Type.RecordRow.External.01") {
    val tpe = Type.mkRecordRowExtend(Name.Label("x", loc), Type.Int32, Type.mkRecordRowExtend(Name.Label("y", loc), Type.Str, Type.RecordRowEmpty, loc), loc)

    val expected = "( x = Int32, y = String )"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.RecordRow.External.02") {
    val rest = Type.Var(new Symbol.KindedTypeVarSym(0, VarText.Absent, Kind.RecordRow, scope, loc), loc)
    val tpe = Type.mkRecordRowExtend(Name.Label("x", loc), Type.Int32, rest, loc)

    val expected = "( x = Int32 | r0 )"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.RecordRow.External.03") {
    val name = Name.mkQName("MyEmptyRecordRow")
    val ident = name.ident
    val sym = Symbol.mkTypeAliasSym(name.namespace, ident)
    val alias = Type.Alias(TypeAliasSymUse(sym, loc), Nil, Type.RecordRowEmpty, loc)

    val tpe = Type.mkRecordRowExtend(Name.Label("x", loc), Type.Int32, alias, loc)

    val expected = "( x = Int32 | MyEmptyRecordRow )"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Record.External.04") {
    val name = Name.mkQName("MyEmptyRecordRow")
    val ident = name.ident
    val sym = Symbol.mkTypeAliasSym(name.namespace, ident)
    val alias = Type.Alias(TypeAliasSymUse(sym, loc), Nil, Type.RecordRowEmpty, loc)

    val tpe = Type.mkRecord(alias, loc)

    val expected = "{ MyEmptyRecordRow }"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Arrow.External.01") {
    val paramType = Type.Var(new Symbol.KindedTypeVarSym(0, VarText.Absent, Kind.Star, scope, loc), loc)
    val tpe = Type.mkArrowWithEffect(paramType, Type.Pure, paramType, loc)

    val expected = "t0 -> t0"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Arrow.External.02") {
    val paramType = Type.Var(new Symbol.KindedTypeVarSym(0, VarText.Absent, Kind.Star, scope, loc), loc)
    val returnType = Type.Var(new Symbol.KindedTypeVarSym(1, VarText.Absent, Kind.Star, scope, loc), loc)
    val effectType = Type.Var(new Symbol.KindedTypeVarSym(2, VarText.Absent, Kind.Eff, scope, loc), loc)
    val tpe = Type.mkArrowWithEffect(paramType, effectType, returnType, loc)

    val expected = raw"t0 -> t1 \ e2"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Arrow.External.03") {
    val paramType = Type.Var(new Symbol.KindedTypeVarSym(0, VarText.Absent, Kind.Star, scope, loc), loc)
    val returnType = Type.Var(new Symbol.KindedTypeVarSym(1, VarText.Absent, Kind.Star, scope, loc), loc)
    val tpe = Type.mkArrowWithEffect(paramType, Type.IO, returnType, loc)

    val expected = raw"t0 -> t1 \ IO"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Arrow.External.04") {
    val tpe = Type.mkIoUncurriedArrow(Nel.of(Type.Int8, Type.Int16), Type.Int32, loc)

    val expected = raw"Int8 -> (Int16 -> Int32 \ IO)"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Arrow.External.05") {
    val eff = Type.mkUnion(Type.Var(new Symbol.KindedTypeVarSym(1, VarText.Absent, Kind.Eff, scope, loc), loc), Type.Var(new Symbol.KindedTypeVarSym(2, VarText.Absent, Kind.Eff, scope, loc), loc), loc)
    val tpe = Type.mkArrowWithEffect(Type.BigInt, eff, Type.Bool, loc)

    val expected = raw"BigInt -> Bool \ e1 + e2"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Arrow.External.06") {
    val arg = Type.mkTuple(List(Type.Bool, Type.Bool), loc)
    val tpe = Type.mkPureArrow(arg, Type.Str, loc)

    val expected = "((Bool, Bool)) -> String"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Schema.External.01") {
    val relationType = Type.mkRelation(Type.Int32 :: Type.Str :: Nil, loc)
    val tpe = Type.mkSchema(Type.mkSchemaRowExtend(Name.Pred("S", loc), relationType, Type.SchemaRowEmpty, loc), loc)

    val expected = "#{ S(Int32, String) }"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Schema.External.02") {
    val latticeType1 = Type.mkLattice(List(Type.Str, Type.Bool), loc)
    val latticeType2 = Type.mkLattice(List(Type.Int32, Type.Str), loc)
    val restType = Type.Var(new Symbol.KindedTypeVarSym(5, VarText.Absent, Kind.SchemaRow, scope, loc), loc)
    val tpe = Type.mkSchema(Type.mkSchemaRowExtend(Name.Pred("A", loc), latticeType1, Type.mkSchemaRowExtend(Name.Pred("B", loc), latticeType2, restType, loc), loc), loc)

    val expected = "#{ A(String; Bool), B(Int32; String) | s5 }"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.SchemaRow.External.01") {
    val relationType = Type.mkRelation(Type.Int32 :: Type.Str :: Nil, loc)
    val tpe = Type.mkSchemaRowExtend(Name.Pred("S", loc), relationType, Type.SchemaRowEmpty, loc)

    val expected = "#( S(Int32, String) )"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.SchemaRow.External.02") {
    val latticeType1 = Type.mkLattice(List(Type.Str, Type.Bool), loc)
    val latticeType2 = Type.mkLattice(List(Type.Int32, Type.Str), loc)
    val restType = Type.Var(new Symbol.KindedTypeVarSym(5, VarText.Absent, Kind.SchemaRow, scope, loc), loc)
    val tpe = Type.mkSchemaRowExtend(Name.Pred("A", loc), latticeType1, Type.mkSchemaRowExtend(Name.Pred("B", loc), latticeType2, restType, loc), loc)

    val expected = "#( A(String; Bool), B(Int32; String) | s5 )"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.SchemaRow.External.03") {
    val name = Name.mkQName("MyEmptySchemaRow")
    val ident = name.ident
    val sym = Symbol.mkTypeAliasSym(name.namespace, ident)
    val alias = Type.Alias(TypeAliasSymUse(sym, loc), Nil, Type.RecordRowEmpty, loc)

    val latticeType1 = Type.mkLattice(List(Type.Str, Type.Bool), loc)

    val tpe = Type.mkSchemaRowExtend(Name.Pred("X", loc), latticeType1, alias, loc)

    val expected = "#( X(String; Bool) | MyEmptySchemaRow )"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.SchemaRow.External.04") {
    val name = Name.mkQName("X")
    val ident = name.ident
    val sym = Symbol.mkTypeAliasSym(name.namespace, ident)
    val latticeType1 = Type.mkLattice(List(Type.Str, Type.Bool), loc)
    val alias = Type.Alias(TypeAliasSymUse(sym, loc), Nil, latticeType1, loc)


    val tpe = Type.mkSchemaRowExtend(Name.Pred("X", loc), alias, Type.SchemaRowEmpty, loc)

    val expected = "#( X(String; Bool) )"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Enum.External.07") {
    val tvar1 = Type.Var(new Symbol.KindedTypeVarSym(1, VarText.Absent, Kind.Star, scope, loc), loc)
    val tvar2 = Type.Var(new Symbol.KindedTypeVarSym(2, VarText.Absent, Kind.Star, scope, loc), loc)
    val tvar3 = Type.Var(new Symbol.KindedTypeVarSym(3, VarText.Absent, Kind.Star, scope, loc), loc)
    val tpe = Type.mkEnum(Symbol.mkEnumSym("Triplet"), List(tvar1, tvar2, tvar3), loc)

    val expected = "Triplet[t1, t2, t3]"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Type.Record.Internal.01") {
    val tpe = Type.mkRecord(Type.mkRecordRowExtend(Name.Label("x", loc), Type.Int32, Type.mkRecordRowExtend(Name.Label("y", loc), Type.Str, Type.RecordRowEmpty, loc), loc), loc)

    val expected = "{ x = Int32, y = String }"
    val actual = Pretty.tpe(tpe)(FormatOptions.Internal).plain

    assert(actual == expected)
  }

  test("FormatType.Record.Internal.02") {
    val rest = Type.Var(new Symbol.KindedTypeVarSym(0, VarText.Absent, Kind.RecordRow, scope, loc), loc)
    val tpe = Type.mkRecord(Type.mkRecordRowExtend(Name.Label("x", loc), Type.Int32, rest, loc), loc)

    val expected = "{ x = Int32 | r0 }"
    val actual = Pretty.tpe(tpe)(FormatOptions.Internal).plain

    assert(actual == expected)
  }

  test("FormatType.Arrow.Internal.01") {
    val paramType = Type.Var(new Symbol.KindedTypeVarSym(0, VarText.Absent, Kind.Star, scope, loc), loc)
    val tpe = Type.mkArrowWithEffect(paramType, Type.Pure, paramType, loc)

    val expected = "t0 -> t0"
    val actual = Pretty.tpe(tpe)(FormatOptions.Internal).plain

    assert(actual == expected)
  }

  test("FormatType.Arrow.Internal.02") {
    val paramType = Type.Var(new Symbol.KindedTypeVarSym(0, VarText.Absent, Kind.Star, scope, loc), loc)
    val returnType = Type.Var(new Symbol.KindedTypeVarSym(1, VarText.Absent, Kind.Star, scope, loc), loc)
    val effectType = Type.Var(new Symbol.KindedTypeVarSym(2, VarText.Absent, Kind.Eff, scope, loc), loc)
    val tpe = Type.mkArrowWithEffect(paramType, effectType, returnType, loc)

    val expected = raw"t0 -> t1 \ e2"
    val actual = Pretty.tpe(tpe)(FormatOptions.Internal).plain

    assert(actual == expected)
  }

  test("FormatType.Schema.Internal.01") {
    val relationType = Type.mkRelation(List(Type.Int32, Type.Str), loc)
    val tpe = Type.mkSchema(Type.mkSchemaRowExtend(Name.Pred("S", loc), relationType, Type.SchemaRowEmpty, loc), loc)

    val expected = "#{ S(Int32, String) }"
    val actual = Pretty.tpe(tpe)(FormatOptions.Internal).plain

    assert(actual == expected)
  }

  test("FormatType.Bool.Internal.01") {
    val a = Type.Var(new Symbol.KindedTypeVarSym(1, VarText.SourceText("a"), Kind.Bool, scope, loc), loc)
    val b = Type.Var(new Symbol.KindedTypeVarSym(2, VarText.SourceText("b"), Kind.Bool, scope, loc), loc)
    val c = Type.Var(new Symbol.KindedTypeVarSym(3, VarText.SourceText("c"), Kind.Bool, scope, loc), loc)
    val tpe = Type.mkAnd(List(a, b, c), loc)

    val expected = "b1 and b2 and b3"
    val actual = Pretty.tpe(tpe)(FormatOptions.Internal).plain

    assert(actual == expected)
  }

  test("FormatType.Bool.Internal.02") {
    val a = Type.Var(new Symbol.KindedTypeVarSym(1, VarText.SourceText("a"), Kind.Bool, scope, loc), loc)
    val b = Type.Var(new Symbol.KindedTypeVarSym(2, VarText.SourceText("b"), Kind.Bool, scope, loc), loc)
    val c = Type.Var(new Symbol.KindedTypeVarSym(3, VarText.SourceText("c"), Kind.Bool, scope, loc), loc)
    val tpe = Type.mkOr(List(a, b, c), loc)

    val expected = "b1 or b2 or b3"
    val actual = Pretty.tpe(tpe)(FormatOptions.Internal).plain

    assert(actual == expected)
  }

  test("FormatType.Schema.Internal.02") {
    val latticeType1 = Type.mkLattice(List(Type.Str, Type.Bool), loc)
    val latticeType2 = Type.mkLattice(List(Type.Int32, Type.Str), loc)
    val restType = Type.Var(new Symbol.KindedTypeVarSym(5, VarText.Absent, Kind.SchemaRow, scope, loc), loc)
    val tpe = Type.mkSchema(Type.mkSchemaRowExtend(Name.Pred("A", loc), latticeType1, Type.mkSchemaRowExtend(Name.Pred("B", loc), latticeType2, restType, loc), loc), loc)

    val expected = "#{ A(String; Bool), B(Int32; String) | s5 }"
    val actual = Pretty.tpe(tpe)(FormatOptions.Internal).plain

    assert(actual == expected)
  }

  test("FormatType.Enum.Internal.07") {
    val tvar1 = Type.Var(new Symbol.KindedTypeVarSym(1, VarText.Absent, Kind.Star, scope, loc), loc)
    val tvar2 = Type.Var(new Symbol.KindedTypeVarSym(2, VarText.Absent, Kind.Star, scope, loc), loc)
    val tvar3 = Type.Var(new Symbol.KindedTypeVarSym(3, VarText.Absent, Kind.Star, scope, loc), loc)
    val tpe = Type.mkEnum(Symbol.mkEnumSym("Triplet"), List(tvar1, tvar2, tvar3), loc)

    val expected = "Triplet[t1, t2, t3]"
    val actual = Pretty.tpe(tpe)(FormatOptions.Internal).plain

    assert(actual == expected)
  }

  test("FormatType.Boolean.External.01") {
    val tvar1 = Type.Var(new Symbol.KindedTypeVarSym(1, VarText.SourceText("a"), Kind.Eff, scope, loc), loc)
    val tvar2 = Type.Var(new Symbol.KindedTypeVarSym(2, VarText.SourceText("b"), Kind.Eff, scope, loc), loc)
    val tvar3 = Type.Var(new Symbol.KindedTypeVarSym(3, VarText.SourceText("c"), Kind.Eff, scope, loc), loc)
    val tpe = Type.mkUnion(tvar1, tvar2, tvar3, loc)

    val expected = "a + b + c"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Alias.External.01") {
    val name = Name.mkQName("MyType")
    val ident = name.ident
    val sym = Symbol.mkTypeAliasSym(name.namespace, ident)
    val tpe = Type.Alias(TypeAliasSymUse(sym, loc), Nil, Type.Int32, loc)

    val expected = "MyType"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Var.External.01") {
    val m = Type.Var(new Symbol.KindedTypeVarSym(1, VarText.SourceText("m"), Kind.Star ->: Kind.Star, scope, loc), loc)
    val a = Type.Var(new Symbol.KindedTypeVarSym(2, VarText.SourceText("a"), Kind.Star, scope, loc), loc)

    val ma = Type.mkApply(m, List(a), loc)

    val expected = "m[a]"
    val actual = Pretty.tpe(ma)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Bool.External.01") {
    val a = Type.Var(new Symbol.KindedTypeVarSym(1, VarText.SourceText("a"), Kind.Bool, scope, loc), loc)
    val b = Type.Var(new Symbol.KindedTypeVarSym(2, VarText.SourceText("b"), Kind.Bool, scope, loc), loc)
    val c = Type.Var(new Symbol.KindedTypeVarSym(3, VarText.SourceText("c"), Kind.Bool, scope, loc), loc)
    val tpe = Type.mkAnd(List(a, b, c), loc)

    val expected = "a and b and c"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Bool.External.02") {
    val a = Type.Var(new Symbol.KindedTypeVarSym(1, VarText.SourceText("a"), Kind.Bool, scope, loc), loc)
    val b = Type.Var(new Symbol.KindedTypeVarSym(2, VarText.SourceText("b"), Kind.Bool, scope, loc), loc)
    val c = Type.Var(new Symbol.KindedTypeVarSym(3, VarText.SourceText("c"), Kind.Bool, scope, loc), loc)
    val tpe = Type.mkOr(List(a, b, c), loc)

    val expected = "a or b or c"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.VarNames.01") {
    // Two types that are shown together share one naming.
    val x = Type.Var(new Symbol.KindedTypeVarSym(10, VarText.Absent, Kind.Star, scope, loc), loc)
    val y = Type.Var(new Symbol.KindedTypeVarSym(20, VarText.Absent, Kind.Star, scope, loc), loc)
    val tpe1 = Type.mkTuple(List(x, y), loc)
    val tpe2 = Type.mkPureArrow(y, y, loc)
    val opts = FormatOptions.forTypes(tpe1, tpe2)

    assert(Pretty.tpe(tpe1)(opts).plain == "(t0, t1)")
    assert(Pretty.tpe(tpe2)(opts).plain == "t1 -> t1")
  }

  test("FormatType.VarNames.02") {
    // Without a naming, a variable with no source text is written by its id.
    val x = Type.Var(new Symbol.KindedTypeVarSym(10, VarText.Absent, Kind.Star, scope, loc), loc)
    val y = Type.Var(new Symbol.KindedTypeVarSym(20, VarText.Absent, Kind.Star, scope, loc), loc)
    val tpe = Type.mkTuple(List(x, y), loc)

    val expected = "(t10, t20)"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.VarNames.03") {
    // A variable with a source text keeps it and takes no number.
    val a = Type.Var(new Symbol.KindedTypeVarSym(10, VarText.SourceText("a"), Kind.Star, scope, loc), loc)
    val x = Type.Var(new Symbol.KindedTypeVarSym(20, VarText.Absent, Kind.Star, scope, loc), loc)
    val tpe = Type.mkTuple(List(a, x), loc)

    val expected = "(a, t0)"
    val actual = Pretty.tpe(tpe)(FormatOptions.forTypes(tpe)).plain

    assert(actual == expected)
  }

  test("FormatType.VarNames.04") {
    // A number is skipped if its name is the source text of another variable.
    val a = Type.Var(new Symbol.KindedTypeVarSym(20, VarText.SourceText("t0"), Kind.Star, scope, loc), loc)
    val x = Type.Var(new Symbol.KindedTypeVarSym(10, VarText.Absent, Kind.Star, scope, loc), loc)
    val tpe = Type.mkTuple(List(a, x), loc)

    val expected = "(t0, t1)"
    val actual = Pretty.tpe(tpe)(FormatOptions.forTypes(tpe)).plain

    assert(actual == expected)
  }

  test("FormatType.VarNames.05") {
    // The numbers run across the kinds.
    val x = Type.Var(new Symbol.KindedTypeVarSym(10, VarText.Absent, Kind.Star, scope, loc), loc)
    val y = Type.Var(new Symbol.KindedTypeVarSym(20, VarText.Absent, Kind.Star, scope, loc), loc)
    val e = Type.Var(new Symbol.KindedTypeVarSym(30, VarText.Absent, Kind.Eff, scope, loc), loc)
    val tpe = Type.mkArrowWithEffect(x, e, y, loc)

    val expected = "t0 -> t1 \\ e2"
    val actual = Pretty.tpe(tpe)(FormatOptions.forTypes(tpe)).plain

    assert(actual == expected)
  }

  test("FormatType.VarNames.06") {
    // An occurrence with a source text gives its text to every occurrence of the variable.
    val x1 = Type.Var(new Symbol.KindedTypeVarSym(10, VarText.Absent, Kind.Star, scope, loc), loc)
    val x2 = Type.Var(new Symbol.KindedTypeVarSym(10, VarText.SourceText("a"), Kind.Star, scope, loc), loc)
    val y = Type.Var(new Symbol.KindedTypeVarSym(20, VarText.Absent, Kind.Star, scope, loc), loc)
    val tpe1 = Type.mkTuple(List(x1, y), loc)
    val tpe2 = Type.mkPureArrow(x2, y, loc)
    val opts = FormatOptions.forTypes(tpe1, tpe2)

    assert(Pretty.tpe(tpe1)(opts).plain == "(a, t0)")
    assert(Pretty.tpe(tpe2)(opts).plain == "a -> t0")
  }

  test("FormatType.VarNames.07") {
    // A variable that the naming was not built from is written by its id.
    val x = Type.Var(new Symbol.KindedTypeVarSym(10, VarText.Absent, Kind.Star, scope, loc), loc)
    val y = Type.Var(new Symbol.KindedTypeVarSym(20, VarText.Absent, Kind.Star, scope, loc), loc)
    val tpe = Type.mkTuple(List(x, y), loc)

    val expected = "(t0, t20)"
    val actual = Pretty.tpe(tpe)(FormatOptions.forTypes(x)).plain

    assert(actual == expected)
  }
}
