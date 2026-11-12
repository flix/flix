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
import ca.uwaterloo.flix.language.ast.{Kind, Name, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.collection.Nel
import org.scalatest.funsuite.AnyFunSuite

class TestFormatType extends AnyFunSuite with TestUtils {

  private val scope = RegionScope.Top
  private val loc = SourceLocation.Unknown
  private val standardFormat = FormatOptions(
    FormatOptions.VarName.NameBased
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

  test("FormatType.Union.External.01") {
    // A repeated effect is printed once.
    val tpe = union(effA, effB, effA)

    val expected = "A + B"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Union.External.02") {
    // The members are printed in the order they are given.
    val tpe = union(effC, effVar(1, "ef"), effB, effA)

    val expected = "C + ef + B + A"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Union.External.03") {
    // A repeated variable is printed once.
    val tpe = union(effVar(2, "ef2"), effVar(1, "ef1"), effVar(2, "ef2"))

    val expected = "ef2 + ef1"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Union.External.04") {
    // A pure member is not printed.
    val tpe = union(Type.Pure, effA, Type.Pure)

    val expected = "A"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Union.External.05") {
    // A union of nothing but pure members is pure, so the arrow is printed without an effect.
    val tpe = Type.mkArrowWithEffect(Type.Unit, union(Type.Pure, Type.Pure), Type.Unit, loc)

    val expected = "Unit -> Unit"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Union.External.06") {
    // A union inside a type is simplified too.
    val tpe = Type.mkArrowWithEffect(Type.Unit, union(effB, effA, effB), Type.Unit, loc)

    val expected = raw"Unit -> Unit \ B + A"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Union.External.07") {
    // A union with the universe is the universe.
    val tpe = union(effVar(1, "ef"), Type.Univ)

    val expected = "Univ"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Union.External.08") {
    // An intersection is kept as it is.
    val ef = effVar(1, "ef")
    val tpe = union(effB, Type.mkIntersection(effA, ef, loc), ef)

    val expected = "B + (A & ef) + ef"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Union.External.09") {
    // A difference is kept as it is, but the unions inside it are simplified.
    val tpe = Type.mkDifference(union(effB, effA, effB), effA, loc)

    val expected = "(B + A) - A"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Union.External.10") {
    // An alias keeps its name and is printed once.
    val sym = Symbol.mkTypeAliasSym(Name.RootNS, Name.Ident("AB", loc))
    val alias = Type.Alias(TypeAliasSymUse(sym, loc), Nil, union(effA, effB), loc)
    val tpe = union(effC, alias, alias)

    val expected = "C + AB"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Union.External.11") {
    // An effect applied to a type is printed once.
    val sym = Symbol.mkEffSym(Name.RootNS, Name.Ident("St", loc))
    val st = Type.Apply(Type.Cst(TypeConstructor.Effect(sym, Kind.Star ->: Kind.Eff), loc), Type.Int32, loc)
    val tpe = union(st, effA, st)

    val expected = "St[Int32] + A"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Difference.External.01") {
    // An intersection with a complement is printed as a difference.
    val ef = effVar(1, "ef")
    val tpe = Type.mkIntersection(ef, Type.mkComplement(effA, loc), loc)

    val expected = "ef - A"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Difference.External.02") {
    // The complement may come first.
    val ef = effVar(1, "ef")
    val tpe = Type.mkIntersection(Type.mkComplement(effA, loc), ef, loc)

    val expected = "ef - A"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Difference.External.03") {
    // Every complement is subtracted.
    val ef = effVar(1, "ef")
    val tpe = Type.mkIntersection(ef, Type.mkIntersection(Type.mkComplement(effA, loc), Type.mkComplement(effB, loc), loc), loc)

    val expected = "ef - A - B"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Difference.External.04") {
    // A complement is subtracted from a difference without parentheses.
    val ef = effVar(1, "ef")
    val tpe = Type.mkIntersection(Type.mkIntersection(ef, Type.mkComplement(effA, loc), loc), Type.mkComplement(effB, loc), loc)

    val expected = "ef - A - B"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Difference.External.05") {
    // The complements are subtracted from the intersection of the rest.
    val tpe = Type.mkIntersection(effVar(1, "ef1"), Type.mkIntersection(effVar(2, "ef2"), Type.mkComplement(effA, loc), loc), loc)

    val expected = "(ef1 & ef2) - A"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Difference.External.06") {
    // The complement of a union is subtracted as a union.
    val ef = effVar(1, "ef")
    val tpe = Type.mkIntersection(ef, Type.mkComplement(union(effA, effB), loc), loc)

    val expected = "ef - (A + B)"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Difference.External.07") {
    // A difference is parenthesized as a member of a union.
    val ef = effVar(1, "ef")
    val tpe = union(Type.mkIntersection(ef, Type.mkComplement(effA, loc), loc), effB)

    val expected = "(ef - A) + B"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Difference.External.08") {
    // An intersection of nothing but complements is kept as it is.
    val tpe = Type.mkIntersection(Type.mkComplement(effA, loc), Type.mkComplement(effB, loc), loc)

    val expected = "(~A) & (~B)"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  test("FormatType.Difference.External.09") {
    // An intersection with a complement inside a type is printed as a difference too.
    val ef = effVar(1, "ef")
    val tpe = Type.mkArrowWithEffect(Type.Unit, Type.mkIntersection(ef, Type.mkComplement(effA, loc), loc), Type.Unit, loc)

    val expected = raw"Unit -> Unit \ ef - A"
    val actual = Pretty.tpe(tpe)(standardFormat).plain

    assert(actual == expected)
  }

  private val effA: Type = mkEff("A")
  private val effB: Type = mkEff("B")
  private val effC: Type = mkEff("C")

  /** Returns the effect with the given `name`. */
  private def mkEff(name: String): Type =
    Type.Cst(TypeConstructor.Effect(Symbol.mkEffSym(Name.RootNS, Name.Ident(name, loc)), Kind.Eff), loc)

  /** Returns the effect variable with the given `id` and `name`. */
  private def effVar(id: Int, name: String): Type =
    Type.Var(new Symbol.KindedTypeVarSym(id, VarText.SourceText(name), Kind.Eff, scope, loc), loc)

  /** Returns the union of the given `tpes`, built without the smart constructors. */
  private def union(tpes: Type*): Type =
    tpes.reduceRight((tpe1, tpe2) => Type.Apply(Type.Apply(Type.Union, tpe1, loc), tpe2, loc))
}
