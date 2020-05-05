package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.ast.{Kind, Rigidity, SourceLocation, Symbol, Type, TypeConstructor}
import org.scalatest.FunSuite

import scala.util.Random

class TestFormatType extends FunSuite with TestUtils {
  test("FormatType.WellFormedType.Record.External.01") {
    val tpe = Type.mkRecordExtend("x", Type.Int32, Type.mkRecordExtend("y", Type.Str, Type.RecordEmpty))

    val expected = "{ x: Int32, y: Str }"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Record.External.02") {
    val rest = Type.Var(0, Kind.Record, Rigidity.Rigid)
    rest.setText("theRest")
    val tpe = Type.mkRecordExtend("x", Type.Int32, rest)

    val expected = "{ x: Int32 | theRest }"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Arrow.External.01") {
    val paramType = Type.Var(0, Kind.Star, Rigidity.Rigid)
    paramType.setText("t1")
    val tpe = Type.mkArrow(paramType, Type.Pure, paramType)

    val expected = "t1 -> t1"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Arrow.External.02") {
    val paramType = Type.Var(0, Kind.Star, Rigidity.Rigid)
    val returnType = Type.Var(1, Kind.Star, Rigidity.Rigid)
    val effectType = Type.Var(2, Kind.Effect, Rigidity.Rigid)
    val tpe = Type.mkArrow(paramType, effectType, returnType)

    val expected = "a -> b & c"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Schema.External.01") {
    val tupleType = Type.mkTuple(List(Type.Int32, Type.Str))
    val tpe = Type.mkSchemaExtend("S", tupleType, Type.SchemaEmpty)

    val expected = "#{ S(Int32, Str) }"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Schema.External.02") {
    val tupleType1 = Type.Str
    val tupleType2 = Type.mkTuple(List(Type.Int32, Type.Str))
    val restType = Type.Var(5, Kind.Schema, Rigidity.Flexible)
    restType.setText("theRest")
    val tpe = Type.mkSchemaExtend("A", tupleType1, Type.mkSchemaExtend("B", tupleType2, restType))

    val expected = "#{ A(Str), B(Int32, Str) | theRest }"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Enum.External.07") {
    val enumConstructor = TypeConstructor.Enum(Symbol.mkEnumSym("Triplet"), Kind.Star ->: Kind.Star ->: Kind.Star ->: Kind.Star)
    val tvar1 = Type.Var(1, Kind.Star, Rigidity.Flexible)
    val tvar2 = Type.Var(2, Kind.Star, Rigidity.Flexible)
    val tvar3 = Type.Var(3, Kind.Star, Rigidity.Flexible)
    tvar1.setText("a")
    tvar2.setText("b")
    tvar3.setText("c")
    val tpe = Type.mkApply(Type.Cst(enumConstructor), List(tvar1, tvar2, tvar3))

    val expected = "Triplet[a, b, c]"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.External)

    assert(actual == expected)
  }

  test("FormatType.WellFormedType.Record.Internal.01") {
    val tpe = Type.mkRecordExtend("x", Type.Int32, Type.mkRecordExtend("y", Type.Str, Type.RecordEmpty))

    val expected = "{ x: Int32, y: Str }"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Record.Internal.02") {
    val rest = Type.Var(0, Kind.Record, Rigidity.Rigid)
    rest.setText("theRest")
    val tpe = Type.mkRecordExtend("x", Type.Int32, rest)

    val expected = "{ x: Int32 | '0 }"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Arrow.Internal.01") {
    val paramType = Type.Var(0, Kind.Star, Rigidity.Rigid)
    paramType.setText("t1")
    val tpe = Type.mkArrow(paramType, Type.Pure, paramType)

    val expected = "'0 -> '0"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Arrow.Internal.02") {
    val paramType = Type.Var(0, Kind.Star, Rigidity.Rigid)
    val returnType = Type.Var(1, Kind.Star, Rigidity.Rigid)
    val effectType = Type.Var(2, Kind.Effect, Rigidity.Rigid)
    val tpe = Type.mkArrow(paramType, effectType, returnType)

    val expected = "'0 -> '1 & ''2"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Schema.Internal.01") {
    val tupleType = Type.mkTuple(List(Type.Int32, Type.Str))
    val tpe = Type.mkSchemaExtend("S", tupleType, Type.SchemaEmpty)

    val expected = "#{ S(Int32, Str) }"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Schema.Internal.02") {
    val tupleType1 = Type.Str
    val tupleType2 = Type.mkTuple(List(Type.Int32, Type.Str))
    val restType = Type.Var(5, Kind.Schema, Rigidity.Flexible)
    restType.setText("theRest")
    val tpe = Type.mkSchemaExtend("A", tupleType1, Type.mkSchemaExtend("B", tupleType2, restType))

    val expected = "#{ A(Str), B(Int32, Str) | '5 }"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Enum.Internal.07") {
    val enumConstructor = TypeConstructor.Enum(Symbol.mkEnumSym("Triplet"), Kind.Star ->: Kind.Star ->: Kind.Star ->: Kind.Star)
    val tvar1 = Type.Var(1, Kind.Star, Rigidity.Flexible)
    val tvar2 = Type.Var(2, Kind.Star, Rigidity.Flexible)
    val tvar3 = Type.Var(3, Kind.Star, Rigidity.Flexible)
    tvar1.setText("a")
    tvar2.setText("b")
    tvar3.setText("c")
    val tpe = Type.mkApply(Type.Cst(enumConstructor), List(tvar1, tvar2, tvar3))

    val expected = "Triplet['1, '2, '3]"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.Internal)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Record.External.01") {
    val tpe = Type.Cst(TypeConstructor.RecordExtend("x"))

    val expected = "{ x: ??? }"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Record.External.02") {
    val tpe = Type.Apply(Type.Cst(TypeConstructor.RecordExtend("x")), Type.Int32)

    val expected = "{ x: Int32 | ??? }"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Record.External.03") {
    val tpe = Type.mkApply(Type.Cst(TypeConstructor.RecordExtend("x")), List(Type.Int32, Type.Int32, Type.Str))

    val expected = "RecordExtend(x)[Int32, Int32, Str]"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Schema.External.01") {
    val tpe = Type.Cst(TypeConstructor.SchemaExtend("X"))

    val expected = "#{ X(???) }"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Schema.External.02") {
    val tpe = Type.Apply(Type.Cst(TypeConstructor.SchemaExtend("X")), Type.Int32)

    val expected = "#{ X(Int32) | ??? }"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Schema.External.03") {
    val tpe = Type.mkApply(Type.Cst(TypeConstructor.SchemaExtend("X")), List(Type.Int32, Type.Int32, Type.Str))

    val expected = "SchemaExtend(X)[Int32, Int32, Str]"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Tuple.External.01") {
    val tpe = Type.mkApply(Type.Cst(TypeConstructor.Tuple(2)), List(Type.Str))

    val expected = "(Str, ???)"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Tuple.External.02") {
    val tpe = Type.mkApply(Type.Cst(TypeConstructor.Tuple(2)), List(Type.Str, Type.Int32, Type.Float32))

    val expected = "(Str, Int32)[Float32]"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.External)

    assert(actual == expected)
  }
}
