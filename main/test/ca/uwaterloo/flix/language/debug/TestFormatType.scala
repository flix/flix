package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.ast.{Kind, Rigidity, SourceLocation, Symbol, Type, TypeConstructor}
import org.scalatest.FunSuite

import scala.util.Random

class TestFormatType extends FunSuite with TestUtils {
  test("FormatType.WellFormedType.External.01") {
    val tpe = Type.mkRecordExtend("x", Type.Int32, Type.mkRecordExtend("y", Type.Str, Type.RecordEmpty))

    val expected = "{ x: Int32, y: Str }"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.External.02") {
    val rest = Type.Var(0, Kind.Star, Rigidity.Rigid)
    rest.setText("theRest")
    val tpe = Type.mkRecordExtend("x", Type.Int32, rest)

    val expected = "{ x: Int32 | theRest }"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.External.03") {
    val paramType = Type.Var(0, Kind.Star, Rigidity.Rigid)
    paramType.setText("t1")
    val tpe = Type.mkArrow(paramType, Type.Pure, paramType)

    val expected = "t1 -> t1"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.External.04") {
    val paramType = Type.Var(0, Kind.Star, Rigidity.Rigid)
    val returnType = Type.Var(1, Kind.Star, Rigidity.Rigid)
    val effectType = Type.Var(2, Kind.Effect, Rigidity.Rigid)
    val tpe = Type.mkArrow(paramType, effectType, returnType)

    val expected = "a -> b & (c)"
    val actual = FormatType2.format(tpe)(FormatType2.Audience.External)

    assert(actual == expected)
  }

  private def randomType(complexity: Int): Type = {

    def randomChoice[T](list: List[T]): T = {
      list(Random.nextInt(list.length))
    }

    def randomKind(complexity: Int): Kind = {
      if (complexity <= 0) {
        Kind.Star
      } else {
        val generators = List(
          () => Kind.Star,
          () => Kind.Record,
          () => Kind.Schema,
          () => Kind.Nat,
          () => Kind.Effect,
          () => Kind.Arrow(List(randomKind(complexity - 1)), randomKind(complexity - 1))
        )
        randomChoice(generators).apply()
      }
    }

    def randomRigidity(): Rigidity = {
      randomChoice(List(Rigidity.Rigid, Rigidity.Flexible))
    }

    def randomLower(): String = {
      randomChoice("abcdefghijklmnopqrstuvwxyz".toList.map(_.toString))
    }

    def randomUpper(): String = {
      randomChoice("ABCDEFGHIJKLMNOPQRSTUVWXYZ".toList.map(_.toString))
    }

    def randomEnumSym(): Symbol.EnumSym = {
      new Symbol.EnumSym(List(), randomUpper(), SourceLocation.Unknown)
    }

    def randomTypeConstructor(complexity: Int): TypeConstructor = {
      if (complexity <= 0) {
        TypeConstructor.Unit
      } else {
        val generators = List(
          () => TypeConstructor.Unit,
          () => TypeConstructor.Bool,
          () => TypeConstructor.Char,
          () => TypeConstructor.Float32,
          () => TypeConstructor.Float64,
          () => TypeConstructor.Int8,
          () => TypeConstructor.Int16,
          () => TypeConstructor.Int32,
          () => TypeConstructor.Int64,
          () => TypeConstructor.BigInt,
          () => TypeConstructor.Str,
          () => TypeConstructor.RecordEmpty,
          () => TypeConstructor.RecordExtend(randomLower()),
          () => TypeConstructor.SchemaEmpty,
          () => TypeConstructor.SchemaExtend(randomLower()),
          () => TypeConstructor.Array,
          () => TypeConstructor.Channel,
          () => TypeConstructor.Enum(randomEnumSym(), randomKind(complexity - 1)),
          () => TypeConstructor.Native(classOf[Object]),
          () => TypeConstructor.Ref,
          () => TypeConstructor.Tuple(Random.nextInt()),
          () => TypeConstructor.Vector,
          () => TypeConstructor.Relation,
          () => TypeConstructor.Lattice,
          () => TypeConstructor.Pure,
          () => TypeConstructor.Impure,
          () => TypeConstructor.Not,
          () => TypeConstructor.And,
          () => TypeConstructor.Or
        )
        randomChoice(generators).apply()
      }
    }

    def randomTypeVar(complexity: Int): Type.Var = {
      Type.Var(Random.nextInt(), randomKind(complexity - 1), randomRigidity())
    }

    if (complexity <= 0) {
      Type.Var(Random.nextInt(), randomKind(0), randomRigidity())
    } else {
      val generators = List(
        () => Type.Var(Random.nextInt(), randomKind(complexity - 1), randomRigidity()),
        () => Type.Cst(randomTypeConstructor(complexity - 1)),
        () => Type.Arrow(Random.nextInt(), randomType(complexity - 1)),
        () => Type.Succ(Random.nextInt(), randomType(complexity - 1)),
        () => Type.Lambda(randomTypeVar(complexity - 1), randomType(complexity - 1))
      )
      randomChoice(generators).apply()
    }

  }

  // MATT probably don't want to keep this, just for development/checking for holes
  test("FormatRandomType.01") {
    for (_ <- 0 to 10) {
      val tpe = randomType(100)
      System.err.println(FormatType2.format(tpe)(FormatType2.Audience.External))
    }
  }
}
