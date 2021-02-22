package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.language.ast.ErasedAst.JType._
import ca.uwaterloo.flix.language.ast.ErasedAst.{ErasedType, Expression, JType}
import ca.uwaterloo.flix.language.ast.SourceLocation

object BytecodeCompiler {

  sealed trait Stack

  sealed trait StackNil extends Stack

  sealed case class StackCons[R <: Stack, T <: JType](rest: R, top: T) extends Stack

  type **[R <: Stack, T <: JType] = StackCons[R, T]

  sealed trait F[T]

  def compileExp[R <: Stack, T <: JType](exp: Expression[T]): F[R] => F[R ** T] = exp match {
    case Expression.Unit(loc) => WithSource[R](loc) ~ pushUnit()
    case Expression.Null(tpe, loc) => WithSource[R](loc) ~ pushNull()
    case Expression.True(loc) => WithSource[R](loc) ~ pushBool(true)
    case Expression.False(loc) => WithSource[R](loc) ~ pushBool(false)
    case Expression.Char(lit, loc) => WithSource[R](loc) ~ pushChar(lit)
    case Expression.Float32(lit, loc) => ???
    case Expression.Float64(lit, loc) => ???
    case Expression.Int8(lit, loc) => ???
    case Expression.Int16(lit, loc) => ???
    case Expression.Int32(lit, loc) => ???
    case Expression.Int64(lit, loc) => ???
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => branch(compileExp(exp1), compileExp(exp2), compileExp(exp3))
    case Expression.Ref(exp, tpe, loc) => ???
    //      NEW("class name") ~
    //      DUP ~
    //      compileExp(exp) ~
    //      INVOKESPECIAL("class name", "constructor signature")
    case Expression.Let(sym, exp1, exp2, tpe, loc) =>
      //      exp1.tpe match {
      //        case ErasedType.Int32() => WithSource[R](null) ~ compileExp(exp1) ~ popInt()
      //        case _ => ???
      //      }
      compileExp(exp2)
    case Expression.ArrayLoad(base, index, tpe, loc) =>
      WithSource[R](loc) ~
        compileExp(base) ~
        compileExp(index) ~
        ArrayInstruction.XALoad(tpe)
    case Expression.ArrayStore(base, index, elm, tpe, loc) =>
      WithSource[R](loc) ~
        compileExp(base) ~
        compileExp(index) ~
        compileExp(elm) ~
        ArrayInstruction.XAStore(elm.tpe)
    case _ => ???
  }

  def WithSource[R <: Stack](loc: SourceLocation): F[R] => F[R] = ???

  def pushUnit[R <: Stack](): F[R] => F[R ** JUnit] = ???

  def pushNull[R <: Stack](): F[R] => F[R ** JObject] = ???

  def pushBool[R <: Stack](b: Boolean): F[R] => F[R ** PrimInt32] = ???

  def pushInt[R <: Stack](n: Int): F[R] => F[R ** PrimInt32] = ???

  def pushChar[R <: Stack](c: Char): F[R] => F[R ** PrimChar] = ???

  object ArrayInstruction {
    def BALoad[R <: Stack](): F[R ** JArray[PrimInt8] ** PrimInt32] => F[R ** PrimInt8] = ???

    def SALoad[R <: Stack](): F[R ** JArray[PrimInt16] ** PrimInt32] => F[R ** PrimInt16] = ???

    def IALoad[R <: Stack](): F[R ** JArray[PrimInt32] ** PrimInt32] => F[R ** PrimInt32] = ???

    def LALoad[R <: Stack](): F[R ** JArray[PrimInt64] ** PrimInt32] => F[R ** PrimInt64] = ???

    def CALoad[R <: Stack](): F[R ** JArray[PrimChar] ** PrimInt32] => F[R ** PrimChar] = ???

    def FALoad[R <: Stack](): F[R ** JArray[PrimFloat32] ** PrimInt32] => F[R ** PrimFloat32] = ???

    def DALoad[R <: Stack](): F[R ** JArray[PrimFloat64] ** PrimInt32] => F[R ** PrimFloat64] = ???

    def AALoad[R <: Stack](): F[R ** JArray[JObject] ** PrimInt32] => F[R ** JObject] = ???

    def XALoad[R <: Stack, T <: JType](tpe: ErasedType[T]): F[R ** JArray[T] ** PrimInt32] => F[R ** T] = tpe match {
      case ErasedType.Unit() => ???
      case ErasedType.Bool() => ???
      case ErasedType.BoxedBool() => ???
      case ErasedType.Char() => CALoad()
      case ErasedType.BoxedChar() => ???
      case ErasedType.Float32() => ???
      case ErasedType.BoxedFloat32() => ???
      case ErasedType.Float64() => ???
      case ErasedType.BoxedFloat64() => ???
      case ErasedType.Int8() => BALoad()
      case ErasedType.BoxedInt8() => ???
      case ErasedType.Int16() => SALoad()
      case ErasedType.BoxedInt16() => ???
      case ErasedType.Int32() => IALoad()
      case ErasedType.BoxedInt32() => ???
      case ErasedType.Int64() => LALoad()
      case ErasedType.BoxedInt64() => ???
      case ErasedType.Array(tpe) => ???
      case ErasedType.Channel(tpe) => ???
      case ErasedType.Lazy(tpe) => ???
      case ErasedType.Ref(tpe) => ???
      case _: ErasedType.Str | _: ErasedType.BigInt | _: ErasedType.Tuple | _: ErasedType.Enum | _: ErasedType.Arrow |
           _: ErasedType.RecordEmpty | _: ErasedType.RecordExtend | _: ErasedType.SchemaEmpty |
           _: ErasedType.SchemaExtend | _: ErasedType.Relation | _: ErasedType.Native | _: ErasedType.Lattice =>
        AALoad()
      case ErasedType.Var(id) => ???
    }

    def BAStore[R <: Stack](): F[R ** JArray[PrimInt8] ** PrimInt32 ** PrimInt8] => F[R ** JUnit] = ???

    def SAStore[R <: Stack](): F[R ** JArray[PrimInt16] ** PrimInt32 ** PrimInt16] => F[R ** JUnit] = ???

    def IAStore[R <: Stack](): F[R ** JArray[PrimInt32] ** PrimInt32 ** PrimInt32] => F[R ** JUnit] = ???

    def LAStore[R <: Stack](): F[R ** JArray[PrimInt64] ** PrimInt32 ** PrimInt64] => F[R ** JUnit] = ???

    def CAStore[R <: Stack](): F[R ** JArray[PrimChar] ** PrimInt32 ** PrimChar] => F[R ** JUnit] = ???

    def FAStore[R <: Stack](): F[R ** JArray[PrimFloat32] ** PrimInt32 ** PrimFloat32] => F[R ** JUnit] = ???

    def DAStore[R <: Stack](): F[R ** JArray[PrimFloat64] ** PrimInt32 ** PrimFloat64] => F[R ** JUnit] = ???

    def AAStore[R <: Stack](): F[R ** JArray[JObject] ** PrimInt32 ** JObject] => F[R ** JUnit] = ???

    def XAStore[R <: Stack, T <: JType](tpe: ErasedType[T]): F[R ** JArray[T] ** PrimInt32 ** T] => F[R ** JUnit] = tpe match {
      case ErasedType.Unit() => ???
      case ErasedType.Bool() => ???
      case ErasedType.BoxedBool() => ???
      case ErasedType.Char() => CAStore()
      case ErasedType.BoxedChar() => ???
      case ErasedType.Float32() => ???
      case ErasedType.BoxedFloat32() => ???
      case ErasedType.Float64() => ???
      case ErasedType.BoxedFloat64() => ???
      case ErasedType.Int8() => BAStore()
      case ErasedType.BoxedInt8() => ???
      case ErasedType.Int16() => SAStore()
      case ErasedType.BoxedInt16() => ???
      case ErasedType.Int32() => IAStore()
      case ErasedType.BoxedInt32() => ???
      case ErasedType.Int64() => LAStore()
      case ErasedType.BoxedInt64() => ???
      case ErasedType.Array(tpe) => ???
      case ErasedType.Channel(tpe) => ???
      case ErasedType.Lazy(tpe) => ???
      case ErasedType.Ref(tpe) => ???
      case _: ErasedType.Str | _: ErasedType.BigInt | _: ErasedType.Tuple | _: ErasedType.Enum | _: ErasedType.Arrow |
           _: ErasedType.RecordEmpty | _: ErasedType.RecordExtend | _: ErasedType.SchemaEmpty |
           _: ErasedType.SchemaExtend | _: ErasedType.Relation | _: ErasedType.Native | _: ErasedType.Lattice =>
        AAStore()
      case ErasedType.Var(id) => ???
    }
  }

  def popInt[R <: Stack](): F[R ** PrimInt32] => F[R] = ???

  implicit class ComposeOps[A, B](f: F[A] => F[B]) {
    def ~[C](that: F[B] => F[C]): F[A] => F[C] = ???
  }

  def compose[A <: Stack, B <: Stack, C <: Stack](f: F[A] => F[B], g: F[B] => F[C]): F[A] => F[C] = ???

  def branch[R1 <: Stack, R2 <: Stack](cond: F[R1] => F[R1 ** PrimInt32], thenBranch: F[R1] => F[R2], elseBranch: F[R1] => F[R2]): F[R1] => F[R2] = ???

  def makeRef[R <: Stack, T <: JType](): F[R ** T] => F[R ** JRef[T]] = ???
}
