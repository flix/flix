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
    case Expression.Unit(loc) => pushUnit[R]()
    case Expression.Null(tpe, loc) => pushNull[R]()
    case Expression.True(loc) => pushBool[R](true)
    case Expression.False(loc) => pushBool[R](false)
    case Expression.Char(lit, loc) => ???
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
    case _ => ???
  }

  def WithSource[R <: Stack](loc: SourceLocation): F[R] => F[R] = ???

  def pushUnit[R <: Stack](): F[R] => F[R ** JUnit] = ???

  def pushNull[R <: Stack](): F[R] => F[R ** JObject] = ???

  def pushBool[R <: Stack](b: Boolean): F[R] => F[R ** PrimInt32] = ???

  def pushInt[R <: Stack](n: Int): F[R] => F[R ** PrimInt32] = ???

  def popInt[R <: Stack](): F[R ** PrimInt32] => F[R] = ???

  implicit class ComposeOps[A, B](f: F[A] => F[B]){
    def ~[C](that: F[B] => F[C]): F[A] => F[C] = ???
  }

  def compose[A <: Stack, B <: Stack, C <: Stack](f: F[A] => F[B], g: F[B] => F[C]): F[A] => F[C] = ???

  def branch[R1 <: Stack, R2 <: Stack](cond: F[R1] => F[R1 ** PrimInt32], thenBranch: F[R1] => F[R2], elseBranch: F[R1] => F[R2]): F[R1] => F[R2] = ???

  def makeRef[R <: Stack, T <: JType](): F[R ** T] => F[R ** JRef[T]] = ???
}
