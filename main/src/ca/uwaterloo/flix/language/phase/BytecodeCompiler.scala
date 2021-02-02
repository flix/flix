package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.ErasedAst.JType._
import ca.uwaterloo.flix.language.ast.ErasedAst.{Expression, JType}

object BytecodeCompiler {

  sealed trait Stack
  sealed trait StackNil extends Stack
  sealed case class StackCons[+R <: Stack, +T <: JType](rest: R, top: T) extends Stack

  type **[R <: Stack, T <: JType] = StackCons[R, T]
  sealed trait F[+T]

  def compileExp[R <: Stack, T <: JType](exp : Expression[T]): F[R] => F[R ** T]  = exp match {
    case Expression.Unit(loc) => pushUnit()
    case Expression.Null(tpe, loc) => pushNull()
    case Expression.True(loc) => pushBool(true)
    case Expression.False(loc) => pushBool(false)
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
    case _ => ???
  }

  def pushUnit[R <: Stack](): F[R] => F[R ** JObject] = ???

  def pushNull[R <: Stack](): F[R] => F[R ** JObject] = ???

  def pushBool[R <: Stack](b : Boolean): F[R] => F[R ** PrimInt32] = ???

  def pushInt[R <: Stack](n : Int): F[R] => F[R ** PrimInt32] = ???

  def compose[A, B, C](f: F[A] => F[B], g: F[B] => F[C]): F[A] => F[C] = ???

  def branch[R1 <: Stack, R2 <: Stack](cond: F[R1] => F[R1 ** PrimInt32], thenBranch: F[R1] => F[R2], elseBranch: F[R1] => F[R2]): F[R1] => F[R2] = ???

  def makeRef[R <: Stack, T <: JType](): F[R ** T] => F[R ** JRef[T]] = ???
}
