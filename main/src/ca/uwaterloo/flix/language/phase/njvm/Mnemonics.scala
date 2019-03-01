package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.language.phase.jvm.JvmType

object Mnemonics {

  // TODO

  trait Stack

  trait StackNil extends Stack

  case class StackCons[+R <: Stack, +T](rest: R, top: T) extends Stack

  type **[A, B] = (A, B)

  trait F[T] {

  }

  def bipush[R <: Stack](value: Byte): F[R] => F[R ** Int] = ???

  def fakepush[R <: Stack]: F[R] => F[R ** JvmType.Reference] = ???

  def areturn[R <: Stack]: F[R ** JvmType.Reference] => F[R] = ???

  def combine[A <: Stack, B <: Stack, C <: Stack](t1: F[A] => F[B], t2: F[B] => F[C]): F[A] => F[C] = ???

  //val foo = combine(fakepush, areturn)

}
