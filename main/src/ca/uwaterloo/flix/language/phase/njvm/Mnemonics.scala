package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.language.phase.jvm.JvmType

object Mnemonics {

  // TODO

  trait Stack

  trait StackNil extends Stack

  case class StackCons[+R <: Stack, +T](rest: R, top: T) extends Stack

  type **[R <: Stack, T] = StackCons[R, T]

  class F[T] {

    def compile(): Array[Byte] = ???

  }

  def bipush[R <: Stack](value: Byte): F[R] => F[R ** Int] = ???

  def iadd[R <: Stack](): F[R ** Int ** Int] => F[R ** Int] = ???

  def fakepush[R <: Stack](): F[R] => F[R ** Int] = ???

  def areturn[R <: Stack]: F[R ** JvmType.Reference] => F[R] = ???

  //
  // A = ?
  // B = R1 ** Int
  //

  implicit class ComposeOps[A <: Stack, B <: Stack](f: F[A] => F[B]) {
    def |>>[C <: Stack](g: F[B] => F[C]): F[A] => F[C] = (x: F[A]) => g(f(x))
  }

  def |>[A <: Stack, B <: Stack, C <: Stack](t1: F[A] => F[B], t2: F[B] => F[C]): F[A] => F[C] = (x: F[A]) => t2.apply(t1.apply(x))

  val f = fakepush().apply(new F[StackNil])
  val g = fakepush().apply(f)

  def h[R <: Stack](): F[R] => F[R ** Int ** Int] = |>(fakepush(), fakepush())

  def w[R <: Stack](): F[R] => F[R ** Int ** Int] = fakepush() |>> fakepush()

  //val r: F[StackNil ** JvmType.Reference ** JvmType.Reference] = f.apply(new F[StackNil])


}
