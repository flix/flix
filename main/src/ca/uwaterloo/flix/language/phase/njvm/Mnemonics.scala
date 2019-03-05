package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.language.phase.jvm.JvmType

object Mnemonics {

  // TODO

  trait Stack

  trait StackNil extends Stack

  case class StackCons[+R <: Stack, +T](rest: R, top: T) extends Stack

  type **[R <: Stack, T] = StackCons[R, T]

  class F[T] {

    import org.objectweb.asm.MethodVisitor
    import org.objectweb.asm.Opcodes

    private val mv: MethodVisitor = ???

    def compile(): Array[Byte] = ???

    def areturn[S](): F[S] = {
      mv.visitInsn(Opcodes.ARETURN)
      this.asInstanceOf[F[S]]
    }

  }


  def bipush[R <: Stack](value: Byte): F[R] => F[R ** Int] = ???

  def iadd[R <: Stack](): F[R ** Int ** Int] => F[R ** Int] = ???

  def fakepush[R <: Stack](): F[R] => F[R ** Int] = ???

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

  object Instructions {

    /**
      * Polymorphic UNCHECKED return.
      */
    def UNCHECKED_RETURN[R <: Stack, A](t: JvmType): F[R ** A] => F[R] = ???

    /**
      * Returns an object reference.
      */
    def ARETURN[R <: Stack]: F[R ** JvmType.Reference] => F[R] = t => t.areturn()

    /**
      * Returns a primitive float.
      */
    def FRETURN[R <: Stack]: F[R ** JvmType.PrimFloat.type] => F[R] = ???

    /**
      * Returns a primitive double.
      */
    def DRETURN[R <: Stack]: F[R ** JvmType.PrimDouble.type] => F[R] = ???

    /**
      * Pushes the result of adding the two top-most ints.
      */
    def IADD[R <: Stack]: F[R ** JvmType.PrimInt.type ** JvmType.PrimInt.type] => F[R ** JvmType.PrimInt.type] = ???

  }

}
