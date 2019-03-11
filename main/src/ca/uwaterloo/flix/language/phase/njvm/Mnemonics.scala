package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.language.phase.jvm.JvmType

object Mnemonics {

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

  implicit class ComposeOps[A <: Stack, B <: Stack](f: F[A] => F[B]) {
    def |>>[C <: Stack](g: F[B] => F[C]): F[A] => F[C] = (x: F[A]) => g(f(x))
  }

  object Instructions {

    /**
      * Polymorphic UNCHECKED return.
      */
    def UNCHECKED_RETURN[A](t: JvmType): F[StackNil ** A] => F[Nothing] = ???

    /**
      * Pushes the value of `this` onto the stack.
      */
    def THIS[R <: Stack]: F[R] => F[R ** JvmType.Reference] = ???

    /**
      * Returns without a value.
      */
    def RETURN: F[StackNil] => F[Nothing] = ???

    /**
      * Returns an object reference.
      */
    def ARETURN: F[StackNil ** JvmType.Reference] => F[Nothing] = t => t.areturn()

    /**
      * Returns a primitive float.
      */
    def FRETURN: F[StackNil ** JvmType.PrimFloat.type] => F[Nothing] = ???

    /**
      * Returns a primitive double.
      */
    def DRETURN: F[StackNil ** JvmType.PrimDouble.type] => F[Nothing] = ???

    /**
      * Pushes the result of adding the two top-most ints.
      */
    def IADD[R <: Stack]: F[R ** JvmType.PrimInt.type ** JvmType.PrimInt.type] => F[R ** JvmType.PrimInt.type] = ???

  }

}
