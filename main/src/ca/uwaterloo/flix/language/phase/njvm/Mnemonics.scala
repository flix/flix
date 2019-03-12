package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.language.phase.jvm.{AsmOps, JvmType}
import org.objectweb.asm.{ClassWriter, MethodVisitor}
import org.objectweb.asm.Opcodes._
import scala.reflect.runtime.universe.{typeOf, TypeTag}

object Mnemonics {

  trait Stack

  trait StackNil extends Stack

  case class StackCons[+R <: Stack, +T](rest: R, top: T) extends Stack

  type **[R <: Stack, T] = StackCons[R, T]

  object JvmModifier extends Enumeration {
    type JvmModifier = Int
    val PUBLIC : Int = ACC_PUBLIC
    val PRIVATE : Int = ACC_PRIVATE
    val PROTECTED : Int = ACC_PROTECTED
    val STATIC : Int = ACC_STATIC
    val FINAL : Int = ACC_FINAL
    val SUPER : Int = ACC_SUPER
    val SYNCHRONIZED : Int = ACC_SYNCHRONIZED
    val VOLATILE : Int = ACC_VOLATILE
    val BRIDGE : Int = ACC_BRIDGE
    val VARARGS : Int = ACC_VARARGS
    val TRANSIENT : Int = ACC_TRANSIENT
    val NATIVE : Int = ACC_NATIVE
    val INTERFACE : Int = ACC_INTERFACE
    val ABSTRACT : Int = ACC_ABSTRACT
    val STRICT : Int= ACC_STRICT
    val SYNTHETIC : Int= ACC_SYNTHETIC
    val ANNOTATION : Int= ACC_ANNOTATION
    val ENUM : Int= ACC_ENUM
    val MANDATED : Int = ACC_MANDATED
    val DEPRECATED : Int = ACC_DEPRECATED
  }

  import JvmModifier._

  class F[T](mv : MethodVisitor, ct : JvmType.Reference) {

    def load[S](fieldType: JvmType, location : Int): F[S] = {
      val iLoad = AsmOps.getLoadInstruction(fieldType)
      mv.visitVarInsn(iLoad, location)
      this.asInstanceOf[F[S]]
    }

    def getField[S](fieldName: String, fieldType : JvmType): F[S] = {
      mv.visitFieldInsn(GETFIELD, ct.name.toInternalName, fieldName, fieldType.toDescriptor)
      this.asInstanceOf[F[S]]
    }

    def putField[S](fieldName: String, fieldType : JvmType): F[S] = {
      mv.visitFieldInsn(PUTFIELD, ct.name.toInternalName, fieldName, fieldType.toDescriptor)
      this.asInstanceOf[F[S]]
    }

    def _return[S](fieldType : JvmType): F[S] = {
      val iRet = AsmOps.getReturnInstruction(fieldType)
      mv.visitInsn(ARETURN)
      this.asInstanceOf[F[S]]
    }

    def _return[S](): F[S] = {
      mv.visitInsn(RETURN)
      this.asInstanceOf[F[S]]
    }
  }

  implicit class ComposeOps[A <: Stack, B <: Stack](f: F[A] => F[B]) {
    def |>>[C <: Stack](g: F[B] => F[C]): F[A] => F[C] = (x: F[A]) => g(f(x))
  }

  object Instructions {


    //BEGIN OF UNCHECKED METHODS
    /**
      * Polymorphic UNCHECKED return.
      */
    def UNCHECKED_RETURN[T](t: JvmType): F[StackNil ** T] => F[StackNil] = ???

    /**
      * Pushes onto the stack field UNCHECKED.
      */
    def UNCHECKED_GETFIELD[R <: Stack, T :TypeTag](fieldName: String, fieldType: JvmType): F[R ** JvmType.Reference] => F[R ** T] = {
      println(typeOf[T].typeSymbol.name.toString)
      t => t.getField(fieldName, fieldType)}

    /**
      * Pops the topmost element from the stack and stores the value in the proper field UNCHECKED.
      */
    def UNCHECKED_PUTFIELD[R <: Stack, T](fieldName: String, fieldType: JvmType): F[R ** JvmType.Reference ** T] => F[R] =
      t => t.putField(fieldName, fieldType)
    /**
      * Pops the topmost element from the stack and stores the value in the proper field UNCHECKED.
      */
    def UNCHECKED_LOAD[R <: Stack, T](fieldType: JvmType, location: Int): F[R] => F[R ** T] =
      t => t.load(fieldType, location)

    //END OF UNCHECKED METHODS


    /**
      * Pushes the value of `this` onto the stack.
      */
    def THIS[R <: Stack]: F[R] => F[R ** JvmType.Reference] =  t => t.load(JvmType.Object, 0)

    /**
      * Returns without a value.
      */
    def RETURN: F[StackNil] => F[StackNil] = t => t._return()

    /**
      * Returns an object reference.
      */
    def ARETURN: F[StackNil ** JvmType.Reference] => F[StackNil] = t => t._return(JvmType.Object)

    /**
      * Returns a primitive float.
      */
    def FRETURN: F[StackNil ** JvmType.PrimFloat.type] => F[StackNil] =  t => t._return(JvmType.PrimFloat)

    /**
      * Returns a primitive double.
      */
    def DRETURN: F[StackNil ** JvmType.PrimDouble.type] => F[StackNil] = t => t._return(JvmType.PrimDouble)

    /**
      * Pushes the result of adding the two top-most ints.
      */
    def IADD[R <: Stack]: F[R ** JvmType.PrimInt.type ** JvmType.PrimInt.type] => F[R ** JvmType.PrimInt.type] = ???

  }

  class ClassGenerator(classType: JvmType.Reference) {

    private val cw: ClassWriter = AsmOps.mkClassWriter()

    def GenMethod(modifiers: List[JvmModifier], methodName: String,
                  argsType: List[JvmType], returnType: JvmType,
                  frameTransformer : F[StackNil] => F[StackNil]) : Unit = {

      val modifierVal = modifiers.sum

      val mv = cw.visitMethod(modifierVal, methodName, AsmOps.getMethodDescriptor(argsType, returnType),
        null, null)
      mv.visitCode()
      frameTransformer(new F[StackNil](mv, classType))

      mv.visitMaxs(1,1)
      mv.visitEnd()

    }

    def compile(): Array[Byte] = {
      cw.visitEnd()
      cw.toByteArray
    }
  }

}
