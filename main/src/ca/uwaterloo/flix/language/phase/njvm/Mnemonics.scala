package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.language.phase.jvm.{AsmOps, JvmType}
import org.objectweb.asm.{ClassWriter, MethodVisitor}
import org.objectweb.asm.Opcodes._

object Mnemonics {

  trait Stack

  trait StackNil extends Stack

  case class StackCons[+R <: Stack, +T](rest: R, top: T) extends Stack

  type **[R <: Stack, T] = StackCons[R, T]

  sealed trait JvmModifier
  object JvmModifier {
    case object Public extends JvmModifier
    case object Private extends JvmModifier
    case object Protected extends JvmModifier
    case object Static extends JvmModifier
    case object Final extends JvmModifier
    case object Super extends JvmModifier
    case object Synchronized extends JvmModifier
    case object Volatile extends JvmModifier
    case object Bridge extends JvmModifier
    case object VarArgs extends JvmModifier
    case object Transient extends JvmModifier
    case object Native extends JvmModifier
    case object Interface extends JvmModifier
    case object Abstract extends JvmModifier
    case object Strict extends JvmModifier
    case object Synthetic extends JvmModifier
    case object Annotation extends JvmModifier
    case object Enum extends JvmModifier
    case object Mandated extends JvmModifier
    case object Deprecated extends JvmModifier
  }

  def toInternal(modifier : JvmModifier): Int = modifier match {
    case JvmModifier.Public => ACC_PUBLIC
    case JvmModifier.Private => ACC_PRIVATE
    case JvmModifier.Protected => ACC_PROTECTED
    case JvmModifier.Static => ACC_STATIC
    case JvmModifier.Final => ACC_FINAL
    case JvmModifier.Super => ACC_SUPER
    case JvmModifier.Synchronized => ACC_SYNCHRONIZED
    case JvmModifier.Volatile => ACC_VOLATILE
    case JvmModifier.Bridge => ACC_BRIDGE
    case JvmModifier.VarArgs => ACC_VARARGS
    case JvmModifier.Transient => ACC_TRANSIENT
    case JvmModifier.Native => ACC_NATIVE
    case JvmModifier.Interface => ACC_INTERFACE
    case JvmModifier.Abstract => ACC_ABSTRACT
    case JvmModifier.Strict => ACC_STRICT
    case JvmModifier.Synthetic => ACC_SYNTHETIC
    case JvmModifier.Annotation => ACC_ANNOTATION
    case JvmModifier.Enum => ACC_ENUM
    case JvmModifier.Mandated => ACC_MANDATED
    case JvmModifier.Deprecated => ACC_DEPRECATED
  }

  class F[T](mv : MethodVisitor, ct : JvmType.Reference) {

    def load[S](localType: JvmType, location : Int): F[S] = {
      val iLoad = AsmOps.getLoadInstruction(localType)
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

    //END OF UNCHECKED METHODS

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

  //Capabilities
  /**
    * Capability which allows to load/store a local variable
    */
  sealed trait Local[T]{

    val localType : JvmType
    val location : Int

    def LOAD[R <: Stack]() : F[R] => F[R**T] =  t => t.load(localType, location)
    def STORE[R <: Stack]() : F[R**T] => F[R] =  t => t.load(localType, location)
  }

  class LocalC[T](val localType: JvmType, val location: Int) extends Local[T]

  /**
    * Capability which allows to get/put a field
    */
  sealed trait Field[T]{
    val fieldName : String
    val fieldType : JvmType

    def GET_FIELD[R <: Stack]() : F[R ** JvmType.Reference] => F[R ** T] =  t => t.getField(fieldName, fieldType)
    def PUT_FIELD[R <: Stack]() : F[R ** JvmType.Reference ** T] => F[R] =  t => t.putField(fieldName, fieldType)
  }

  class FieldC[T](val fieldName: String, val fieldType: JvmType) extends Field[T]

  type FunLocals0 = Local[JvmType.Reference]
  type FunLocals1[T1] = (Local[JvmType.Reference], Local[T1])
  type FunLocals2[T1, T2] = (Local[JvmType.Reference], Local[T1], Local[T2])

  def mkFunSig0[R](classType: JvmType, returnType : JvmType) : FunLocals0 =
    new LocalC[JvmType.Reference](classType, 0)

  def mkFunSig1[T1, R](classType: JvmType, arg1Type: JvmType ,returnType : JvmType) : FunLocals1[T1] = {

    ( new LocalC[JvmType.Reference](classType, 0),
      new LocalC[T1](arg1Type, 1))
  }

  def mkFunSig2[T1, T2, R](classType: JvmType, arg1Type: JvmType, arg2Type: JvmType ,returnType : JvmType) :
  FunLocals2[T1,T2] = {

    ( new LocalC[JvmType.Reference](classType, 0),
      new LocalC[T1](arg1Type, 1),
      new LocalC[T2](arg2Type, 2))
  }

  def mkField[T](fieldName: String, fieldType: JvmType) : Field[T] = {
    new FieldC[T](fieldName, fieldType)
  }


  class ClassGenerator(classType: JvmType.Reference) {

    private val cw: ClassWriter = AsmOps.mkClassWriter()

    def GenMethod(modifiers: List[JvmModifier], methodName: String,
                  argsType: List[JvmType], returnType: JvmType,
                  frameTransformer : F[StackNil] => F[StackNil]) : Unit = {

      val modifierVal = modifiers.map(modifier => toInternal(modifier)).sum

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
