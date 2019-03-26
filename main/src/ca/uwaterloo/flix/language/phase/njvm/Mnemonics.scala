package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.phase.jvm.{AsmOps, JvmName, JvmType}

import scala.reflect.runtime.universe._
import org.objectweb.asm.{ClassWriter, MethodVisitor}
import org.objectweb.asm.Opcodes._

object Mnemonics {

  trait Stack

  trait StackNil extends Stack

  case class StackCons[+R <: Stack, +T](rest: R, top: T) extends Stack

  type **[R <: Stack, T] = StackCons[R, T]

  sealed trait JvmModifier {
    def toInternal: Int = this match {
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

      case JvmModifier.InvokeSpecial => INVOKESPECIAL
    }
  }

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


    case object InvokeSpecial extends JvmModifier

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

    def invoke[S](invokeCode : JvmModifier, className : String, methodName: String,
                  args : List[JvmType], returnType : JvmType): F[S] = {

      mv.visitMethodInsn(invokeCode.toInternal, className, methodName, AsmOps.getMethodDescriptor(args, returnType), false)
      this.asInstanceOf[F[S]]
    }

    def _return[S](fieldType : JvmType): F[S] = {
      val ret = AsmOps.getReturnInstruction(fieldType)
      mv.visitInsn(ret)
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

  def getJvmType[T : TypeTag]: JvmType = typeOf[T] match {
    case t if t =:= typeOf[JvmType.Void.type] => JvmType.Void
    case t if t =:= typeOf[JvmType.PrimBool.type] => JvmType.PrimBool
    case t if t =:= typeOf[JvmType.PrimChar.type] => JvmType.PrimChar
    case t if t =:= typeOf[JvmType.PrimByte.type] => JvmType.PrimByte
    case t if t =:= typeOf[JvmType.PrimShort.type] => JvmType.PrimShort
    case t if t =:= typeOf[JvmType.PrimInt.type] => JvmType.PrimInt
    case t if t =:= typeOf[JvmType.PrimLong.type] => JvmType.PrimLong
    case t if t =:= typeOf[JvmType.PrimFloat.type] => JvmType.PrimFloat
    case t if t =:= typeOf[JvmType.PrimDouble.type] => JvmType.PrimDouble
    case t if t =:= typeOf[JvmType.Reference] => JvmType.Object
  }


  object Instructions {


    /**
      * Polymorphic return.
      */

    def RETURN[T : TypeTag]: F[StackNil ** T] => F[StackNil] = {

      val jvmType = getJvmType[T]

      t => t._return(jvmType)
    }
    /**
      * Returns without a value.
      */
    def RETURN: F[StackNil] => F[StackNil] = t => t._return()

    /**
      * Pushes the result of adding the two top-most ints.
      */
    def IADD[R <: Stack]: F[R ** JvmType.PrimInt.type ** JvmType.PrimInt.type] => F[R ** JvmType.PrimInt.type] = ???


  }

  //Capabilities
  /**
    * Capability which allows to load/store a local variable
    */
   class Local[T : TypeTag](location: Int){

    private val localType = getJvmType[T]

    def LOAD [R <: Stack]: F[R] => F[R**T] =  t => t.load(localType, location)
    def STORE[R <: Stack]: F[R**T] => F[R] =  t => t.load(localType, location)
  }


  /**
    * Capability which allows to get/put a field
    */
  class Field[T : TypeTag](fieldName: String){

    private val fieldType = getJvmType[T]

    def GET_FIELD[R <: Stack]: F[R ** JvmType.Reference] => F[R ** T] =  t => t.getField(fieldName, fieldType)
    def PUT_FIELD[R <: Stack]: F[R ** JvmType.Reference ** T] => F[R] =  t => t.putField(fieldName, fieldType)
  }



  abstract class MethodHandler{
    def INVOKE[R <: Stack]: F[R ** JvmType.Reference] => F[R]  =
      t => t.invoke(JvmModifier.InvokeSpecial, JvmName.Object.toInternalName, "<init>" ,Nil, JvmType.Void)

  }
  class MethodHandler0[R : TypeTag] () extends MethodHandler{}
  class MethodHandler1[T1: TypeTag, R : TypeTag] () extends MethodHandler{}


  abstract class FunSig[R : TypeTag](cw: ClassWriter, ct : JvmType.Reference, modifiers : List[JvmModifier], methodName : String) {


    def getModifiers : List[JvmModifier] = modifiers

    def getMethodName : String = methodName

    def getArgsType : List[JvmType]

    def getReturnType : JvmType = getJvmType[R]

    def genMethod(ft : F[StackNil] => F[StackNil ** R]) : MethodHandler
    def genVoidMethod(ft : F[StackNil] => F[StackNil]) : MethodHandler

    protected def genMethodWithoutHandler(ft : F[StackNil] => F[StackNil ** R]) : Unit = {

      genMethodCommon(ft |>> Instructions.RETURN[R])
    }

    protected def genVoidMethodWithoutHandler(ft : F[StackNil] => F[StackNil]) : Unit = {
      genMethodCommon(ft |>>  Instructions.RETURN)
    }

    private def genMethodCommon(ft : F[StackNil] => F[StackNil]) : Unit = {
      val modifierVal = getModifiers.map(modifier => modifier.toInternal).sum

      val mv = cw.visitMethod(modifierVal, getMethodName,
      AsmOps.getMethodDescriptor(getArgsType, getReturnType), null, null)
      mv.visitCode()

      ft(new F[StackNil](mv, ct))

      mv.visitMaxs(1,1)
      mv.visitEnd()
    }


  }

  class FunSig0[R : TypeTag] (cw: ClassWriter, ct : JvmType.Reference, modifiers : List[JvmModifier], methodName : String)
    extends FunSig[R](cw, ct, modifiers, methodName) {


    def getArg0 : Local[JvmType.Reference] = new Local[JvmType.Reference](0)

    override def getArgsType: List[JvmType] = List()

    override def genMethod(ft: F[StackNil] => F[StackNil ** R]): MethodHandler0[R] = {
      genMethodWithoutHandler(ft)
      new MethodHandler0[R]
    }

    override def genVoidMethod(ft: F[StackNil] => F[StackNil]): MethodHandler0[R] = {
      genVoidMethodWithoutHandler(ft)
      new MethodHandler0[R]
    }
  }

  class FunSig1[T1 : TypeTag, R : TypeTag] (cw: ClassWriter, ct : JvmType.Reference, modifiers : List[JvmModifier], methodName : String)
    extends FunSig[R](cw, ct, modifiers, methodName) {

    def getArg0 : Local[JvmType.Reference] = new Local[JvmType.Reference](0)

    def getArg1 : Local[T1] = new Local[T1](1)

    override def getArgsType: List[JvmType] = List(getJvmType[T1])

    override def genMethod(ft: F[StackNil] => F[StackNil ** R]) : MethodHandler1[T1, R] = {
      genMethodWithoutHandler(ft)
      new MethodHandler1[T1, R]
    }
    override def genVoidMethod(ft: F[StackNil] => F[StackNil]): MethodHandler1[T1, R] = {
      genVoidMethodWithoutHandler(ft)
      new MethodHandler1[T1, R]
    }
  }



  class ClassGenerator(classType: JvmType.Reference,
                       modifiers : List[JvmModifier], superClass : String, implementedInterfaces : Array[String])
                      (implicit flix: Flix) {


    private val cw: ClassWriter = {
      val cw = AsmOps.mkClassWriter()
      val modifierVal = modifiers.map(modifier => modifier.toInternal).sum
      cw.visit(AsmOps.JavaVersion, modifierVal, classType.name.toInternalName, null,
        superClass, implementedInterfaces)
      cw
    }


    def mkFunSig0[R : TypeTag](modifiers : List[JvmModifier], methodName : String)  : FunSig0[R] = {
      new FunSig0[R](cw, classType, modifiers, methodName)
    }

    def mkFunSig1[T1 : TypeTag, R : TypeTag](modifiers : List[JvmModifier], methodName : String)  : FunSig1[T1, R] = {
      new FunSig1[T1, R](cw, classType, modifiers, methodName)
    }

    def compileField[T : TypeTag](modifiers: List[JvmModifier] ,fieldName: String) : Field[T] = {

      val fieldType = getJvmType[T]
      val modifierVal = modifiers.map(modifier => modifier.toInternal).sum
      val field = cw.visitField(modifierVal, fieldName, fieldType.toDescriptor, null, null)
      field.visitEnd()

      new Field[T](fieldName)
    }

    def compile(): Array[Byte] = {
      cw.visitEnd()
      cw.toByteArray
    }
  }

}
