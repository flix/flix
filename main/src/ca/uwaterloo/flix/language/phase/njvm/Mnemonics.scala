/*
 * Copyright 2019 Miguel Fialho
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.ast.{MonoType, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.JvmOps.{getJvmType, getRecordInterfaceType, getRefClassType, getTupleInterfaceType, stringify, _}
import ca.uwaterloo.flix.language.phase.jvm._
import ca.uwaterloo.flix.language.phase.njvm.Api.Java
import ca.uwaterloo.flix.language.phase.njvm.Api.Java.Lang.{Object, _}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.JvmModifier._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.reflect.runtime.universe._
import org.objectweb.asm.{ClassWriter, Label, MethodVisitor}
import org.objectweb.asm.Opcodes._
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._
import ca.uwaterloo.flix.language.phase.njvm.classes._
import ca.uwaterloo.flix.language.phase.njvm.interfaces._

object Mnemonics {

  // TODO: Miguel: Add some comments for these very critical components :)

  trait Stack

  trait StackNil extends Stack

  case class StackCons[+R <: Stack, +T](rest: R, top: T) extends Stack

  type **[R <: Stack, T] = StackCons[R, T]

  /**
    * Enum describing all the possible JvmModifier.
    * It includes methods, fields, interfaces, Invoker for functions etc
    *
    */
  //TODO: DO better partition of the modifiers and better naming as this one isn't 100% correct
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
      case JvmModifier.InvokeVirtual => INVOKEVIRTUAL
      case JvmModifier.InvokeInterface => INVOKEINTERFACE
      case JvmModifier.InvokeStatic => INVOKESTATIC
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

    case object InvokeVirtual extends JvmModifier

    case object InvokeInterface extends JvmModifier

    case object InvokeStatic extends JvmModifier

  }

  /**
    * F class as in Mnemonics
    * It simply serves as way to interface with the underlying method of generating Java bytecode
    * in this case we use the org.web.asm library
    *
    * It contains methods in an ad-hoc manner. The class and its underlying methods don't serve to enforce the
    * type safety because of this all of them simply cast in the to correct stack type
    */
  class F[T](mv: MethodVisitor, ct: NJvmType.Reference) {

    /**
      * Emits the correct Jvm load instruction give the localType
      *
      * @param localType the jvmType of the local we want to load
      * @param location  the of the local
      * @pre This method should only be called if local in the specified location matches the jvmType,
      *      to avoid verifier errors
      */
    def emitLoad[S](localType: NJvmType, location: Int): F[S] = {
      val loadIns = getLoadInstruction(localType)
      mv.visitVarInsn(loadIns, location)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a getField instruction given the fieldName and it's type
      *
      * @param fieldName the of the field
      * @param fieldType the jvmType of the field we want to get
      * @pre This method should only be called if field with the specified fieldName matches the jvmType,
      *      to avoid verifier errors
      */
    def emitGetField[S](fieldName: String, fieldType: NJvmType): F[S] = {
      mv.visitFieldInsn(GETFIELD, ct.name.toInternalName, fieldName, fieldType.toDescriptor)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a putField instruction given the fieldName and it's type
      *
      * @param fieldName the of the field
      * @param fieldType the jvmType of the field we want to get
      * @pre This method should only be called if field with the specified fieldName matches the jvmType,
      *      to avoid verifier errors
      */
    def emitPutField[S](fieldName: String, fieldType: NJvmType): F[S] = {
      mv.visitFieldInsn(PUTFIELD, ct.name.toInternalName, fieldName, fieldType.toDescriptor)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a invoke instruction given the invokeCode, className, methodName, args and returnType
      *
      * @param invokeCode of the method we want to call the List of possible values are
      *                   InvokeSpecial, InvokeVirtual, InvokeStatic, InvokeInterface
      * @param className  name of the class which contains the method we pretend to invoke
      * @param methodName name of the method we pretend to invoke
      * @param args       List of the JvmType of the args to call the method
      * @param returnType the JvmType of the return type of the calling method
      * @pre: There are a few conditions which need to be ensured in order for this method to not generate
      *       verifier errors. The invokeCode should match the type of method which is being called i.e. if it's an
      *       interface method invokecode should be InvokeInterface, if its a virtual function invokeCode should be
      *       InvokeVirtual and so on. The args and returnType should match the description of the method in the .class
      *       file which can be found by using the classname and methodname. The method should also obviously exist
      *
      */
    def emitInvoke[S](invokeCode: JvmModifier, className: String, methodName: String,
                      args: List[NJvmType], returnType: NJvmType): F[S] = {

      //Last argument is true if invoking interface
      val flag = if (invokeCode == JvmModifier.InvokeInterface) true else false
      mv.visitMethodInsn(invokeCode.toInternal, className, methodName, getMethodDescriptor(args, returnType), flag)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a return instruction given the fieldType
      *
      * @param jt the jvmType of what we want to return.
      * @pre This method should only be called if jt matches the jvmType of what is currently on the
      *      Top of stack, jt should also be differnt from JvmType.Void.
      *      Finally jt should also be the same type as the return type of the function we want to return from,
      *      to avoid verifier erros
      */
    def emitReturn[S](jt: NJvmType): F[S] = {
      val ret = getReturnInstruction(jt)
      mv.visitInsn(ret)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a return instruction which returns nothing (or void)
      *
      * @pre This method should only be called if the return type of the function we want to return from is Void
      */
    def emitReturnVoid[S](): F[S] = {
      mv.visitInsn(RETURN)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a 'new' instruction which creates a new instance of the provided JvmType.Reference
      *
      */
    def emitNew[S](jt: NJvmType.Reference): F[S] = {
      mv.visitTypeInsn(NEW, jt.name.toInternalName)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a 'dup'  which duplicates whatever is on top of stack
      *
      */
    def emitDup[S](): F[S] = {
      mv.visitInsn(DUP)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a Ldc instruction which loads a constant onto the top of the stack
      *
      */
    def emitLdc[S](value: Object): F[S] = {
      mv.visitLdcInsn(value)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a throw instruction which is used to throw an exception
      *
      * @pre To avoid verifier errors there should be an instance of an exception on Top of the stack
      *
      */
    def emitThrow[S](): F[S] = {
      mv.visitInsn(ATHROW)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a ifeq instruction which jumps to the provided label if the value of top of the stack is a bool
      * which it's value is true
      *
      * @pre To avoid verifier errors there should be a bool on top of the stack and the label should exist
      *
      */
    def emitIfeq[S](label: Label): F[S] = {
      mv.visitJumpInsn(IFEQ, label)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a goto instruction which jumps to the provided label unconditionally
      *
      * @pre To avoid verifier errors the label should exist
      *
      */
    def emitGoto[S](label: Label): F[S] = {
      mv.visitJumpInsn(GOTO, label)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits the provided label
      *
      * @pre To avoid verifier errors the label shouldn't exist
      *
      */
    def emitLabel[S](label: Label): F[S] = {
      mv.visitLabel(label)
      this.asInstanceOf[F[S]]
    }

    def emitCheckCast[S](jt: NJvmType.Reference): F[S] = {
      mv.visitTypeInsn(CHECKCAST, jt.name.toInternalName)
      this.asInstanceOf[F[S]]
    }
  }

  /**
    * This class and it's inner function |>> is used to provide a nice way to append a series of
    *  Mnemonics.Instructions. It is essentially syntatic sugar for function composition
    */
  implicit class ComposeOps[A <: Stack, B <: Stack](f: F[A] => F[B]) {
    def |>>[C <: Stack](g: F[B] => F[C]): F[A] => F[C] = (x: F[A]) => g(f(x))
  }

  trait MnemonicsTypes

  trait MObject
  trait MString extends MObject

  object MnemonicsTypes {
    case class MVoid() extends MnemonicsTypes

    case class MBool() extends MnemonicsTypes
    case class MChar() extends MnemonicsTypes
    case class MByte() extends MnemonicsTypes

    case class MShort() extends MnemonicsTypes
    case class MInt() extends MnemonicsTypes
    case class MLong() extends MnemonicsTypes
    case class MFloat() extends MnemonicsTypes
    case class MDouble() extends MnemonicsTypes
    class Ref[+ T <: MObject] extends MnemonicsTypes
  }

  /**
    * This function allows to create Runtime values by using a compile time value.
    * It takes a type parameter T and maps it to the correct runtime JvmType value
    *
    * This is used to circumvent the use of dependent types which are not supported by scala.
    * It allows us to have a framework which more type-safe compared to a version which doesn't use this features
    */
  def getJvmType[T <: MnemonicsTypes : TypeTag](implicit root: Root, flix: Flix): NJvmType = typeOf[T] match {
//    case t if t =:= typeOf[Void] => NJvmType.Void
    case t if t =:= typeOf[MBool] => PrimBool
    case t if t =:= typeOf[MChar] => PrimChar
    case t if t =:= typeOf[MByte] => PrimByte
    case t if t =:= typeOf[MShort] => PrimShort
    case t if t =:= typeOf[MInt] => PrimInt
    case t if t =:= typeOf[MLong] => PrimLong
    case t if t =:= typeOf[MFloat] => PrimFloat
    case t if t =:= typeOf[MDouble] => PrimDouble

    case t if t =:= typeOf[Ref[MString]] => NJvmType.String
    case t if t =:= typeOf[Ref[MObject]] => NJvmType.Object

    case t if t =:= typeOf[Ref[Context]] => NJvmType.Context

    case t if t =:= typeOf[Ref[RecordInterface]] =>
      val name = "IRecord"

      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))

    case t if t =:= typeOf[Ref[RecordEmpty]] =>
      val name = "RecordEmpty"

      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))

    case t if t =:= typeOf[Ref[RecordExtend[MBool]]] =>
      val name = "RecordExtend$" + stringify(PrimBool)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[RecordExtend[MChar]]] =>
      val name = "RecordExtend$" + stringify(PrimChar)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[RecordExtend[MByte]]] =>
      val name = "RecordExtend$" + stringify(PrimByte)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[RecordExtend[MShort]]] =>
      val name = "RecordExtend$" + stringify(PrimShort)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[RecordExtend[MInt]]] =>
      val name = "RecordExtend$" + stringify(PrimInt)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[RecordExtend[MLong]]] =>
      val name = "RecordExtend$" + stringify(PrimLong)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[RecordExtend[MFloat]]] =>
      val name = "RecordExtend$" + stringify(PrimFloat)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[RecordExtend[MDouble]]] =>
      val name = "RecordExtend$" + stringify(PrimDouble)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[RecordExtend[Ref[MObject]]]] =>
      val name = "RecordExtend$" + stringify(Reference(JvmName.Object))
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))

    case t if t =:= typeOf[Ref[RefClass[MBool]]] =>
      val name = "Ref$" + stringify(PrimBool)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[RefClass[MChar]]] =>
      val name = "Ref$" + stringify(PrimChar)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[RefClass[MByte]]] =>
      val name = "Ref$" + stringify(PrimByte)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[RefClass[MShort]]] =>
      val name = "Ref$" + stringify(PrimShort)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[RefClass[MInt]]] =>
      val name = "Ref$" + stringify(PrimInt)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[RefClass[MLong]]] =>
      val name = "Ref$" + stringify(PrimLong)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[RefClass[MFloat]]] =>
      val name = "Ref$" + stringify(PrimFloat)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[RefClass[MDouble]]] =>
      val name = "Ref$" + stringify(PrimDouble)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[RefClass[Ref[MObject]]]] =>
      val name = "Ref$" + stringify(Reference(JvmName.Object))
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))

    case t if t =:= typeOf[Ref[ContinuationInterface[MBool]]] =>
      val name = "Cont$" + stringify(PrimBool)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[ContinuationInterface[MChar]]] =>
      val name = "Cont$" + stringify(PrimChar)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[ContinuationInterface[MByte]]] =>
      val name = "Cont$" + stringify(PrimByte)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[ContinuationInterface[MShort]]] =>
      val name = "Cont$" + stringify(PrimShort)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[ContinuationInterface[MInt]]] =>
      val name = "Cont$" + stringify(PrimInt)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[ContinuationInterface[MLong]]] =>
      val name = "Cont$" + stringify(PrimLong)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[ContinuationInterface[MFloat]]] =>
      val name = "Cont$" + stringify(PrimFloat)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[ContinuationInterface[MDouble]]] =>
      val name = "Cont$" + stringify(PrimDouble)
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
    case t if t =:= typeOf[Ref[ContinuationInterface[Ref[MObject]]]] =>
      val name = "Cont$" + stringify(Reference(JvmName.Object))
      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))

    case _ => NJvmType.Object

  }

  /**
    * Singleton which contains the operations which compose the core of how we keep track of the JvmStack at compile.
    * Each of the functions in this object return a function which maps a stack state to another according to the
    * semantics of the Jvm instruction which it is trying to model
    */
  object Instructions {

    /**
      * Polymorphic return.
      */
    def RETURN[T <: MnemonicsTypes : TypeTag](implicit root: Root, flix: Flix): F[StackNil ** T] => F[StackNil] = {
      val jvmType = getJvmType[T]
      t => t.emitReturn(jvmType)
    }

    /**
      * Returns without a value.
      */
    def RETURN_VOID: F[StackNil] => F[StackNil] = t => t.emitReturnVoid()

    /**
      * Pushes the result of adding the two top-most ints.
      */
    def IADD[S <: Stack]: F[S ** MInt ** MInt] => F[S ** MInt] = ???

    /**
      * Creates a new instance of the type parameter T which should be a subtype of JvmType.Reference and puts
      * it on top of the stack
      */
    def NEW[S <: Stack](jt: Reference)(implicit root: Root, flix: Flix): F[S] => F[S ** Ref[MObject]] =
      t => t.emitNew(jt)

    /**
      * Duplicates whatever is on Top of the stack
      */
    def DUP[S <: Stack, T: TypeTag]: F[S ** T] => F[S ** T ** T] =
      t => t.emitDup()

    /**
      * Loads a constant string onto the stack
      */
    def LDC_STRING[S <: Stack](value: String): F[S] => F[S ** Ref[MString]] =
      t => t.emitLdc(value)

    /**
      * Throws the exception which is currently on top of the stack
      */
    def THROW: F[StackNil ** Ref[MObject]] => F[StackNil] =
      t => t.emitThrow()

    def IFEQ[S <: Stack](f: F[S] => F[S]): F[S ** MBool] => F[S] = {
      val skipLabel = new Label
      ((t: F[S ** MBool]) => t.emitIfeq[S](skipLabel)) |>>
        f |>>
        (t => t.emitLabel(skipLabel))
    }

    def CHECK_CAST[S <: Stack](jt: Reference): F[S ** Ref[MObject]] => F[S ** Ref[MObject]] =
      t => t.emitCheckCast(jt)

    def POP[S <: Stack, T](): F[S ** T] => F[S] = ???

  }

  /**
    * Capability which allows to load/store a local variable
    */
  class Local[T <: MnemonicsTypes : TypeTag](location: Integer)(implicit root: Root, flix: Flix) {

    private val localType = getJvmType[T]

    def LOAD[S <: Stack]: F[S] => F[S ** T] = t => t.emitLoad(localType, location)

    def STORE[S <: Stack]: F[S ** T] => F[S] = t => t.emitLoad(localType, location)
  }

  /**
    * Capability which allows to get/put a field
    */
  class Field[T <: MnemonicsTypes : TypeTag](fieldName: String)(implicit root: Root, flix: Flix) {

    private val fieldType = getJvmType[T]

    def GET_FIELD[S <: Stack, T1 <: Ref[MObject]]: F[S ** T1] => F[S ** T] = t => t.emitGetField(fieldName, fieldType)

    def PUT_FIELD[S <: Stack, T1 <: Ref[MObject]]: F[S ** T1 ** T] => F[S] = t => t.emitPutField(fieldName, fieldType)
  }

  /**
    * Capability which allows acess the function locals in this case a function with 0 arguments
    */
  //TODO: Maybe need to have different types for signatures of static method as the 1st local (0 index) of static
  //TODO: functions isn't a local of type JvmType.Reference
  class FunSig0[R<: MnemonicsTypes : TypeTag]() {
  }

  /**
    * Capability which allows acess the function locals in this case a function with 1 argument
    */
  //TODO: Same as FunSig0
  class FunSig1[T1<: MnemonicsTypes : TypeTag, R<: MnemonicsTypes : TypeTag] {

    def getArg1(implicit root: Root, flix: Flix): Local[T1] = new Local(0)
  }

  /**
    * Capability which allows acess the function locals in this case a function with 2 arguments
    */
  //TODO: Same as FunSig0
  class FunSig2[T1<: MnemonicsTypes : TypeTag, T2<: MnemonicsTypes : TypeTag, R<: MnemonicsTypes : TypeTag] {

    def getArg1(implicit root: Root, flix: Flix): Local[T1] = new Local(0)

    def getArg2(implicit root: Root, flix: Flix): Local[T2] = new Local(1)
  }

  /**
    * Capability which allows acess the function locals in this case a function with 3 arguments
    */
  //TODO: Same as FunSig0
  class FunSig3[T1<: MnemonicsTypes : TypeTag, T2<: MnemonicsTypes : TypeTag, T3<: MnemonicsTypes : TypeTag, R<: MnemonicsTypes : TypeTag] {

    def getArg1(implicit root: Root, flix: Flix): Local[T1] = new Local(0)

    def getArg2(implicit root: Root, flix: Flix): Local[T2] = new Local(1)

    def getArg3(implicit root: Root, flix: Flix): Local[T3] = new Local(2)

  }

  /**
    * Capability which allows acess the function locals in this case a function with 4 arguments
    */
  //TODO: Same as FunSig0
  class FunSig4[T1<: MnemonicsTypes : TypeTag, T2<: MnemonicsTypes : TypeTag, T3<: MnemonicsTypes : TypeTag, T4<: MnemonicsTypes : TypeTag, R<: MnemonicsTypes : TypeTag] {

    def getArg1(implicit root: Root, flix: Flix): Local[T1] = new Local(0)

    def getArg2(implicit root: Root, flix: Flix): Local[T2] = new Local(1)

    def getArg3(implicit root: Root, flix: Flix): Local[T3] = new Local(2)

    def getArg4(implicit root: Root, flix: Flix): Local[T4] = new Local(3)
  }

  /**
    * Capability which allows acess the function locals in this case a function with 4 arguments
    */
  //TODO: Same as FunSig0
  class UncheckedFunSig(args : List[NJvmType], returnType : NJvmType)(implicit root: Root, flix: Flix) {

    for((arg, index) <- args.zipWithIndex) {
        arg match {
          case PrimBool => new Local[MBool](index)
          case PrimChar => new Local[MChar](index)
          case PrimByte => new Local[MShort](index)
          case PrimShort => new Local[MShort](index)
          case PrimInt => new Local[MInt](index)
          case PrimLong => new Local[MLong](index)
          case PrimFloat => new Local[MFloat](index)
          case PrimDouble => new Local[MDouble](index)
          case Reference(_) => new Local[Ref[MObject]](index)
          case _ => ???
      }
    }

    def getArg[T1 <: MnemonicsTypes : TypeTag](index : Int): Local[T1] ={
      new Local[T1](index)
    }
  }

  /**
    * Capability which allows to invoke a (non-void) method with 0 arguments
    */
  //TODO: Similar to FunSig might need to have different types in order to ensure the type safety with
  //TODO: Static methods, interface methods, etc.
  class Method0[R<: MnemonicsTypes : TypeTag](invokeCode: JvmModifier, ct: Reference, methodName: String) {

    def INVOKE[S <: Stack](implicit root: Root, flix: Flix): F[S] => F[S ** R] = {
      val argsList = invokeCode match {
        case JvmModifier.InvokeStatic => List()
        case JvmModifier.InvokeInterface | JvmModifier.InvokeSpecial | JvmModifier.InvokeVirtual => ???
        case _ => ???
      }
      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, argsList, getJvmType[R])
    }
  }

  /**
    * Capability which allows to invoke a (void) method with 0 arguments
    */
  class VoidMethod0(invokeCode: JvmModifier, ct: Reference, methodName: String) {
    def INVOKE[S <: Stack](implicit root: Root, flix: Flix): F[S] => F[S] = {
      val argsList = invokeCode match {
        case JvmModifier.InvokeStatic => List()
        case JvmModifier.InvokeInterface | JvmModifier.InvokeSpecial | JvmModifier.InvokeVirtual => ???
        case _ => ???
      }
      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, argsList, Void)
    }
  }

  /**
    * Capability which allows to invoke a (non-void) method with 1 argument
    */
  //TODO: Similar to Method0
  class Method1[T1<: MnemonicsTypes : TypeTag, R<: MnemonicsTypes : TypeTag](invokeCode: JvmModifier, ct: Reference, methodName: String) {

    def INVOKE[S <: Stack](implicit root: Root, flix: Flix): F[S **  T1] => F[S ** R] = {
      val argsList = invokeCode match {
        case JvmModifier.InvokeStatic => List(getJvmType[T1])
        case JvmModifier.InvokeInterface | JvmModifier.InvokeSpecial | JvmModifier.InvokeVirtual => List()
        case _ => ???
      }
      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, argsList, getJvmType[R])
    }
  }

  /**
    * Capability which allows to invoke a (void) method with 1 argument
    */
  class VoidMethod1[T1<: MnemonicsTypes : TypeTag](invokeCode: JvmModifier, ct: Reference, methodName: String) {
    def INVOKE[S <: Stack, T <: T1](implicit root: Root, flix: Flix): F[S ** T] => F[S] = {
      val argsList = invokeCode match {
        case JvmModifier.InvokeStatic => List(getJvmType[T1])
        case JvmModifier.InvokeInterface | JvmModifier.InvokeSpecial | JvmModifier.InvokeVirtual => List()
        case _ => ???
      }
      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, argsList, Void)
    }
  }

  /**
    * Capability which allows to invoke a (non-void) method with 2 arguments
    */
  //TODO: Similar to Method0
  class Method2[T1<: MnemonicsTypes : TypeTag, T2<: MnemonicsTypes : TypeTag, R<: MnemonicsTypes : TypeTag](invokeCode: JvmModifier, ct: Reference, methodName: String) {

    def INVOKE[S <: Stack](implicit root: Root, flix: Flix): F[S ** T1 ** T2] => F[S ** R] = {
      val argsList = invokeCode match {
        case JvmModifier.InvokeStatic => List(getJvmType[T1], getJvmType[T2])
        case JvmModifier.InvokeInterface | JvmModifier.InvokeSpecial | JvmModifier.InvokeVirtual => List(getJvmType[T2])
        case _ => ???
      }
      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, argsList, getJvmType[R])
    }
  }

  /**
    * Capability which allows to invoke a (void) method with 2 arguments
    */
  class VoidMethod2[T1<: MnemonicsTypes : TypeTag, T2<: MnemonicsTypes : TypeTag](invokeCode: JvmModifier, ct: Reference, methodName: String) {
    def INVOKE[S <: Stack](implicit root: Root, flix: Flix): F[S ** T1 ** T2] => F[S] =  {
      val argsList = invokeCode match {
        case JvmModifier.InvokeStatic => List(getJvmType[T1], getJvmType[T2])
        case JvmModifier.InvokeInterface | JvmModifier.InvokeSpecial | JvmModifier.InvokeVirtual => List(getJvmType[T2])
        case _ => ???
      }
      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, argsList, Void)
    }
  }


  /**
    * Capability which allows to invoke a method with 3 arguments
    */
  //TODO: Similar to Method0
  class Method3[T1<: MnemonicsTypes : TypeTag, T2<: MnemonicsTypes : TypeTag, T3<: MnemonicsTypes : TypeTag, R<: MnemonicsTypes : TypeTag](invokeCode: JvmModifier, ct: Reference, methodName: String) {

    def INVOKE[S <: Stack](implicit root: Root, flix: Flix): F[S ** T1 ** T2 ** T3] => F[S ** R] = {
      val argsList = invokeCode match {
        case JvmModifier.InvokeStatic => List(getJvmType[T1], getJvmType[T2], getJvmType[T3])
        case JvmModifier.InvokeInterface | JvmModifier.InvokeSpecial | JvmModifier.InvokeVirtual => List(getJvmType[T2], getJvmType[T3])
        case _ => ???
      }
      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, argsList, getJvmType[R])
    }
  }

  /**
    * Capability which allows to invoke a (void) method with 3 arguments
    */
  class VoidMethod3[T1<: MnemonicsTypes : TypeTag, T2<: MnemonicsTypes : TypeTag, T3<: MnemonicsTypes : TypeTag](invokeCode: JvmModifier, ct: Reference, methodName: String) {
    def INVOKE[S <: Stack](implicit root: Root, flix: Flix): F[S ** T1 ** T2 ** T3] => F[S] = {
      val argsList = invokeCode match {
        case JvmModifier.InvokeStatic => List(getJvmType[T1], getJvmType[T2], getJvmType[T3])
        case JvmModifier.InvokeInterface | JvmModifier.InvokeSpecial | JvmModifier.InvokeVirtual => List(getJvmType[T2], getJvmType[T3])
        case _ => ???
      }
      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, argsList, Void)
    }
  }


  /**
    * Capability which allows to invoke a method with 4 arguments
    */
  //TODO: Similar to Method0
  class Method4[T1<: MnemonicsTypes : TypeTag, T2<: MnemonicsTypes : TypeTag, T3<: MnemonicsTypes : TypeTag, T4<: MnemonicsTypes :TypeTag, R<: MnemonicsTypes : TypeTag](invokeCode: JvmModifier, ct: Reference, methodName: String) {

    def INVOKE[S <: Stack](implicit root: Root, flix: Flix): F[S ** T1 ** T2 ** T3 ** T4] => F[S ** R] = {
      val argsList = invokeCode match {
        case JvmModifier.InvokeStatic => List(getJvmType[T1], getJvmType[T2], getJvmType[T3], getJvmType[T4])
        case JvmModifier.InvokeInterface | JvmModifier.InvokeSpecial | JvmModifier.InvokeVirtual => List(getJvmType[T2], getJvmType[T3], getJvmType[T4])
        case _ => ???
      }
      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, argsList, getJvmType[R])
    }
  }

  /**
    * Capability which allows to invoke a (void) method with 4 arguments
    */
  class VoidMethod4[T1<: MnemonicsTypes : TypeTag, T2<: MnemonicsTypes : TypeTag, T3<: MnemonicsTypes : TypeTag, T4 <: MnemonicsTypes : TypeTag ](invokeCode: JvmModifier, ct: Reference, methodName: String) {
    def INVOKE[S <: Stack](implicit root: Root, flix: Flix): F[S ** T1 ** T2 ** T3 ** T4] => F[S] = {
      val argsList = invokeCode match {
        case JvmModifier.InvokeStatic => List(getJvmType[T1], getJvmType[T2], getJvmType[T3], getJvmType[T4])
        case JvmModifier.InvokeInterface | JvmModifier.InvokeSpecial | JvmModifier.InvokeVirtual => List(getJvmType[T2], getJvmType[T3], getJvmType[T4])
        case _ => ???
      }
      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, argsList, Void)
    }
  }

  /**
    * Unchecked Capability which allows to invoke a method
    */
  class UncheckedMethod(invokeCode: JvmModifier, ct: Reference, methodName: String, args : List[NJvmType], returnType : NJvmType) {

    def INVOKE[S1 <: Stack, S2 <: Stack](implicit root: Root, flix: Flix): F[S1] => F[S2] = {

      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, args, returnType)
    }
  }

  /**
    * Unchecked Capability which allows to invoke a void method
    */
  class UncheckedVoidMethod(invokeCode: JvmModifier, ct: Reference, methodName: String, args : List[NJvmType]) {

    def INVOKE[S1 <: Stack, S2 <: Stack](implicit root: Root, flix: Flix): F[S1] => F[S2] = {

      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, args, Void)
    }
  }


  /**
    * This class allows to generate classes. It includes support to generate(CompileField) and methods
    *
    * @param ct                    classType of the class we want to generate
    * @param modifiers             list of modififers which we want to generate the class with (public, abstract, etc..)
    * @param interfaces Array of interfaces which this new class shall implement
    */
  class ClassGenerator(ct: Reference, interfaces: List[Reference],
                       modifiers: List[JvmModifier] = List(Public, Final))
                      (implicit root: Root, flix: Flix) {

    //Create the class writer and initialize, by providing the correct params.
    //The modifiers, superclass, implementedInterfaces
    private val cw: ClassWriter = {
      val superClassName = NJvmType.Object.name.toInternalName
      val interfacesNames = interfaces.map(interface => interface.name.toInternalName).toArray

      val cw = AsmOps.mkClassWriter()
      val modifierVal = modifiers.map(modifier => modifier.toInternal).sum
      cw.visit(AsmOps.JavaVersion, modifierVal, ct.name.toInternalName, null,
        superClassName, interfacesNames)
      cw
    }

    // TODO: Miguel: Should this not be in Instructions?
    def SUPER[T <: Ref[MObject]]: F[StackNil ** T] => F[StackNil] =
      Object.constructor.INVOKE

    /**
      * This method generates in the current class we are generating a (non-void) method with 0 arguments
      * given the provided params. It returns the capability to invoke the (non-void) method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param modifiers  list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param methodName name which we want to give to the method we are generating
      * @param f          , transformer which receives a funSig and return a frame transformer. This returned frame transfomer
      *                   will describe what instruction the method we are generating will execute.
      * @return returns a Method0 capability allows now the possibility to invoke this new (non-void)  method with 0 arguments
      */
    def mkMethod0[R<: MnemonicsTypes : TypeTag](methodName: String, f: FunSig0[R] => F[StackNil] => F[StackNil],
                              modifiers: List[JvmModifier] = List(Public, Final)): Method0[R] = {

      val returnType = getJvmType[R]
      val funSig = new FunSig0[R]()

      emitClassMethod(modifiers, methodName, List(), returnType, f(funSig))

      new Method0(JvmModifier.InvokeStatic, ct, methodName)
    }

    /**
      * This method generates in the current class we are generating a (void) method with 0 arguments
      * given the provided params. It returns the capability to invoke the (void) method. This way we ensure we only call methods4
      * we've generated bytecode for.
      *
      * @param modifiers  list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param methodName name which we want to give to the method we are generating
      * @param f          , transformer which receives a funSig and return a frame transformer. This returned frame transfomer
      *                   will describe what instruction the method we are generating will execute.
      * @return returns a VoidMethod0 capability allows now the possibility to invoke this new (void) method with 0 arguments
      */
    def mkVoidMethod0(methodName: String, f: FunSig0[MVoid] => F[StackNil] => F[StackNil],
                      modifiers: List[JvmModifier] = List(Public, Final)): VoidMethod0 = {

      val returnType = Void
      val funSig = new FunSig0[MVoid]()

      emitClassMethod(modifiers, methodName, List(), returnType, f(funSig))

      new VoidMethod0(JvmModifier.InvokeStatic, ct, methodName)
    }


    /**
      * This method generates in the current class we are generating a (non-void) method with 1 argument
      * given the provided params. It returns the capability to invoke the (non-void) method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param modifiers  list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param methodName name which we want to give to the method we are generating
      * @param f          , transformer which receives a funSig and return a frame transformer. This returned frame transfomer
      *                   will describe what instruction the method we are generating will execute.
      * @return returns a Method1 capability allows now the possibility to invoke this new (non-void) method with 1 argument
      */
    def mkMethod1[T1<: MnemonicsTypes : TypeTag, R<: MnemonicsTypes : TypeTag](methodName: String, f: FunSig1[T1, R] => F[StackNil] => F[StackNil],
                                           modifiers: List[JvmModifier] = List(Public, Final), isStatic : Boolean = false): Method1[T1, R] = {
      val arg1Type = getJvmType[T1]
      val returnType = getJvmType[R]
      val funSig = new FunSig1[T1, R]()

      val (invokeCode, argsList) = if (isStatic) (JvmModifier.InvokeStatic, List(arg1Type)) else (JvmModifier.InvokeVirtual, List())

      emitClassMethod(modifiers, methodName, argsList, returnType, f(funSig))

      new Method1(invokeCode, ct, methodName)
    }


    /**
      * This method generates in the current class we are generating a (void) method with 1 argument
      * given the provided params. It returns the capability to invoke the (void) method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param modifiers  list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param methodName name which we want to give to the method we are generating
      * @param f          , transformer which receives a funSig and return a frame transformer. This returned frame transfomer
      *                   will describe what instruction the method we are generating will execute.
      * @return returns a VoidMethod0 capability allows now the possibility to invoke this new (void) method with 1 argument
      */
    def mkVoidMethod1[T1<: MnemonicsTypes : TypeTag](methodName: String, f: FunSig1[T1, MVoid] => F[StackNil] => F[StackNil],
                                   modifiers: List[JvmModifier] = List(Public, Final), isStatic : Boolean = false): VoidMethod1[T1] = {

      val arg1Type = getJvmType[T1]
      val returnType = Void
      val funSig = new FunSig1[T1, MVoid]()

      val (invokeCode, argsList) = if(isStatic) (JvmModifier.InvokeStatic, List(arg1Type)) else (JvmModifier.InvokeVirtual, List())

      emitClassMethod(modifiers, methodName, argsList, returnType, f(funSig))

      new VoidMethod1(invokeCode, ct, methodName)
    }

    /**
      * This method generates in the current class we are generating a constructor with 1 argument
      * given the provided params. It returns the capability to invoke the constructor. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param modifiers list of modififers which we want to generate the constructor with (public, abstract, etc..)
      * @param f         , transformer which receives a funSig and return a frame transformer. This returned frame transfomer
      *                  will describe what instruction the constructor we are generating will execute.
      * @return returns a VoidMethod0 capability allows now the possibility to invoke this new constructor with 1 argument
      */
    def mkConstructor1[T1<: MnemonicsTypes : TypeTag](f: FunSig1[T1, MVoid] => F[StackNil] => F[StackNil],
                                    modifiers: List[JvmModifier] = List(Public)): VoidMethod1[T1] = {

      val funSig = new FunSig1[T1, MVoid]()

      emitClassMethod(modifiers, "<init>", List(), Void, f(funSig))

      new VoidMethod1(JvmModifier.InvokeSpecial, ct, "<init>")
    }

    /**
      * This method generates in the current class we are generating a (non-void) method with 2 arguments
      * given the provided params. It returns the capability to invoke the (non-void) method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param modifiers  list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param methodName name which we want to give to the method we are generating
      * @param f          , transformer which receives a funSig and return a frame transformer. This returned frame transfomer
      *                   will describe what instruction the method we are generating will execute.
      * @return returns a Method2 capability allows now the possibility to invoke this new  (non-void) method with 2 arguments
      */
    def mkMethod2[T1<: MnemonicsTypes : TypeTag, T2<: MnemonicsTypes : TypeTag, R<: MnemonicsTypes : TypeTag](methodName: String, f: FunSig2[T1, T2, R] => F[StackNil] => F[StackNil],
                                                        modifiers: List[JvmModifier] = List(Public, Final), isStatic : Boolean = false): Method2[T1, T2, R] = {
      val arg1Type = getJvmType[T1]
      val arg2Type = getJvmType[T2]
      val returnType = getJvmType[R]

      val funSig = new FunSig2[T1, T2, R]()


      val (invokeCode, argsList) = if(isStatic) (JvmModifier.InvokeStatic, List(arg1Type, arg2Type)) else (JvmModifier.InvokeVirtual, List(arg2Type))

      emitClassMethod(modifiers, methodName, argsList, returnType, f(funSig))

      new Method2(invokeCode, ct, methodName)
    }

    /**
      * This method generates in the current class we are generating a (void) method with 2 arguments
      * given the provided params. It returns the capability to invoke the (void) method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param modifiers  list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param methodName name which we want to give to the method we are generating
      * @param f          , transformer which receives a funSig and return a frame transformer. This returned frame transfomer
      *                   will describe what instruction the method we are generating will execute.
      * @return returns a VoidMethod0 capability allows now the possibility to invoke this new (void) method with 2 arguments
      */
    def mkVoidMethod2[T1<: MnemonicsTypes : TypeTag, T2<: MnemonicsTypes : TypeTag](methodName: String, f: FunSig2[T1, T2, MVoid] => F[StackNil] => F[StackNil],
                                                modifiers: List[JvmModifier] = List(Public, Final), isStatic : Boolean = false): VoidMethod2[T1, T2] = {
      val arg1Type = getJvmType[T1]
      val arg2Type = getJvmType[T2]

      val returnType = Void
      val funSig = new FunSig2[T1, T2, MVoid]()

      val (invokeCode, argsList) = if(isStatic) (JvmModifier.InvokeStatic, List(arg1Type, arg2Type)) else (JvmModifier.InvokeVirtual, List(arg2Type))

      emitClassMethod(modifiers, methodName, argsList, returnType, f(funSig))

      new VoidMethod2(invokeCode, ct, methodName)
    }

    /**
      * This method generates in the current class we are generating a constructor with 2 arguments
      * given the provided params. It returns the capability to invoke the constructor. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param modifiers list of modififers which we want to generate the constructor with (public, abstract, etc..)
      * @param f         , transformer which receives a funSig and return a frame transformer. This returned frame transfomer
      *                  will describe what instruction the constructor we are generating will execute.
      * @return returns a VoidMethod0 capability allows now the possibility to invoke this new constructor with 2 arguments
      */
    def mkConstructor2[T1<: MnemonicsTypes : TypeTag, T2<: MnemonicsTypes : TypeTag](f: FunSig2[T1, T2, MVoid] => F[StackNil] => F[StackNil],
                                                 modifiers: List[JvmModifier] = List(Public)): VoidMethod2[T1, T2] = {

      val funSig = new FunSig2[T1, T2, MVoid]()

      emitClassMethod(modifiers, "<init>", List(getJvmType[T2]), Void, f(funSig))

      new VoidMethod2(JvmModifier.InvokeSpecial, ct, "<init>")
    }

    /**
      * This method generates in the current class we are generating a (non-void) method with 3 arguments
      * given the provided params. It returns the capability to invoke the (non-void) method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param modifiers  list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param methodName name which we want to give to the method we are generating
      * @param f          , transformer which receives a funSig and return a frame transformer. This returned frame transfomer
      *                   will describe what instruction the method we are generating will execute.
      * @return returns a Method3 capability allows now the possibility to invoke this new (non-void) method with 3 arguments
      */

    def mkMethod3[T1<: MnemonicsTypes : TypeTag, T2<: MnemonicsTypes : TypeTag, T3<: MnemonicsTypes : TypeTag, R<: MnemonicsTypes : TypeTag](methodName: String, f: FunSig3[T1, T2, T3, R] => F[StackNil] => F[StackNil],
                                                                     modifiers: List[JvmModifier] = List(Public, Final), isStatic : Boolean = false): Method3[T1, T2, T3, R] = {
      val arg1Type = getJvmType[T1]
      val arg2Type = getJvmType[T2]
      val arg3Type = getJvmType[T3]
      val returnType = getJvmType[R]

      val funSig = new FunSig3[T1, T2, T3, R]()

      val (invokeCode, argsList) = if(isStatic) (JvmModifier.InvokeStatic, List(arg1Type, arg2Type, arg3Type)) else (JvmModifier.InvokeVirtual, List(arg2Type, arg3Type))

      emitClassMethod(modifiers, methodName, argsList, returnType, f(funSig))
      new Method3(invokeCode, ct, methodName)
    }

    /**
      * This method generates in the current class we are generating a (void) method with 3 arguments
      * given the provided params. It returns the capability to invoke the (void) method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param modifiers  list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param methodName name which we want to give to the method we are generating
      * @param f          , transformer which receives a funSig and return a frame transformer. This returned frame transfomer
      *                   will describe what instruction the method we are generating will execute.
      * @return returns a VoidMethod0 capability allows now the possibility to invoke this new (void) method with 3 arguments
      */
    def mkVoidMethod3[T1<: MnemonicsTypes : TypeTag, T2<: MnemonicsTypes : TypeTag, T3<: MnemonicsTypes : TypeTag](methodName: String, f: FunSig3[T1, T2, T3, MVoid] => F[StackNil] => F[StackNil],
                                                             modifiers: List[JvmModifier] = List(Public, Final), isStatic : Boolean = false): VoidMethod3[T1, T2, T3] = {

      val arg1Type = getJvmType[T1]
      val arg2Type = getJvmType[T2]
      val arg3Type = getJvmType[T3]

      val returnType = Void
      val funSig = new FunSig3[T1, T2, T3, MVoid]()

      val (invokeCode, argsList) = if(isStatic) (JvmModifier.InvokeStatic, List(arg1Type, arg2Type, arg3Type)) else (JvmModifier.InvokeVirtual, List(arg2Type, arg3Type))

      emitClassMethod(modifiers, methodName, argsList, returnType, f(funSig))

      new VoidMethod3(invokeCode, ct, methodName)
    }

    /**
      * This method generates in the current class we are generating a constructor with 3 arguments
      * given the provided params. It returns the capability to invoke the constructor. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param modifiers list of modififers which we want to generate the constructor with (public, abstract, etc..)
      * @param f         , transformer which receives a funSig and return a frame transformer. This returned frame transfomer
      *                  will describe what instruction the constructor we are generating will execute.
      * @return returns a VoidMethod0 capability allows now the possibility to invoke this new constructor with 3 arguments
      */
    def mkConstructor3[T1<: MnemonicsTypes : TypeTag, T2<: MnemonicsTypes : TypeTag, T3<: MnemonicsTypes : TypeTag](f: FunSig3[T1, T2, T3, MVoid] => F[StackNil] => F[StackNil],
                                                              modifiers: List[JvmModifier] = List(Public)): VoidMethod3[T1, T2, T3] = {

      val funSig = new FunSig3[T1, T2, T3, MVoid]()

      emitClassMethod(modifiers, "<init>", List(getJvmType[T2], getJvmType[T3]), Void, f(funSig))

      new VoidMethod3(JvmModifier.InvokeSpecial, ct, "<init>")
    }


    /**
      * This method generates in the current class we are generating a constructor with 4 arguments
      * given the provided params. It returns the capability to invoke the constructor. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param modifiers list of modififers which we want to generate the constructor with (public, abstract, etc..)
      * @param f         , transformer which receives a funSig and return a frame transformer. This returned frame transfomer
      *                  will describe what instruction the constructor we are generating will execute.
      * @return returns a VoidMethod0 capability allows now the possibility to invoke this new constructor with 4 arguments
      */
    def mkConstructor4[T1<: MnemonicsTypes : TypeTag, T2<: MnemonicsTypes : TypeTag, T3<: MnemonicsTypes : TypeTag, T4<: MnemonicsTypes  : TypeTag](f: FunSig4[T1, T2, T3, T4, MVoid] => F[StackNil] => F[StackNil],
                                                              modifiers: List[JvmModifier] = List(Public)): VoidMethod4[T1, T2, T3, T4] = {

      val funSig = new FunSig4[T1, T2, T3, T4, MVoid]()

      emitClassMethod(modifiers, "<init>", List(getJvmType[T2], getJvmType[T3], getJvmType[T4]), Void, f(funSig))

      new VoidMethod4(JvmModifier.InvokeSpecial, ct, "<init>")
    }


    /**
      * This method generates in the current class we are generating a constructor with 4 arguments
      * given the provided params. It returns the capability to invoke the constructor. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param modifiers list of modififers which we want to generate the constructor with (public, abstract, etc..)
      * @param f         , transformer which receives a funSig and return a frame transformer. This returned frame transfomer
      *                  will describe what instruction the constructor we are generating will execute.
      * @return returns a VoidMethod0 capability allows now the possibility to invoke this new constructor with 4 arguments
      */
    def mkUncheckedConstructor(args : List[NJvmType], f: UncheckedFunSig => F[StackNil] => F[StackNil],
                               modifiers: List[JvmModifier] = List(Public)): UncheckedVoidMethod = {


      val funSig = new UncheckedFunSig(args, Void)
      val argsList = if(args.length <= 1) Nil else args.tail
      emitClassMethod(modifiers, "<init>",  argsList , Void, f(funSig))

      new UncheckedVoidMethod(JvmModifier.InvokeSpecial, ct, "<init>", args)
    }

    /**
      * Auxiliary method which actually emits the method bytecode onto the current class.
      * Should be used by functions which make method
      *
      * @param modifiers  list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param methodName name which we want to give to the method we are generating
      * @param args       List of the arguments JvmType of the function we are generating code for
      * @param returnType the JvmType of the return type of the function we are generating code for
      * @param f          is a frame transfomer  it will describe what instruction the method we are generating will execute.
      */
    private def emitClassMethod(modifiers: List[JvmModifier], methodName: String,
                                  args: List[NJvmType], returnType: NJvmType, f: F[StackNil] => F[StackNil]): Unit = {
      val modifierVal = modifiers.map(modifier => modifier.toInternal).sum
      val mv = cw.visitMethod(modifierVal, methodName,
        getMethodDescriptor(args, returnType), null, null)
      mv.visitCode()

      f(new F[StackNil](mv, ct))

      mv.visitMaxs(1, 1)
      mv.visitEnd()
    }

    /**
      * Method which receives a list of JvmModifiers and fieldName. It will emit code to generate a field
      * with the correspoding type parameter T.
      *
      * @param modifiers list of modififers which we want to generate the field with (public, private, final, etc..)
      * @param fieldName name which we want to give to the field we are generating
      * @return returns the capability to acess the created field. Similar to the methods this ensure we only
      *         acess field we've generated
      */
    def mkField[T <: MnemonicsTypes : TypeTag](fieldName: String, modifiers: List[JvmModifier] = List(Private)): Field[T] = {
      val fieldType = getJvmType[T]
      val modifierVal = modifiers.map(modifier => modifier.toInternal).sum
      val field = cw.visitField(modifierVal, fieldName, fieldType.toDescriptor, null, null)
      field.visitEnd()

      new Field(fieldName)
    }

    /**
      * Method which compiles the class. Should be called when done creating the class.
      * It will return the corresponding byte array (bytecode) for the class we are generating
      */
    def compile(): Array[Byte] = {
      cw.visitEnd()
      cw.toByteArray
    }
  }

  /**
    * This class allows to generate interfaces. It includes support to interface methods
    *
    * @param it                    classType of the interaface we want to generate
    * @param modifiers             list of modififers which we want to generate the class with (public, abstract, etc..)
    * @param interfaces Array of interfaces which this new class shall implement
    */
  class InterfaceGenerator(it: Reference, interfaces: List[Reference], modifiers: List[JvmModifier] = List(Public, Abstract, Interface))
                          (implicit root: Root, flix: Flix) {


    //Create the class writer and initialize, by providing the correct params.
    //The modifiers, superclass, implementedInterfaces
    private val cw: ClassWriter = {

      val superClassName = NJvmType.Object.name.toInternalName
      val interfacesNames = interfaces.map(interface => interface.name.toInternalName).toArray

      val cw = AsmOps.mkClassWriter()
      val modifierVal = modifiers.map(modifier => modifier.toInternal).sum
      cw.visit(AsmOps.JavaVersion, modifierVal, it.name.toInternalName, null,
        superClassName, interfacesNames)
      cw
    }

    /**
      * This method generates in the current interface we are generating a (non-void) method with 1 argument
      * given the provided params. It returns the capability to invoke the (non-void) method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param modifiers  list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param methodName name which we want to give to the method we are generating
      * @return returns a Method1 capability allows now the possibility to invoke this new (non-void) method with arguments
      */
    def mkMethod1[T1<: MnemonicsTypes : TypeTag, R<: MnemonicsTypes : TypeTag](methodName: String, modifiers: List[JvmModifier] = List(Public, Abstract)): Method1[T1, R] = {

      val returnType = getJvmType[R]

      emitInterfaceMethod(modifiers, methodName, List(), returnType)

      new Method1(JvmModifier.InvokeInterface, it, methodName)
    }

    /**
      * This method generates in the current interface we are generating a (void) method with 1 argument
      * given the provided params. It returns the capability to invoke the (void) method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param modifiers  list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param methodName name which we want to give to the method we are generating
      * @return returns a Method0 capability allows now the possibility to invoke this new (void) method with 1 argument
      */
    def mkVoidMethod1[T1<: MnemonicsTypes : TypeTag](methodName: String, modifiers: List[JvmModifier] = List(Public, Abstract)): VoidMethod1[T1] = {

      val returnType = Void

      emitInterfaceMethod(modifiers, methodName, List(), returnType)

      new VoidMethod1(JvmModifier.InvokeInterface, it, methodName)
    }

    /**
      * This method generates in the current interface we are generating a (non-void) method with 2 arguments
      * given the provided params. It returns the capability to invoke the (non-void) method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param modifiers  list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param methodName name which we want to give to the method we are generating
      * @return returns a Method2 capability allows now the possibility to invoke this new (non-void) method with 2 arguments
      */
    def mkMethod2[T1<: MnemonicsTypes : TypeTag, T2<: MnemonicsTypes : TypeTag, R<: MnemonicsTypes : TypeTag](methodName: String, modifiers: List[JvmModifier] = List(Public, Abstract)): Method2[T1, T2, R] = {

      val arg2Type = getJvmType[T2]
      val returnType = getJvmType[R]

      emitInterfaceMethod(modifiers, methodName, List(arg2Type), returnType)

      new Method2(JvmModifier.InvokeInterface, it, methodName)
    }

    /**
      * This method generates in the current interface we are generating a (void) method with 2 arguments
      * given the provided params. It returns the capability to invoke the (void) method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param modifiers  list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param methodName name which we want to give to the method we are generating
      * @return returns a Method0 capability allows now the possibility to invoke this new (void) method with 2 arguments
      */
    def mkVoidMethod2[T1<: MnemonicsTypes : TypeTag, T2<: MnemonicsTypes : TypeTag](methodName: String, modifiers: List[JvmModifier] = List(Public, Abstract)): VoidMethod2[T1, T2] = {

      val arg2Type = getJvmType[T2]
      val returnType = Void

      emitInterfaceMethod(modifiers, methodName, List(arg2Type), returnType)

      new VoidMethod2(JvmModifier.InvokeInterface, it, methodName)
    }

    /**
      * This method generates in the current interface we are generating a (non-void) method with 3 arguments
      * given the provided params. It returns the capability to invoke the (non-void) method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param modifiers  list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param methodName name which we want to give to the method we are generating
      * @return returns a Method3 capability allows now the possibility to invoke this new (non-void) method with 3 arguments
      */
    def mkMethod3[T1<: MnemonicsTypes : TypeTag, T2<: MnemonicsTypes : TypeTag, T3<: MnemonicsTypes : TypeTag, R<: MnemonicsTypes : TypeTag](methodName: String, modifiers: List[JvmModifier] = List(Public, Abstract)): Method3[T1, T2, T3, R] = {

      val arg2Type = getJvmType[T2]
      val arg3Type = getJvmType[T3]
      val returnType = getJvmType[R]

      emitInterfaceMethod(modifiers, methodName, List(arg2Type, arg3Type), returnType)

      new Method3(JvmModifier.InvokeInterface, it, methodName)
    }

    /**
      * This method generates in the current interface we are generating a (void) method with 2 arguments
      * given the provided params. It returns the capability to invoke the (void) method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param modifiers  list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param methodName name which we want to give to the method we are generating
      * @return returns a Method0 capability allows now the possibility to invoke this new (void) method with 2 arguments
      */
    def mkVoidMethod3[T1<: MnemonicsTypes : TypeTag, T2<: MnemonicsTypes : TypeTag, T3<: MnemonicsTypes : TypeTag](methodName: String, modifiers: List[JvmModifier] = List(Public, Abstract)): VoidMethod3[T1, T2, T3] = {

      val arg2Type = getJvmType[T2]
      val arg3Type = getJvmType[T2]

      val returnType = Void

      emitInterfaceMethod(modifiers, methodName, List(arg2Type, arg3Type), returnType)

      new VoidMethod3(JvmModifier.InvokeInterface, it, methodName)
    }

    /**
      * Auxiliary method which actually emits the method bytecode onto the current interface.
      * Should be used by functions which make method.
      * Since this is an interface there is no actuall instructions which are emitted for the method
      *
      * @param modifiers  list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param methodName name which we want to give to the method we are generating
      * @param args       List of the arguments JvmType of the function we are generating code for
      * @param returnType the JvmType of the return type of the function we are generating code for
      */
    private def emitInterfaceMethod(modifiers: List[JvmModifier], methodName: String,
                                    args: List[NJvmType], returnType: NJvmType): Unit = {
      val modifierVal = modifiers.map(modifier => modifier.toInternal).sum
      val mv = cw.visitMethod(modifierVal, methodName,
        getMethodDescriptor(args, returnType), null, null)
      mv.visitEnd()
    }

    /**
      * Method which compiles the interface. Should be called when done creating the class.
      * It will return the corresponding byte array (bytecode) for the class we are generating
      */
    def compile(): Array[Byte] = {
      cw.visitEnd()
      cw.toByteArray
    }
  }

  /**
    * A stack transformer that throws an unsupported operation exception for toString.
    */
  def toStringNotImplemented(implicit root: Root, flix: Flix): F[StackNil] => F[StackNil] = {
    newUnsupportedOperationExceptionInstructions("toString shouldn't be called")
  }

  /**
    * A stack transformer that throws an unsupported operation exception for equals.
    */
  def equalsNotImplemented(implicit root: Root, flix: Flix): F[StackNil] => F[StackNil] = {
    newUnsupportedOperationExceptionInstructions("equals shouldn't be called")
  }

  /**
    * A stack transformer that throws an unsupported operation exception for hashCode.
    */
  def hashCodeNotImplemented(implicit root: Root, flix: Flix): F[StackNil] => F[StackNil] = {
    newUnsupportedOperationExceptionInstructions("hashCode shouldn't be called")
  }

  //TODO: Might need to similarly to InterfaceGenerator and ClassGenerator generate a StaticGenerator
  /**
    * Auxiliary method which returns the transformer to generate the body of a function which simply throws an
    * Exception (UnsupportedOperationException in this case)
    *
    * @param message which we want to display when the exception is thrown
    */
  def newUnsupportedOperationExceptionInstructions(message: String)(implicit root: Root, flix: Flix): F[StackNil] => F[StackNil] = {
    Instructions.NEW[StackNil](Reference(JvmName.UnsupportedOperationException)) |>>
      Instructions.DUP |>>
      Instructions.LDC_STRING(message) |>>
      Java.Lang.UnsupportedOperationException.constructor.INVOKE |>>
      Instructions.THROW
  }

  /**
    * Returns the load instruction for the value of the type specified by `tpe`
    */
  def getLoadInstruction(tpe: NJvmType): Int = tpe match {
    case Void => throw InternalCompilerException(s"Unexpected type $tpe")
    case PrimBool | PrimChar | PrimByte | PrimShort | PrimInt => ILOAD
    case PrimLong => LLOAD
    case PrimFloat => FLOAD
    case PrimDouble => DLOAD
    case Reference(_) => ALOAD
  }

  /**
    * Returns the load instruction corresponding to the given type `tpe`
    */
  def getReturnInstruction(tpe: NJvmType): Int = tpe match {
    case Void => throw InternalCompilerException(s"Unexpected type $tpe")
    case PrimBool | PrimChar | PrimByte | PrimShort | PrimInt => IRETURN
    case PrimLong => LRETURN
    case PrimFloat => FRETURN
    case PrimDouble => DRETURN
    case Reference(_) => ARETURN
  }

  /**
    * Returns the descriptor of a method take takes the given `argumentTypes` and returns the given `resultType`.
    */
  def getMethodDescriptor(argumentTypes: List[NJvmType], resultType: NJvmType): String = {
    // Descriptor of result
    val resultDescriptor = resultType.toDescriptor

    // Descriptor of arguments
    val argumentDescriptor = argumentTypes.map(_.toDescriptor).mkString

    // Descriptor of the method
    s"($argumentDescriptor)$resultDescriptor"
  }

  /**
    * Returns stringified name of the given JvmType `tpe`.
    *
    * The stringified name is short hand used for generation of interface and class names.
    */
  def stringify(tpe: NJvmType): String = tpe match {
    case Void => "Void"
    case PrimBool => "Bool"
    case PrimChar => "Char"
    case PrimFloat => "Float32"
    case PrimDouble => "Float64"
    case PrimByte => "Int8"
    case PrimShort => "Int16"
    case PrimInt => "Int32"
    case PrimLong => "Int64"
    case Reference(_) => "Obj"
  }


  /**
    * Returns the tuple interface type `TX$Y$Z` for the given type `tpe`.
    *
    * For example,
    *
    * (Int, Int)              =>    T2$Int$Int
    * (Int, Int, Int)         =>    T3$Int$Int$Int
    * (Bool, Char, Int)       =>    T3$Bool$Char$Int
    *
    * NB: The given type `tpe` must be a tuple type.
    */
  def getTupleInterfaceType(elms: List[NJvmType])(implicit root: Root, flix: Flix): Reference = {
      // Compute the arity of the tuple.
      val arity = elms.length

      // Compute the stringified erased type of each type argument.
      val args = elms.map(tpe => stringify(tpe))

      // The JVM name is of the form TArity$Arg0$Arg1$Arg2
      val name = "ITuple" + arity + "$" + args.mkString("$")

      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
  }

  /**
    * Returns the tuple class type `TupleX$Y$Z` for the given type `tpe`.
    *
    * For example,
    *
    * (Int, Int)              =>    Tuple2$Int$Int
    * (Int, Int, Int)         =>    Tuple3$Int$Int$Int
    * (Bool, Char, Int)       =>    Tuple3$Bool$Char$Int
    * (Bool, List[Int])       =>    Tuple2$Bool$Obj
    * (Bool, (Int, Int))      =>    Tuple2$Bool$Obj
    *
    * NB: The given type `tpe` must be a tuple type.
    */
  def getTupleClassType(elms: List[NJvmType])(implicit root: Root, flix: Flix): Reference = {
      // Compute the arity of the tuple.
      val arity = elms.length

      // Compute the stringified erased type of each type argument.
      val args = elms.map(tpe => stringify(tpe))

      // The JVM name is of the form TupleArity$Arg0$Arg1$Arg2
      val name = "Tuple" + arity + "$" + args.mkString("$")

      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
  }


  /**
    * Returns the function interface type `FnX$Y$Z` for the given type `tpe`.
    *
    * For example:
    *
    * Int -> Int          =>  Fn2$Int$Int
    * (Int, Int) -> Int   =>  Fn3$Int$Int$Int
    *
    * NB: The given type `tpe` must be an arrow type.
    */
  def getFunctionInterfaceType(elms: List[NJvmType], returnType : NJvmType)(implicit root: Root, flix: Flix): Reference = {
      // Compute the arity of the function interface.
      // We subtract one since the last argument is the return type.
      val arity = elms.length

      // Compute the stringified erased type of each type argument.
      val args = (elms:::List(returnType)).map(tpe => stringify(tpe))

      // The JVM name is of the form FnArity$Arg0$Arg1$Arg2
      val name = "Fn" + arity + "$" + args.mkString("$")

      // The type resides in the root package.
      Reference(JvmName(RootPackage, name))
  }


  /**
    * Returns the enum interface type `Enum$X$Y$Z` for the given type `tpe`.
    *
    * For example,
    *
    * Color                 =>      IColor
    * Option[Int]           =>      IOption$Int
    * Result[Char, Int]     =>      IResult$Char$Int
    *
    * NB: The given type `tpe` must be an enum type.
    */
  def getEnumInterfaceType(sym : Symbol.EnumSym,elms: List[NJvmType])(implicit root: Root, flix: Flix): Reference = {
      // Compute the stringified erased type of each type argument.
      val args = elms.map(tpe => stringify(tpe))

      // The JVM name is of the form Option$ or Option$Int
      val name = if (args.isEmpty) "I" + sym.name else "I" + sym.name + "$" + args.mkString("$")

      // The enum resides in its namespace package.
      Reference(JvmName(sym.namespace, name))
  }


  /**
    * Returns the record interface type `IRecord`.
    *
    * For example,
    *
    * {}                  =>    IRecord
    * {x : Int}           =>    IRecord
    * {x : Str, y : Int}  =>    IRecord
    */
  def getRecordInterfaceType(implicit root: Root, flix: Flix): Reference = {
    getJvmType[Ref[RecordInterface]].asInstanceOf[Reference]
  }

  /**
    * Returns the empty record class type `RecordEmtpy`
    *
    * For example,
    *
    * {}         =>    RecordEmpty
    *
    */
  def getRecordEmptyClassType(implicit root: Root, flix: Flix): Reference = {
    getJvmType[Ref[RecordEmpty]].asInstanceOf[Reference]

  }

  /**
    * Returns the extended record class type `RecordExtend$X` for the given type 'tpe'
    *
    * For example,
    *
    * {+z : Int  | {}}                =>    RecordExtend$Int
    * {+y : Char | {z : Int}          =>    RecordExtend$Char
    * {+x : Str |{y : Char, z : Int}  =>    RecordExtend$Obj
    *
    * NB: The given type `tpe` must be a Record type
    */
  def getRecordExtendClassType[T <: MnemonicsTypes : TypeTag](implicit root: Root, flix: Flix): Reference = {
    getJvmType[Ref[RecordExtend[T]]].asInstanceOf[Reference]
  }


  /**
    * Returns reference class type for the given type `tpe`.
    *
    * Ref[Bool]              =>    Ref$Bool
    * Ref[List[Int]          =>    Ref$Obj
    *
    * NB: The type must be a reference type.
    */
  def getRefClassType[T<: MnemonicsTypes  : TypeTag](implicit root: Root, flix: Flix): Reference = {
    getJvmType[Ref[RefClass[T]]].asInstanceOf[Reference]
  }


  /**
    * Returns the continuation interface type `Cont$X` for the given type `tpe`.
    *
    * Int -> Int          =>  Cont$Int
    * (Int, Int) -> Int   =>  Cont$Int
    *
    * NB: The given type `tpe` must be an arrow type.
    */
  def getContinuationInterfaceType[T1 <: MnemonicsTypes : TypeTag](implicit root: Root, flix: Flix): Reference = {
    getJvmType[Ref[ContinuationInterface[T1]]].asInstanceOf[Reference]
  }


  /**
    * Returns the erased JvmType of the given Flix type `tpe`.
    */
  def getErasedJvmType(tpe: MonoType)(implicit root: Root, flix: Flix): NJvmType = {
    /**
      * Returns the erased JvmType of the given JvmType `tpe`.
      *
      * Every primitive type is mapped to itself and every other type is mapped to Object.
      */
    def erase(tpe: JvmType): NJvmType = tpe match {
      case JvmType.Void => NJvmType.Void
      case JvmType.PrimBool => NJvmType.PrimBool
      case JvmType.PrimChar => NJvmType.PrimChar
      case JvmType.PrimByte => NJvmType.PrimByte
      case JvmType.PrimShort => NJvmType.PrimShort
      case JvmType.PrimInt => NJvmType.PrimInt
      case JvmType.PrimLong => NJvmType.PrimLong
      case JvmType.PrimFloat => NJvmType.PrimFloat
      case JvmType.PrimDouble => NJvmType.PrimDouble
      case JvmType.Reference(_) => NJvmType.Object
    }

    erase(JvmOps.getJvmType(tpe))
  }



  /**
    * Trait which represents a MnemonicClass. This way we can have a Map of JvmName -> MnemonicsClass
    * Allowing us to get the Class we want with the JvmName (which should be unique). Then if we cast the
    * MnemonicClass to the proper class we can acess the capabilities to call the methods in the class
    * which we are sure we have generated as the class is in the Map
    */

  trait MnemonicsClass extends MObject {
    def getJvmClass: JvmClass

    def getClassMapping: (JvmName, MnemonicsClass)
  }


  /**
    * Trait which allows to have a bunch of MnemonicsGenerator objects. This way we can do a nice foldLeft on a List
    * of MnemonicsGenerator in order to generate all the required classes.
    */
  //TODO: better naming?
  trait MnemonicsGenerator {

    /**
      * Method should receive a Map of all the generated classes so far. It should generate all the new classes
      * and return an updated map with the new generated classes.
      *
      * @param map of all the generated classes so far.
      * @param ts  set of Monotypes this will be used to generate certain classes such as Enum.
      * @return update map with new generated classes
      */
    def gen(map: Map[JvmName, MnemonicsClass], ts: Set[MonoType])(implicit root: Root, flix: Flix): Map[JvmName, MnemonicsClass]
  }

}

