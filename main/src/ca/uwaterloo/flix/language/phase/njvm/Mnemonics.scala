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
import ca.uwaterloo.flix.language.ast.FinalAst.{Def, Root}
import ca.uwaterloo.flix.language.ast.{MonoType, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.JvmOps.{RootPackage, getTagsOf, mangle}
import ca.uwaterloo.flix.language.phase.jvm._
import ca.uwaterloo.flix.language.phase.njvm.Api.Java
import ca.uwaterloo.flix.language.phase.njvm.Api.Java.Lang.{Object, _}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.JvmModifier._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.reflect.runtime.universe._
import org.objectweb.asm.{ClassWriter, Label, MethodVisitor}
import org.objectweb.asm.Opcodes.{DUP, NEW, _}
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._
import ca.uwaterloo.flix.language.phase.njvm.classes._
import ca.uwaterloo.flix.language.phase.njvm.interfaces._

object Mnemonics {


  //We are modeling the jvm runtime stack therefore it is crucial to encode it
  //in the scala type system
  trait Stack

  //A stack can either be empty i.e. StackNill
  trait StackNil extends Stack

  //Or it can be a cons between a stack a type T
  case class StackCons[+R <: Stack, +T](rest: R, top: T) extends Stack

  //Sugar for stackcons
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
      * @param offset  the of the local
      * @pre This method should only be called if local in the specified offset matches the jvmType,
      *      to avoid verifier errors
      */
    def emitLoad[S](localType: NJvmType, offset: Int): F[S] = {
      val loadIns = getLoadInstruction(localType)
      mv.visitVarInsn(loadIns, offset)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits the correct Jvm load instruction give the localType
      *
      * @param localType the jvmType of the local we want to load
      * @param offset  the of the local
      * @pre This method should only be called if local in the specified offset matches the jvmType,
      *      to avoid verifier errors
      */
    def emitStore[S](localType: NJvmType, offset: Int): F[S] = {
      val storeIns = getStoreInstruction(localType)
      mv.visitVarInsn(storeIns, offset)
      this.asInstanceOf[F[S]]
    }


    def emitArrayLoad[S](localType: NJvmType): F[S] = {
      val arrayLoadIns = getArrayLoadInstruction(localType)
      mv.visitInsn(arrayLoadIns)
      this.asInstanceOf[F[S]]
    }


    def emitArrayStore[S](localType: NJvmType): F[S] = {
      val arrayStoreIns = getArrayStoreInstruction(localType)
      mv.visitInsn(arrayStoreIns)
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
      * Emits a getField instruction given the fieldName, it's type and the class reference which contains the field
      *
      * @param fieldName the of the field
      * @param ct reference of the class which contains the fields
      * @param fieldType the jvmType of the field we want to get
      * @pre This method should only be called if field with the specified fieldName matches the jvmType,
      *      to avoid verifier errors
      */
    def emitGetField[S](fieldName: String, ct: Reference, fieldType: NJvmType): F[S] = {
      mv.visitFieldInsn(GETFIELD, ct.name.toInternalName, fieldName, fieldType.toDescriptor)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a getStatic instruction given the fieldName and it's type
      *
      * @param fieldName the of the field
      * @param fieldType the jvmType of the field we want to get
      * @pre This method should only be called if field with the specified fieldName matches the jvmType,
      *      to avoid verifier errors
      */
    def emitGetStatic[S](fieldName: String, fieldType: NJvmType): F[S] = {
      mv.visitFieldInsn(GETSTATIC, ct.name.toInternalName, fieldName, fieldType.toDescriptor)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a PutStatic instruction given the fieldName and it's type
      *
      * @param fieldName the of the field
      * @param fieldType the jvmType of the field we want to get
      * @pre This method should only be called if field with the specified fieldName matches the jvmType,
      *      to avoid verifier errors
      */
    def emitPutStatic[S](fieldName: String, fieldType: NJvmType): F[S] = {
      mv.visitFieldInsn(PUTSTATIC, ct.name.toInternalName, fieldName, fieldType.toDescriptor)
      this.asInstanceOf[F[S]]
    }


    /**
      * Emits a series of instructions that get a field and boxes it
      * @pre the field should be of a primitive type i.e to allow boxing
      *
      * @param fieldName the of the field
      * @param fieldType the jvmType of the field we want to get
      */
    def emitGetFieldAndBox[S](fieldName: String, fieldType: NJvmType): F[S] = {

      /**
        * This method will box the primitive on top of the stack
        */
      def box(boxedObjectInternalName: String, signature: String): Unit = {
        mv.visitTypeInsn(NEW, boxedObjectInternalName)
        mv.visitInsn(DUP)
        mv.visitVarInsn(ALOAD, 0)
        mv.visitFieldInsn(GETFIELD, ct.name.toInternalName, fieldName, fieldType.toDescriptor)
        mv.visitMethodInsn(INVOKESPECIAL, boxedObjectInternalName, "<init>", signature, false)
      }

      // based on the type of the field, we pick the appropriate class that boxes the primitive
      fieldType match {
        case PrimBool => box(JvmName.Boolean.toInternalName, getMethodDescriptor(List(PrimBool), Void))
        case PrimChar => box(JvmName.Character.toInternalName, getMethodDescriptor(List(PrimChar), Void))
        case PrimByte => box(JvmName.Byte.toInternalName, getMethodDescriptor(List(PrimByte), Void))
        case PrimShort => box(JvmName.Short.toInternalName, getMethodDescriptor(List(PrimShort), Void))
        case PrimInt => box(JvmName.Integer.toInternalName, getMethodDescriptor(List(PrimInt), Void))
        case PrimLong => box(JvmName.Long.toInternalName, getMethodDescriptor(List(PrimLong), Void))
        case PrimFloat => box(JvmName.Float.toInternalName, getMethodDescriptor(List(PrimFloat), Void))
        case PrimDouble => box(JvmName.Double.toInternalName, getMethodDescriptor(List(PrimDouble), Void))
        case _ => throw InternalCompilerException(s"Unexpected type $fieldType")
      }
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
      * Emits a putField instruction given the fieldName, it's type and the class where the field is
      *
      * @param fieldName the of the field
      * @param ct the reference of the class which contains the field we want to get
      * @param fieldType the jvmType of the field we want to get
      * @pre This method should only be called if field with the specified fieldName matches the jvmType,
      *      to avoid verifier errors
      */
    def emitPutField[S](fieldName: String, ct: Reference, fieldType: NJvmType): F[S] = {
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


    def emitInvoke2[S](invokeCode: JvmModifier, className: String, methodName: String,
                       descriptor: String): F[S] = {

      //Last argument is true if invoking interface
      val flag = if (invokeCode == JvmModifier.InvokeInterface) true else false
      mv.visitMethodInsn(invokeCode.toInternal, className, methodName,descriptor, flag)
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



    def emitIadd[S]: F[S] = {
      mv.visitInsn(IADD)
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
    def emitDup[S]: F[S] = {
      mv.visitInsn(DUP)
      this.asInstanceOf[F[S]]
    }

    def emitDup2[S]: F[S] = {
      mv.visitInsn(DUP2)
      this.asInstanceOf[F[S]]
    }

    def emitInstanceOf[S](jt : Reference): F[S] = {
      mv.visitTypeInsn(INSTANCEOF, jt.name.toInternalName)
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

    def emitIfICmpGe[S](label: Label): F[S] = {
      mv.visitJumpInsn(IF_ICMPGE, label)
      this.asInstanceOf[F[S]]
    }

    def emitIfACmpEq[S](label: Label): F[S] = {
      mv.visitJumpInsn(IF_ACMPEQ, label)
      this.asInstanceOf[F[S]]
    }

    def emitIfICmpLt[S](label: Label): F[S] = {
      mv.visitJumpInsn(IF_ICMPLT, label)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emit a ifnonnull instruction with a given label
      */
    def emitIfNonNull[S](label: Label): F[S] = {
      mv.visitJumpInsn(IFNONNULL, label)
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

    /**
      * Emits a check cast instruction to the provided jt
      *
      */
    def emitCheckCast[S](jt: NJvmType.Reference): F[S] = {
      mv.visitTypeInsn(CHECKCAST, jt.name.toInternalName)
      this.asInstanceOf[F[S]]
    }

    def emitCheckCastArray[S](jt: NJvmType.Reference): F[S] = {
      mv.visitTypeInsn(CHECKCAST, "[" + jt.name.toInternalName)
      this.asInstanceOf[F[S]]
    }


    def emitArrayLength[S]: F[S] = {
      mv.visitInsn(ARRAYLENGTH)
      this.asInstanceOf[F[S]]
    }

    def emitCheckCastArray2[S](jt:String): F[S] = {
      mv.visitTypeInsn(CHECKCAST, jt)
      this.asInstanceOf[F[S]]
    }

    def emitSwap[S]: F[S] = {
      mv.visitInsn(SWAP)
      this.asInstanceOf[F[S]]
    }

    def emitPop[S]: F[S] = {
      mv.visitInsn(POP)
      this.asInstanceOf[F[S]]
    }

    def emitDupX2[S]: F[S] = {
      mv.visitInsn(DUP2_X2)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a i2l instruction
      *
      */
    def emitI2L[S]: F[S] = {
      mv.visitInsn(I2L)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a new array instruction to the provided jt
      *
      */
    def emitNewArray[S](jt: NJvmType.Reference): F[S] = {

      mv.visitTypeInsn(ANEWARRAY, jt.name.toInternalName)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a const null instruction
      *
      */
    def emitConstNull[S]: F[S] = {
      mv.visitInsn(ACONST_NULL)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a AASTORE
      *
      */
    def emitAAStore[S]: F[S] = {
      mv.visitInsn(AASTORE)
      this.asInstanceOf[F[S]]
    }

    def emitAALOAD[S]: F[S] = {
      mv.visitInsn(AALOAD)
      this.asInstanceOf[F[S]]
    }


    /**
      * Emits a ICONST
      *
      */
    def emitICONST[S](i : Int): F[S] = {
      i match {
        case -1 => mv.visitInsn(ICONST_M1)
        case 0 => mv.visitInsn(ICONST_0)
        case 1 => mv.visitInsn(ICONST_1)
        case 2 => mv.visitInsn(ICONST_2)
        case 3 => mv.visitInsn(ICONST_3)
        case 4 => mv.visitInsn(ICONST_4)
        case 5 => mv.visitInsn(ICONST_5)
        case _ => throw InternalCompilerException("Unexpected integer constant " + i)
      }
      this.asInstanceOf[F[S]]
    }


    /**
      * Emits a ICONST
      *
      */
    def emitLCONST[S](l : Long): F[S] = {
      l match {
        case 0 => mv.visitInsn(LCONST_0)
        case 1 => mv.visitInsn(LCONST_1)
        case _ => throw InternalCompilerException("Unexpected integer constant " + l)
      }
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a FCONST
      *
      */
    def emitFCONST[S](f : Float): F[S] = {
      f match {
        case 0f => mv.visitInsn(FCONST_0)
        case 1f => mv.visitInsn(FCONST_1)
        case 2f => mv.visitInsn(FCONST_2)
        case _ => throw InternalCompilerException("Unexpected integer constant " + f)
      }
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a DCONST
      *
      */
    def emitDCONST[S](d : Double): F[S] = {
      d match {
        case 0d => mv.visitInsn(DCONST_0)
        case 1d => mv.visitInsn(DCONST_1)
        case _ => throw InternalCompilerException("Unexpected integer constant " + d)
      }
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a BIPUSH
      *
      */
    def emitBIPUSH[S](b : Byte): F[S] = {
      mv.visitIntInsn(BIPUSH, b)
      this.asInstanceOf[F[S]]
    }

    /**
      * Emits a SIPUSH
      *
      */
    def emitSIPUSH[S](s : Short): F[S] = {
      mv.visitIntInsn(SIPUSH, s)
      this.asInstanceOf[F[S]]
    }

    /**
      * Adds source line for debugging
      *
      */
    def addSourceLine[S](loc: SourceLocation): F[S] = {
      val label = new Label()
      mv.visitLabel(label)
      mv.visitLineNumber(loc.beginLine, label)
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

  trait MnemonicsPrimTypes extends MnemonicsTypes

  trait MObject extends MnemonicsTypes

  trait MString extends MObject

  trait MUnit extends MObject

  trait MBigInt extends MObject

  trait MProxyObject extends MObject

  trait MFunction extends MObject

  trait MRefiedSource extends MObject

  trait MHoleError extends MObject

  trait MConstraintSystem extends MObject

  trait MSpawn extends MObject

  trait MChannel extends MObject


  object MnemonicsTypes {

    case class MVoid() extends MnemonicsPrimTypes

    case class MBool() extends MnemonicsPrimTypes

    case class MChar() extends MnemonicsPrimTypes

    case class MByte() extends MnemonicsPrimTypes

    case class MShort() extends MnemonicsPrimTypes

    case class MInt() extends MnemonicsPrimTypes

    case class MLong() extends MnemonicsPrimTypes

    case class MFloat() extends MnemonicsPrimTypes

    case class MDouble() extends MnemonicsPrimTypes

    class Ref[+T <: MnemonicsTypes] extends MnemonicsTypes

    class MArray[+T <: MnemonicsTypes] extends MnemonicsTypes

  }

  private val tPrimBool = typeOf[MBool]
  private val tPrimChar = typeOf[MChar]
  private val tPrimByte = typeOf[MByte]
  private val tPrimShort = typeOf[MShort]
  private val tPrimInt = typeOf[MInt]
  private val tPrimLong = typeOf[MLong]
  private val tPrimFloat = typeOf[MFloat]
  private val tPrimDouble = typeOf[MDouble]
  private val tString = typeOf[Ref[MString]]
  private val tObject = typeOf[Ref[MObject]]
  private val tProxy = typeOf[Ref[MProxyObject]]
  private val tFunction = typeOf[Ref[MFunction]]
  private val tRefiedSource = typeOf[Ref[MRefiedSource]]
  private val tHoleError = typeOf[Ref[MHoleError]]
  private val tConstraintSystem = typeOf[Ref[MConstraintSystem]]
  private val tSpawn = typeOf[Ref[MSpawn]]
  private val tChannel = typeOf[Ref[MChannel]]


  private val tUnit = typeOf[Ref[MUnit]]
  private val tBoolean = typeOf[Ref[MBool]]
  private val tCharacter = typeOf[Ref[MChar]]
  private val tByte = typeOf[Ref[MByte]]
  private val tShort = typeOf[Ref[MShort]]
  private val tInteger = typeOf[Ref[MInt]]
  private val tLong = typeOf[Ref[MLong]]
  private val tFloat = typeOf[Ref[MFloat]]
  private val tDouble = typeOf[Ref[MDouble]]
  private val tArrayString = typeOf[MArray[MString]]
  private val tArrayObject = typeOf[MArray[MObject]]
  private val tArrayProxy = typeOf[MArray[MProxyObject]]

  private val tContext = typeOf[Ref[Context]]
  private val tMain = typeOf[Ref[Main[_]]]
  private val tRecordInterface = typeOf[Ref[RecordInterface]]
  private val tRecordEmpty = typeOf[Ref[RecordEmpty]]
  private val tRecordExtendBool = typeOf[Ref[RecordExtend[MBool]]]
  private val tRecordExtendChar = typeOf[Ref[RecordExtend[MChar]]]
  private val tRecordExtendByte = typeOf[Ref[RecordExtend[MByte]]]
  private val tRecordExtendShort = typeOf[Ref[RecordExtend[MShort]]]
  private val tRecordExtendInt = typeOf[Ref[RecordExtend[MInt]]]
  private val tRecordExtendLong = typeOf[Ref[RecordExtend[MLong]]]
  private val tRecordExtendFloat = typeOf[Ref[RecordExtend[MFloat]]]
  private val tRecordExtendDouble = typeOf[Ref[RecordExtend[MDouble]]]
  private val tRecordExtendObj = typeOf[Ref[RecordExtend[Ref[MObject]]]]

  private val tRefBool = typeOf[Ref[RefClass[MBool]]]
  private val tRefChar = typeOf[Ref[RefClass[MChar]]]
  private val tRefByte = typeOf[Ref[RefClass[MByte]]]
  private val tRefShort = typeOf[Ref[RefClass[MShort]]]
  private val tRefInt = typeOf[Ref[RefClass[MInt]]]
  private val tRefLong = typeOf[Ref[RefClass[MLong]]]
  private val tRefFloat = typeOf[Ref[RefClass[MFloat]]]
  private val tRefDouble = typeOf[Ref[RefClass[MDouble]]]
  private val tRefObj = typeOf[Ref[RefClass[Ref[MObject]]]]

  private val tContIfaceBool = typeOf[Ref[ContinuationInterface[MBool]]]
  private val tContIfaceChar = typeOf[Ref[ContinuationInterface[MChar]]]
  private val tContIfaceByte = typeOf[Ref[ContinuationInterface[MByte]]]
  private val tContIfaceShort = typeOf[Ref[ContinuationInterface[MShort]]]
  private val tContIfaceInt = typeOf[Ref[ContinuationInterface[MInt]]]
  private val tContIfaceLong = typeOf[Ref[ContinuationInterface[MLong]]]
  private val tContIfaceFloat = typeOf[Ref[ContinuationInterface[MFloat]]]
  private val tContIfaceDouble = typeOf[Ref[ContinuationInterface[MDouble]]]
  private val tContIfaceObj = typeOf[Ref[ContinuationInterface[Ref[MObject]]]]

  /**
    * This function allows to create Runtime values by using a compile time value.
    * It takes a type parameter T and maps it to the correct runtime JvmType value
    *
    * This is used to circumvent the use of dependent types which are not supported by scala.
    * It allows us to have a framework which more type-safe compared to a version which doesn't use this features
    */
  final def getJvmType[T <: MnemonicsTypes : TypeTag](implicit root: Root, flix: Flix): NJvmType = {
    typeOf[T] match {
      //    case t if t =:= typeOf[Void] => NJvmType.Void
      case t if t =:= tPrimBool => PrimBool
      case t if t =:= tPrimChar => PrimChar
      case t if t =:= tPrimByte => PrimByte
      case t if t =:= tPrimShort => PrimShort
      case t if t =:= tPrimInt => PrimInt
      case t if t =:= tPrimLong => PrimLong
      case t if t =:= tPrimFloat => PrimFloat
      case t if t =:= tPrimDouble => PrimDouble

      case t if t =:= tString => NJvmType.String
      case t if t =:= tObject => NJvmType.Object
      case t if t =:= tUnit => NJvmType.Unit
      case t if t =:= tProxy => Reference(JvmName.ProxyObject)
      case t if t =:= tFunction => Reference(JvmName.Function)
      case t if t =:= tRefiedSource => Reference(JvmName.Runtime.ReifiedSourceLocation)
      case t if t =:= tHoleError => Reference(JvmName.Runtime.HoleError)
      case t if t =:= tConstraintSystem => Reference(JvmName.Runtime.Fixpoint.ConstraintSystem)
      case t if t =:= tSpawn => NJvmType.Spawnable
      case t if t =:= tChannel => Reference(JvmName.Channel)

      case t if t =:= tBoolean => Reference(JvmName.Boolean)
      case t if t =:= tCharacter => Reference(JvmName.Character)
      case t if t =:= tByte => Reference(JvmName.Byte)
      case t if t =:= tShort => Reference(JvmName.Short)
      case t if t =:= tInteger => Reference(JvmName.Integer)
      case t if t =:= tLong => Reference(JvmName.Long)
      case t if t =:= tFloat => Reference(JvmName.Float)
      case t if t =:= tDouble => Reference(JvmName.Double)

      case t if t =:= tArrayString => JArray(NJvmType.String)
      case t if t =:= tArrayObject => JArray(NJvmType.Object)
      case t if t =:= tArrayProxy => JArray(NJvmType.ProxyObject)


      case t if t =:= tContext => NJvmType.Context

      case t if t =:= tMain => Reference(JvmName(RootPackage, "Main"))

      case t if t =:= tRecordInterface =>
        val name = "IRecord"
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tRecordEmpty =>
        val name = "RecordEmpty"
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tRecordExtendBool =>
        val name = "RecordExtend$" + stringify(PrimBool)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tRecordExtendChar =>
        val name = "RecordExtend$" + stringify(PrimChar)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tRecordExtendByte =>
        val name = "RecordExtend$" + stringify(PrimByte)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tRecordExtendShort =>
        val name = "RecordExtend$" + stringify(PrimShort)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tRecordExtendInt =>
        val name = "RecordExtend$" + stringify(PrimInt)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tRecordExtendLong =>
        val name = "RecordExtend$" + stringify(PrimLong)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tRecordExtendFloat =>
        val name = "RecordExtend$" + stringify(PrimFloat)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tRecordExtendDouble =>
        val name = "RecordExtend$" + stringify(PrimDouble)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tRecordExtendObj =>
        val name = "RecordExtend$" + stringify(Reference(JvmName.Object))
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))

      case t if t =:= tRefBool =>
        val name = "Ref$" + stringify(PrimBool)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tRefChar =>
        val name = "Ref$" + stringify(PrimChar)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tRefByte =>
        val name = "Ref$" + stringify(PrimByte)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tRefShort =>
        val name = "Ref$" + stringify(PrimShort)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tRefInt =>
        val name = "Ref$" + stringify(PrimInt)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tRefLong =>
        val name = "Ref$" + stringify(PrimLong)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tRefFloat =>
        val name = "Ref$" + stringify(PrimFloat)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tRefDouble =>
        val name = "Ref$" + stringify(PrimDouble)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tRefObj =>
        val name = "Ref$" + stringify(Reference(JvmName.Object))
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))

      case t if t =:= tContIfaceBool =>
        val name = "Cont$" + stringify(PrimBool)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tContIfaceChar =>
        val name = "Cont$" + stringify(PrimChar)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tContIfaceByte =>
        val name = "Cont$" + stringify(PrimByte)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tContIfaceShort =>
        val name = "Cont$" + stringify(PrimShort)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tContIfaceInt =>
        val name = "Cont$" + stringify(PrimInt)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tContIfaceLong =>
        val name = "Cont$" + stringify(PrimLong)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tContIfaceFloat =>
        val name = "Cont$" + stringify(PrimFloat)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tContIfaceDouble =>
        val name = "Cont$" + stringify(PrimDouble)
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))
      case t if t =:= tContIfaceObj =>
        val name = "Cont$" + stringify(Reference(JvmName.Object))
        // The type resides in the root package.
        Reference(JvmName(RootPackage, name))

      case _ => NJvmType.Object
    }
  }

  def getJvmType(tpe: MonoType)(implicit root: Root, flix: Flix): NJvmType = tpe match {
    // Primitives
    case MonoType.Unit => Unit
    case MonoType.Bool => PrimBool
    case MonoType.Char => PrimChar
    case MonoType.Float32 => PrimFloat
    case MonoType.Float64 => PrimDouble
    case MonoType.Int8 => PrimByte
    case MonoType.Int16 => PrimShort
    case MonoType.Int32 => PrimInt
    case MonoType.Int64 => PrimLong
    case MonoType.BigInt => BigInteger
    case MonoType.Str => NJvmType.String

    // Compound
    case MonoType.Array(_) => NJvmType.Object
    case MonoType.Channel(_) => NJvmType.Object
    case MonoType.Ref(_) => getRefClassType(tpe)
    case MonoType.Tuple(elms) => getTupleInterfaceType(elms.map(getErasedJvmType))
    case MonoType.RecordEmpty() => getRecordInterfaceType
    case MonoType.RecordExtend(label, value, rest) => getRecordInterfaceType
    case MonoType.Enum(sym, elms) => getEnumInterfaceType(sym, elms.map(getErasedJvmType))
    case MonoType.Arrow(targs, tresult) => getFunctionInterfaceType(targs.map(getErasedJvmType), getErasedJvmType(tresult))
    case MonoType.Relation(sym, attr) => Reference(JvmName.PredSym)
    case MonoType.Native(clazz) => NJvmType.Object
    case MonoType.SchemaEmpty() => Reference(JvmName.Runtime.Fixpoint.ConstraintSystem)
    case MonoType.SchemaExtend(_, _, _) => Reference(JvmName.Runtime.Fixpoint.ConstraintSystem)

    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
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
      * Returns without value
      */
    def RETURN_VOID[T <: MnemonicsTypes : TypeTag](implicit root: Root, flix: Flix): F[StackNil ** T] => F[StackNil] = {
      t => t.emitReturnVoid()
    }


    def RETURN_VOID2[S<: Stack](implicit root: Root, flix: Flix): F[S] => F[StackNil] = {
      t => t.emitReturnVoid()
    }

    /**
      * Pushes the result of adding the two top-most ints.
      */
    def IADD[S <: Stack]: F[S ** MInt ** MInt] => F[S ** MInt] =
      t => t.emitIadd

    /**
      * Creates a new instance of the type parameter T which should be a subtype of JvmType.Reference and puts
      * it on top of the stack
      */
    def NEW[S <: Stack, T <: Ref[MObject]](jt: Reference)(implicit root: Root, flix: Flix): F[S] => F[S ** T] =
      t => t.emitNew(jt)

    /**
      * Duplicates whatever is on Top of the stack
      */
    def DUP[S <: Stack, T <: MnemonicsTypes : TypeTag]: F[S ** T] => F[S ** T ** T] =
      t => t.emitDup

    def DUP2[S <: Stack, T1 <: MnemonicsTypes : TypeTag, T2 <: MnemonicsTypes : TypeTag]: F[S ** T1 ** T2] => F[S ** T1 ** T2 ** T1 ** T2] =
      t => t.emitDup2

    def INSTANCE_OF[S <: Stack, T <: MnemonicsTypes : TypeTag](jt : Reference): F[S ** T] => F[S ** MBool] =
      t => t.emitInstanceOf(jt)

    /**
      * Loads a constant string onto the stack
      */
    def LDC_STRING[S <: Stack](value: String): F[S] => F[S ** Ref[MString]] =
      t => t.emitLdc(value)

    /**
      * Loads a constant int onto the stack
      */
    def LDC_INT[S <: Stack](value: Int): F[S] => F[S ** MInt] =
      t => t.emitLdc(value.asInstanceOf[Object])


    /**
      * Loads a constant int onto the stack
      */
    def LDC_LONG[S <: Stack](value: Long): F[S] => F[S ** MLong] =
      t => t.emitLdc(value.asInstanceOf[Object])


    /**
      * Transform an integer into a long
      */
    def I2L[S <: Stack]: F[S ** MInt] => F[S ** MLong] =
      t => t.emitI2L

    /**
      * Loads a constant float onto the stack
      */
    def LDC_FLOAT[S <: Stack](value: Float): F[S] => F[S ** MFloat] =
      t => t.emitLdc(value.asInstanceOf[Object])

    /**
      * Loads a constant double onto the stack
      */
    def LDC_DOUBLE[S <: Stack](value: Double): F[S] => F[S ** MDouble] =
      t => t.emitLdc(value.asInstanceOf[Object])

    /**
      * Loads a constant int onto the stack
      */
    def CONST_NULL[S <: Stack, T <: Ref[MObject]]: F[S] => F[S ** T] =
      t => t.emitConstNull

    /**
      * Loads a constant int onto the stack
      */
    def NEWARRAY[S <: Stack, T <: MObject : TypeTag](implicit root: Root, flix: Flix): F[S ** MInt] => F[S ** MArray[Ref[T]]] =
      t => t.emitNewArray(getJvmType[Ref[T]].asInstanceOf[Reference])

    /**
      * Throws the exception which is currently on top of the stack
      */
    def THROW: F[StackNil ** Ref[MObject]] => F[StackNil] =
      t => t.emitThrow()

    def THROW2[S <: Stack]: F[S ** Ref[MObject]] => F[S] =
      t => t.emitThrow()
    /**
      * Instruction which allows us to do if statements
      * In this case if the bool on top is true then we do the series of instructions provided in f
      */
    def IFEQ[S <: Stack](f: F[S] => F[S]): F[S ** MBool] => F[S] = {
      val skipLabel = new Label
      ((t: F[S ** MBool]) => t.emitIfeq[S](skipLabel)) |>>
        f |>>
        (t => t.emitLabel(skipLabel))
    }

    def IF_ACMPEQ[S <: Stack, T1 <: Ref[MObject], T2 <: Ref[MObject]](f: F[S] => F[S]): F[S ** T1 ** T2 ] => F[S] = {
      val skipLabel = new Label
      ((t: F[S ** T1 ** T2]) => t.emitIfACmpEq[S](skipLabel)) |>>
        f |>>
        (t => t.emitLabel(skipLabel))
    }


    def FOR_GE[S <: Stack](init: F[S] => F[S], test : F[S] => F[S ** MInt ** MInt], body : F[S] => F[S] ): F[S] => F[S] = {
      val loopEntry = new Label()
      val loopExit = new Label()

      init |>>
        (t => t.emitLabel[S](loopEntry)) |>>
        test |>>
        ((t : F[S**MInt ** MInt]) => t.emitIfICmpGe[S](loopExit)) |>>
        body |>>
        (t => t.emitGoto[S](loopEntry)) |>>
        (t => t.emitLabel[S](loopExit))

    }

    def IFEQ_ELSE[S <: Stack, T1 <: MnemonicsTypes](f1: F[S] => F[S**T1], f2 : F[S] => F[S**T1]): F[S ** MBool] => F[S ** T1] = {
      val ifElse = new Label
      val ifEnd = new Label

      ((t: F[S ** MBool]) => t.emitIfeq[S](ifElse)) |>>
        f1 |>>
        (t => t.emitGoto[S](ifEnd)) |>>
        (t => t.emitLabel[S](ifElse)) |>>
        f2 |>>
        (t => t.emitLabel(ifEnd))

    }

    /**
      * Instruction which allows us to do , do while loops
      */
    def IFNONNULL[S <: Stack](f: F[S] => F[S ** Ref[MObject]]): F[S] => F[S] = {
      val loop = new Label
      ((t: F[S]) => t.emitLabel[S](loop)) |>>
        f |>>
        (t => t.emitIfNonNull[S](loop))
    }

    /**
      * Instruction which allows us to do check casts
      */
    def CHECK_CAST[S <: Stack](jt: Reference): F[S ** Ref[MObject]] => F[S ** Ref[MObject]] =
      t => t.emitCheckCast(jt)

    def CHECK_CAST2[S <: Stack, T1 <: MnemonicsTypes, T2 <: MnemonicsTypes](jt: NJvmType.Reference): F[S ** T1] => F[S ** T2] =
      t => t.emitCheckCast(jt)

    def CHECK_CAST_ARRAY[S <: Stack, T1 <: MnemonicsTypes, T2 <: MnemonicsTypes](jt: NJvmType.Reference): F[S ** T1] => F[S ** T2] = {
      t => t.emitCheckCastArray(jt)
    }


    def ARRAYLENGTH[S <: Stack, T1 <: MArray[_]]: F[S ** T1] => F[S ** MInt] = {
      t => t.emitArrayLength
    }
    def CHECK_CAST_ARRAY2[S <: Stack, T1 <: MnemonicsTypes, T2 <: MnemonicsTypes](jt: String): F[S ** T1] => F[S ** T2] = {
      t => t.emitCheckCastArray2(jt)
    }

    def SWAP[S <: Stack, T1 <: MnemonicsTypes, T2 <: MnemonicsTypes]: F[S ** T1 ** T2] => F[S ** T2 ** T1] =
      t => t.emitSwap

    def POP[S <: Stack, T <: MnemonicsTypes]: F[S ** T] => F[S] =
      t => t.emitPop

    def DUP_X2_1[S <: Stack, T1 <: MnemonicsTypes,  T2 <: MnemonicsTypes,  T3 <: MnemonicsTypes]: F[S ** T1 ** T2 ** T3] => F[S** T3 ** T1 ** T2 ** T3] =
      t => t.emitDupX2

    def DUP_X2_2[T1 <: MnemonicsTypes,  T2 <: MnemonicsTypes]: F[StackNil ** T1 ** T2 ] => F[StackNil ** T2 ** T1 ** T2] =
      t => t.emitDupX2

    def Unchecked_DUP_X2[S1 <: Stack, S2 <: Stack]: F[S1] => F[S2] =
      t => t.emitDupX2

    /**
      * Instruction which is a NO_OP, usefull when we need to accumulate instructions this can be the initial one.
      * It is simply the indentity function
      */
    def NO_OP[S <: Stack]: F[S] => F[S] = t => t

    /**
      * Instruction which store an object in an array
      */
    def AASTORE[S <: Stack, T <: Ref[MObject], T1 <: Ref[_]]: F[S ** MArray[T] ** MInt ** T1] => F[S] = t => t.emitAAStore

    def AALOAD[S <: Stack, T <: Ref[MObject], T1 <: Ref[_]]: F[S ** MArray[T] ** MInt ] => F[S ** T1] = t => t.emitAAStore

    def ADD_SOURCE_LINE[S <: Stack](loc : SourceLocation): F[S] => F[S] = t => t.addSourceLine(loc)

    def TRUE[S <: Stack] : F[S] => F[S ** MBool] = t => t.emitICONST(1)
    def FALSE[S <: Stack] : F[S] => F[S ** MBool] = t => t.emitICONST(0)

    def ICONST[S <: Stack](i : Int) : F[S] => F[S ** MInt] = t => t.emitICONST(i)
    def LCONST[S <: Stack](l : Long) : F[S] => F[S ** MLong] = t => t.emitLCONST(l)


    def FCONST[S <: Stack](f : Float) : F[S] => F[S ** MFloat] = t => t.emitFCONST(f)
    def DCONST[S <: Stack](d : Double) : F[S] => F[S ** MDouble] = t => t.emitDCONST(d)


    def BIPUSH[S <: Stack](b : Byte) : F[S] => F[S ** MByte] = t => t.emitBIPUSH(b)
    def SIPUSH[S <: Stack](s : Short) : F[S] => F[S ** MShort] = t => t.emitSIPUSH(s)

    def IGE[S <: Stack] : F[S ** MInt ** MInt] => F[S** MBool] = {


      val condElse = new Label()
      val condEnd = new Label()
      ((t :  F[S ** MInt ** MInt]) => t.emitIfICmpLt[S](condElse)) |>>
      (t => t.emitICONST[S](1)) |>>
      (t => t.emitGoto[S](condEnd)) |>>
      (t => t.emitLabel[S](condElse)) |>>
      (t => t.emitICONST[S](0)) |>>
      (t => t.emitLabel[S ** MBool](condEnd))
    }
  }

  /**
    * Capability which allows to load/store a local variable
    */
  class Local[T <: MnemonicsTypes : TypeTag](offset: Integer)(implicit root: Root, flix: Flix) {

    private val localType = getJvmType[T]

    def LOAD[S <: Stack]: F[S] => F[S ** T] = t => t.emitLoad(localType, offset)

    def STORE[S <: Stack]: F[S ** T] => F[S] = t => t.emitStore(localType, offset)
  }

  class ArrayLocal[T <: MnemonicsTypes : TypeTag](offset: Integer)(implicit root: Root, flix: Flix) {

    private val elementType = getJvmType[T]

    def LOAD_ARRAY[S <: Stack]: F[S] => F[S ** MArray[T]] = t => t.emitLoad(Reference(JvmName.Object), offset)

    def STORE_ARRAY[S <: Stack]: F[S ** MArray[T]] => F[S] = t => t.emitStore(Reference(JvmName.Object), offset)

    def LOAD_ARRAY_ELEMENT[S <: Stack]: F[S ** MArray[T] ** MInt] => F[S ** T] = t => t.emitArrayLoad(elementType)

    def STORE_ARRAY_ELEMENT[S <: Stack]: F[S ** MArray[T] ** MInt ** T] => F[S] = t => t.emitArrayStore(elementType)

  }
  class PrimArrayLocal[T <: MnemonicsPrimTypes : TypeTag](offset: Integer)(implicit root: Root, flix: Flix) {

  }

  /**
    * Unchecked Capability which allows to load/store a local variable
    */
  class UncheckedLocal(localType: NJvmType, offset: Integer)(implicit root: Root, flix: Flix) {

    private val extraOffset = localType match {
      case PrimLong | PrimDouble => 1
      case _ => 0
    }

    def LOAD[S <: Stack, T <: MnemonicsTypes]: F[S] => F[S ** T] = t => t.emitLoad(localType, offset)

    def STORE[S <: Stack, T <: MnemonicsTypes]: F[S ** T] => F[S] = t => t.emitStore(localType, offset)
  }

  /**
    * Capability which allows to get/put a field
    */
  class Field[T <: MnemonicsTypes : TypeTag](fieldName: String)(implicit root: Root, flix: Flix) {

    protected val fieldType: NJvmType = getJvmType[T]

    def GET_FIELD[S <: Stack, T1 <: Ref[MObject]]: F[S ** T1] => F[S ** T] = t => t.emitGetField(fieldName, fieldType)

    def PUT_FIELD[S <: Stack, T1 <: Ref[MObject]]: F[S ** T1 ** T] => F[S] = t => t.emitPutField(fieldName, fieldType)
  }

  /**
    * Capability which allows to get/put a field a prim field. it's diferent from Field as this allows to box
    * values as the type is a prim type
    */
  class PrimField[T <: MnemonicsPrimTypes : TypeTag](fieldName: String)(implicit root: Root, flix: Flix) extends Field[T](fieldName) {

    def GET_BOXED_FIELD[S <: Stack]: F[S] => F[S ** Ref[T]] = t => t.emitGetFieldAndBox(fieldName, fieldType)
  }


  /**
    * Capability which allows to get/put a field in the given ct class.
    * This method is partially unchecked as we don't encode the ct in the type system as a type parameter
    */
  class ClassField[T <: MnemonicsTypes : TypeTag](fieldName: String, ct: Reference)(implicit root: Root, flix: Flix) {

    protected val fieldType: NJvmType = getJvmType[T]

    def GET_FIELD[S <: Stack, T1 <: Ref[MObject]]: F[S ** T1] => F[S ** T] = t => t.emitGetField(fieldName, ct, fieldType)

    def PUT_FIELD[S <: Stack, T1 <: Ref[MObject]]: F[S ** T1 ** T] => F[S] = t => t.emitPutField(fieldName, ct, fieldType)
  }

  /**
    * Unchecked capability which allows to get/put a field
    */
  class UncheckedField(fieldName: String, fieldType: NJvmType)(implicit root: Root, flix: Flix) {

    def GET_FIELD[S <: Stack, T1 <: Ref[MObject], T2 <: MnemonicsTypes]: F[S ** T1] => F[S ** T2] = t => t.emitGetField(fieldName, fieldType)

    def PUT_FIELD[S <: Stack, T1 <: Ref[MObject], T2 <: MnemonicsTypes]: F[S ** T1 ** T2] => F[S] = t => t.emitPutField(fieldName, fieldType)
  }


  /**
    * Unchecked Capability which allows to get/put a field in the given ct class.
    * This method is completely unchecked as we don't encode the ct in the type system as a type parameter nor the fieldtype
    */
  class UncheckedClassField(fieldName: String, ct: Reference, fieldType: NJvmType)(implicit root: Root, flix: Flix) {

    def GET_FIELD[S <: Stack, T1 <: Ref[MObject], T2 <: MnemonicsTypes]: F[S ** T1] => F[S ** T2] = t => t.emitGetField(fieldName, ct, fieldType)

    def PUT_FIELD[S <: Stack, T1 <: Ref[MObject], T2 <: MnemonicsTypes]: F[S ** T1 ** T2] => F[S] = t => t.emitPutField(fieldName, ct, fieldType)
  }


  /**
    * Unchecked capability which allows to get/put a static field
    */
  class UncheckedStaticField(fieldName: String, fieldType: NJvmType)(implicit root: Root, flix: Flix) {

    def GET_STATIC[S <: Stack, T <: MnemonicsTypes]: F[S] => F[S ** T] = t => t.emitGetStatic(fieldName, fieldType)

    def PUT_STATIC[S <: Stack, T <: MnemonicsTypes]: F[S ** T] => F[S] = t => t.emitPutStatic(fieldName, fieldType)
  }

  class MLabel(label: Label){
    def GOTO[S <: Stack] : F[S] => F[S] = t => t.emitGoto(label)

    def EMIT_LABEL[S <: Stack] :  F[S] => F[S] = t => t.emitLabel(label)
  }


  /**
    * Capability which allows acess the function locals in this case a function with 0 arguments
    */
  //TODO: Maybe need to have different types for signatures of static method as the 1st local (0 index) of static
  //TODO: functions isn't a local of type JvmType.Reference
  class FunSig0[R <: MnemonicsTypes : TypeTag]() {
  }

  /**
    * Capability which allows acess the function locals in this case a function with 1 argument
    */
  //TODO: Same as FunSig0
  class FunSig1[T1 <: MnemonicsTypes : TypeTag, R <: MnemonicsTypes : TypeTag] {

    def getArg1(implicit root: Root, flix: Flix): Local[T1] = new Local(0)
  }

  /**
    * Capability which allows acess the function locals in this case a function with 2 arguments
    */
  //TODO: Same as FunSig0
  class FunSig2[T1 <: MnemonicsTypes : TypeTag, T2 <: MnemonicsTypes : TypeTag, R <: MnemonicsTypes : TypeTag] {

    def getArg1(implicit root: Root, flix: Flix): Local[T1] = new Local(0)

    def getArg2(implicit root: Root, flix: Flix): Local[T2] = new Local(1)
  }

  /**
    * Capability which allows acess the function locals in this case a function with 3 arguments
    */
  //TODO: Same as FunSig0
  class FunSig3[T1 <: MnemonicsTypes : TypeTag, T2 <: MnemonicsTypes : TypeTag, T3 <: MnemonicsTypes : TypeTag, R <: MnemonicsTypes : TypeTag] {

    def getArg1(implicit root: Root, flix: Flix): Local[T1] = new Local(0)

    def getArg2(implicit root: Root, flix: Flix): Local[T2] = new Local(1)

    def getArg3(implicit root: Root, flix: Flix): Local[T3] = new Local(2)

  }

  /**
    * Capability which allows acess the function locals in this case a function with 4 arguments
    */
  //TODO: Same as FunSig0
  class FunSig4[T1 <: MnemonicsTypes : TypeTag, T2 <: MnemonicsTypes : TypeTag, T3 <: MnemonicsTypes : TypeTag, T4 <: MnemonicsTypes : TypeTag, R <: MnemonicsTypes : TypeTag] {

    def getArg1(implicit root: Root, flix: Flix): Local[T1] = new Local(0)

    def getArg2(implicit root: Root, flix: Flix): Local[T2] = new Local(1)

    def getArg3(implicit root: Root, flix: Flix): Local[T3] = new Local(2)

    def getArg4(implicit root: Root, flix: Flix): Local[T4] = new Local(3)
  }

  /**
    * Unchecked apability which allows acess the function locals in this case a function with 4 arguments
    */
  //TODO: Same as FunSig0
  class UncheckedFunSig(args: List[NJvmType], returnType: NJvmType)(implicit root: Root, flix: Flix) {

    for ((arg, index) <- args.zipWithIndex) {
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
        case _ => throw InternalCompilerException(s"Unexpected type $arg")
      }
    }

    def getArg[T1 <: MnemonicsTypes : TypeTag](offset: Int): Local[T1] = {
      new Local[T1](offset)
    }
  }

  /**
    * Capability which allows to invoke a (non-void) method with 0 arguments
    */
  //TODO: Similar to FunSig might need to have different types in order to ensure the type safety with
  //TODO: Static methods, interface methods, etc.
  class Method0[R <: MnemonicsTypes : TypeTag](invokeCode: JvmModifier, ct: Reference, methodName: String) {

    def INVOKE[S <: Stack](implicit root: Root, flix: Flix): F[S] => F[S ** R] = {
      val argsList = invokeCode match {
        case JvmModifier.InvokeStatic => List()
        case _ => throw InternalCompilerException("Unexpected instruction")
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
        case _ => throw InternalCompilerException("Unexpected instruction")
      }
      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, argsList, Void)
    }
  }

  /**
    * Capability which allows to invoke a (non-void) method with 1 argument
    */
  //TODO: Similar to Method0
  class Method1[T1 <: MnemonicsTypes : TypeTag, R <: MnemonicsTypes : TypeTag](invokeCode: JvmModifier, ct: Reference, methodName: String) {

    def INVOKE[S <: Stack](implicit root: Root, flix: Flix): F[S ** T1] => F[S ** R] = {
      val argsList = invokeCode match {
        case JvmModifier.InvokeStatic => List(getJvmType[T1])
        case JvmModifier.InvokeInterface | JvmModifier.InvokeSpecial | JvmModifier.InvokeVirtual => List()
        case _ => throw InternalCompilerException("Unexpected instruction")
      }
      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, argsList, getJvmType[R])
    }
  }

  /**
    * Capability which allows to invoke a (void) method with 1 argument
    */
  class VoidMethod1[T1 <: MnemonicsTypes : TypeTag](invokeCode: JvmModifier, ct: Reference, methodName: String) {
    def INVOKE[S <: Stack, T <: T1](implicit root: Root, flix: Flix): F[S ** T] => F[S] = {
      val argsList = invokeCode match {
        case JvmModifier.InvokeStatic => List(getJvmType[T1])
        case JvmModifier.InvokeInterface | JvmModifier.InvokeSpecial | JvmModifier.InvokeVirtual => List()
        case _ => throw InternalCompilerException("Unexpected instruction")
      }
      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, argsList, Void)
    }
  }

  /**
    * Capability which allows to invoke a (non-void) method with 2 arguments
    */
  //TODO: Similar to Method0
  class Method2[T1 <: MnemonicsTypes : TypeTag, T2 <: MnemonicsTypes : TypeTag, R <: MnemonicsTypes : TypeTag](invokeCode: JvmModifier, ct: Reference, methodName: String) {

    def INVOKE[S <: Stack](implicit root: Root, flix: Flix): F[S ** T1 ** T2] => F[S ** R] = {
      val argsList = invokeCode match {
        case JvmModifier.InvokeStatic => List(getJvmType[T1], getJvmType[T2])
        case JvmModifier.InvokeInterface | JvmModifier.InvokeSpecial | JvmModifier.InvokeVirtual => List(getJvmType[T2])
        case _ => throw InternalCompilerException("Unexpected instruction")
      }
      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, argsList, getJvmType[R])
    }
  }

  /**
    * Capability which allows to invoke a (void) method with 2 arguments
    */
  class VoidMethod2[T1 <: MnemonicsTypes : TypeTag, T2 <: MnemonicsTypes : TypeTag](invokeCode: JvmModifier, ct: Reference, methodName: String) {
    def INVOKE[S <: Stack](implicit root: Root, flix: Flix): F[S ** T1 ** T2] => F[S] = {
      val argsList = invokeCode match {
        case JvmModifier.InvokeStatic => List(getJvmType[T1], getJvmType[T2])
        case JvmModifier.InvokeInterface | JvmModifier.InvokeSpecial | JvmModifier.InvokeVirtual => List(getJvmType[T2])
        case _ => throw InternalCompilerException("Unexpected instruction")
      }
      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, argsList, Void)
    }
  }


  /**
    * Capability which allows to invoke a method with 3 arguments
    */
  //TODO: Similar to Method0
  class Method3[T1 <: MnemonicsTypes : TypeTag, T2 <: MnemonicsTypes : TypeTag, T3 <: MnemonicsTypes : TypeTag, R <: MnemonicsTypes : TypeTag](invokeCode: JvmModifier, ct: Reference, methodName: String) {

    def INVOKE[S <: Stack](implicit root: Root, flix: Flix): F[S ** T1 ** T2 ** T3] => F[S ** R] = {
      val argsList = invokeCode match {
        case JvmModifier.InvokeStatic => List(getJvmType[T1], getJvmType[T2], getJvmType[T3])
        case JvmModifier.InvokeInterface | JvmModifier.InvokeSpecial | JvmModifier.InvokeVirtual => List(getJvmType[T2], getJvmType[T3])
        case _ => throw InternalCompilerException("Unexpected instruction")
      }
      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, argsList, getJvmType[R])
    }
  }

  /**
    * Capability which allows to invoke a (void) method with 3 arguments
    */
  class VoidMethod3[T1 <: MnemonicsTypes : TypeTag, T2 <: MnemonicsTypes : TypeTag, T3 <: MnemonicsTypes : TypeTag](invokeCode: JvmModifier, ct: Reference, methodName: String) {
    def INVOKE[S <: Stack](implicit root: Root, flix: Flix): F[S ** T1 ** T2 ** T3] => F[S] = {
      val argsList = invokeCode match {
        case JvmModifier.InvokeStatic => List(getJvmType[T1], getJvmType[T2], getJvmType[T3])
        case JvmModifier.InvokeInterface | JvmModifier.InvokeSpecial | JvmModifier.InvokeVirtual => List(getJvmType[T2], getJvmType[T3])
        case _ => throw InternalCompilerException("Unexpected instruction")
      }
      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, argsList, Void)
    }
  }


  /**
    * Capability which allows to invoke a method with 4 arguments
    */
  //TODO: Similar to Method0
  class Method4[T1 <: MnemonicsTypes : TypeTag, T2 <: MnemonicsTypes : TypeTag, T3 <: MnemonicsTypes : TypeTag, T4 <: MnemonicsTypes : TypeTag, R <: MnemonicsTypes : TypeTag](invokeCode: JvmModifier, ct: Reference, methodName: String) {

    def INVOKE[S <: Stack](implicit root: Root, flix: Flix): F[S ** T1 ** T2 ** T3 ** T4] => F[S ** R] = {
      val argsList = invokeCode match {
        case JvmModifier.InvokeStatic => List(getJvmType[T1], getJvmType[T2], getJvmType[T3], getJvmType[T4])
        case JvmModifier.InvokeInterface | JvmModifier.InvokeSpecial | JvmModifier.InvokeVirtual => List(getJvmType[T2], getJvmType[T3], getJvmType[T4])
        case _ => throw InternalCompilerException("Unexpected instruction")
      }
      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, argsList, getJvmType[R])
    }
  }

  /**
    * Capability which allows to invoke a (void) method with 4 arguments
    */
  class VoidMethod4[T1 <: MnemonicsTypes : TypeTag, T2 <: MnemonicsTypes : TypeTag, T3 <: MnemonicsTypes : TypeTag, T4 <: MnemonicsTypes : TypeTag](invokeCode: JvmModifier, ct: Reference, methodName: String) {
    def INVOKE[S <: Stack](implicit root: Root, flix: Flix): F[S ** T1 ** T2 ** T3 ** T4] => F[S] = {
      val argsList = invokeCode match {
        case JvmModifier.InvokeStatic => List(getJvmType[T1], getJvmType[T2], getJvmType[T3], getJvmType[T4])
        case JvmModifier.InvokeInterface | JvmModifier.InvokeSpecial | JvmModifier.InvokeVirtual => List(getJvmType[T2], getJvmType[T3], getJvmType[T4])
        case _ => throw InternalCompilerException("Unexpected instruction")
      }
      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, argsList, Void)
    }
  }


  class VoidMethod6[T1 <: MnemonicsTypes : TypeTag, T2 <: MnemonicsTypes : TypeTag, T3 <: MnemonicsTypes : TypeTag,
                  T4 <: MnemonicsTypes : TypeTag,  T5 <: MnemonicsTypes : TypeTag,  T6 <: MnemonicsTypes : TypeTag](invokeCode: JvmModifier, ct: Reference, methodName: String) {
    def INVOKE[S <: Stack](implicit root: Root, flix: Flix): F[S ** T1 ** T2 ** T3 ** T4 ** T5 ** T6] => F[S] = {
      val argsList = invokeCode match {
        case JvmModifier.InvokeStatic => List(getJvmType[T1], getJvmType[T2], getJvmType[T3], getJvmType[T4], getJvmType[T5], getJvmType[T6])
        case JvmModifier.InvokeInterface | JvmModifier.InvokeSpecial | JvmModifier.InvokeVirtual => List(getJvmType[T2], getJvmType[T3], getJvmType[T4], getJvmType[T5], getJvmType[T6])
        case _ => throw InternalCompilerException("Unexpected instruction")
      }
      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, argsList, Void)
    }
  }

  /**
    * Unchecked Capability which allows to invoke a method
    */
  class UncheckedMethod(invokeCode: JvmModifier, ct: Reference, methodName: String, args: List[NJvmType], returnType: NJvmType) {

    def INVOKE[S1 <: Stack, S2 <: Stack](implicit root: Root, flix: Flix): F[S1] => F[S2] = {

      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, args, returnType)
    }
  }

  /**
    * Unchecked Capability which allows to invoke a void method
    */
  class UncheckedVoidMethod(invokeCode: JvmModifier, ct: Reference, methodName: String, args: List[NJvmType]) {

    def INVOKE[S1 <: Stack, S2 <: Stack](implicit root: Root, flix: Flix): F[S1] => F[S2] = {

      t => t.emitInvoke(invokeCode, ct.name.toInternalName, methodName, args, Void)
    }
  }

  class UncheckedVoidMethod2(invokeCode: JvmModifier, declatration: String, name: String, descriptor: String) {

    def INVOKE[S1 <: Stack, S2 <: Stack](implicit root: Root, flix: Flix): F[S1] => F[S2] = {

      t => t.emitInvoke2(invokeCode, declatration, name, descriptor)
    }
  }


  /**
    * This class allows to generate classes. It includes support to generate(CompileField) and methods
    *
    * @param ct         classType of the class we want to generate
    * @param modifiers  list of modififers which we want to generate the class with (public, abstract, etc..)
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
      * static field initialization of the provided unchecked static field
      */
    def UncheckedStaticFieldInit(field: UncheckedStaticField, f: F[StackNil] => F[StackNil]): Unit = {

      emitClassMethod(List(JvmModifier.Static), "<clinit>", List(), Void, f)
    }


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
    def mkMethod0[R <: MnemonicsTypes : TypeTag](methodName: String, f: FunSig0[R] => F[StackNil] => F[StackNil],
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
    def mkMethod1[T1 <: MnemonicsTypes : TypeTag, R <: MnemonicsTypes : TypeTag](methodName: String, f: FunSig1[T1, R] => F[StackNil] => F[StackNil],
                                                                                 modifiers: List[JvmModifier] = List(Public, Final), isStatic: Boolean = false): Method1[T1, R] = {
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
    def mkVoidMethod1[T1 <: MnemonicsTypes : TypeTag](methodName: String, f: FunSig1[T1, MVoid] => F[StackNil] => F[StackNil],
                                                      modifiers: List[JvmModifier] = List(Public, Final), isStatic: Boolean = false): VoidMethod1[T1] = {

      val arg1Type = getJvmType[T1]
      val returnType = Void
      val funSig = new FunSig1[T1, MVoid]()

      val (invokeCode, argsList) = if (isStatic) (JvmModifier.InvokeStatic, List(arg1Type)) else (JvmModifier.InvokeVirtual, List())

      emitClassMethod(modifiers, methodName, argsList, returnType, f(funSig))

      new VoidMethod1(invokeCode, ct, methodName)
    }

    /**
      * This method generates in the current class we are generating a static (void) method with 1 argument
      * given the provided params. It returns the capability to invoke the (void) method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param modifiers  list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param methodName name which we want to give to the method we are generating
      * @param f          , transformer which receives a funSig and return a frame transformer. This returned frame transfomer
      *                   will describe what instruction the method we are generating will execute.
      * @return returns a VoidMethod0 capability allows now the possibility to invoke this new (void) method with 1 argument
      */
    def mkStaticVoidMethod1[T1 <: MnemonicsTypes : TypeTag](methodName: String, f: FunSig1[T1, MVoid] => F[StackNil] => F[StackNil],
                                                            modifiers: List[JvmModifier] = List(Public, Static), isStatic: Boolean = true): VoidMethod1[T1] = {

      val arg1Type = getJvmType[T1]
      val returnType = Void
      val funSig = new FunSig1[T1, MVoid]()

      val (invokeCode, argsList) = (JvmModifier.InvokeStatic, List(arg1Type))

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
    def mkConstructor1[T1 <: MnemonicsTypes : TypeTag](f: FunSig1[T1, MVoid] => F[StackNil] => F[StackNil],
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
    def mkMethod2[T1 <: MnemonicsTypes : TypeTag, T2 <: MnemonicsTypes : TypeTag, R <: MnemonicsTypes : TypeTag](methodName: String, f: FunSig2[T1, T2, R] => F[StackNil] => F[StackNil],
                                                                                                                 modifiers: List[JvmModifier] = List(Public, Final), isStatic: Boolean = false): Method2[T1, T2, R] = {
      val arg1Type = getJvmType[T1]
      val arg2Type = getJvmType[T2]
      val returnType = getJvmType[R]

      val funSig = new FunSig2[T1, T2, R]()


      val (invokeCode, argsList) = if (isStatic) (JvmModifier.InvokeStatic, List(arg1Type, arg2Type)) else (JvmModifier.InvokeVirtual, List(arg2Type))

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
    def mkVoidMethod2[T1 <: MnemonicsTypes : TypeTag, T2 <: MnemonicsTypes : TypeTag](methodName: String, f: FunSig2[T1, T2, MVoid] => F[StackNil] => F[StackNil],
                                                                                      modifiers: List[JvmModifier] = List(Public, Final), isStatic: Boolean = false): VoidMethod2[T1, T2] = {
      val arg1Type = getJvmType[T1]
      val arg2Type = getJvmType[T2]

      val returnType = Void
      val funSig = new FunSig2[T1, T2, MVoid]()

      val (invokeCode, argsList) = if (isStatic) (JvmModifier.InvokeStatic, List(arg1Type, arg2Type)) else (JvmModifier.InvokeVirtual, List(arg2Type))

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
    def mkConstructor2[T1 <: MnemonicsTypes : TypeTag, T2 <: MnemonicsTypes : TypeTag](f: FunSig2[T1, T2, MVoid] => F[StackNil] => F[StackNil],
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

    def mkMethod3[T1 <: MnemonicsTypes : TypeTag, T2 <: MnemonicsTypes : TypeTag, T3 <: MnemonicsTypes : TypeTag, R <: MnemonicsTypes : TypeTag](methodName: String, f: FunSig3[T1, T2, T3, R] => F[StackNil] => F[StackNil],
                                                                                                                                                 modifiers: List[JvmModifier] = List(Public, Final), isStatic: Boolean = false): Method3[T1, T2, T3, R] = {
      val arg1Type = getJvmType[T1]
      val arg2Type = getJvmType[T2]
      val arg3Type = getJvmType[T3]
      val returnType = getJvmType[R]

      val funSig = new FunSig3[T1, T2, T3, R]()

      val (invokeCode, argsList) = if (isStatic) (JvmModifier.InvokeStatic, List(arg1Type, arg2Type, arg3Type)) else (JvmModifier.InvokeVirtual, List(arg2Type, arg3Type))

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
    def mkVoidMethod3[T1 <: MnemonicsTypes : TypeTag, T2 <: MnemonicsTypes : TypeTag, T3 <: MnemonicsTypes : TypeTag](methodName: String, f: FunSig3[T1, T2, T3, MVoid] => F[StackNil] => F[StackNil],
                                                                                                                      modifiers: List[JvmModifier] = List(Public, Final), isStatic: Boolean = false): VoidMethod3[T1, T2, T3] = {

      val arg1Type = getJvmType[T1]
      val arg2Type = getJvmType[T2]
      val arg3Type = getJvmType[T3]

      val returnType = Void
      val funSig = new FunSig3[T1, T2, T3, MVoid]()

      val (invokeCode, argsList) = if (isStatic) (JvmModifier.InvokeStatic, List(arg1Type, arg2Type, arg3Type)) else (JvmModifier.InvokeVirtual, List(arg2Type, arg3Type))

      emitClassMethod(modifiers, methodName, argsList, returnType, f(funSig))

      new VoidMethod3(invokeCode, ct, methodName)
    }

    /**
      * This method generates in the current class we are generating a (void) method with an arbitrary number of arguments
      * hence it is unchecked
      * given the provided params. It returns the capability to invoke the (void) method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param modifiers  list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param methodName name which we want to give to the method we are generating
      * @param f          , transformer which receives a funSig and return a frame transformer. This returned frame transfomer
      *                   will describe what instruction the method we are generating will execute.
      * @return returns a UnchekcVoidMethod capability allows now the possibility to invoke this new (void) method
      *         with an arbitrary number of arguments
      */
    def mkUncheckedVoidMethod(methodName: String, f: UncheckedFunSig => F[StackNil] => F[StackNil], args: List[NJvmType],
                              modifiers: List[JvmModifier] = List(Public, Final), isStatic: Boolean = false): UncheckedVoidMethod = {

      val returnType = Void
      val funSig = new UncheckedFunSig(args, returnType)

      val (invokeCode, argsList) = if (isStatic) (JvmModifier.InvokeStatic, args) else (JvmModifier.InvokeVirtual, args.tail)

      emitClassMethod(modifiers, methodName, argsList, returnType, f(funSig))

      new UncheckedVoidMethod(invokeCode, ct, methodName, args)
    }


    /**
      * This method generates in the current class we are generating a (non-void) method with an arbitrary number of arguments
      * hence it is unchecked
      * given the provided params. It returns the capability to invoke the (non-void) method. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param modifiers  list of modififers which we want to generate the method with (public, abstract, etc..)
      * @param methodName name which we want to give to the method we are generating
      * @param f          , transformer which receives a funSig and return a frame transformer. This returned frame transfomer
      *                   will describe what instruction the method we are generating will execute.
      * @return returns a UnchekcVoidMethod capability allows now the possibility to invoke this new (void) method
      *         with an arbitrary number of arguments
      */
    def mkUncheckedMethod(methodName: String, f: UncheckedFunSig => F[StackNil] => F[StackNil], args: List[NJvmType], returnType: NJvmType,
                          modifiers: List[JvmModifier] = List(Public, Final), isStatic: Boolean = false): UncheckedMethod = {

      val funSig = new UncheckedFunSig(args, returnType)

      val (invokeCode, argsList) = if (isStatic) (JvmModifier.InvokeStatic, args) else (JvmModifier.InvokeVirtual, args.tail)

      emitClassMethod(modifiers, methodName, argsList, returnType, f(funSig))

      new UncheckedMethod(invokeCode, ct, methodName, args, returnType)
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
    def mkConstructor3[T1 <: MnemonicsTypes : TypeTag, T2 <: MnemonicsTypes : TypeTag, T3 <: MnemonicsTypes : TypeTag](f: FunSig3[T1, T2, T3, MVoid] => F[StackNil] => F[StackNil],
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
    def mkConstructor4[T1 <: MnemonicsTypes : TypeTag, T2 <: MnemonicsTypes : TypeTag, T3 <: MnemonicsTypes : TypeTag, T4 <: MnemonicsTypes : TypeTag](f: FunSig4[T1, T2, T3, T4, MVoid] => F[StackNil] => F[StackNil],
                                                                                                                                                       modifiers: List[JvmModifier] = List(Public)): VoidMethod4[T1, T2, T3, T4] = {

      val funSig = new FunSig4[T1, T2, T3, T4, MVoid]()

      emitClassMethod(modifiers, "<init>", List(getJvmType[T2], getJvmType[T3], getJvmType[T4]), Void, f(funSig))

      new VoidMethod4(JvmModifier.InvokeSpecial, ct, "<init>")
    }


    /**
      * This method generates in the current class we are generating a constructor with an arbitrary number of arguments
      * given the provided params. It returns the capability to invoke the constructor. This way we ensure we only call methods
      * we've generated bytecode for.
      *
      * @param modifiers list of modififers which we want to generate the constructor with (public, abstract, etc..)
      * @param f         , transformer which receives a funSig and return a frame transformer. This returned frame transfomer
      *                  will describe what instruction the constructor we are generating will execute.
      * @return returns a VoidMethod0 capability allows now the possibility to invoke this new constructor with
      *         an arbitrary number of arguments
      */
    def mkUncheckedConstructor(args: List[NJvmType], f: UncheckedFunSig => F[StackNil] => F[StackNil],
                               modifiers: List[JvmModifier] = List(Public)): UncheckedVoidMethod = {


      val funSig = new UncheckedFunSig(args, Void)
      val argsList = if (args.length <= 1) Nil else args.tail
      emitClassMethod(modifiers, "<init>", argsList, Void, f(funSig))

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
      * Method which receives a list of JvmModifiers and fieldName. It will emit code to generate an unchecked field
      * with the correspoding field type
      *
      * @param modifiers list of modififers which we want to generate the field with (public, private, final, etc..)
      * @param fieldName name which we want to give to the field we are generating
      * @return returns the capability to acess the created field. Similar to the methods this ensure we only
      *         acess field we've generated
      */
    def mkUncheckedField(fieldName: String, fieldType: NJvmType, modifiers: List[JvmModifier] = List(Private)): UncheckedField = {
      val modifierVal = modifiers.map(modifier => modifier.toInternal).sum
      val field = cw.visitField(modifierVal, fieldName, fieldType.toDescriptor, null, null)
      field.visitEnd()

      new UncheckedField(fieldName, fieldType)
    }

    /**
      * Method which receives a list of JvmModifiers and fieldName. It will emit code to generate an unchecked static field
      * with the correspoding field type
      *
      * @param modifiers list of modififers which we want to generate the field with (public, private, final, etc..)
      * @param fieldName name which we want to give to the field we are generating
      * @return returns the capability to acess the created field. Similar to the methods this ensure we only
      *         acess field we've generated
      */
    def mkUncheckedStaticField(fieldName: String, fieldType: NJvmType, modifiers: List[JvmModifier] = List(Private)): UncheckedStaticField = {
      val modifierVal = modifiers.map(modifier => modifier.toInternal).sum
      val field = cw.visitField(modifierVal, fieldName, fieldType.toDescriptor, null, null)
      field.visitEnd()

      new UncheckedStaticField(fieldName, fieldType)
    }


    /**
      * Method which receives a list of JvmModifiers and fieldName. It will emit code to generate a Primfield
      * with the correspoding type parameter T.
      *
      * @param modifiers list of modififers which we want to generate the field with (public, private, final, etc..)
      * @param fieldName name which we want to give to the field we are generating
      * @return returns the capability to acess the created field. Similar to the methods this ensure we only
      *         acess field we've generated
      */
    def mkPrimField[T <: MnemonicsPrimTypes : TypeTag](fieldName: String, modifiers: List[JvmModifier] = List(Private)): PrimField[T] = {
      val fieldType = getJvmType[T]
      val modifierVal = modifiers.map(modifier => modifier.toInternal).sum
      val field = cw.visitField(modifierVal, fieldName, fieldType.toDescriptor, null, null)
      field.visitEnd()

      new PrimField(fieldName)
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
    * @param it         classType of the interaface we want to generate
    * @param modifiers  list of modififers which we want to generate the class with (public, abstract, etc..)
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
    def mkMethod1[T1 <: MnemonicsTypes : TypeTag, R <: MnemonicsTypes : TypeTag](methodName: String, modifiers: List[JvmModifier] = List(Public, Abstract)): Method1[T1, R] = {

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
    def mkVoidMethod1[T1 <: MnemonicsTypes : TypeTag](methodName: String, modifiers: List[JvmModifier] = List(Public, Abstract)): VoidMethod1[T1] = {

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
    def mkMethod2[T1 <: MnemonicsTypes : TypeTag, T2 <: MnemonicsTypes : TypeTag, R <: MnemonicsTypes : TypeTag](methodName: String, modifiers: List[JvmModifier] = List(Public, Abstract)): Method2[T1, T2, R] = {

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
    def mkVoidMethod2[T1 <: MnemonicsTypes : TypeTag, T2 <: MnemonicsTypes : TypeTag](methodName: String, modifiers: List[JvmModifier] = List(Public, Abstract)): VoidMethod2[T1, T2] = {

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
    def mkMethod3[T1 <: MnemonicsTypes : TypeTag, T2 <: MnemonicsTypes : TypeTag, T3 <: MnemonicsTypes : TypeTag, R <: MnemonicsTypes : TypeTag](methodName: String, modifiers: List[JvmModifier] = List(Public, Abstract)): Method3[T1, T2, T3, R] = {

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
    def mkVoidMethod3[T1 <: MnemonicsTypes : TypeTag, T2 <: MnemonicsTypes : TypeTag, T3 <: MnemonicsTypes : TypeTag](methodName: String, modifiers: List[JvmModifier] = List(Public, Abstract)): VoidMethod3[T1, T2, T3] = {

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
    Instructions.NEW[StackNil, Ref[MObject]](Reference(JvmName.UnsupportedOperationException)) |>>
      Instructions.DUP |>>
      Instructions.LDC_STRING(message) |>>
      Java.Lang.UnsupportedOperationException.constructor.INVOKE |>>
      Instructions.THROW
  }

  /**
    * Returns the load instruction for the value of the type specified by `tpe`
    */
  def getLoadInstruction(tpe: NJvmType): Int = tpe match {
    case PrimBool | PrimChar | PrimByte | PrimShort | PrimInt => ILOAD
    case PrimLong => LLOAD
    case PrimFloat => FLOAD
    case PrimDouble => DLOAD
    case Reference(_) => ALOAD
    case _ => throw InternalCompilerException(s"Unexpected type $tpe")
  }


  /**
    * Returns the store instruction for the value of the type specified by `tpe`
    */
  def getStoreInstruction(tpe: NJvmType): Int = tpe match {
    case PrimBool | PrimChar | PrimByte | PrimShort | PrimInt => ISTORE
    case PrimLong => LSTORE
    case PrimFloat => FSTORE
    case PrimDouble => DSTORE
    case Reference(_) => ASTORE
    case _ => throw InternalCompilerException(s"Unexpected type $tpe")

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
    case Reference(_) | JArray(_) => ARETURN
  }

  /**
    * Returns the array load instruction for arrays of the given JvmType tpe
    */
  def getArrayLoadInstruction(tpe: NJvmType): Int = tpe match {
    case PrimBool => BALOAD
    case PrimChar => CALOAD
    case PrimByte => BALOAD
    case PrimShort => SALOAD
    case PrimInt => IALOAD
    case PrimLong => LALOAD
    case PrimFloat => FALOAD
    case PrimDouble => DALOAD
    case Reference(_) => AALOAD
    case _=> throw InternalCompilerException(s"Unexpected type $tpe")

  }

  /**
    * Returns the array store instruction for arrays of the given JvmType tpe
    */
  def getArrayStoreInstruction(tpe: NJvmType): Int = tpe match {
    case PrimBool => BASTORE
    case PrimChar => CASTORE
    case PrimByte => BASTORE
    case PrimShort => SASTORE
    case PrimInt => IASTORE
    case PrimLong => LASTORE
    case PrimFloat => FASTORE
    case PrimDouble => DASTORE
    case Reference(_) => AASTORE
    case _=> throw InternalCompilerException(s"Unexpected type $tpe")


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
    case _ => throw InternalCompilerException(s"Unexpected type $tpe")
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
    * Returns the field name of a defn as used in a namespace class.
    *
    * For example:
    *
    * find      =>  f_find
    * length    =>  f_length
    */
  def getDefFieldNameInNamespaceClass(sym: Symbol.DefnSym): String = "f_" + mangle(sym.name)

  /**
    * Returns the method name of a defn as used in a namespace class.
    *
    * For example:
    *
    * find      =>  m_find
    * length    =>  m_length
    */
  def getDefMethodNameInNamespaceClass(sym: Symbol.DefnSym): String = "m_" + mangle(sym.name)

  /**
    * Returns the function definition class for the given symbol.
    *
    * For example:
    *
    * print         =>  Def$print
    * List.length   =>  List.Def$length
    */
  def getFunctionDefinitionClassType(sym: Symbol.DefnSym)(implicit root: Root, flix: Flix): Reference = {
    val pkg = sym.namespace
    val name = "Def$" + mangle(sym.name)
    Reference(JvmName(pkg, name))
  }

  /**
    * Returns the namespace type for the given namespace `ns`.
    *
    * For example:
    *
    * <root>      =>  Ns
    * Foo         =>  Foo.Ns
    * Foo.Bar     =>  Foo.Bar.Ns
    * Foo.Bar.Baz =>  Foo.Bar.Baz.Ns
    */
  def getNamespaceClassType(ns: NamespaceInfo)(implicit root: Root, flix: Flix): Reference = {
    val pkg = ns.ns
    val name = "Ns"
    Reference(JvmName(pkg, name))
  }

  /**
    * Returns the field name of a namespace as used in the Context class.
    *
    * For example:
    *
    * <root>      =>  Ns$Root$
    * Foo         =>  Foo$Ns
    * Foo.Bar     =>  Foo$Bar$Ns
    * Foo.Bar.Baz =>  Foo$Bar$Baz$Ns
    */
  def getNamespaceFieldNameInContextClass(ns: NamespaceInfo): String =
    if (ns.isRoot)
      "ns$Root$"
    else
      "ns$" + ns.ns.mkString("$")

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
  def getContextClassType(implicit root: Root, flix: Flix): Reference = {
    getJvmType[Ref[Context]].asInstanceOf[Reference]
  }


  /**
    * Returns the Main  `Main`
    */
  def getMainClassType(implicit root: Root, flix: Flix): Reference = {

    getJvmType[Ref[Main[_]]].asInstanceOf[Reference]
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
  def getFunctionInterfaceType(elms: List[NJvmType], returnType: NJvmType)(implicit root: Root, flix: Flix): Reference = {
    // Compute the arity of the function interface.
    // We subtract one since the last argument is the return type.
    val arity = elms.length

    // Compute the stringified erased type of each type argument.
    val args = (elms ::: List(returnType)).map(tpe => stringify(tpe))

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
  def getEnumInterfaceType(sym: Symbol.EnumSym, elms: List[NJvmType])(implicit root: Root, flix: Flix): Reference = {
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
    * Returns the namespace info of the given definition symbol `sym`.
    */
  def getNamespace(sym: Symbol.DefnSym)(implicit root: Root, flix: Flix): NamespaceInfo = {
    NamespaceInfo(sym.namespace, Map.empty) // TODO: Magnus: Empty map.
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
  def getRefClassType[T <: MnemonicsTypes : TypeTag](implicit root: Root, flix: Flix): Reference = {
    getJvmType[Ref[RefClass[T]]].asInstanceOf[Reference]
  }

  def getRefClassType(tpe : MonoType)(implicit root: Root, flix: Flix): Reference = {
    val jvmType = getErasedJvmType(tpe)
    jvmType  match {
      case PrimBool =>
        getJvmType[Ref[RefClass[MBool]]].asInstanceOf[Reference]
      case PrimChar =>
        getJvmType[Ref[RefClass[MChar]]].asInstanceOf[Reference]
      case PrimByte =>
        getJvmType[Ref[RefClass[MByte]]].asInstanceOf[Reference]
      case PrimShort =>
        getJvmType[Ref[RefClass[MShort]]].asInstanceOf[Reference]
      case PrimInt =>
        getJvmType[Ref[RefClass[MInt]]].asInstanceOf[Reference]
      case PrimLong =>
        getJvmType[Ref[RefClass[MLong]]].asInstanceOf[Reference]
      case PrimFloat =>
        getJvmType[Ref[RefClass[MFloat]]].asInstanceOf[Reference]
      case PrimDouble =>
        getJvmType[Ref[RefClass[MDouble]]].asInstanceOf[Reference]
      case Reference(_) =>
        getJvmType[Ref[RefClass[Ref[MObject]]]].asInstanceOf[Reference]
      case _ => throw InternalCompilerException(s"Unexpected type " + jvmType)

    }

  }

  /**
    * Returns the closure class `Clo$Name` for the given closure.
    *
    * String.charAt     =>    String/Clo$charAt
    * List.length       =>    List/Clo$length
    * List.map          =>    List/Clo$map
    */
  def getClosureClassType(closure: ClosureInfo)(implicit root: Root, flix: Flix): Reference = closure.tpe match {
    case MonoType.Arrow(targs, tresult) =>
      // Compute the arity of the function interface.
      // We subtract one since the last argument is the return type.
      val arity = targs.length

      // Compute the stringified erased type of each type argument.
      val args = (targs ::: tresult :: Nil).map(tpe => stringify(getErasedJvmType(tpe)))

      // The JVM name is of the form Clo$sym.name
      val name = "Clo" + "$" + mangle(closure.sym.name)

      // The JVM package is the namespace of the symbol.
      val pkg = closure.sym.namespace

      // The result type.
      Reference(JvmName(pkg, name))

    case tpe => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
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
    * Returns the tag class `Tag$X$Y$Z` for the given tag.
    *
    * For example,
    *
    * None: Option[Int]         =>    None
    * Some: Option[Char]        =>    Some$Char
    * Some: Option[Int]         =>    Some$Int
    * Some: Option[String]      =>    Some$Obj
    * Ok: Result[Bool, Char]    =>    Ok$Bool$Char
    * Err: Result[Bool, Char]   =>    Err$Bool$Char
    */
  // TODO: Magnus: Can we improve the representation w.r.t. unused type variables?
  def getTagClassType(tag: TagInfo)(implicit root: Root, flix: Flix): Reference = {
    // Retrieve the tag name.
    val tagName = tag.tag

    // Retrieve the type arguments.
    val args = tag.tparams.map(tpe => stringify(getErasedJvmType(tpe)))

    // The JVM name is of the form Tag$Arg0$Arg1$Arg2
    val name = if (args.isEmpty) tagName else tagName + "$" + args.mkString("$")

    // The tag class resides in its namespace package.
    Reference(JvmName(tag.sym.namespace, name))
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
    * Returns the stack size of a variable of type `tpe` in jvm.
    */
  def getStackSize(tpe: NJvmType): Int = tpe match {
    case PrimBool => 1
    case PrimChar => 1
    case PrimFloat => 1
    case PrimDouble => 2
    case PrimByte => 1
    case PrimShort => 1
    case PrimInt => 1
    case PrimLong => 2
    case Reference(_) => 1
    case _ => throw InternalCompilerException(s"Unexpected type: $tpe")
  }


  /**
    * Returns the tag info for the given `tpe` and `tag`
    */
  // TODO: Magnus: Should use getTags and then just find the correct tag.
  def getTagInfo(tpe: MonoType, tag: String)(implicit root: Root, flix: Flix): TagInfo = tpe match {
    case MonoType.Enum(_, _) =>
      val tags = getTagsOf(tpe)
      tags.find(_.tag == tag).get
    case _ => throw InternalCompilerException(s"Unexpected type: $tpe")
  }

  /**
    * Returns the CheckCast type for the value of the type specified by `tpe`
    */
  def getArrayType(tpe: NJvmType): String = tpe match {
    case PrimBool => "[Z"
    case PrimChar => "[C"
    case PrimByte => "[B"
    case PrimShort => "[S"
    case PrimInt => "[I"
    case PrimLong => "[J"
    case PrimFloat => "[F"
    case PrimDouble => "[D"
    case NJvmType.String => "[Ljava/lang/String;"
    case Reference(_) => "[Ljava/lang/Object;"
    case _ => throw InternalCompilerException(s"Unexpected type $tpe")
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
  trait MnemonicsGenerator {

    /**
      * Method should receive a Map of all the generated classes so far. It should generate all the new classes
      * and return an updated map with the new generated classes.
      *
      * @param map   of all the generated classes so far.
      * @param types set of Monotypes this will be used to generate certain classes such as Enum.
      * @param tags  set of tags
      * @return update map with new generated classes
      */
    def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo],
            ns: Set[NamespaceInfo], closures: Set[ClosureInfo], defs: Map[Symbol.DefnSym, Def])
           (implicit root: Root, flix: Flix): Map[JvmName, MnemonicsClass]
  }

  //Generators
  object GenClosuresClasses extends MnemonicsGenerator {
    /**
      * Method should receive a Map of all the generated classes so far. It should generate all the new classes
      * and return an updated map with the new generated classes.
      *
      * @param map   of all the generated classes so far.
      * @param types set of Monotypes this will be used to generate certain classes such as Enum.
      * @return update map with new generated classes
      */
    def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo],
            ns: Set[NamespaceInfo], closures: Set[ClosureInfo], defs: Map[Symbol.DefnSym, Def])
           (implicit root: Root, flix: Flix): Map[JvmName, MnemonicsClass] = {
      closures.foldLeft(map) {
        case (macc, closure) =>
          macc + new Closure(map, closure).getClassMapping
      }
    }
  }

  object GenNamespacesClasses extends MnemonicsGenerator {

    /**
      * Method should receive a Map of all the generated classes so far. It should generate all the new classes
      * and return an updated map with the new generated classes.
      *
      * @param map   of all the generated classes so far.
      * @param types set of Monotypes this will be used to generate certain classes such as Enum.
      * @return update map with new generated classes
      */
    def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo],
            ns: Set[NamespaceInfo], closures: Set[ClosureInfo], defs: Map[Symbol.DefnSym, Def])
           (implicit root: Root, flix: Flix): Map[JvmName, MnemonicsClass] = {
      map + new Context(ns).getClassMapping

      ns.foldLeft(map) {
        case (macc, namespace) =>
          macc + new Namespace(namespace).getClassMapping
      }
    }
  }

  object GenContextClass extends MnemonicsGenerator {
    /**
      * Method should receive a Map of all the generated classes so far. It should generate all the new classes
      * and return an updated map with the new generated classes.
      *
      * @param map   of all the generated classes so far.
      * @param types set of Monotypes this will be used to generate certain classes such as Enum.
      * @return update map with new generated classes
      */
    def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo],
            ns: Set[NamespaceInfo], closures: Set[ClosureInfo], defs: Map[Symbol.DefnSym, Def])
           (implicit root: Root, flix: Flix): Map[JvmName, MnemonicsClass] = {
      map + new Context(ns).getClassMapping
    }
  }

  object GenContinuationInterfaces extends MnemonicsGenerator {
    /**
      * Method should receive a Map of all the generated classes so far. It should generate all the new classes
      * and return an updated map with the new generated classes.
      *
      * @param map   of all the generated classes so far.
      * @param types set of Monotypes this will be used to generate certain classes such as Enum.
      * @return update map with new generated classes
      */
    def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo],
            ns: Set[NamespaceInfo], closures: Set[ClosureInfo], defs: Map[Symbol.DefnSym, Def])
           (implicit root: Root, flix: Flix): Map[JvmName, MnemonicsClass] = {
      map + (
        new ContinuationInterface[MBool].getClassMapping,
        new ContinuationInterface[MChar].getClassMapping,
        new ContinuationInterface[MFloat].getClassMapping,
        new ContinuationInterface[MDouble].getClassMapping,
        new ContinuationInterface[MByte].getClassMapping,
        new ContinuationInterface[MShort].getClassMapping,
        new ContinuationInterface[MInt].getClassMapping,
        new ContinuationInterface[MLong].getClassMapping,
        new ContinuationInterface[Ref[MObject]].getClassMapping)
    }
  }

  object GenEnumInterfaces extends MnemonicsGenerator {
    /**
      * Method should receive a Map of all the generated classes so far. It should generate all the new classes
      * and return an updated map with the new generated classes.
      *
      * @param map   of all the generated classes so far.
      * @param types set of Monotypes this will be used to generate certain classes such as Enum.
      * @return update map with new generated classes
      */
    def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo],
            ns: Set[NamespaceInfo], closures: Set[ClosureInfo], defs: Map[Symbol.DefnSym, Def])
           (implicit root: Root, flix: Flix): Map[JvmName, MnemonicsClass] = {
      types.foldLeft(map) {
        case (macc, MonoType.Enum(sym, elms)) =>
          // Case 1: The type constructor is an enum.
          // Construct enum interface.
          val args = elms.map(getErasedJvmType)

          macc + new EnumInterface(sym, args).getClassMapping
        case (macc, _) =>
          // Case 2: The type constructor is a non-tuple.
          // Nothing to be done. Return the map.
          macc
      }
    }
  }

  object GenFunctionInterfaces extends MnemonicsGenerator {
    /**
      * Method should receive a Map of all the generated classes so far. It should generate all the new classes
      * and return an updated map with the new generated classes.
      *
      * @param map   of all the generated classes so far.
      * @param types set of Monotypes this will be used to generate certain classes such as Enum.
      * @return update map with new generated classes
      */
    def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo],
            ns: Set[NamespaceInfo], closures: Set[ClosureInfo], defs: Map[Symbol.DefnSym, Def])
           (implicit root: Root, flix: Flix): Map[JvmName, MnemonicsClass] = {

      //
      // Generate a function interface for each type and collect the results in a map.
      //
      types.foldLeft(map) {
        case (macc, MonoType.Arrow(targs, tresult)) =>
          val elms = targs.map(getErasedJvmType)

          val returnType = getErasedJvmType(tresult)
          macc + new FunctionInterface(elms, returnType).getClassMapping
        case (macc, _) =>
          // Case 2: The type constructor is a non-tuple.
          // Nothing to be done. Return the map.
          macc
      }
    }
  }

  object GenMainClass extends MnemonicsGenerator {
    /**
      * Method should receive a Map of all the generated classes so far. It should generate all the new classes
      * and return an updated map with the new generated classes.
      *
      * @param map   of all the generated classes so far.
      * @param types set of Monotypes this will be used to generate certain classes such as Enum.
      * @return update map with new generated classes
      */
    def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo],
            ns: Set[NamespaceInfo], closures: Set[ClosureInfo], defs: Map[Symbol.DefnSym, Def])
           (implicit root: Root, flix: Flix): Map[JvmName, MnemonicsClass] =
      getMain(root) match {
        case None => map
        case Some(defn) =>
          map + (getErasedJvmType(defn.tpe) match {
            case PrimBool => new Main[MBool](map).getClassMapping
            case PrimChar => new Main[MChar](map).getClassMapping
            case PrimByte => new Main[MByte](map).getClassMapping
            case PrimShort => new Main[MShort](map).getClassMapping
            case PrimInt => new Main[MInt](map).getClassMapping
            case PrimLong => new Main[MLong](map).getClassMapping
            case PrimFloat => new Main[MFloat](map).getClassMapping
            case PrimDouble => new Main[MDouble](map).getClassMapping
            case Reference(_) => new Main[Ref[MObject]](map).getClassMapping
            case _ => throw InternalCompilerException(s"Unexpected type $defn.tpe")
          })
      }

    /**
      * Optionally returns the main definition in the given AST `root`.
      */
    private def getMain(root: Root): Option[Def] = {
      // The main function must be called `main` and occur in the root namespace.
      val sym = Symbol.mkDefnSym("main")

      // Check if the main function exists.
      root.defs.get(sym) flatMap {
        case defn =>
          // The main function must take zero arguments.
          Some(defn)
      }
    }
  }

  object GenNamespaceClasses extends MnemonicsGenerator {

    /**
      * Method should receive a Map of all the generated classes so far. It should generate all the new classes
      * and return an updated map with the new generated classes.
      *
      * @param map   of all the generated classes so far.
      * @param types set of Monotypes this will be used to generate certain classes such as Enum.
      * @return update map with new generated classes
      */
    def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo],
            ns: Set[NamespaceInfo], closures: Set[ClosureInfo], defs: Map[Symbol.DefnSym, Def])
           (implicit root: Root, flix: Flix): Map[JvmName, MnemonicsClass] = {
      ns.foldLeft(map) {
        case (macc, namespace) =>
          macc + new Namespace(namespace).getClassMapping
      }
    }
  }

  object GenRecordEmpty extends MnemonicsGenerator {
    /**
      * Method should receive a Map of all the generated classes so far. It should generate all the new classes
      * and return an updated map with the new generated classes.
      *
      * @param map   of all the generated classes so far.
      * @param types set of Monotypes this will be used to generate certain classes such as Enum.
      * @return update map with new generated classes
      */
    def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo],
            ns: Set[NamespaceInfo], closures: Set[ClosureInfo], defs: Map[Symbol.DefnSym, Def])
           (implicit root: Root, flix: Flix): Map[JvmName, MnemonicsClass] = {
      map + new RecordEmpty(map).getClassMapping
    }

  }

  object GenRecordExtend extends MnemonicsGenerator {
    /**
      * Method should receive a Map of all the generated classes so far. It should generate all the new classes
      * and return an updated map with the new generated classes.
      *
      * @param map   of all the generated classes so far.
      * @param types set of Monotypes this will be used to generate certain classes such as Enum.
      * @return update map with new generated classes
      */
    def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo],
            ns: Set[NamespaceInfo], closures: Set[ClosureInfo], defs: Map[Symbol.DefnSym, Def])
           (implicit root: Root, flix: Flix): Map[JvmName, MnemonicsClass] = {
      map + (
        new RecordExtend[MBool](map).getClassMapping,
        new RecordExtend[MChar](map).getClassMapping,
        new RecordExtend[MFloat](map).getClassMapping,
        new RecordExtend[MDouble](map).getClassMapping,
        new RecordExtend[MByte](map).getClassMapping,
        new RecordExtend[MShort](map).getClassMapping,
        new RecordExtend[MInt](map).getClassMapping,
        new RecordExtend[MLong](map).getClassMapping,
        new RecordExtend[Ref[MObject]](map).getClassMapping)
    }
  }

  object GenRecordInterface extends MnemonicsGenerator {

    /**
      * Method should receive a Map of all the generated classes so far. It should generate all the new classes
      * and return an updated map with the new generated classes.
      *
      * @param map   of all the generated classes so far.
      * @param types set of Monotypes this will be used to generate certain classes such as Enum.
      * @return update map with new generated classes
      */
    def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo],
            ns: Set[NamespaceInfo], closures: Set[ClosureInfo], defs: Map[Symbol.DefnSym, Def])
           (implicit root: Root, flix: Flix): Map[JvmName, MnemonicsClass] = {
      map + new RecordInterface().getClassMapping
    }
  }

  object GenRefClasses extends MnemonicsGenerator {

    /**
      * Method should receive a Map of all the generated classes so far. It should generate all the new classes
      * and return an updated map with the new generated classes.
      *
      * @param map   of all the generated classes so far.
      * @param types set of Monotypes this will be used to generate certain classes such as Enum.
      * @return update map with new generated classes
      */
    def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo],
            ns: Set[NamespaceInfo], closures: Set[ClosureInfo], defs: Map[Symbol.DefnSym, Def])
           (implicit root: Root, flix: Flix): Map[JvmName, MnemonicsClass] = {
      map + (
        new RefClass[MBool].getClassMapping,
        new RefClass[MChar].getClassMapping,
        new RefClass[MFloat].getClassMapping,
        new RefClass[MDouble].getClassMapping,
        new RefClass[MByte].getClassMapping,
        new RefClass[MShort].getClassMapping,
        new RefClass[MInt].getClassMapping,
        new RefClass[MLong].getClassMapping,
        new RefClass[Ref[MObject]].getClassMapping)
    }

  }

  object GenTagClasses extends MnemonicsGenerator {
    /**
      * Method should receive a Map of all the generated classes so far. It should generate all the new classes
      * and return an updated map with the new generated classes.
      *
      * @param map   of all the generated classes so far.
      * @param types set of Monotypes this will be used to generate certain classes such as Enum.
      * @return update map with new generated classes
      */
    def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo],
            ns: Set[NamespaceInfo], closures: Set[ClosureInfo], defs: Map[Symbol.DefnSym, Def])
           (implicit root: Root, flix: Flix): Map[JvmName, MnemonicsClass] = {
      tags.foldLeft(map) {
        case (macc, tag) =>
          macc + (getErasedJvmType(tag.tagType) match {
            case PrimBool => new TagClass[MBool](tag).getClassMapping
            case PrimChar => new TagClass[MChar](tag).getClassMapping
            case PrimByte => new TagClass[MByte](tag).getClassMapping
            case PrimShort => new TagClass[MShort](tag).getClassMapping
            case PrimInt => new TagClass[MInt](tag).getClassMapping
            case PrimLong => new TagClass[MLong](tag).getClassMapping
            case PrimFloat => new TagClass[MFloat](tag).getClassMapping
            case PrimDouble => new TagClass[MDouble](tag).getClassMapping
            case Reference(_) => new TagClass[Ref[MObject]](tag).getClassMapping
            case _ => throw InternalCompilerException(s"Unexpected type $tag.tagType")
          })
      }
    }
  }

  object GenTupleClasses extends MnemonicsGenerator {
    /**
      * Method should receive a Map of all the generated classes so far. It should generate all the new classes
      * and return an updated map with the new generated classes.
      *
      * @param map   of all the generated classes so far.
      * @param types set of Monotypes this will be used to generate certain classes such as Enum.
      * @return update map with new generated classes
      */
    def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo],
            ns: Set[NamespaceInfo], closures: Set[ClosureInfo], defs: Map[Symbol.DefnSym, Def])
           (implicit root: Root, flix: Flix): Map[JvmName, MnemonicsClass] = {
      types.foldLeft(map) {
        case (macc, MonoType.Tuple(elms)) =>
          val targs = elms.map(getErasedJvmType)

          macc + new TupleClass(macc, targs).getClassMapping
        case (macc, _) =>
          // Case 2: The type constructor is a non-tuple.
          // Nothing to be done. Return the map.
          macc
      }
    }
  }

  object GenTupleInterfaces extends MnemonicsGenerator {
    /**
      * Method should receive a Map of all the generated classes so far. It should generate all the new classes
      * and return an updated map with the new generated classes.
      *
      * @param map   of all the generated classes so far.
      * @param types set of Monotypes this will be used to generate certain classes such as Enum.
      * @return update map with new generated classes
      */
    def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo],
            ns: Set[NamespaceInfo], closures: Set[ClosureInfo], defs: Map[Symbol.DefnSym, Def])
           (implicit root: Root, flix: Flix): Map[JvmName, MnemonicsClass] = {
      types.foldLeft(map) {
        case (macc, MonoType.Tuple(elms)) =>
          val targs = elms.map(getErasedJvmType)

          macc + new TupleInterface(targs).getClassMapping
        case (macc, _) =>
          // Case 2: The type constructor is a non-tuple.
          // Nothing to be done. Return the map.
          macc
      }
    }
  }

  object GenFunctionClasses extends MnemonicsGenerator {
    /**
      * Method should receive a Map of all the generated classes so far. It should generate all the new classes
      * and return an updated map with the new generated classes.
      *
      * @param map   of all the generated classes so far.
      * @param types set of Monotypes this will be used to generate certain classes such as Enum.
      * @return update map with new generated classes
      */
    def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo],
            ns: Set[NamespaceInfo], closures: Set[ClosureInfo], defs: Map[Symbol.DefnSym, Def])
           (implicit root: Root, flix: Flix): Map[JvmName, MnemonicsClass] = {
      defs.foldLeft(map) {
        case (macc, (sym, defn)) if !defn.ann.isLaw =>

          macc + new FunctionClass(map, sym, defn).getClassMapping
        case (macc, _) => macc
      }
    }
  }

}
