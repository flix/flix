/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.PType._
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.ast.{Cat1, Cat2, ErasedAst, PRefType, PType, RRefType, RType, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm.{Label, Opcodes}

object Instructions {

  private def castF[S1 <: Stack, S2 <: Stack](f: F[S1]): F[S2] = f.asInstanceOf[F[S2]]

  def WithSource[R <: Stack](loc: SourceLocation): F[R] => F[R] = f => {
    val label = new Label()
    f.visitor.visitLabel(label)
    f.visitor.visitLineNumber(loc.beginLine, label)
    f
  }

  def tag[T]: T = null.asInstanceOf[T]

  def START
  [R <: Stack]:
  F[R] => F[R] =
    f => f

  def compileClosureApplication
  [R <: Stack, T1 <: PRefType, T2 <: PType]
  (resultType: RType[T2]):
  F[R ** PReference[PAnyObject]] => F[R ** T2] =
    ???

  def WITHMONITOR
  [R <: Stack, S <: PRefType, T <: PType]
  (e: RType[T], tpe: S = tag[T])(f: F[R ** PReference[S]] => F[R ** PReference[S] ** T]):
  F[R ** PReference[S]] => F[R ** T] = {
    // TODO(JLS): why is NOP/the type needed here?
    NOP ~[R ** PReference[S] ** PReference[S]]
      DUP ~
      MONITORENTER ~
      f ~
      XSWAP(e, RType.RReference(null)) ~ // TODO(JLS): fix and make partial automatic swap
      MONITOREXIT
  }

  private def MONITORENTER
  [R <: Stack, S <: PRefType]:
  F[R ** PReference[S]] => F[R] = f => {
    f.visitor.visitInsn(Opcodes.MONITORENTER)
    castF(f)
  }

  private def MONITOREXIT
  [R <: Stack, S <: PRefType]:
  F[R ** PReference[S]] => F[R] = f => {
    f.visitor.visitInsn(Opcodes.MONITOREXIT)
    castF(f)
  }

  def RUNIFTRUE
  [R <: Stack]
  (f: F[R] => F[R]):
  F[R ** PInt32] => F[R] = {
    val label = new Label
    IFNE(label) ~
      f ~
      (f0 => {
        f0.visitor.visitLabel(label);
        castF(f0)
      })
  }

  private def IFNE
  [R <: Stack]
  (label: Label):
  F[R ** PInt32] => F[R] = f => {
    f.visitor.visitJumpInsn(Opcodes.IFNE, label)
    castF(f)
  }

  // NATIVE
  def SWAP
  [R <: Stack, T1 <: PType with Cat1, T2 <: PType with Cat1]:
  F[R ** T2 ** T1] => F[R ** T1 ** T2] = {
    f =>
      f.visitor.visitInsn(Opcodes.SWAP)
      castF(f)
  }

  def SWAP_cat1_onCat2
  [R <: Stack, T1 <: PType with Cat1, T2 <: PType with Cat2]:
  F[R ** T2 ** T1] => F[R ** T1 ** T2] =
    ???

  def SWAP_cat2_onCat1
  [R <: Stack, T1 <: PType with Cat2, T2 <: PType with Cat1]:
  F[R ** T2 ** T1] => F[R ** T1 ** T2] =
    ???

  def SWAP_cat2_onCat2
  [R <: Stack, T1 <: PType with Cat2, T2 <: PType with Cat2]:
  F[R ** T2 ** T1] => F[R ** T1 ** T2] =
    ???

  // TODO(JLS): fix automatic swap

  def SWAP_cat1_onSomething
  [R <: Stack, T1 <: PType with Cat1, T2 <: PType]
  (t2: RType[T2]):
  F[R ** T2 ** T1] => F[R ** T1 ** T2] = t2 match {
    case RBool => ??? //SWAP
    case RInt8 => ??? //SWAP
    case RInt16 => ??? //SWAP
    case RInt32 => ??? //SWAP
    case RInt64 => ??? //SWAP_cat1_onCat2
    case RChar => ??? //SWAP
    case RFloat32 => ??? //SWAP
    case RFloat64 => ??? //SWAP_cat1_onCat2
    case RReference(referenceType) => ??? //SWAP
  }

  // NATIVE
  def XSWAP
  [R <: Stack, T1 <: PType, T2 <: PType]
  (t1: RType[T1], t2: RType[T2]):
  F[R ** T2 ** T1] => F[R ** T1 ** T2] = (t1, t2) match {
    case (RInt64, RInt64) => ??? //SWAP_cat2_onCat2
    case (RInt64, RFloat64) => ??? //SWAP_cat2_onCat2
    case (RFloat64, RInt64) => ??? //SWAP_cat2_onCat2
    case (RFloat64, RFloat64) => ??? //SWAP_cat2_onCat2

    case (RInt64, _) => ??? //SWAP_cat2_onCat1
    case (RFloat64, _) => ??? //SWAP_cat2_onCat1

    case (_, RInt64) => ??? //SWAP_cat1_onCat2
    case (_, RFloat64) => ??? //SWAP_cat1_onCat2

    case _ => ??? //SWAP
  }

  // META
  def NOP
  [R <: Stack]:
  F[R] => F[R] =
    x => x

  // NATIVE
  def XGETFIELD
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (classType: RReference[T2], fieldName: String, fieldType: RType[T1]):
  F[R ** PReference[T2]] => F[R ** T1] =
    fieldType match {
      case RBool => GetBoolField(classType, fieldName)
      case RInt8 => GetInt8Field(classType, fieldName)
      case RInt16 => GetInt16Field(classType, fieldName)
      case RInt32 => GetInt32Field(classType, fieldName)
      case RInt64 => GetInt64Field(classType, fieldName)
      case RChar => GetCharField(classType, fieldName)
      case RFloat32 => GetFloat32Field(classType, fieldName)
      case RFloat64 => GetFloat64Field(classType, fieldName)
      case RReference(referenceType) => GetClassField(classType, fieldName, referenceType)
    }

  // NATIVE
  def GetBoolField
  [R <: Stack, T2 <: PRefType]
  (classType: RReference[T2], fieldName: String):
  F[R ** PReference[T2]] => F[R ** PInt32] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, classType.toInternalName, fieldName, RBool.toDescriptor)
    castF(f)
  }

  // NATIVE
  def GetInt8Field
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (classType: RReference[T2], fieldName: String):
  F[R ** PReference[T2]] => F[R ** PInt8] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, classType.toInternalName, fieldName, RInt8.toDescriptor)
    castF(f)
  }

  // NATIVE
  def GetInt16Field
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (classType: RReference[T2], fieldName: String):
  F[R ** PReference[T2]] => F[R ** PInt16] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, classType.toInternalName, fieldName, RInt16.toDescriptor)
    castF(f)
  }

  // NATIVE
  def GetInt32Field
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (classType: RReference[T2], fieldName: String):
  F[R ** PReference[T2]] => F[R ** PInt32] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, classType.toInternalName, fieldName, RInt32.toDescriptor)
    castF(f)
  }

  // NATIVE
  def GetInt64Field
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (classType: RReference[T2], fieldName: String):
  F[R ** PReference[T2]] => F[R ** PInt64] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, classType.toInternalName, fieldName, RInt64.toDescriptor)
    castF(f)
  }

  // NATIVE
  def GetCharField
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (classType: RReference[T2], fieldName: String):
  F[R ** PReference[T2]] => F[R ** PChar] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, classType.toInternalName, fieldName, RChar.toDescriptor)
    castF(f)
  }

  // NATIVE
  def GetFloat32Field
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (classType: RReference[T2], fieldName: String):
  F[R ** PReference[T2]] => F[R ** PFloat32] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, classType.toInternalName, fieldName, RFloat32.toDescriptor)
    castF(f)
  }

  // NATIVE
  def GetFloat64Field
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (classType: RReference[T2], fieldName: String):
  F[R ** PReference[T2]] => F[R ** PFloat64] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, classType.toInternalName, fieldName, RFloat64.toDescriptor)
    castF(f)
  }

  def GetClassField
  [R <: Stack, T1 <: PRefType, T2 <: PRefType]
  (classType: RReference[T2], fieldName: String, referenceType: RRefType[T1]):
  F[R ** PReference[T2]] => F[R ** PReference[T1]] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, classType.toInternalName, fieldName, referenceType.toInternalName)
    castF(f)
  }

  def GetObjectField
  [R <: Stack, T1 <: PRefType, T2 <: PRefType]
  (classType: RReference[T2], fieldName: String, tpe: T1 = tag[T1]):
  F[R ** PReference[T2]] => F[R ** PReference[T1]] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, classType.toInternalName, fieldName, JvmName.Java.Lang.Object.toDescriptor)
    castF(f)
  }

  // NATIVE
  def PUTFIELD
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (classType: RReference[T2], fieldName: String, fieldType: RType[T1]):
  F[R ** PReference[T2] ** T1] => F[R] =
    ???

  // NATIVE
  def CAST
  [R <: Stack, T <: PRefType]
  (e: RRefType[T]):
  F[R ** PReference[_ <: PRefType]] => F[R ** PReference[T]] = f => {
    f.visitor.visitTypeInsn(Opcodes.CHECKCAST, e.toInternalName)
    castF(f)
  }

  // TODO(JLS): What to do here
//  def CastIfNotPrim
//  [R <: Stack, T1 <: PType, T2 <: PType]
//  (expType: RType[T1], newType: RType[T2]):
//  F[R ** T1] => F[R ** T2] = (expType, newType) match {
//    case (RBool, RBool) => NOP
//    case (RInt8, RInt8) => NOP
//    case (RInt16, RInt16) => NOP
//    case (RInt32, RInt32) => NOP
//    case (RInt64, RInt64) => NOP
//    case (RChar, RChar) => NOP
//    case (RFloat32, RFloat32) => NOP
//    case (RFloat64, RFloat64) => NOP
//    case (RReference(_), castType@RReference(b)) =>
//      SCAFFOLD//CAST(b)
//    case _ =>
//  }

  // NATIVE
  def NEW
  [R <: Stack, T <: PRefType]
  (classType: RReference[T]):
  F[R] => F[R ** PReference[T]] = f => {
    f.visitor.visitTypeInsn(Opcodes.NEW, classType.toInternalName)
    castF(f)
  }

  // TODO(JLS): type should be constructed along with descriptor string
  // NATIVE
  def INVOKESPECIAL
  [R <: Stack, T <: PRefType]
  (classType: RReference[T], constructorDescriptor: String):
  F[R ** PReference[T]] => F[R] = f => {
    f.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, classType.toInternalName, JvmName.constructorMethod, constructorDescriptor, false)
    castF(f)
  }

  def INVOKEOBJECTCONSTRUCTOR
  [R <: Stack, T <: PRefType]:
  F[R ** PReference[T]] => F[R] = f => {
    f.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, JvmName.Java.Lang.Object.toInternalName, JvmName.constructorMethod, JvmName.nothingToVoid, false)
    castF(f)
  }

  // TODO(JLS): delete
  def SCAFFOLD
  [R1 <: Stack, R2 <: Stack]:
  F[R1] => F[R2] =
    ???

  // NATIVE
  // TODO(JLS): maybe return Nothing (Nothing <: F[_]). atleast something better than StackEnd
  def RETURN[R <: Stack]: F[StackNil] => F[StackEnd] = f => {
    f.visitor.visitInsn(Opcodes.RETURN)
    castF(f)
  }

  def RETURNNULL[R <: Stack]: F[StackNil] => F[StackEnd] = f => {
    f.visitor.visitInsn(Opcodes.ACONST_NULL)
    f.visitor.visitInsn(Opcodes.ARETURN)
    castF(f)
  }

  def ARETURN[R <: Stack, T <: PRefType]: F[StackNil ** PReference[T]] => F[StackEnd] = f => {
    f.visitor.visitInsn(Opcodes.ARETURN)
    castF(f)
  }

  def IRETURN[R <: Stack]: F[StackNil ** PInt32] => F[StackEnd] = f => {
    f.visitor.visitInsn(Opcodes.IRETURN)
    castF(f)
  }

  def BRETURN[R <: Stack]: F[StackNil ** PInt8] => F[StackEnd] = f => {
    f.visitor.visitInsn(Opcodes.IRETURN)
    castF(f)
  }

  def SRETURN[R <: Stack]: F[StackNil ** PInt16] => F[StackEnd] = f => {
    f.visitor.visitInsn(Opcodes.IRETURN)
    castF(f)
  }

  def CRETURN[R <: Stack]: F[StackNil ** PChar] => F[StackEnd] = f => {
    f.visitor.visitInsn(Opcodes.IRETURN)
    castF(f)
  }

  def LRETURN[R <: Stack]: F[StackNil ** PInt64] => F[StackEnd] = f => {
    f.visitor.visitInsn(Opcodes.LRETURN)
    castF(f)
  }

  def FRETURN[R <: Stack]: F[StackNil ** PFloat32] => F[StackEnd] = f => {
    f.visitor.visitInsn(Opcodes.FRETURN)
    castF(f)
  }

  def DRETURN[R <: Stack]: F[StackNil ** PFloat64] => F[StackEnd] = f => {
    f.visitor.visitInsn(Opcodes.DRETURN)
    castF(f)
  }

  // NATIVE
  def XRETURN
  [R <: Stack, T <: PType]
  (e: RType[T]):
  F[StackNil ** T] => F[StackEnd] = e match {
    case RType.RBool => IRETURN
    case RType.RInt8 => BRETURN
    case RType.RInt16 => SRETURN
    case RType.RInt32 => IRETURN
    case RType.RInt64 => LRETURN
    case RType.RChar => CRETURN
    case RType.RFloat32 => FRETURN
    case RType.RFloat64 => DRETURN
    case RReference(_) => ARETURN
  }

  // NATIVE
  def POP
  [R <: Stack, T <: PType with Cat1]:
  F[R ** T] => F[R] = f => {
    f.visitor.visitInsn(Opcodes.POP)
    castF(f)
  }

  // NATIVE
  def DUP_X1
  [R <: Stack, T1 <: PType with Cat1, T2 <: PType with Cat1]:
  F[R ** T2 ** T1] => F[R ** T1 ** T2 ** T1] = f => {
    f.visitor.visitInsn(Opcodes.DUP_X1)
    castF(f)
  }

  // NATIVE
  def DUP_X2_onCat1
  [R <: Stack, T1 <: PType with Cat1, T2 <: PType with Cat1, T3 <: PType with Cat1]:
  F[R ** T3 ** T2 ** T1] => F[R ** T1 ** T3 ** T2 ** T1] = f => {
    f.visitor.visitInsn(Opcodes.DUP_X2)
    castF(f)
  }

  // NATIVE
  def DUP_X2_onCat2
  [R <: Stack, T1 <: PType with Cat1, T2 <: PType with Cat2]:
  F[R ** T2 ** T1] => F[R ** T1 ** T2 ** T1] = f => {
    f.visitor.visitInsn(Opcodes.DUP_X2)
    castF(f)
  }

  // NATIVE
  def DUP2_X2_cat1_onCat1
  [R <: Stack, T1 <: PType with Cat1, T2 <: PType with Cat1, T3 <: PType with Cat1, T4 <: PType with Cat1]:
  F[R ** T4 ** T3 ** T2 ** T1] => F[R ** T2 ** T1 ** T4 ** T3 ** T2 ** T1] = f => {
    f.visitor.visitInsn(Opcodes.DUP2_X2)
    castF(f)
  }

  // NATIVE
  def DUP2_X2_cat2_onCat1
  [R <: Stack, T1 <: PType with Cat2, T2 <: PType with Cat1, T3 <: PType with Cat1]:
  F[R ** T3 ** T2 ** T1] => F[R ** T1 ** T3 ** T2 ** T1] = f => {
    f.visitor.visitInsn(Opcodes.DUP2_X2)
    castF(f)
  }

  // NATIVE
  def DUP2_X2_cat1_onCat2
  [R <: Stack, T1 <: PType with Cat1, T2 <: PType with Cat1, T3 <: PType with Cat2]:
  F[R ** T3 ** T2 ** T1] => F[R ** T2 ** T1 ** T3 ** T2 ** T1] = f => {
    f.visitor.visitInsn(Opcodes.DUP2_X2)
    castF(f)
  }

  // NATIVE
  def DUP2_X2_cat2_onCat2
  [R <: Stack, T1 <: PType with Cat2, T2 <: PType with Cat2]:
  F[R ** T2 ** T1] => F[R ** T1 ** T2 ** T1] = f => {
    f.visitor.visitInsn(Opcodes.DUP2_X2)
    castF(f)
  }

  // NATIVE
  def DUP
  [R <: Stack, T <: PType with Cat1]:
  F[R ** T] => F[R ** T ** T] = f => {
    f.visitor.visitInsn(Opcodes.DUP)
    castF(f)
  }

  // NATIVE
  def ISUB
  [R <: Stack]:
  F[R ** PInt32 ** PInt32] => F[R ** PInt32] = f => {
    f.visitor.visitInsn(Opcodes.ISUB)
    castF(f)
  }

  // NATIVE
  def pushUnit
  [R <: Stack]:
  F[R] => F[R ** PReference[PUnit]] = f => {
    val className = RUnit.toInternalName
    val classDescriptor = RUnit.toDescriptor
    f.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, className, "getInstance", classDescriptor, false)
    castF(f)
  }

  // NATIVE
  def pushNull
  [R <: Stack, T <: PRefType]:
  F[R] => F[R ** PReference[T]] = f => {
    f.visitor.visitInsn(Opcodes.ACONST_NULL)
    castF(f)
  }

  // NATIVE
  def pushBool
  [R <: Stack]
  (b: Boolean):
  F[R] => F[R ** PInt32] =
    pushInt32(if (b) 1 else 0)

  // NATIVE
  def pushInt8
  [R <: Stack]
  (n: Int):
  F[R] => F[R ** PInt8] =
    ???

  // NATIVE
  def pushInt16
  [R <: Stack]
  (n: Int):
  F[R] => F[R ** PInt16] =
    ???

  // NATIVE
  def pushInt32
  [R <: Stack]
  (n: Int):
  F[R] => F[R ** PInt32] =
    ???

  // NATIVE
  def pushInt64
  [R <: Stack]
  (n: Long):
  F[R] => F[R ** PInt64] =
    ???

  // NATIVE
  def pushFloat32
  [R <: Stack]
  (n: Float):
  F[R] => F[R ** PFloat32] =
    ???

  // NATIVE
  def pushFloat64
  [R <: Stack]
  (n: Double):
  F[R] => F[R ** PFloat64] =
    ???

  // NATIVE
  def pushChar
  [R <: Stack]
  (c: scala.Char):
  F[R] => F[R ** PChar] =
    f => castF(pushInt16(c)(f))

  // NATIVE
  def ALOAD
  [R <: Stack, T <: PRefType]
  (index: Int, tpe: T = tag[T]):
  F[R] => F[R ** PReference[T]] = f => {
    f.visitor.visitVarInsn(Opcodes.ALOAD, index)
    castF(f)
  }

  def THISLOAD
  [R <: Stack, T <: PRefType]
  (tpe: T = tag[T]):
  F[R] => F[R ** PReference[T]] =
    ALOAD(0)

  // TODO(JLS): This should both return StackEnd (no code should follow) and R ** T (compileExp should push T on stack)
  // TODO(JLS): TAILCALL/CALL only differ in the last instructions => rewrite and reuse
  def TAILCALL
  [R <: Stack, T <: PType]
  (arguments: List[ErasedAst.Expression[_ <: PType]], funClassName: JvmName, tpe: T = tag[T]):
  F[R ** PReference[PFunction]] => F[R ** T] = f => {
    for (argIndex <- arguments.indices) {
      val arg = arguments(argIndex)
      f.visitor.visitInsn(Opcodes.DUP)
      compileExp(arg)(f)
      f.visitor.visitFieldInsn(Opcodes.PUTFIELD, funClassName.toInternalName, GenFunctionInterfaces.argFieldName(argIndex), arg.tpe.toDescriptor)
    }
    f.visitor.visitInsn(Opcodes.ARETURN)
    castF(f)
  }

  def CALL
  [R <: Stack, T <: PType]
  (arguments: List[ErasedAst.Expression[_ <: PType]], funClassName: JvmName, contName: JvmName, returnType: RType[T]):
  F[R ** PReference[PFunction]] => F[R ** T] = f => {
    for (argIndex <- arguments.indices) {
      val arg = arguments(argIndex)
      f.visitor.visitInsn(Opcodes.DUP)
      compileExp(arg)(f)
      f.visitor.visitFieldInsn(Opcodes.PUTFIELD, funClassName.toInternalName, GenFunctionInterfaces.argFieldName(argIndex), arg.tpe.toDescriptor)
    }
    f.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, contName.toInternalName, GenContinuationInterfaces.unwindMethodName, returnType.nothingToThisMethodDescriptor, false)
    castF(f)
  }

  // NATIVE
  def FLOAD
  [R <: Stack]
  (index: Int):
  F[R] => F[R ** PFloat32] =
    ???

  // NATIVE
  def DLOAD
  [R <: Stack]
  (index: Int):
  F[R] => F[R ** PFloat64] =
    ???

  // NATIVE
  def ILOAD
  [R <: Stack]
  (index: Int):
  F[R] => F[R ** PInt32] =
    ???

  // NATIVE
  def LLOAD
  [R <: Stack]
  (index: Int):
  F[R] => F[R ** PInt64] =
    ???

  // META
  def XLOAD
  [R <: Stack, T <: PType]
  (tpe: RType[T], index: Int):
  F[R] => F[R ** T] =
    tpe match {
      case RBool => ???
      case RInt8 => ???
      case RInt16 => ???
      case RInt32 => ILOAD(index)
      case RInt64 => LLOAD(index)
      case RChar => ???
      case RFloat32 => FLOAD(index)
      case RFloat64 => DLOAD(index)
      case RReference(_) => ALOAD(index)
    }

  // NATIVE
  def BALoad
  [R <: Stack]:
  F[R ** PReference[PArray[PInt8]] ** PInt32] => F[R ** PInt8] =
    ???

  // NATIVE
  def SALoad
  [R <: Stack]:
  F[R ** PReference[PArray[PInt16]] ** PInt32] => F[R ** PInt16] =
    ???

  // NATIVE
  def IALoad
  [R <: Stack]:
  F[R ** PReference[PArray[PInt32]] ** PInt32] => F[R ** PInt32] =
    ???

  // NATIVE
  def LALoad
  [R <: Stack]:
  F[R ** PReference[PArray[PInt64]] ** PInt32] => F[R ** PInt64] =
    ???

  // NATIVE
  def CALoad
  [R <: Stack]:
  F[R ** PReference[PArray[PChar]] ** PInt32] => F[R ** PChar] =
    ???

  // NATIVE
  def FALoad
  [R <: Stack]:
  F[R ** PReference[PArray[PFloat32]] ** PInt32] => F[R ** PFloat32] =
    ???

  // NATIVE
  def DALoad
  [R <: Stack]:
  F[R ** PReference[PArray[PFloat64]] ** PInt32] => F[R ** PFloat64] =
    ???

  // NATIVE
  def AALoad
  [R <: Stack, T <: PRefType]:
  F[R ** PReference[PArray[PReference[T]]] ** PInt32] => F[R ** PReference[T]] =
    ???

  // META
  def XALoad
  [R <: Stack, T <: PType]
  (tpe: RType[T]):
  F[R ** PReference[PArray[T]] ** PInt32] => F[R ** T] =
    tpe match {
      case RBool | RInt32 => IALoad
      case RChar => CALoad
      case RFloat32 => FALoad
      case RFloat64 => DALoad
      case RInt8 => BALoad
      case RInt16 => SALoad
      case RInt64 => LALoad
      case RReference(_) => AALoad
    }

  // NATIVE
  def BAStore
  [R <: Stack]:
  F[R ** PReference[PArray[PInt8]] ** PInt32 ** PInt8] => F[R] =
    ???

  // NATIVE
  def SAStore
  [R <: Stack]:
  F[R ** PReference[PArray[PInt16]] ** PInt32 ** PInt16] => F[R] =
    ???

  // NATIVE
  def IAStore
  [R <: Stack]:
  F[R ** PReference[PArray[PInt32]] ** PInt32 ** PInt32] => F[R] =
    ???

  // NATIVE
  def LAStore
  [R <: Stack]:
  F[R ** PReference[PArray[PInt64]] ** PInt32 ** PInt64] => F[R] =
    ???

  // NATIVE
  def CAStore
  [R <: Stack]:
  F[R ** PReference[PArray[PChar]] ** PInt32 ** PChar] => F[R] =
    ???

  // NATIVE
  def FAStore
  [R <: Stack]:
  F[R ** PReference[PArray[PFloat32]] ** PInt32 ** PFloat32] => F[R] =
    ???

  // NATIVE
  def DAStore
  [R <: Stack]:
  F[R ** PReference[PArray[PFloat64]] ** PInt32 ** PFloat64] => F[R] =
    ???

  // NATIVE
  def AAStore
  [R <: Stack, T <: PRefType]:
  F[R ** PReference[PArray[PReference[T]]] ** PInt32 ** PReference[T]] => F[R] =
    ???

  // META
  def XAStore
  [R <: Stack, T <: PType]
  (tpe: RType[T]):
  F[R ** PReference[PArray[T]] ** PInt32 ** T] => F[R] =
    tpe match {
      case RChar => CAStore
      case RFloat32 => FAStore
      case RFloat64 => DAStore
      case RInt8 => BAStore
      case RInt16 => SAStore
      case RBool | RInt32 => IAStore
      case RInt64 => LAStore
      case RReference(_) => AAStore
    }

  val symOffsetOffset = 1

  // I/S/B/C-Store are all just jvm ISTORE
  // NATIVE
  def IStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PInt32] => F[R] = f => {
    f.visitor.visitVarInsn(Opcodes.ISTORE, sym.getStackOffset + symOffsetOffset)
    castF(f)
  }

  // NATIVE
  def SStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PInt16] => F[R] = f => {
    f.visitor.visitVarInsn(Opcodes.ISTORE, sym.getStackOffset + symOffsetOffset)
    castF(f)
  }

  // NATIVE
  def BStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PInt8] => F[R] = f => {
    f.visitor.visitVarInsn(Opcodes.ISTORE, sym.getStackOffset + symOffsetOffset)
    castF(f)
  }

  // NATIVE
  def CStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PChar] => F[R] = f => {
    f.visitor.visitVarInsn(Opcodes.ISTORE, sym.getStackOffset + symOffsetOffset)
    castF(f)
  }

  // NATIVE
  def LStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PInt64] => F[R] = f => {
    f.visitor.visitVarInsn(Opcodes.LSTORE, sym.getStackOffset + symOffsetOffset)
    castF(f)
  }

  // NATIVE
  def FStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PFloat32] => F[R] = f => {
    f.visitor.visitVarInsn(Opcodes.FSTORE, sym.getStackOffset + symOffsetOffset)
    castF(f)
  }

  // NATIVE
  def DStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PFloat64] => F[R] = f => {
    f.visitor.visitVarInsn(Opcodes.DSTORE, sym.getStackOffset + symOffsetOffset)
    castF(f)
  }

  // NATIVE
  def AStore
  [R <: Stack, T <: PRefType]
  (sym: Symbol.VarSym):
  F[R ** PReference[T]] => F[R] = f => {
    f.visitor.visitVarInsn(Opcodes.ASTORE, sym.getStackOffset + symOffsetOffset)
    castF(f)
  }

  // META
  def XStore
  [R <: Stack, T <: PType]
  (sym: Symbol.VarSym, tpe: RType[T]):
  F[R ** T] => F[R] =
    tpe match {
      case RChar => CStore(sym)
      case RFloat32 => FStore(sym)
      case RFloat64 => DStore(sym)
      case RInt8 => BStore(sym)
      case RInt16 => SStore(sym)
      case RBool | RInt32 => IStore(sym)
      case RInt64 => LStore(sym)
      case RReference(_) => AStore(sym)
    }

  // NATIVE
  def BOOLNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PInt32]]] =
    ???

  // NATIVE
  def CNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PChar]]] =
    ???

  // NATIVE
  def FNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PFloat32]]] =
    ???

  // NATIVE
  def DNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PFloat64]]] =
    ???

  // NATIVE
  def BNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PInt8]]] =
    ???

  // NATIVE
  def SNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PInt16]]] =
    ???

  // NATIVE
  def INEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PInt32]]] =
    ???

  // NATIVE
  def LNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PInt64]]] =
    ???

  // NATIVE
  def ANEWARRAY
  [R <: Stack, T <: PRefType]:
  F[R ** PInt32] => F[R ** PReference[PArray[PReference[T]]]] =
  // from genExpression:
  // visitor.visitTypeInsn(ANEWARRAY, "java/lang/Object")
  // should type be built?
    ???

  // META
  def XNEWARRAY
  [R <: Stack, T <: PType]
  (arrayType: RType[PReference[PArray[T]]]):
  F[R ** PInt32] => F[R ** PReference[PArray[T]]] =
    arrayType match {
      case RReference(RArray(tpe)) => tpe match {
        case RBool => BOOLNEWARRAY
        case RChar => CNEWARRAY
        case RFloat32 => FNEWARRAY
        case RFloat64 => DNEWARRAY
        case RInt8 => BNEWARRAY
        case RInt16 => SNEWARRAY
        case RInt32 => INEWARRAY
        case RInt64 => LNEWARRAY
        case RReference(_) => ANEWARRAY
      }
      case _ => throw InternalCompilerException("unexpected non-array type")
    }

  def systemArrayCopy
  [R <: Stack, S <: PRefType]:
  F[R ** PReference[S] ** PInt32 ** PReference[S] ** PInt32 ** PInt32] => F[R] =
    ???

  def arrayLength
  [R <: Stack, T <: PType]
  (arrayType: RType[PReference[PArray[T]]]):
  F[R ** PReference[PArray[T]]] => F[R ** PInt32] = f => {
    // TODO(JLS): checkcast needed?
    f.visitor.visitTypeInsn(Opcodes.CHECKCAST, arrayType.toDescriptor)
    f.visitor.visitInsn(Opcodes.ARRAYLENGTH)
    castF(f)
  }

  def setRefValue
  [R <: Stack, T <: PType]
  (classType: RReference[PRef[T]], innerType: RType[T]):
  F[R ** PReference[PRef[T]] ** T] => F[R] = f => {
    f.visitor.visitFieldInsn(Opcodes.PUTFIELD, classType.toInternalName, GenRefClasses.ValueFieldName, innerType.toDescriptor)
    castF(f)
  }

  // also make void
  def defMakeFunction
  [R <: Stack, T <: PType]
  (x: F[R], t: RType[T]):
  F[R ** T] = {
    // TODO(JLS): where is this string stored
    //t.toDescriptor
    ???
  }

  def foo[R <: Stack] = (r: F[R]) => defMakeFunction(defMakeFunction(r, RInt32), RInt32)

  // META
  implicit class ComposeOps[A <: Stack, B <: Stack](ab: F[A] => F[B]) {
    def ~[C <: Stack](bc: F[B] => F[C]): F[A] => F[C] =
      f => bc(ab(f))
  }

}
