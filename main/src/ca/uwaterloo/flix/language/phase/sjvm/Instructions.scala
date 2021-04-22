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
import ca.uwaterloo.flix.language.ast.{Cat1, Cat2, PRefType, PType, RRefType, RType, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm.{Label, Opcodes}

object Instructions {

  def getInternalName[T <: PType](eType: RType[T]): String =
    RType.toInternalName(eType)

  def getInternalName[T <: PRefType](eRefType: RRefType[T]): String =
    getInternalName(RReference(eRefType))

  def getDescriptor[T1 <: PType, T2 <: PType](args: String, result: String): String =
    s"($args)$result"

  def getDescriptor[T <: PType](eType: RType[T]): String = eType match {
    case RReference(referenceType) => s"L${getInternalName(referenceType)};"
    case other => getInternalName(other)
  }

  def getDescriptor[T <: PRefType](eRefType: RRefType[T]): String =
    getDescriptor(RReference(eRefType))

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
    //todo why is NOP/the type needed here?
    NOP ~[R ** PReference[S] ** PReference[S]]
      DUP ~
      MONITORENTER ~
      f ~
      XSWAP(e, RType.RReference(RObject)) ~ // TODO: make partial automatic swap
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
  [R <: Stack, T1 <: PType with Cat1, T2 <: PType with Cat1]:
  F[R ** T2 ** T1] => F[R ** T1 ** T2] =
    ???

  def SWAP_cat2_onCat1
  [R <: Stack, T1 <: PType with Cat1, T2 <: PType with Cat1]:
  F[R ** T2 ** T1] => F[R ** T1 ** T2] =
    ???

  def SWAP_cat2_onCat2
  [R <: Stack, T1 <: PType with Cat1, T2 <: PType with Cat1]:
  F[R ** T2 ** T1] => F[R ** T1 ** T2] =
    ???

  // NATIVE
  def XSWAP
  [R <: Stack, T1 <: PType, T2 <: PType]
  (t1: RType[T1], t2: RType[T2]):
  F[R ** T2 ** T1] => F[R ** T1 ** T2] = (t1, t2) match {
    case (RInt64(), RInt64()) => ??? //SWAP_cat2_onCat2
    case (RInt64(), RFloat64()) => ??? //SWAP_cat2_onCat2
    case (RFloat64(), RInt64()) => ??? //SWAP_cat2_onCat2
    case (RFloat64(), RFloat64()) => ??? //SWAP_cat2_onCat2

    case (RInt64(), _) => ??? //SWAP_cat2_onCat1
    case (RFloat64(), _) => ??? //SWAP_cat2_onCat1

    case (_, RInt64()) => ??? //SWAP_cat1_onCat2
    case (_, RFloat64()) => ??? //SWAP_cat1_onCat2

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
  (className: String, fieldName: String, fieldType: RType[T1]):
  F[R ** PReference[T2]] => F[R ** T1] =
    fieldType match {
      case RBool() => GetBoolField(className, fieldName)
      case RInt8() => GetInt8Field(className, fieldName)
      case RInt16() => GetInt16Field(className, fieldName)
      case RInt32() => GetInt32Field(className, fieldName)
      case RInt64() => GetInt64Field(className, fieldName)
      case RChar() => GetCharField(className, fieldName)
      case RFloat32() => GetFloat32Field(className, fieldName)
      case RFloat64() => GetFloat64Field(className, fieldName)
      case RReference(referenceType) => GetClassField(className, fieldName, referenceType)
    }

  // NATIVE
  def GetBoolField
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (className: String, fieldName: String):
  F[R ** PReference[T2]] => F[R ** PInt32] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, className, fieldName, RBool().toDescriptor)
    castF(f)
  }

  // NATIVE
  def GetInt8Field
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (className: String, fieldName: String):
  F[R ** PReference[T2]] => F[R ** PInt8] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, className, fieldName, RInt8().toDescriptor)
    castF(f)
  }

  // NATIVE
  def GetInt16Field
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (className: String, fieldName: String):
  F[R ** PReference[T2]] => F[R ** PInt16] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, className, fieldName, RInt16().toDescriptor)
    castF(f)
  }

  // NATIVE
  def GetInt32Field
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (className: String, fieldName: String):
  F[R ** PReference[T2]] => F[R ** PInt32] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, className, fieldName, RInt32().toDescriptor)
    castF(f)
  }

  // NATIVE
  def GetInt64Field
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (className: String, fieldName: String):
  F[R ** PReference[T2]] => F[R ** PInt64] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, className, fieldName, RInt64().toDescriptor)
    castF(f)
  }

  // NATIVE
  def GetCharField
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (className: String, fieldName: String):
  F[R ** PReference[T2]] => F[R ** PChar] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, className, fieldName, RChar().toDescriptor)
    castF(f)
  }

  // NATIVE
  def GetFloat32Field
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (className: String, fieldName: String):
  F[R ** PReference[T2]] => F[R ** PFloat32] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, className, fieldName, RFloat32().toDescriptor)
    castF(f)
  }

  // NATIVE
  def GetFloat64Field
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (className: String, fieldName: String):
  F[R ** PReference[T2]] => F[R ** PFloat64] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, className, fieldName, RFloat64().toDescriptor)
    castF(f)
  }

  def GetClassField
  [R <: Stack, T1 <: PRefType, T2 <: PRefType]
  (className: String, fieldName: String, referenceType: RRefType[T1]):
  F[R ** PReference[T2]] => F[R ** PReference[T1]] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, className, fieldName, referenceType.toInternalName)
    castF(f)
  }

  def GetObjectField
  [R <: Stack, T1 <: PRefType, T2 <: PRefType]
  (className: String, fieldName: String, tpe: T1 = tag[T1]):
  F[R ** PReference[T2]] => F[R ** PReference[T1]] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, className, fieldName, JvmName.Java.Lang.Object.toDescriptor)
    castF(f)
  }

  // NATIVE
  def PUTFIELD
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (className: String, fieldName: String, fieldType: RType[T1]):
  F[R ** PReference[T2] ** T1] => F[R] =
    ???

  // NATIVE
  def CAST
  [R <: Stack, T <: PRefType]
  (e: RRefType[T]):
  F[R ** PReference[PAnyObject]] => F[R ** PReference[T]] = f => {
    f.visitor.visitTypeInsn(Opcodes.CHECKCAST, e.toInternalName)
    castF(f)
  }

  // NATIVE
  def NEW
  [R <: Stack, T <: PRefType]
  (className: String):
  F[R] => F[R ** PReference[T]] = f => {
    f.visitor.visitTypeInsn(Opcodes.NEW, className)
    castF(f)
  }

  // TODO: type should be constructed along with descriptor string
  // NATIVE
  def INVOKESPECIAL
  [R <: Stack, T <: PRefType]
  (className: String, constructorDescriptor: String):
  F[R ** PReference[T]] => F[R] = f => {
    f.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, className, JvmName.constructorMethod, constructorDescriptor, false)
    castF(f)
  }

  // todo delete
  def SCAFFOLD
  [R1 <: Stack, R2 <: Stack]:
  F[R1] => F[R2] =
    ???

  // NATIVE
  def RETURN[R <: Stack]: F[StackNil] => F[StackEnd] = f => {
    f.visitor.visitInsn(Opcodes.RETURN)
    castF(f)
  }

  // NATIVE
  def XRETURN
  [R <: Stack, T <: PType]
  (e: RType[T]):
  F[StackNil ** T] => F[StackEnd] =
    ???

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
    val className = getInternalName(RRefType.RUnit())
    val classDescriptor = getDescriptor(RRefType.RUnit())
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
      case RBool() => ???
      case RInt8() => ???
      case RInt16() => ???
      case RInt32() => ILOAD(index)
      case RInt64() => LLOAD(index)
      case RChar() => ???
      case RFloat32() => FLOAD(index)
      case RFloat64() => DLOAD(index)
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
      case RBool() | RInt32() => IALoad
      case RChar() => CALoad
      case RFloat32() => FALoad
      case RFloat64() => DALoad
      case RInt8() => BALoad
      case RInt16() => SALoad
      case RInt64() => LALoad
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
      case RChar() => CAStore
      case RFloat32() => FAStore
      case RFloat64() => DAStore
      case RInt8() => BAStore
      case RInt16() => SAStore
      case RBool() | RInt32() => IAStore
      case RInt64() => LAStore
      case RReference(_) => AAStore
    }

  // I/S/B/C-Store are all just jvm ISTORE
  // NATIVE
  def IStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PInt32] => F[R] =
    ???

  // NATIVE
  def SStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PInt16] => F[R] =
    ???

  // NATIVE
  def BStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PInt8] => F[R] =
    ???

  // NATIVE
  def CStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PChar] => F[R] =
    ???

  // NATIVE
  def LStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PInt64] => F[R] =
    ???

  // NATIVE
  def FStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PFloat32] => F[R] =
    ???

  // NATIVE
  def DStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PFloat64] => F[R] =
    ???

  // NATIVE
  def AStore
  [R <: Stack, T <: PRefType]
  (sym: Symbol.VarSym):
  F[R ** PReference[T]] => F[R] =
    ???

  // META
  def XStore
  [R <: Stack, T <: PType]
  (sym: Symbol.VarSym, tpe: RType[T]):
  F[R ** T] => F[R] =
    tpe match {
      case RChar() => CStore(sym)
      case RFloat32() => FStore(sym)
      case RFloat64() => DStore(sym)
      case RInt8() => BStore(sym)
      case RInt16() => SStore(sym)
      case RBool() | RInt32() => IStore(sym)
      case RInt64() => LStore(sym)
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
        case RBool() => BOOLNEWARRAY
        case RChar() => CNEWARRAY
        case RFloat32() => FNEWARRAY
        case RFloat64() => DNEWARRAY
        case RInt8() => BNEWARRAY
        case RInt16() => SNEWARRAY
        case RInt32() => INEWARRAY
        case RInt64() => LNEWARRAY
        case RReference(_) => ANEWARRAY
      }
      case _ => throw InternalCompilerException("unexpected non-array type")
    }

  def systemArrayCopy
  [R <: Stack, S <: PRefType]:
  F[R ** PReference[S] ** PInt32 ** PReference[S] ** PInt32 ** PInt32] => F[R] =
    ???

  def arrayLength
  [R <: Stack, T <: PType]:
  F[R ** PReference[PArray[T]]] => F[R ** PInt32] =
    ???

  def setRefValue
  [R <: Stack, T <: PType]
  (className: String, innerType: RType[T]):
  F[R ** PReference[PRef[T]] ** T] => F[R] = f => {
    f.visitor.visitFieldInsn(Opcodes.PUTFIELD, className, GenRefClasses.fieldName, getInternalName(innerType))
    castF(f)
  }

  // also make void
  def defMakeFunction
  [R <: Stack, T <: PType]
  (x: F[R], t: RType[T]):
  F[R ** T] = {
    //todo where is this string stored
    getInternalName(t)
    ???
  }

  def foo[R <: Stack] = (r: F[R]) => defMakeFunction(defMakeFunction(r, RInt32()), RInt32())

  // META
  implicit class ComposeOps[A, B](f: F[A] => F[B]) {
    def ~[C](that: F[B] => F[C]): F[A] => F[C] = ???
  }

}
