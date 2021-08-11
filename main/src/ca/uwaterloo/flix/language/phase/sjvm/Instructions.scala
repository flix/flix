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
import ca.uwaterloo.flix.language.ast.{ErasedAst, PRefType, PType, RRefType, RType, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm
import org.objectweb.asm.{Label, MethodVisitor, Opcodes}

object Instructions {

  private def castF[S1 <: Stack, S2 <: Stack](f: F[S1]): F[S2] = f.asInstanceOf[F[S2]]

  trait Tag[T]

  def tagOf[T]: Tag[T] = null

  implicit class TagBox[X <: PType](x: RType[X]) {
    def tagOf: Tag[X] = null
  }

  def WithSource[R <: Stack](loc: SourceLocation): F[R] => F[R] = f => {
    val label = new Label()
    f.visitor.visitLabel(label)
    f.visitor.visitLineNumber(loc.beginLine, label)
    f
  }

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
  (e: RType[T])(f: F[R ** PReference[S]] => F[R ** PReference[S] ** T]):
  F[R ** PReference[S]] => F[R ** T] = {
    // TODO(JLS): why is NOP/the type needed here?
    NOP ~[R ** PReference[S] ** PReference[S]]
      DUP ~
      MONITORENTER ~
      f ~
      XSWAP(e, RReference(null)) ~ // TODO(JLS): fix and make partial automatic swap
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
    val label = new Label()
    IFNE(label) ~
      f ~
      (f0 => {
        f0.visitor.visitLabel(label)
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

  def multiComposition[A, R <: Stack](xs: IterableOnce[A])(generator: A => F[R] => F[R]): F[R] => F[R] = f => {
    xs.iterator.foreach(x => generator(x)(f))
    f
  }

  def SWAP
  [R <: Stack, T1 <: PType, T2 <: PType]
  (implicit t1: T1 => Cat1[T1], t2: T2 => Cat1[T2]):
  F[R ** T2 ** T1] => F[R ** T1 ** T2] = {
    f =>
      f.visitor.visitInsn(Opcodes.SWAP)
      castF(f)
  }

  def SWAP_cat1_onCat2
  [R <: Stack, T1 <: PType, T2 <: PType]
  (implicit t1: T1 => Cat1[T1], t2: T2 => Cat2[T2]):
  F[R ** T2 ** T1] => F[R ** T1 ** T2] =
    ???

  def SWAP_cat2_onCat1
  [R <: Stack, T1 <: PType, T2 <: PType]
  (implicit t1: T1 => Cat2[T1], t2: T2 => Cat1[T2]):
  F[R ** T2 ** T1] => F[R ** T1 ** T2] =
    ???

  def SWAP_cat2_onCat2
  [R <: Stack, T1 <: PType, T2 <: PType]
  (implicit t1: T1 => Cat2[T1], t2: T2 => Cat2[T2]):
  F[R ** T2 ** T1] => F[R ** T1 ** T2] =
    ???

  def SWAP_cat1_onSomething
  [R <: Stack, T1 <: PType, T2 <: PType]
  (t2: RType[T2])
  (implicit t1: T1 => Cat1[T1]):
  F[R ** T2 ** T1] => F[R ** T1 ** T2] = t2 match {
    case RBool => SWAP
    case RInt8 => SWAP
    case RInt16 => SWAP
    case RInt32 => SWAP
    case RInt64 => SWAP_cat1_onCat2
    case RChar => SWAP
    case RFloat32 => SWAP
    case RFloat64 => SWAP_cat1_onCat2
    case RReference(_) => SWAP
  }

  //TODO(JLS): note: this doesn't work with clever wildcards
  def XSWAP
  [R <: Stack, T1 <: PType, T2 <: PType]
  (t1: RType[T1], t2: RType[T2]):
  F[R ** T2 ** T1] => F[R ** T1 ** T2] = (t1, t2) match {
    case (RInt64, RInt64) => SWAP_cat2_onCat2
    case (RInt64, RFloat64) => SWAP_cat2_onCat2
    case (RFloat64, RInt64) => SWAP_cat2_onCat2
    case (RFloat64, RFloat64) => SWAP_cat2_onCat2

    case (RInt64, RBool | RChar | RInt8 | RInt16 | RInt32 | RFloat32 | RReference(_)) => SWAP_cat2_onCat1
    case (RFloat64, RBool | RChar | RInt8 | RInt16 | RInt32 | RFloat32 | RReference(_)) => SWAP_cat2_onCat1

    case (RBool | RChar | RInt8 | RInt16 | RInt32 | RFloat32 | RReference(_), RInt64) => SWAP_cat1_onCat2
    case (RBool | RChar | RInt8 | RInt16 | RInt32 | RFloat32 | RReference(_), RFloat64) => SWAP_cat1_onCat2

    case (RBool | RChar | RInt8 | RInt16 | RInt32 | RFloat32 | RReference(_), RBool | RChar | RInt8 | RInt16 | RInt32 | RFloat32 | RReference(_)) => SWAP
  }

  def NOP
  [R <: Stack]:
  F[R] => F[R] =
    x => x

  def getStaticField
  [R <: Stack, T <: PType]
  (field: java.lang.reflect.Field, tpe: RType[T]):
  F[R] => F[R ** T] = f => {
    val declaration = asm.Type.getInternalName(field.getDeclaringClass)
    f.visitor.visitFieldInsn(Opcodes.GETSTATIC, declaration, field.getName, tpe.toDescriptor)
    castF(f)
  }

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

  def GetBoolField
  [R <: Stack, T2 <: PRefType]
  (classType: RReference[T2], fieldName: String):
  F[R ** PReference[T2]] => F[R ** PInt32] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, classType.toInternalName, fieldName, RBool.toDescriptor)
    castF(f)
  }

  def GetInt8Field
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (classType: RReference[T2], fieldName: String):
  F[R ** PReference[T2]] => F[R ** PInt8] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, classType.toInternalName, fieldName, RInt8.toDescriptor)
    castF(f)
  }

  def GetInt16Field
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (classType: RReference[T2], fieldName: String):
  F[R ** PReference[T2]] => F[R ** PInt16] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, classType.toInternalName, fieldName, RInt16.toDescriptor)
    castF(f)
  }

  def GetInt32Field
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (classType: RReference[T2], fieldName: String):
  F[R ** PReference[T2]] => F[R ** PInt32] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, classType.toInternalName, fieldName, RInt32.toDescriptor)
    castF(f)
  }

  def GetInt64Field
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (classType: RReference[T2], fieldName: String):
  F[R ** PReference[T2]] => F[R ** PInt64] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, classType.toInternalName, fieldName, RInt64.toDescriptor)
    castF(f)
  }

  def GetCharField
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (classType: RReference[T2], fieldName: String):
  F[R ** PReference[T2]] => F[R ** PChar] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, classType.toInternalName, fieldName, RChar.toDescriptor)
    castF(f)
  }

  def GetFloat32Field
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (classType: RReference[T2], fieldName: String):
  F[R ** PReference[T2]] => F[R ** PFloat32] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, classType.toInternalName, fieldName, RFloat32.toDescriptor)
    castF(f)
  }

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
  (classType: RReference[T2], fieldName: String, fieldType: Tag[T1] = null):
  F[R ** PReference[T2]] => F[R ** PReference[T1]] = f => {
    f.visitor.visitFieldInsn(Opcodes.GETFIELD, classType.toInternalName, fieldName, JvmName.Java.Lang.Object.toDescriptor)
    castF(f)
  }

  //TODO(JLS): Should probably not be used directly (with functions/ref/modelled things)
  def PUTFIELD
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (classType: RReference[T2], fieldName: String, fieldType: RType[T1]):
  F[R ** PReference[T2] ** T1] => F[R] =
    ???

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

  //TODO(JLS): Only use the new+init combo instruction (impl with capability)
  def NEW
  [R <: Stack, T <: PRefType]
  (classType: RReference[T]):
  F[R] => F[R ** PReference[T]] = f => {
    f.visitor.visitTypeInsn(Opcodes.NEW, classType.toInternalName)
    castF(f)
  }

  // TODO(JLS): type should be constructed along with descriptor string
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


  def XRETURN
  [R <: Stack, T <: PType]
  (e: RType[T]):
  F[StackNil ** T] => F[StackEnd] = e match {
    case RBool => IRETURN
    case RInt8 => BRETURN
    case RInt16 => SRETURN
    case RInt32 => IRETURN
    case RInt64 => LRETURN
    case RChar => CRETURN
    case RFloat32 => FRETURN
    case RFloat64 => DRETURN
    case RReference(_) => ARETURN
  }


  def POP
  [R <: Stack, T <: PType]
  (implicit t: T => Cat1[T]):
  F[R ** T] => F[R] = f => {
    f.visitor.visitInsn(Opcodes.POP)
    castF(f)
  }

  def DUP_X1
  [R <: Stack, T1 <: PType, T2 <: PType]
  (implicit t1: T1 => Cat1[T1], t2: T2 => Cat1[T2]):
  F[R ** T2 ** T1] => F[R ** T1 ** T2 ** T1] = f => {
    f.visitor.visitInsn(Opcodes.DUP_X1)
    castF(f)
  }

  def DUP_X2_onCat1
  [R <: Stack, T1 <: PType, T2 <: PType, T3 <: PType]
  (implicit t1: T1 => Cat1[T1], t2: T2 => Cat1[T2], t3: T2 => Cat1[T2]):
  F[R ** T3 ** T2 ** T1] => F[R ** T1 ** T3 ** T2 ** T1] = f => {
    f.visitor.visitInsn(Opcodes.DUP_X2)
    castF(f)
  }

  def DUP_X2_onCat2
  [R <: Stack, T1 <: PType, T2 <: PType]
  (implicit t1: T1 => Cat1[T1], t2: T2 => Cat2[T2]):
  F[R ** T2 ** T1] => F[R ** T1 ** T2 ** T1] = f => {
    f.visitor.visitInsn(Opcodes.DUP_X2)
    castF(f)
  }

  def DUP2_X2_cat1_onCat1
  [R <: Stack, T1 <: PType, T2 <: PType, T3 <: PType, T4 <: PType]
  (implicit t1: T1 => Cat1[T1], t2: T2 => Cat1[T2], t3: T2 => Cat1[T2], t4: T2 => Cat1[T2]):
  F[R ** T4 ** T3 ** T2 ** T1] => F[R ** T2 ** T1 ** T4 ** T3 ** T2 ** T1] = f => {
    f.visitor.visitInsn(Opcodes.DUP2_X2)
    castF(f)
  }

  def DUP2_X2_cat2_onCat1
  [R <: Stack, T1 <: PType, T2 <: PType, T3 <: PType]
  (implicit t1: T1 => Cat2[T1], t2: T2 => Cat1[T2], t3: T2 => Cat1[T2]):
  F[R ** T3 ** T2 ** T1] => F[R ** T1 ** T3 ** T2 ** T1] = f => {
    f.visitor.visitInsn(Opcodes.DUP2_X2)
    castF(f)
  }

  def DUP2_X2_cat1_onCat2
  [R <: Stack, T1 <: PType, T2 <: PType, T3 <: PType]
  (implicit t1: T1 => Cat1[T1], t2: T2 => Cat1[T2], t3: T2 => Cat2[T2]):
  F[R ** T3 ** T2 ** T1] => F[R ** T2 ** T1 ** T3 ** T2 ** T1] = f => {
    f.visitor.visitInsn(Opcodes.DUP2_X2)
    castF(f)
  }

  def DUP2_X2_cat2_onCat2
  [R <: Stack, T1 <: PType, T2 <: PType]
  (implicit t1: T1 => Cat2[T1], t2: T2 => Cat2[T2]):
  F[R ** T2 ** T1] => F[R ** T1 ** T2 ** T1] = f => {
    f.visitor.visitInsn(Opcodes.DUP2_X2)
    castF(f)
  }

  def DUP
  [R <: Stack, T <: PType]
  (implicit t: T => Cat1[T]):
  F[R ** T] => F[R ** T ** T] = f => {
    f.visitor.visitInsn(Opcodes.DUP)
    castF(f)
  }

  def ISUB
  [R <: Stack]:
  F[R ** PInt32 ** PInt32] => F[R ** PInt32] = f => {
    f.visitor.visitInsn(Opcodes.ISUB)
    castF(f)
  }

  def IADD
  [R <: Stack]:
  F[R ** PInt32 ** PInt32] => F[R ** PInt32] = f => {
    f.visitor.visitInsn(Opcodes.IADD)
    castF(f)
  }

  def IF_ICMPEQ32
  [R <: Stack, R2 <: Stack]
  (branch1: F[R] => F[R2], branch2: F[R] => F[R2]):
  F[R ** PInt32 ** PInt32] => F[R2] = f => {
    val b1 = new Label()
    val end = new Label()
    f.visitor.visitJumpInsn(Opcodes.IF_ICMPEQ, b1)

    branch2(castF(f))
    f.visitor.visitJumpInsn(Opcodes.GOTO, end)

    f.visitor.visitLabel(b1)
    branch1(castF(f))
    f.visitor.visitLabel(end)
    castF(f)
  }

  def IF_ICMPNE32
  [R <: Stack, R2 <: Stack]
  (branch1: F[R] => F[R2], branch2: F[R] => F[R2]):
  F[R ** PInt32 ** PInt32] => F[R2] = f => {
    val b1 = new Label()
    val end = new Label()
    f.visitor.visitJumpInsn(Opcodes.IF_ICMPNE, b1)

    branch2(castF(f))
    f.visitor.visitJumpInsn(Opcodes.GOTO, end)

    f.visitor.visitLabel(b1)
    branch1(castF(f))
    f.visitor.visitLabel(end)
    castF(f)
  }

  def IF_ICMPEQ16
  [R <: Stack, R2 <: Stack]
  (branch1: F[R] => F[R2], branch2: F[R] => F[R2]):
  F[R ** PInt16 ** PInt16] => F[R2] = f => {
    IF_ICMPEQ32(branch1, branch2)(castF(f))
    castF(f)
  }

  def IF_ICMPNE16
  [R <: Stack, R2 <: Stack]
  (branch1: F[R] => F[R2], branch2: F[R] => F[R2]):
  F[R ** PInt16 ** PInt16] => F[R2] = f => {
    IF_ICMPNE32(branch1, branch2)(castF(f))
    castF(f)
  }

  def pushUnit
  [R <: Stack]:
  F[R] => F[R ** PReference[PUnit]] = f => {
    val className = RUnit.toInternalName
    val classDescriptor = RUnit.nothingToThisMethodDescriptor
    f.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, className, "getInstance", classDescriptor, false)
    castF(f)
  }

  def pushNull
  [R <: Stack, T <: PRefType]
  (tpe: RType[PReference[T]]):
  F[R] => F[R ** PReference[T]] = f => {
    f.visitor.visitInsn(Opcodes.ACONST_NULL)
    f.visitor.visitTypeInsn(Opcodes.CHECKCAST, internalNameOfReference(tpe))
    castF(f)
  }

  /*
   * Generate code to load an integer constant.
   *
   * Uses the smallest number of bytes necessary, e.g. ICONST_0 takes 1 byte to load a 0, but BIPUSH 7 takes 2 bytes to
   * load a 7, and SIPUSH 200 takes 3 bytes to load a 200. However, note that values on the stack normally take up 4
   * bytes. The exception is if we set `isLong` to true, in which case a cast will be performed if necessary.
   *
   * This is needed because sometimes we expect the operands to be a long, which means two (int) values are popped from
   * the stack and concatenated to form a long.
   */
  private def compileInt(visitor: MethodVisitor, i: Long, isCat2: Boolean = false): Unit = {
    i match {
      case -1 => visitor.visitInsn(Opcodes.ICONST_M1)
      case 0 => if (!isCat2) visitor.visitInsn(Opcodes.ICONST_0) else visitor.visitInsn(Opcodes.LCONST_0)
      case 1 => if (!isCat2) visitor.visitInsn(Opcodes.ICONST_1) else visitor.visitInsn(Opcodes.LCONST_1)
      case 2 => visitor.visitInsn(Opcodes.ICONST_2)
      case 3 => visitor.visitInsn(Opcodes.ICONST_3)
      case 4 => visitor.visitInsn(Opcodes.ICONST_4)
      case 5 => visitor.visitInsn(Opcodes.ICONST_5)
      case _ if scala.Byte.MinValue <= i && i <= scala.Byte.MaxValue => visitor.visitIntInsn(Opcodes.BIPUSH, i.toInt)
      case _ if scala.Short.MinValue <= i && i <= scala.Short.MaxValue => visitor.visitIntInsn(Opcodes.SIPUSH, i.toInt)
      case _ if scala.Int.MinValue <= i && i <= scala.Int.MaxValue => visitor.visitLdcInsn(i.toInt)
      case _ => visitor.visitLdcInsn(i)
    }
    if (isCat2 && scala.Int.MinValue <= i && i <= scala.Int.MaxValue && i != 0 && i != 1) visitor.visitInsn(Opcodes.I2L)
  }

  def pushBool
  [R <: Stack]
  (b: Boolean):
  F[R] => F[R ** PInt32] =
    pushInt32(if (b) 1 else 0)

  def pushInt8
  [R <: Stack]
  (n: Int):
  F[R] => F[R ** PInt8] = f => {
    compileInt(f.visitor, n)
    castF(f)
  }

  def pushInt16
  [R <: Stack]
  (n: Int):
  F[R] => F[R ** PInt16] = f => {
    compileInt(f.visitor, n)
    castF(f)
  }

  def pushInt32
  [R <: Stack]
  (n: Int):
  F[R] => F[R ** PInt32] = f => {
    compileInt(f.visitor, n)
    castF(f)
  }

  def pushInt64
  [R <: Stack]
  (n: Long):
  F[R] => F[R ** PInt64] = f => {
    compileInt(f.visitor, n, isCat2 = true)
    castF(f)
  }

  def pushFloat32
  [R <: Stack]
  (n: Float):
  F[R] => F[R ** PFloat32] = f => {
    n match {
      case 0f => f.visitor.visitInsn(Opcodes.FCONST_0)
      case 1f => f.visitor.visitInsn(Opcodes.FCONST_1)
      case 2f => f.visitor.visitInsn(Opcodes.FCONST_2)
      case _ => f.visitor.visitLdcInsn(f)
    }
    castF(f)
  }

  def pushFloat64
  [R <: Stack]
  (n: Double):
  F[R] => F[R ** PFloat64] = f => {
    n match {
      case 0d => f.visitor.visitInsn(Opcodes.DCONST_0)
      case 1d => f.visitor.visitInsn(Opcodes.DCONST_1)
      case _ => f.visitor.visitLdcInsn(n)
    }
    castF(f)
  }

  def pushChar
  [R <: Stack]
  (c: scala.Char):
  F[R] => F[R ** PChar] =
    f => castF(pushInt16(c)(f))

  def ALOAD
  [R <: Stack, T <: PRefType]
  (index: Int, tpe: Tag[T] = null):
  F[R] => F[R ** PReference[T]] = f => {
    f.visitor.visitVarInsn(Opcodes.ALOAD, index)
    castF(f)
  }

  def THISLOAD
  [R <: Stack, T <: PRefType]
  (tpe: Tag[T] = null):
  F[R] => F[R ** PReference[T]] =
    ALOAD(0)

  // TODO(JLS): This should both return StackEnd (no code should follow) and R ** T (compileExp should push T on stack) (maybe stop flag type on F)
  def TAILCALL
  [R <: Stack, T <: PType]
  (arguments: List[ErasedAst.Expression[_ <: PType]], fnName: JvmName, returnType: Tag[T] = null):
  F[R ** PReference[PFunction]] => F[R ** T] = {
    START[R ** PReference[PFunction]] ~
      setArgs(fnName, arguments, GenFunctionInterfaces.argFieldName) ~
      AReturnNoEnd(tagOf[T])
  }

  private def AReturnNoEnd
  [R <: Stack, T1 <: PRefType, T2 <: PType]
  (t2: Tag[T2] = null):
  F[R ** PReference[T1]] => F[R ** T2] = f => {
    f.visitor.visitInsn(Opcodes.ARETURN)
    castF(f)
  }

  def unwind
  [R <: Stack, T <: PType]
  (contName: JvmName, returnType: RType[T]):
  F[R ** PReference[PFunction]] => F[R ** T] = f => {
    f.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, contName.toInternalName, GenContinuationInterfaces.unwindMethodName, returnType.erasedNothingToThisMethodDescriptor, false)
    undoErasure(returnType, f.visitor)
    castF(f)
  }

  def SELFTAILCALL
  [R <: Stack, T <: PType]
  (arguments: List[ErasedAst.Expression[_ <: PType]], defClassName: JvmName, returnType: Tag[T] = null):
  F[R] => F[R ** T] = {
    START[R] ~
      THISLOAD(tagOf[PFunction]) ~
      setArgs(defClassName, arguments, GenFunctionInterfaces.argFieldName) ~
      AReturnNoEnd(returnType)
  }

  // TODO(JLS): could be made with other instructions
  def makeAndInitDef[R <: Stack]
  (className: JvmName):
  F[R] => F[R ** PReference[PFunction]] = f => {
    f.visitor.visitTypeInsn(Opcodes.NEW, className.toInternalName)
    f.visitor.visitInsn(Opcodes.DUP)
    f.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, className.toInternalName, JvmName.constructorMethod, JvmName.nothingToVoid, false)
    castF(f)
  }

  def setArgs[R <: Stack]
  (defClassName: JvmName, args: List[ErasedAst.Expression[_ <: PType]], fieldName: Int => String):
  F[R ** PReference[PFunction]] => F[R ** PReference[PFunction]] =
    START[R ** PReference[PFunction]] ~
      multiComposition(args.zipWithIndex) {
        case (exp, index) =>
          START[R ** PReference[PFunction]] ~ DUP ~ compileExp(exp) ~ setArg(defClassName, fieldName(index), exp.tpe.erasedDescriptor)
      }

  private def setArg[R <: Stack, T <: PType]
  (defClassName: JvmName, fieldName: String, erasedTpe: String):
  F[R ** PReference[PFunction] ** T] => F[R] = f => {
    f.visitor.visitFieldInsn(Opcodes.PUTFIELD, defClassName.toInternalName, fieldName, erasedTpe)
    castF(f)
  }

  // TODO(JLS): fnName can be Fn interface or Def class but that is a bit confusing
  def CALL
  [R <: Stack, T <: PType]
  (arguments: List[ErasedAst.Expression[_ <: PType]], fnName: JvmName, returnType: RType[T]):
  F[R ** PReference[PFunction]] => F[R ** T] = {
    START[R ** PReference[PFunction]] ~
      setArgs(fnName, arguments, GenFunctionInterfaces.argFieldName) ~
      unwind(fnName, returnType)
  }

  def CREATEDEF
  [R <: Stack, T <: PType]
  (defClassName: JvmName):
  F[R] => F[R ** PReference[PFunction]] = {
    START[R] ~ makeAndInitDef(defClassName)
  }

  def CREATECLOSURE
  [R <: Stack, T <: PType]
  (freeVars: List[ErasedAst.FreeVar], defClassName: JvmName):
  F[R] => F[R ** PReference[PFunction]] =
    makeAndInitDef(defClassName) ~
      setArgs(defClassName, freeVars.map(f => ErasedAst.Expression.Var(f.sym, f.tpe, SourceLocation.Unknown)), GenClosureClasses.cloArgFieldName)
  // TODO(JLS): This added exp could maybe cause trouble

  def ILOAD
  [R <: Stack]
  (index: Int):
  F[R] => F[R ** PInt32] = f => {
    f.visitor.visitVarInsn(Opcodes.ILOAD, index)
    castF(f)
  }

  def BLOAD
  [R <: Stack]
  (index: Int):
  F[R] => F[R ** PInt8] = f => {
    ILOAD(index)(f)
    castF(f)
  }

  def SLOAD
  [R <: Stack]
  (index: Int):
  F[R] => F[R ** PInt16] = f => {
    ILOAD(index)(f)
    castF(f)
  }

  def LLOAD
  [R <: Stack]
  (index: Int):
  F[R] => F[R ** PInt64] = f => {
    f.visitor.visitVarInsn(Opcodes.LLOAD, index)
    castF(f)
  }

  def CLOAD
  [R <: Stack]
  (index: Int):
  F[R] => F[R ** PChar] = f => {
    ILOAD(index)(f)
    castF(f)
  }

  def FLOAD
  [R <: Stack]
  (index: Int):
  F[R] => F[R ** PFloat32] = f => {
    f.visitor.visitVarInsn(Opcodes.FLOAD, index)
    castF(f)
  }

  def DLOAD
  [R <: Stack]
  (index: Int):
  F[R] => F[R ** PFloat64] = f => {
    f.visitor.visitVarInsn(Opcodes.DLOAD, index)
    castF(f)
  }

  def XLOAD
  [R <: Stack, T <: PType]
  (tpe: RType[T], index: Int):
  F[R] => F[R ** T] = tpe match {
    case RBool | RInt32 => ILOAD(index)
    case RInt8 => BLOAD(index)
    case RInt16 => SLOAD(index)
    case RInt64 => LLOAD(index)
    case RChar => CLOAD(index)
    case RFloat32 => FLOAD(index)
    case RFloat64 => DLOAD(index)
    case RReference(_) => ALOAD(index)
  }

  def BALOAD
  [R <: Stack]:
  F[R ** PReference[PArray[PInt8]] ** PInt32] => F[R ** PInt8] = f => {
    f.visitor.visitInsn(Opcodes.BALOAD)
    castF(f)
  }

  def SALOAD
  [R <: Stack]:
  F[R ** PReference[PArray[PInt16]] ** PInt32] => F[R ** PInt16] = f => {
    f.visitor.visitInsn(Opcodes.SALOAD)
    castF(f)
  }

  def IALOAD
  [R <: Stack]:
  F[R ** PReference[PArray[PInt32]] ** PInt32] => F[R ** PInt32] = f => {
    f.visitor.visitInsn(Opcodes.IALOAD)
    castF(f)
  }

  def LALOAD
  [R <: Stack]:
  F[R ** PReference[PArray[PInt64]] ** PInt32] => F[R ** PInt64] = f => {
    f.visitor.visitInsn(Opcodes.LALOAD)
    castF(f)
  }

  def CALOAD
  [R <: Stack]:
  F[R ** PReference[PArray[PChar]] ** PInt32] => F[R ** PChar] = f => {
    f.visitor.visitInsn(Opcodes.CALOAD)
    castF(f)
  }

  def FALOAD
  [R <: Stack]:
  F[R ** PReference[PArray[PFloat32]] ** PInt32] => F[R ** PFloat32] = f => {
    f.visitor.visitInsn(Opcodes.FALOAD)
    castF(f)
  }

  def DALOAD
  [R <: Stack]:
  F[R ** PReference[PArray[PFloat64]] ** PInt32] => F[R ** PFloat64] = f => {
    f.visitor.visitInsn(Opcodes.DALOAD)
    castF(f)
  }

  def AALOAD
  [R <: Stack, T <: PRefType]:
  F[R ** PReference[PArray[PReference[T]]] ** PInt32] => F[R ** PReference[T]] = f => {
    f.visitor.visitInsn(Opcodes.AALOAD)
    castF(f)
  }

  def XALOAD
  [R <: Stack, T <: PType]
  (tpe: RType[T]):
  F[R ** PReference[PArray[T]] ** PInt32] => F[R ** T] = tpe match {
    case RBool | RInt32 => IALOAD
    case RInt8 => BALOAD
    case RInt16 => SALOAD
    case RInt64 => LALOAD
    case RChar => CALOAD
    case RFloat32 => FALOAD
    case RFloat64 => DALOAD
    case RReference(_) => AALOAD
  }

  def BAStore
  [R <: Stack]:
  F[R ** PReference[PArray[PInt8]] ** PInt32 ** PInt8] => F[R] = f => {
    f.visitor.visitInsn(Opcodes.BASTORE)
    castF(f)
  }

  def SAStore
  [R <: Stack]:
  F[R ** PReference[PArray[PInt16]] ** PInt32 ** PInt16] => F[R] = f => {
    f.visitor.visitInsn(Opcodes.SASTORE)
    castF(f)
  }

  def IAStore
  [R <: Stack]:
  F[R ** PReference[PArray[PInt32]] ** PInt32 ** PInt32] => F[R] = f => {
    f.visitor.visitInsn(Opcodes.IASTORE)
    castF(f)
  }

  def LAStore
  [R <: Stack]:
  F[R ** PReference[PArray[PInt64]] ** PInt32 ** PInt64] => F[R] = f => {
    f.visitor.visitInsn(Opcodes.LASTORE)
    castF(f)
  }

  def CAStore
  [R <: Stack]:
  F[R ** PReference[PArray[PChar]] ** PInt32 ** PChar] => F[R] = f => {
    f.visitor.visitInsn(Opcodes.CASTORE)
    castF(f)
  }

  def FAStore
  [R <: Stack]:
  F[R ** PReference[PArray[PFloat32]] ** PInt32 ** PFloat32] => F[R] = f => {
    f.visitor.visitInsn(Opcodes.FASTORE)
    castF(f)
  }

  def DAStore
  [R <: Stack]:
  F[R ** PReference[PArray[PFloat64]] ** PInt32 ** PFloat64] => F[R] = f => {
    f.visitor.visitInsn(Opcodes.DASTORE)
    castF(f)
  }

  def AAStore
  [R <: Stack, T <: PRefType]:
  F[R ** PReference[PArray[PReference[T]]] ** PInt32 ** PReference[T]] => F[R] = f => {
    f.visitor.visitInsn(Opcodes.AASTORE)
    castF(f)
  }

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
  def IStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PInt32] => F[R] = f => {
    f.visitor.visitVarInsn(Opcodes.ISTORE, sym.getStackOffset + symOffsetOffset)
    castF(f)
  }

  def SStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PInt16] => F[R] = f => {
    f.visitor.visitVarInsn(Opcodes.ISTORE, sym.getStackOffset + symOffsetOffset)
    castF(f)
  }

  def BStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PInt8] => F[R] = f => {
    f.visitor.visitVarInsn(Opcodes.ISTORE, sym.getStackOffset + symOffsetOffset)
    castF(f)
  }

  def CStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PChar] => F[R] = f => {
    f.visitor.visitVarInsn(Opcodes.ISTORE, sym.getStackOffset + symOffsetOffset)
    castF(f)
  }

  def LStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PInt64] => F[R] = f => {
    f.visitor.visitVarInsn(Opcodes.LSTORE, sym.getStackOffset + symOffsetOffset)
    castF(f)
  }

  def FStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PFloat32] => F[R] = f => {
    f.visitor.visitVarInsn(Opcodes.FSTORE, sym.getStackOffset + symOffsetOffset)
    castF(f)
  }

  def DStore
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PFloat64] => F[R] = f => {
    f.visitor.visitVarInsn(Opcodes.DSTORE, sym.getStackOffset + symOffsetOffset)
    castF(f)
  }

  def AStore
  [R <: Stack, T <: PRefType]
  (sym: Symbol.VarSym):
  F[R ** PReference[T]] => F[R] = f => {
    f.visitor.visitVarInsn(Opcodes.ASTORE, sym.getStackOffset + symOffsetOffset)
    castF(f)
  }

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

  // TODO(JLS): bools are awkward with no PType, PBool
  def BOOLNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PInt32]]] = f => {
    f.visitor.visitIntInsn(Opcodes.NEWARRAY, Opcodes.T_BOOLEAN)
    castF(f)
  }

  def BNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PInt8]]] = f => {
    f.visitor.visitIntInsn(Opcodes.NEWARRAY, Opcodes.T_BYTE)
    castF(f)
  }

  def SNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PInt16]]] = f => {
    f.visitor.visitIntInsn(Opcodes.NEWARRAY, Opcodes.T_SHORT)
    castF(f)
  }

  def INEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PInt32]]] = f => {
    f.visitor.visitIntInsn(Opcodes.NEWARRAY, Opcodes.T_INT) //TODO(JLS): Save strings somewhere else
    castF(f)
  }

  def LNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PInt64]]] = f => {
    f.visitor.visitIntInsn(Opcodes.NEWARRAY, Opcodes.T_LONG)
    castF(f)
  }

  def CNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PChar]]] = f => {
    f.visitor.visitIntInsn(Opcodes.NEWARRAY, Opcodes.T_CHAR)
    castF(f)
  }

  def FNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PFloat32]]] = f => {
    f.visitor.visitIntInsn(Opcodes.NEWARRAY, Opcodes.T_FLOAT)
    castF(f)
  }

  def DNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PFloat64]]] = f => {
    f.visitor.visitIntInsn(Opcodes.NEWARRAY, Opcodes.T_DOUBLE)
    castF(f)
  }

  def ANEWARRAY
  [R <: Stack, T <: PRefType]
  (elmType: RType[PReference[T]]):
  F[R ** PInt32] => F[R ** PReference[PArray[PReference[T]]]] = f => {
    f.visitor.visitTypeInsn(Opcodes.ANEWARRAY, squeezeReference(elmType).referenceType.toInternalName)
    castF(f)
  }

  def XNEWARRAY
  [R <: Stack, T <: PType]
  (arrayType: RType[PReference[PArray[T]]]):
  F[R ** PInt32] => F[R ** PReference[PArray[T]]] =
    arrayType match {
      case RReference(RArray(tpe)) => tpe match {
        case RBool => BOOLNEWARRAY
        case RInt8 => BNEWARRAY
        case RInt16 => SNEWARRAY
        case RInt32 => INEWARRAY
        case RInt64 => LNEWARRAY
        case RChar => CNEWARRAY
        case RFloat32 => FNEWARRAY
        case RFloat64 => DNEWARRAY
        case RReference(_) => ANEWARRAY(tpe)
      }
      case _ => throw InternalCompilerException("unexpected non-array type")
    }

  def systemArrayCopy
  [R <: Stack, T <: PType]:
  F[R ** PReference[PArray[T]] ** PInt32 ** PReference[PArray[T]] ** PInt32 ** PInt32] => F[R] =
    ???

  def arraysFill
  [R <: Stack, T <: PType]
  (elementType: RType[T]):
  F[R ** PReference[PArray[T]] ** T] => F[R] = f => {
    //TODO(JLS): where to store the name?
    f.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, JvmName.Java.Util.Arrays.toInternalName, "fill", JvmName.getMethodDescriptor(RReference(RArray(elementType)) :: elementType :: Nil, None), false)
    castF(f)
  }

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
  private def defMakeFunction
  [R <: Stack, T <: PType]
  (x: F[R], t: RType[T]):
  F[R ** T] = {
    // TODO(JLS): where is this string stored
    //t.toDescriptor
    ???
  }

  private def foo[R <: Stack] = (r: F[R]) => defMakeFunction(defMakeFunction(r, RInt32), RInt32)

  implicit class ComposeOps[A <: Stack, B <: Stack](ab: F[A] => F[B]) {
    def ~[C <: Stack](bc: F[B] => F[C]): F[A] => F[C] =
      f => bc(ab(f))
  }

}
