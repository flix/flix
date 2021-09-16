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
import ca.uwaterloo.flix.language.ast.{ErasedAst, PRefType, PType, RType, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm.{Label, MethodVisitor, Opcodes}

object Instructions {

  private def castF[R <: Stack](f: F[_]): F[R] = f.asInstanceOf[F[R]]

  trait Tag[T]

  def tagOf[T]: Tag[T] = null

  implicit class TagBox[X <: PType](x: RType[X]) {
    def tagOf: Tag[X] = null
  }

  def WithSource[R <: Stack](loc: SourceLocation): F[R] => F[R] = f => {
    val label = new Label()
    f.visitLabel(label)
    f.visitLineNumber(loc.beginLine, label)
    f
  }

  def placeLabel[R <: Stack](label: Label): F[R] => F[R] = f => {
    f.visitLabel(label)
    f
  }

  def tryCatch[R <: Stack, Res <: Stack](body: F[R] => F[Res], catchCases: List[(Label, Class[_], F[R ** PReference[PAnyObject]] => F[Res])]): F[R] => F[Res] = f => {
    // Introduce a label for before the try block.
    val beforeTryBlock = new Label()

    // Introduce a label for after the try block.
    val afterTryBlock = new Label()

    // Introduce a label after the try block and after all catch rules.
    val afterTryAndCatch = new Label()

    // Emit a try catch block for each catch rule.
    for ((handlerLabel, clazz, _) <- catchCases) {
      f.visitTryCatchBlock(beforeTryBlock, afterTryBlock, handlerLabel, clazz)
    }

    // Emit code for the try block.
    f.visitLabel(beforeTryBlock)
    body(f)
    f.visitLabel(afterTryBlock)
    f.visitJumpInsn(Opcodes.GOTO, afterTryAndCatch)

    // Emit code for each catch rule.
    for ((handlerLabel, _, handleIns) <- catchCases) {
      // Emit the label.
      f.visitLabel(handlerLabel)

      // Emit code for the handler body expression.
      handleIns(f.asInstanceOf[F[R ** PReference[PAnyObject]]])
      f.visitJumpInsn(Opcodes.GOTO, afterTryAndCatch)
    }

    // Add the label after both the try and catch rules.
    f.visitLabel(afterTryAndCatch)
    f.asInstanceOf[F[Res]]
  }

  def throwCompilerError
  [R <: Stack, T <: PType]
  (exceptionName: JvmName, loc: SourceLocation, tag: Tag[T] = null):
  F[R] => F[R ** T] = {
    START[R] ~
      NEW(JvmName.Flix.ReifiedSourceLocation, tagOf[PAnyObject]) ~
      DUP ~
      pushString(loc.source.format) ~
      pushInt32(loc.beginLine) ~
      pushInt32(loc.beginCol) ~
      pushInt32(loc.endLine) ~
      pushInt32(loc.endCol) ~
      (f => {
        f.visitMethodInsn(Opcodes.INVOKESPECIAL, JvmName.Flix.ReifiedSourceLocation.internalName, JvmName.constructorMethod, JvmName.getMethodDescriptor(List(RStr, RInt32, RInt32, RInt32, RInt32), None))
        f.asInstanceOf[F[R ** PReference[PAnyObject]]]
      }) ~
      NEW(exceptionName, tagOf[PAnyObject]) ~
      DUP_X1 ~
      SWAP ~
      (f => {
        f.visitMethodInsn(Opcodes.INVOKESPECIAL, exceptionName.internalName, JvmName.constructorMethod, JvmName.getMethodDescriptor(List(JvmName.Flix.ReifiedSourceLocation), None))
        f.visitInsn(Opcodes.ATHROW)
        f.asInstanceOf[F[R ** T]]
      })
  }

  def throwStringedCompilerError
  [R <: Stack, T <: PType]
  (exceptionName: JvmName, string: String, loc: SourceLocation, tag: Tag[T] = null):
  F[R] => F[R ** T] = {
    START[R] ~
      NEW(JvmName.Flix.ReifiedSourceLocation, tagOf[PAnyObject]) ~
      DUP ~
      pushString(loc.source.format) ~
      pushInt32(loc.beginLine) ~
      pushInt32(loc.beginCol) ~
      pushInt32(loc.endLine) ~
      pushInt32(loc.endCol) ~
      (f => {
        f.visitMethodInsn(Opcodes.INVOKESPECIAL, JvmName.Flix.ReifiedSourceLocation.internalName, JvmName.constructorMethod, JvmName.getMethodDescriptor(List(RStr, RInt32, RInt32, RInt32, RInt32), None))
        f.asInstanceOf[F[R ** PReference[PAnyObject]]]
      }) ~
      NEW(exceptionName, tagOf[PAnyObject]) ~
      DUP_X1 ~
      SWAP ~
      pushString(string) ~
      SWAP ~
      ((f: F[R ** PReference[PAnyObject] ** PReference[PAnyObject] ** PReference[PStr] ** PReference[PAnyObject]]) => {
        f.visitMethodInsn(Opcodes.INVOKESPECIAL, exceptionName.internalName, JvmName.constructorMethod, JvmName.getMethodDescriptor(List(RStr, JvmName.Flix.ReifiedSourceLocation), None))
        f.visitInsn(Opcodes.ATHROW)
        f.asInstanceOf[F[R ** T]]
      })
  }

  def START
  [R <: Stack]:
  F[R] => F[R] =
    f => f

  // TODO(JLS): This needs to propagate exceptions gracefully
  def WITHMONITOR
  [R <: Stack, S <: PRefType, T <: PType]
  (e: RType[T])(f: F[R ** PReference[S]] => F[R ** PReference[S] ** T]):
  F[R ** PReference[S]] => F[R ** T] = {
    START[R ** PReference[S]] ~
      DUP ~
      MONITORENTER ~
      f ~
      XSWAP(e, RReference(null)) ~ // TODO(JLS): add aditional SWAP_Something_on_cat1/2
      MONITOREXIT
  }

  private def MONITORENTER
  [R <: Stack, S <: PRefType]:
  F[R ** PReference[S]] => F[R] = f => {
    f.visitInsn(Opcodes.MONITORENTER)
    castF(f)
  }

  private def MONITOREXIT
  [R <: Stack, S <: PRefType]:
  F[R ** PReference[S]] => F[R] = f => {
    f.visitInsn(Opcodes.MONITOREXIT)
    castF(f)
  }

  private def conditional(branchIns: Int, f: F[_], branch1: Unit => _, branch2: Unit => _): Unit = {
    val b1 = new Label()
    val end = new Label()
    f.visitJumpInsn(branchIns, b1)

    branch2.apply(())
    f.visitJumpInsn(Opcodes.GOTO, end)

    f.visitLabel(b1)
    branch1.apply(())
    f.visitLabel(end)
  }

  // TODO(JLS): maybe tag the starting stack?
  def IF_ACMPEQ
  [R1 <: Stack, R2 <: Stack, T <: PRefType]
  (branch1: F[R1] => F[R2])(branch2: F[R1] => F[R2]):
  F[R1 ** PReference[T] ** PReference[T]] => F[R2] = f => {
    conditional(Opcodes.IF_ACMPEQ, f, _ => branch1(castF(f)), _ => branch2(castF(f)))
    castF(f)
  }

  def IF_ACMPNE
  [R1 <: Stack, R2 <: Stack, T1 <: PRefType, T2 <: PRefType]
  (tag: Tag[R2] = null)(branch1: F[R1] => F[R2])(branch2: F[R1] => F[R2]):
  F[R1 ** PReference[T1] ** PReference[T2]] => F[R2] = f => {
    conditional(Opcodes.IF_ACMPNE, f, _ => branch1(castF(f)), _ => branch2(castF(f)))
    castF(f)
  }

  def IF_ICMPEQ
  [R1 <: Stack, R2 <: Stack, T1 <: PType, T2 <: PType]
  (branch1: F[R1] => F[R2])(branch2: F[R1] => F[R2])
  (implicit t1: T1 => Int32Usable[T1], t2: T2 => Int32Usable[T2]):
  F[R1 ** T2 ** T1] => F[R2] = f => {
    conditional(Opcodes.IF_ICMPEQ, f, _ => branch1(castF(f)), _ => branch2(castF(f)))
    castF(f)
  }

  def IF_ICMPNE
  [R1 <: Stack, R2 <: Stack, T1 <: PType, T2 <: PType]
  (branch1: F[R1] => F[R2])(branch2: F[R1] => F[R2])
  (implicit t1: T1 => Int32Usable[T1], t2: T2 => Int32Usable[T2]):
  F[R1 ** T2 ** T1] => F[R2] = f => {
    conditional(Opcodes.IF_ICMPNE, f, _ => branch1(castF(f)), _ => branch2(castF(f)))
    castF(f)
  }

  def IF_ICMPLT
  [R1 <: Stack, R2 <: Stack, T1 <: PType, T2 <: PType]
  (branch1: F[R1] => F[R2])(branch2: F[R1] => F[R2])
  (implicit t1: T1 => Int32Usable[T1], t2: T2 => Int32Usable[T2]):
  F[R1 ** T2 ** T1] => F[R2] = f => {
    conditional(Opcodes.IF_ICMPLT, f, _ => branch1(castF(f)), _ => branch2(castF(f)))
    castF(f)
  }

  def IF_ICMPGE
  [R1 <: Stack, R2 <: Stack, T1 <: PType, T2 <: PType]
  (branch1: F[R1] => F[R2])(branch2: F[R1] => F[R2])
  (implicit t1: T1 => Int32Usable[T1], t2: T2 => Int32Usable[T2]):
  F[R1 ** T2 ** T1] => F[R2] = f => {
    conditional(Opcodes.IF_ICMPGE, f, _ => branch1(castF(f)), _ => branch2(castF(f)))
    castF(f)
  }

  def IF_ICMPGT
  [R1 <: Stack, R2 <: Stack, T1 <: PType, T2 <: PType]
  (branch1: F[R1] => F[R2])(branch2: F[R1] => F[R2])
  (implicit t1: T1 => Int32Usable[T1], t2: T2 => Int32Usable[T2]):
  F[R1 ** T2 ** T1] => F[R2] = f => {
    conditional(Opcodes.IF_ICMPGT, f, _ => branch1(castF(f)), _ => branch2(castF(f)))
    castF(f)
  }

  def IF_ICMPLE
  [R1 <: Stack, R2 <: Stack, T1 <: PType, T2 <: PType]
  (branch1: F[R1] => F[R2])(branch2: F[R1] => F[R2])
  (implicit t1: T1 => Int32Usable[T1], t2: T2 => Int32Usable[T2]):
  F[R1 ** T2 ** T1] => F[R2] = f => {
    conditional(Opcodes.IF_ICMPLE, f, _ => branch1(castF(f)), _ => branch2(castF(f)))
    castF(f)
  }

  def FCMPG
  [R <: Stack]:
  F[R ** PFloat32 ** PFloat32] => F[R ** PInt32] = f => {
    f.visitInsn(Opcodes.FCMPG)
    castF(f)
  }

  def DCMPG
  [R <: Stack]:
  F[R ** PFloat64 ** PFloat64] => F[R ** PInt32] = f => {
    f.visitInsn(Opcodes.DCMPG)
    castF(f)
  }

  def FCMPL
  [R <: Stack]:
  F[R ** PFloat32 ** PFloat32] => F[R ** PInt32] = f => {
    f.visitInsn(Opcodes.FCMPL)
    castF(f)
  }

  def DCMPL
  [R <: Stack]:
  F[R ** PFloat64 ** PFloat64] => F[R ** PInt32] = f => {
    f.visitInsn(Opcodes.DCMPL)
    castF(f)
  }

  def LCMP
  [R <: Stack]:
  F[R ** PInt64 ** PInt64] => F[R ** PInt32] = f => {
    f.visitInsn(Opcodes.LCMP)
    castF(f)
  }

  def BigIntCompareTo
  [R <: Stack]:
  F[R ** PReference[PBigInt] ** PReference[PBigInt]] => F[R ** PInt32] = f => {
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Java.BigInteger.internalName, "compareTo",
      JvmName.getMethodDescriptor(List(JvmName.Java.BigInteger), JvmName.Java.BigInteger), false)
    castF(f)
  }

  def IFEQ
  [R1 <: Stack, R2 <: Stack]
  (branch1: F[R1] => F[R2])(branch2: F[R1] => F[R2]):
  F[R1 ** PInt32] => F[R2] = f => {
    conditional(Opcodes.IFEQ, f, _ => branch1(castF(f)), _ => branch2(castF(f)))
    castF(f)
  }

  def IFNE
  [R1 <: Stack, R2 <: Stack, T <: PType]
  (branch1: F[R1] => F[R2])(branch2: F[R1] => F[R2])
  (implicit t: T => Int32Usable[T]):
  F[R1 ** T] => F[R2] = f => {
    conditional(Opcodes.IFNE, f, _ => branch1(castF(f)), _ => branch2(castF(f)))
    castF(f)
  }

  def IFLT
  [R1 <: Stack, R2 <: Stack, T <: PType]
  (branch1: F[R1] => F[R2])(branch2: F[R1] => F[R2])
  (implicit t: T => Int32Usable[T]):
  F[R1 ** T] => F[R2] = f => {
    conditional(Opcodes.IFLT, f, _ => branch1(castF(f)), _ => branch2(castF(f)))
    castF(f)
  }

  def IFGE
  [R1 <: Stack, R2 <: Stack, T <: PType]
  (branch1: F[R1] => F[R2])(branch2: F[R1] => F[R2])
  (implicit t: T => Int32Usable[T]):
  F[R1 ** T] => F[R2] = f => {
    conditional(Opcodes.IFGE, f, _ => branch1(castF(f)), _ => branch2(castF(f)))
    castF(f)
  }

  def IFGT
  [R1 <: Stack, R2 <: Stack, T <: PType]
  (branch1: F[R1] => F[R2])(branch2: F[R1] => F[R2])
  (implicit t: T => Int32Usable[T]):
  F[R1 ** T] => F[R2] = f => {
    conditional(Opcodes.IFGT, f, _ => branch1(castF(f)), _ => branch2(castF(f)))
    castF(f)
  }

  def IFLE
  [R1 <: Stack, R2 <: Stack, T <: PType]
  (branch1: F[R1] => F[R2])(branch2: F[R1] => F[R2])
  (implicit t: T => Int32Usable[T]):
  F[R1 ** T] => F[R2] = f => {
    conditional(Opcodes.IFLE, f, _ => branch1(castF(f)), _ => branch2(castF(f)))
    castF(f)
  }

  def IFNULL
  [R1 <: Stack, R2 <: Stack, T <: PRefType]
  (branch1: F[R1] => F[R2])(branch2: F[R1] => F[R2]):
  F[R1 ** PReference[T]] => F[R2] = f => {
    conditional(Opcodes.IFNULL, f, _ => branch1(castF(f)), _ => branch2(castF(f)))
    castF(f)
  }

  def IFNONNULL
  [R1 <: Stack, R2 <: Stack, T <: PRefType]
  (branch1: F[R1] => F[R2])(branch2: F[R1] => F[R2]):
  F[R1 ** PReference[T]] => F[R2] = f => {
    conditional(Opcodes.IFNONNULL, f, _ => branch1(castF(f)), _ => branch2(castF(f)))
    castF(f)
  }

  def ObjEquals
  [R <: Stack, T1 <: PRefType, T2 <: PRefType]:
  F[R ** PReference[T1] ** PReference[T2]] => F[R ** PInt32] = f => {
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Java.Object.internalName, "equals",
      JvmName.getMethodDescriptor(List(RObject), RBool), false)
    castF(f)
  }

  def multiComposition[A, R <: Stack](xs: IterableOnce[A])(generator: A => F[R] => F[R]): F[R] => F[R] = f => {
    xs.iterator.foreach(x => generator(x)(f))
    f
  }

  def SWAP
  [R <: Stack, T1 <: PType, T2 <: PType]
  (implicit t1: T1 => Cat1[T1], t2: T2 => Cat1[T2]):
  F[R ** T2 ** T1] => F[R ** T1 ** T2] = f => {
    f.visitInsn(Opcodes.SWAP)
    castF(f)
  }

  def SWAP_cat1_onCat2
  [R <: Stack, T1 <: PType, T2 <: PType]
  (implicit t1: T1 => Cat1[T1], t2: T2 => Cat2[T2]):
  F[R ** T2 ** T1] => F[R ** T1 ** T2] =
    START[R ** T2 ** T1] ~ DUP_X2_onCat2 ~ POP

  def SWAP_cat2_onCat1
  [R <: Stack, T1 <: PType, T2 <: PType]
  (implicit t1: T1 => Cat2[T1], t2: T2 => Cat1[T2]):
  F[R ** T2 ** T1] => F[R ** T1 ** T2] =
    START[R ** T2 ** T1] ~ DUP2_X1_onCat2 ~ POP2_onCat2

  def SWAP_cat2_onCat2
  [R <: Stack, T1 <: PType, T2 <: PType]
  (implicit t1: T1 => Cat2[T1], t2: T2 => Cat2[T2]):
  F[R ** T2 ** T1] => F[R ** T1 ** T2] =
    START[R ** T2 ** T1] ~ DUP2_X2_cat2_onCat2 ~ POP2_onCat2

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

  //TODO(JLS): note: this doesn't work with "clever" wildcards
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

  def GETSTATIC
  [R <: Stack, T <: PType]
  (className: JvmName, fieldName: String, fieldType: RType[T], undoErasure: Boolean):
  F[R] => F[R ** T] = f => {
    val descriptor = if (undoErasure) fieldType.erasedDescriptor else fieldType.descriptor
    f.visitFieldInsn(Opcodes.GETSTATIC, className.internalName, fieldName, descriptor)
    if (undoErasure) RType.undoErasure(fieldType, f.visitor)
    castF(f)
  }

  def GETSTATIC
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (classType: RType[PReference[T2]], fieldName: String, fieldType: RType[T1], undoErasure: Boolean):
  F[R] => F[R ** T1] =
    GETSTATIC(squeezeReference(classType).jvmName, fieldName, fieldType, undoErasure)

  /**
    * @param undoErasure has no effect when `fieldType` is primitive or the `RReference(Object)` type
    */
  def GETFIELD
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (className: JvmName, fieldName: String, fieldType: RType[T1], undoErasure: Boolean):
  F[R ** PReference[T2]] => F[R ** T1] = f => {
    val descriptor = if (undoErasure) fieldType.erasedDescriptor else fieldType.descriptor
    f.visitFieldInsn(Opcodes.GETFIELD, className.internalName, fieldName, descriptor)
    if (undoErasure) RType.undoErasure(fieldType, f.visitor)
    castF(f)
  }

  /**
    * @param undoErasure has no effect when `fieldType` is primitive or the `RReference(Object)` type
    */
  def GETFIELD
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (classType: RType[PReference[T2]], fieldName: String, fieldType: RType[T1], undoErasure: Boolean):
  F[R ** PReference[T2]] => F[R ** T1] =
    GETFIELD(squeezeReference(classType).jvmName, fieldName, fieldType, undoErasure)

  /**
    * @param undoErasure has no effect when `fieldType` is primitive or the `RReference(Object)` type
    */
  def GETFIELD
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (className: JvmName, fieldName: String, fieldType: JvmName, undoErasure: Boolean, tag: Tag[T1] = null):
  F[R ** PReference[T2]] => F[R ** T1] = f => {
    val descriptor = if (undoErasure) fieldType.erasedDescriptor else fieldType.descriptor
    f.visitFieldInsn(Opcodes.GETFIELD, className.internalName, fieldName, descriptor)
    if (undoErasure) RType.undoErasure(fieldType, f.visitor)
    castF(f)
  }

  // TODO(JLS): make ref/lazy specific versions
  // TODO(JLS): erasedType arg is awkward
  def PUTFIELD
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (classType: RReference[T2], fieldName: String, fieldType: RType[T1], erasedType: Boolean):
  F[R ** PReference[T2] ** T1] => F[R] =
    PUTFIELD(classType.jvmName, fieldName, fieldType, erasedType)

  def PUTFIELD
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (className: JvmName, fieldName: String, fieldType: RType[T1], erasedType: Boolean):
  F[R ** PReference[T2] ** T1] => F[R] = f => {
    val descriptor = if (erasedType) fieldType.erasedDescriptor else fieldType.descriptor
    f.visitFieldInsn(Opcodes.PUTFIELD, className.internalName, fieldName, descriptor)
    castF(f)
  }

  def PUTFIELD
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (classType: RReference[T2], fieldName: String, fieldType: JvmName, erasedType: Boolean):
  F[R ** PReference[T2] ** T1] => F[R] =
    PUTFIELD(classType.jvmName, fieldName, fieldType, erasedType)

  def PUTFIELD
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (className: JvmName, fieldName: String, fieldType: JvmName, erasedType: Boolean):
  F[R ** PReference[T2] ** T1] => F[R] = f => {
    val descriptor = if (erasedType) fieldType.erasedDescriptor else fieldType.descriptor
    f.visitFieldInsn(Opcodes.PUTFIELD, className.internalName, fieldName, descriptor)
    castF(f)
  }

  // made because of weird inference in tuple code gen
  def PUTFIELD
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (classType: RReference[T2], fieldName: String, fieldValue: ErasedAst.Expression[T1], lenv0: Map[Symbol.LabelSym, Label], erasedType: Boolean):
  F[R ** PReference[T2]] => F[R] =
    START[R ** PReference[T2]] ~
      compileExp(fieldValue, lenv0) ~
      PUTFIELD(classType, fieldName, fieldValue.tpe, erasedType)

  def PUTSTATIC
  [R <: Stack, T1 <: PType, T2 <: PRefType]
  (className: JvmName, fieldName: String, fieldType: RType[T1], erasedType: Boolean):
  F[R ** T1] => F[R] = f => {
    val descriptor = if (erasedType) fieldType.erasedDescriptor else fieldType.descriptor
    f.visitFieldInsn(Opcodes.PUTSTATIC, className.internalName, fieldName, descriptor)
    castF(f)
  }

  def CAST
  [R <: Stack, T1 <: PRefType, T2 <: PRefType]
  (castType: RType[PReference[T1]]):
  F[R ** PReference[T2]] => F[R ** PReference[T1]] = CAST(squeezeReference(castType).jvmName)

  def CAST
  [R <: Stack, T1 <: PRefType, T2 <: PRefType]
  (castType: JvmName, tag: Tag[T1] = null):
  F[R ** PReference[T2]] => F[R ** PReference[T1]] = f => {
    f.visitTypeInsn(Opcodes.CHECKCAST, castType.internalName)
    castF(f)
  }

  //TODO(JLS): Only use the new+init combo instruction (impl with capability)
  def NEW
  [R <: Stack, T <: PRefType]
  (classType: RReference[T]):
  F[R] => F[R ** PReference[T]] = f => {
    // TODO(JLS): note: forgot the (f) here previously, easy mistake to miss
    NEW(classType.jvmName)(f)
    castF(f)
  }

  def NEW
  [R <: Stack, T <: PRefType]
  (className: JvmName, t: Tag[T] = null):
  F[R] => F[R ** PReference[T]] = f => {
    f.visitTypeInsn(Opcodes.NEW, className.internalName)
    castF(f)
  }

  def invokeSimpleConstructor
  [R <: Stack, T <: PRefType]
  (classType: RReference[T]):
  F[R ** PReference[T]] => F[R] = f => {
    invokeSimpleConstructor(classType.jvmName)(f)
    castF(f)
  }

  def invokeSimpleConstructor
  [R <: Stack, T <: PRefType]
  (className: JvmName):
  F[R ** PReference[T]] => F[R] = f => {
    f.visitMethodInsn(Opcodes.INVOKESPECIAL, className.internalName, JvmName.constructorMethod, JvmName.nothingToVoid)
    castF(f)
  }

  // TODO(JLS): make this for a general case
  def mkLazy
  [R <: Stack, T <: PType]
  (lazyType: RType[PReference[PLazy[T]]], fnType: RType[PReference[PFunction[T]]], argIns: F[R] => F[R ** PReference[PFunction[T]]]):
  F[R] => F[R ** PReference[PLazy[T]]] = f => {
    val className = squeezeReference(lazyType).internalName
    // Make a new lazy object and dup it to leave it on the stack.
    f.visitTypeInsn(Opcodes.NEW, className)
    f.visitInsn(Opcodes.DUP)
    argIns(castF(f))
    f.visitMethodInsn(Opcodes.INVOKESPECIAL, className, JvmName.constructorMethod, JvmName.getMethodDescriptor(List(fnType), None))
    castF(f)
  }

  def createSimpleObject
  [R <: Stack, T <: PRefType]
  (className: JvmName, tag: Tag[T] = null):
  F[R] => F[R ** PReference[T]] = f => {
    f.visitTypeInsn(Opcodes.NEW, className.internalName)
    f.visitInsn(Opcodes.DUP)
    f.visitMethodInsn(Opcodes.INVOKESPECIAL, className.internalName, JvmName.constructorMethod, JvmName.nothingToVoid)
    castF(f)
  }

  def FORCE
  [R <: Stack, T <: PType]
  (rType: RType[PReference[PLazy[T]]]):
  F[R ** PReference[PLazy[T]]] => F[R ** T] = f => {
    val rRefLazy = squeezeReference(rType)
    val resultType = squeezeLazy(rRefLazy).tpe
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, rRefLazy.internalName, GenLazyClasses.ForceMethod, resultType.erasedType.nothingToThisDescriptor)
    undoErasure(resultType, f.visitor)
    castF(f)
  }

  // TODO(JLS): delete
  def SCAFFOLD
  [R1 <: Stack, R2 <: Stack]:
  F[R1] => F[R2] =
    null

  def stringConcat
  [R <: Stack]:
  F[R ** PReference[PStr] ** PReference[PStr]] => F[R ** PReference[PStr]] = f => {
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Java.String.internalName, "concat",
      JvmName.getMethodDescriptor(List(RStr), RStr), false)
    castF(f)
  }

  // TODO(JLS): maybe return Nothing (Nothing <: F[_]), or add stop tag to F. atleast something better than StackEnd
  def RETURN[R <: Stack]: F[StackNil] => F[StackEnd] = f => {
    f.visitInsn(Opcodes.RETURN)
    castF(f)
  }

  def ARETURN[R <: Stack, T <: PRefType]: F[StackNil ** PReference[T]] => F[StackEnd] = f => {
    f.visitInsn(Opcodes.ARETURN)
    castF(f)
  }

  def IRETURN
  [R <: Stack, T <: PType]
  (implicit t: T => Int32Usable[T]):
  F[StackNil ** T] => F[StackEnd] = f => {
    f.visitInsn(Opcodes.IRETURN)
    castF(f)
  }

  def CRETURN[R <: Stack]: F[StackNil ** PChar] => F[StackEnd] = f => {
    f.visitInsn(Opcodes.IRETURN)
    castF(f)
  }

  def LRETURN[R <: Stack]: F[StackNil ** PInt64] => F[StackEnd] = f => {
    f.visitInsn(Opcodes.LRETURN)
    castF(f)
  }

  def FRETURN[R <: Stack]: F[StackNil ** PFloat32] => F[StackEnd] = f => {
    f.visitInsn(Opcodes.FRETURN)
    castF(f)
  }

  def DRETURN[R <: Stack]: F[StackNil ** PFloat64] => F[StackEnd] = f => {
    f.visitInsn(Opcodes.DRETURN)
    castF(f)
  }

  def XRETURN
  [R <: Stack, T <: PType]
  (e: RType[T]):
  F[StackNil ** T] => F[StackEnd] = e match {
    case RBool => IRETURN
    case RInt8 => IRETURN
    case RInt16 => IRETURN
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
    f.visitInsn(Opcodes.POP)
    castF(f)
  }

  def POP2_onCat1
  [R <: Stack, T1 <: PType, T2 <: PType]
  (implicit t1: T1 => Cat1[T1], t2: T2 => Cat1[T2]):
  F[R ** T2 ** T1] => F[R] = f => {
    f.visitInsn(Opcodes.POP2)
    castF(f)
  }

  def POP2_onCat2
  [R <: Stack, T <: PType]
  (implicit t: T => Cat2[T]):
  F[R ** T] => F[R] = f => {
    f.visitInsn(Opcodes.POP2)
    castF(f)
  }

  def XPOP
  [R <: Stack, T <: PType]
  (tpe: RType[T]):
  F[R ** T] => F[R] = tpe match {
    case RBool | RInt8 | RInt16 | RInt32 | RChar | RFloat32 | RReference(_) => POP
    case RInt64 | RFloat64 => POP2_onCat2
  }

  def DUP_X1
  [R <: Stack, T1 <: PType, T2 <: PType]
  (implicit t1: T1 => Cat1[T1], t2: T2 => Cat1[T2]):
  F[R ** T2 ** T1] => F[R ** T1 ** T2 ** T1] = f => {
    f.visitInsn(Opcodes.DUP_X1)
    castF(f)
  }

  def DUP2_X1_onCat1
  [R <: Stack, T1 <: PType, T2 <: PType, T3 <: PType]
  (implicit t1: T1 => Cat1[T1], t2: T2 => Cat1[T2], t3: T3 => Cat1[T3]):
  F[R ** T3 ** T2 ** T1] => F[R ** T2 ** T1 ** T3 ** T2 ** T1] = f => {
    f.visitInsn(Opcodes.DUP2_X1)
    castF(f)
  }

  def DUP2_X1_onCat2
  [R <: Stack, T1 <: PType, T2 <: PType]
  (implicit t1: T1 => Cat2[T1], t2: T2 => Cat1[T2]):
  F[R ** T2 ** T1] => F[R ** T1 ** T2 ** T1] = f => {
    f.visitInsn(Opcodes.DUP2_X1)
    castF(f)
  }

  def DUP_X2_onCat1
  [R <: Stack, T1 <: PType, T2 <: PType, T3 <: PType]
  (implicit t1: T1 => Cat1[T1], t2: T2 => Cat1[T2], t3: T2 => Cat1[T2]):
  F[R ** T3 ** T2 ** T1] => F[R ** T1 ** T3 ** T2 ** T1] = f => {
    f.visitInsn(Opcodes.DUP_X2)
    castF(f)
  }

  def DUP_X2_onCat2
  [R <: Stack, T1 <: PType, T2 <: PType]
  (implicit t1: T1 => Cat1[T1], t2: T2 => Cat2[T2]):
  F[R ** T2 ** T1] => F[R ** T1 ** T2 ** T1] = f => {
    f.visitInsn(Opcodes.DUP_X2)
    castF(f)
  }

  def DUP2_X2_cat1_onCat1
  [R <: Stack, T1 <: PType, T2 <: PType, T3 <: PType, T4 <: PType]
  (implicit t1: T1 => Cat1[T1], t2: T2 => Cat1[T2], t3: T2 => Cat1[T2], t4: T2 => Cat1[T2]):
  F[R ** T4 ** T3 ** T2 ** T1] => F[R ** T2 ** T1 ** T4 ** T3 ** T2 ** T1] = f => {
    f.visitInsn(Opcodes.DUP2_X2)
    castF(f)
  }

  def DUP2_X2_cat2_onCat1
  [R <: Stack, T1 <: PType, T2 <: PType, T3 <: PType]
  (implicit t1: T1 => Cat2[T1], t2: T2 => Cat1[T2], t3: T2 => Cat1[T2]):
  F[R ** T3 ** T2 ** T1] => F[R ** T1 ** T3 ** T2 ** T1] = f => {
    f.visitInsn(Opcodes.DUP2_X2)
    castF(f)
  }

  def DUP2_X2_cat1_onCat2
  [R <: Stack, T1 <: PType, T2 <: PType, T3 <: PType]
  (implicit t1: T1 => Cat1[T1], t2: T2 => Cat1[T2], t3: T2 => Cat2[T2]):
  F[R ** T3 ** T2 ** T1] => F[R ** T2 ** T1 ** T3 ** T2 ** T1] = f => {
    f.visitInsn(Opcodes.DUP2_X2)
    castF(f)
  }

  def DUP2_X2_cat2_onCat2
  [R <: Stack, T1 <: PType, T2 <: PType]
  (implicit t1: T1 => Cat2[T1], t2: T2 => Cat2[T2]):
  F[R ** T2 ** T1] => F[R ** T1 ** T2 ** T1] = f => {
    f.visitInsn(Opcodes.DUP2_X2)
    castF(f)
  }

  def DUP
  [R <: Stack, T <: PType]
  (implicit t: T => Cat1[T]):
  F[R ** T] => F[R ** T ** T] = f => {
    f.visitInsn(Opcodes.DUP)
    castF(f)
  }

  def DUP2_onCat1
  [R <: Stack, T1 <: PType, T2 <: PType]
  (implicit t1: T1 => Cat1[T1], t2: T2 => Cat1[T2]):
  F[R ** T2 ** T1] => F[R ** T2 ** T1 ** T2 ** T1] = f => {
    f.visitInsn(Opcodes.DUP2)
    castF(f)
  }

  def DUP2_onCat2
  [R <: Stack, T <: PType]
  (implicit t: T => Cat2[T]):
  F[R ** T] => F[R ** T ** T] = f => {
    f.visitInsn(Opcodes.DUP2)
    castF(f)
  }

  // TODO(JLS): the Int32Usable doesnt fit perfectly here... shouldnt allow B2B or B2S, only downwards
  def I2B
  [R <: Stack, T <: PType]
  (implicit t: T => Int32Usable[T]):
  F[R ** T] => F[R ** PInt8] = f => {
    f.visitInsn(Opcodes.I2B)
    castF(f)
  }

  def I2S
  [R <: Stack, T <: PType]
  (implicit t: T => Int32Usable[T]):
  F[R ** T] => F[R ** PInt16] = f => {
    f.visitInsn(Opcodes.I2S)
    castF(f)
  }

  def I2L
  [R <: Stack, T <: PType]
  (implicit t: T => Int32Usable[T]):
  F[R ** T] => F[R ** PInt64] = f => {
    f.visitInsn(Opcodes.I2L)
    castF(f)
  }

  def I2C
  [R <: Stack, T <: PType]
  (implicit t: T => Int32Usable[T]):
  F[R ** T] => F[R ** PChar] = f => {
    f.visitInsn(Opcodes.I2C)
    castF(f)
  }

  def I2F
  [R <: Stack, T <: PType]
  (implicit t: T => Int32Usable[T]):
  F[R ** T] => F[R ** PFloat32] = f => {
    f.visitInsn(Opcodes.I2F)
    castF(f)
  }

  def I2D
  [R <: Stack, T <: PType]
  (implicit t: T => Int32Usable[T]):
  F[R ** T] => F[R ** PFloat64] = f => {
    f.visitInsn(Opcodes.I2D)
    castF(f)
  }

  def D2F
  [R <: Stack, T <: PType]:
  F[R ** PFloat64] => F[R ** PFloat32] = f => {
    f.visitInsn(Opcodes.D2F)
    castF(f)
  }

  def D2I
  [R <: Stack, T <: PType]:
  F[R ** PFloat64] => F[R ** PInt32] = f => {
    f.visitInsn(Opcodes.D2I)
    castF(f)
  }

  def D2L
  [R <: Stack, T <: PType]:
  F[R ** PFloat64] => F[R ** PInt64] = f => {
    f.visitInsn(Opcodes.D2L)
    castF(f)
  }

  def F2D
  [R <: Stack, T <: PType]:
  F[R ** PFloat32] => F[R ** PFloat64] = f => {
    f.visitInsn(Opcodes.F2D)
    castF(f)
  }

  def F2I
  [R <: Stack, T <: PType]:
  F[R ** PFloat32] => F[R ** PInt32] = f => {
    f.visitInsn(Opcodes.F2I)
    castF(f)
  }

  def F2L
  [R <: Stack, T <: PType]:
  F[R ** PFloat32] => F[R ** PInt64] = f => {
    f.visitInsn(Opcodes.F2L)
    castF(f)
  }

  def L2D
  [R <: Stack, T <: PType]:
  F[R ** PInt64] => F[R ** PFloat64] = f => {
    f.visitInsn(Opcodes.L2D)
    castF(f)
  }

  def L2F
  [R <: Stack, T <: PType]:
  F[R ** PInt64] => F[R ** PFloat32] = f => {
    f.visitInsn(Opcodes.L2F)
    castF(f)
  }

  def L2I
  [R <: Stack, T <: PType]:
  F[R ** PInt64] => F[R ** PInt32] = f => {
    f.visitInsn(Opcodes.L2I)
    castF(f)
  }

  def INEG
  [R <: Stack, T <: PType]
  (implicit t: T => Int32Usable[T]):
  F[R ** T] => F[R ** PInt32] = f => {
    f.visitInsn(Opcodes.INEG)
    castF(f)
  }

  def LNEG
  [R <: Stack]:
  F[R ** PInt64] => F[R ** PInt64] = f => {
    f.visitInsn(Opcodes.LNEG)
    castF(f)
  }

  def FNEG
  [R <: Stack]:
  F[R ** PFloat32] => F[R ** PFloat32] = f => {
    f.visitInsn(Opcodes.FNEG)
    castF(f)
  }

  def DNEG
  [R <: Stack]:
  F[R ** PFloat64] => F[R ** PFloat64] = f => {
    f.visitInsn(Opcodes.DNEG)
    castF(f)
  }

  def BigIntNeg
  [R <: Stack]:
  F[R ** PReference[PBigInt]] => F[R ** PReference[PBigInt]] = f => {
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Java.BigInteger.internalName, "negate",
      JvmName.Java.BigInteger.nothingToThisDescriptor)
    castF(f)
  }

  def BigIntNot
  [R <: Stack]:
  F[R ** PReference[PBigInt]] => F[R ** PReference[PBigInt]] = f => {
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Java.BigInteger.internalName, "not",
      JvmName.Java.BigInteger.nothingToThisDescriptor)
    castF(f)
  }

  def BSHR
  [R <: Stack]:
  F[R ** PInt8 ** PInt8] => F[R ** PInt8] = f => {
    ISHR(f.asInstanceOf[F[R ** PInt32 ** PInt32]])
    castF(f)
  }

  def SSHR
  [R <: Stack]:
  F[R ** PInt16 ** PInt16] => F[R ** PInt16] = f => {
    ISHR(f.asInstanceOf[F[R ** PInt32 ** PInt32]])
    castF(f)
  }

  def ISHR
  [R <: Stack]:
  F[R ** PInt32 ** PInt32] => F[R ** PInt32] = f => {
    f.visitInsn(Opcodes.ISHR)
    castF(f)
  }

  def LSHR
  [R <: Stack]:
  F[R ** PInt64 ** PInt64] => F[R ** PInt64] = f => {
    f.visitInsn(Opcodes.LSHR)
    castF(f)
  }

  def BigIntSHR
  [R <: Stack]:
  F[R ** PReference[PBigInt] ** PReference[PBigInt]] => F[R ** PReference[PBigInt]] = f => {
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Java.BigInteger.internalName, "shr",
      JvmName.getMethodDescriptor(JvmName.Java.BigInteger, JvmName.Java.BigInteger))
    castF(f)
  }

  def ISHL
  [R <: Stack, T1 <: PType, T2 <: PType]
  (implicit t1: T1 => Int32Usable[T1], t2: T2 => Int32Usable[T2]):
  F[R ** T2 ** T1] => F[R ** PInt32] = f => {
    f.visitInsn(Opcodes.ISHL)
    castF(f)
  }

  def LSHL
  [R <: Stack]:
  F[R ** PInt64 ** PInt64] => F[R ** PInt64] = f => {
    f.visitInsn(Opcodes.LSHL)
    castF(f)
  }

  def BigIntSHL
  [R <: Stack]:
  F[R ** PReference[PBigInt] ** PReference[PBigInt]] => F[R ** PReference[PBigInt]] = f => {
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Java.BigInteger.internalName, "shiftLeft",
      JvmName.getMethodDescriptor(JvmName.Java.BigInteger, JvmName.Java.BigInteger))
    castF(f)
  }

  def BXOR
  [R <: Stack]:
  F[R ** PInt8 ** PInt8] => F[R ** PInt8] = f => {
    IXOR(f.asInstanceOf[F[R ** PInt32 ** PInt32]])
    castF(f)
  }

  def SXOR
  [R <: Stack]:
  F[R ** PInt16 ** PInt16] => F[R ** PInt16] = f => {
    IXOR(f.asInstanceOf[F[R ** PInt32 ** PInt32]])
    castF(f)
  }

  def IXOR
  [R <: Stack]:
  F[R ** PInt32 ** PInt32] => F[R ** PInt32] = f => {
    f.visitInsn(Opcodes.IXOR)
    castF(f)
  }

  def LXOR
  [R <: Stack]:
  F[R ** PInt64 ** PInt64] => F[R ** PInt64] = f => {
    f.visitInsn(Opcodes.LXOR)
    castF(f)
  }

  def BigIntXOR
  [R <: Stack]:
  F[R ** PReference[PBigInt] ** PReference[PBigInt]] => F[R ** PReference[PBigInt]] = f => {
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Java.BigInteger.internalName, "xor",
      JvmName.getMethodDescriptor(JvmName.Java.BigInteger, JvmName.Java.BigInteger))
    castF(f)
  }

  def BOR
  [R <: Stack]:
  F[R ** PInt8 ** PInt8] => F[R ** PInt8] = f => {
    IOR(f.asInstanceOf[F[R ** PInt32 ** PInt32]])
    castF(f)
  }

  def SOR
  [R <: Stack]:
  F[R ** PInt16 ** PInt16] => F[R ** PInt16] = f => {
    IOR(f.asInstanceOf[F[R ** PInt32 ** PInt32]])
    castF(f)
  }

  def IOR
  [R <: Stack]:
  F[R ** PInt32 ** PInt32] => F[R ** PInt32] = f => {
    f.visitInsn(Opcodes.IOR)
    castF(f)
  }

  def LOR
  [R <: Stack]:
  F[R ** PInt64 ** PInt64] => F[R ** PInt64] = f => {
    f.visitInsn(Opcodes.LOR)
    castF(f)
  }

  def BigIntOR
  [R <: Stack]:
  F[R ** PReference[PBigInt] ** PReference[PBigInt]] => F[R ** PReference[PBigInt]] = f => {
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Java.BigInteger.internalName, "or",
      JvmName.getMethodDescriptor(JvmName.Java.BigInteger, JvmName.Java.BigInteger))
    castF(f)
  }

  def BAND
  [R <: Stack]:
  F[R ** PInt8 ** PInt8] => F[R ** PInt8] = f => {
    IAND(f.asInstanceOf[F[R ** PInt32 ** PInt32]])
    castF(f)
  }

  def SAND
  [R <: Stack]:
  F[R ** PInt16 ** PInt16] => F[R ** PInt16] = f => {
    IAND(f.asInstanceOf[F[R ** PInt32 ** PInt32]])
    castF(f)
  }

  def IAND
  [R <: Stack]:
  F[R ** PInt32 ** PInt32] => F[R ** PInt32] = f => {
    f.visitInsn(Opcodes.IAND)
    castF(f)
  }

  def LAND
  [R <: Stack]:
  F[R ** PInt64 ** PInt64] => F[R ** PInt64] = f => {
    f.visitInsn(Opcodes.LAND)
    castF(f)
  }

  def BigIntAND
  [R <: Stack]:
  F[R ** PReference[PBigInt] ** PReference[PBigInt]] => F[R ** PReference[PBigInt]] = f => {
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Java.BigInteger.internalName, "and",
      JvmName.getMethodDescriptor(JvmName.Java.BigInteger, JvmName.Java.BigInteger), false)
    castF(f)
  }

  def IREM
  [R <: Stack, T1 <: PType, T2 <: PType]
  (implicit t1: T1 => Int32Usable[T1], t2: T2 => Int32Usable[T2]):
  F[R ** T2 ** T1] => F[R ** PInt32] = f => {
    f.visitInsn(Opcodes.IREM)
    castF(f)
  }

  def LREM
  [R <: Stack]:
  F[R ** PInt64 ** PInt64] => F[R ** PInt64] = f => {
    f.visitInsn(Opcodes.LREM)
    castF(f)
  }

  def FREM
  [R <: Stack]:
  F[R ** PFloat32 ** PFloat32] => F[R ** PFloat32] = f => {
    f.visitInsn(Opcodes.FREM)
    castF(f)
  }

  def DREM
  [R <: Stack]:
  F[R ** PFloat64 ** PFloat64] => F[R ** PFloat64] = f => {
    f.visitInsn(Opcodes.DREM)
    castF(f)
  }

  def BigIntREM
  [R <: Stack]:
  F[R ** PReference[PBigInt] ** PReference[PBigInt]] => F[R ** PReference[PBigInt]] = f => {
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Java.BigInteger.internalName, "remainder",
      JvmName.getMethodDescriptor(JvmName.Java.BigInteger, JvmName.Java.BigInteger), false)
    castF(f)
  }

  // TODO(JLS): The starting stack of the given ins could be many things (stacknil vs actual stack vs
  //  new stack variable), what is the best option?
  def DoublePow
  [R <: Stack]
  (argIns: F[StackNil] => F[StackNil ** PFloat64 ** PFloat64]):
  F[R] => F[R ** PFloat64] = f => {
    val className = JvmName.Scala.Package
    f.visitFieldInsn(Opcodes.GETSTATIC, className.internalName, "MODULE$", className.descriptor)
    argIns(castF(f))
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, className.internalName, "pow",
      JvmName.getMethodDescriptor(List(RFloat64, RFloat64), RFloat64), false)
    castF(f)
  }

  def IDIV
  [R <: Stack, T1 <: PType, T2 <: PType]
  (implicit t1: T1 => Int32Usable[T1], t2: T2 => Int32Usable[T2]):
  F[R ** T2 ** T1] => F[R ** PInt32] = f => {
    f.visitInsn(Opcodes.IDIV)
    castF(f)
  }

  def LDIV
  [R <: Stack]:
  F[R ** PInt64 ** PInt64] => F[R ** PInt64] = f => {
    f.visitInsn(Opcodes.LDIV)
    castF(f)
  }

  def FDIV
  [R <: Stack]:
  F[R ** PFloat32 ** PFloat32] => F[R ** PFloat32] = f => {
    f.visitInsn(Opcodes.FDIV)
    castF(f)
  }

  def DDIV
  [R <: Stack]:
  F[R ** PFloat64 ** PFloat64] => F[R ** PFloat64] = f => {
    f.visitInsn(Opcodes.DDIV)
    castF(f)
  }

  def BigIntDIV
  [R <: Stack]:
  F[R ** PReference[PBigInt] ** PReference[PBigInt]] => F[R ** PReference[PBigInt]] = f => {
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Java.BigInteger.internalName, "divide",
      JvmName.getMethodDescriptor(JvmName.Java.BigInteger, JvmName.Java.BigInteger), false)
    castF(f)
  }

  def IMUL
  [R <: Stack, T1 <: PType, T2 <: PType]
  (implicit t1: T1 => Int32Usable[T1], t2: T2 => Int32Usable[T2]):
  F[R ** T2 ** T1] => F[R ** PInt32] = f => {
    f.visitInsn(Opcodes.IMUL)
    castF(f)
  }

  def LMUL
  [R <: Stack]:
  F[R ** PInt64 ** PInt64] => F[R ** PInt64] = f => {
    f.visitInsn(Opcodes.LMUL)
    castF(f)
  }

  def FMUL
  [R <: Stack]:
  F[R ** PFloat32 ** PFloat32] => F[R ** PFloat32] = f => {
    f.visitInsn(Opcodes.FMUL)
    castF(f)
  }

  def DMUL
  [R <: Stack]:
  F[R ** PFloat64 ** PFloat64] => F[R ** PFloat64] = f => {
    f.visitInsn(Opcodes.DMUL)
    castF(f)
  }

  def BigIntMUL
  [R <: Stack]:
  F[R ** PReference[PBigInt] ** PReference[PBigInt]] => F[R ** PReference[PBigInt]] = f => {
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Java.BigInteger.internalName, "multiply",
      JvmName.getMethodDescriptor(JvmName.Java.BigInteger, JvmName.Java.BigInteger), false)
    castF(f)
  }

  def ISUB
  [R <: Stack, T1 <: PType, T2 <: PType]
  (implicit t1: T1 => Int32Usable[T1], t2: T2 => Int32Usable[T2]):
  F[R ** T2 ** T1] => F[R ** PInt32] = f => {
    f.visitInsn(Opcodes.ISUB)
    castF(f)
  }

  def LSUB
  [R <: Stack]:
  F[R ** PInt64 ** PInt64] => F[R ** PInt64] = f => {
    f.visitInsn(Opcodes.LSUB)
    castF(f)
  }

  def FSUB
  [R <: Stack]:
  F[R ** PFloat32 ** PFloat32] => F[R ** PFloat32] = f => {
    f.visitInsn(Opcodes.FSUB)
    castF(f)
  }

  def DSUB
  [R <: Stack]:
  F[R ** PFloat64 ** PFloat64] => F[R ** PFloat64] = f => {
    f.visitInsn(Opcodes.DSUB)
    castF(f)
  }

  def BigIntSUB
  [R <: Stack]:
  F[R ** PReference[PBigInt] ** PReference[PBigInt]] => F[R ** PReference[PBigInt]] = f => {
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Java.BigInteger.internalName, "subtract",
      JvmName.getMethodDescriptor(List(JvmName.Java.BigInteger), JvmName.Java.BigInteger), false)
    castF(f)
  }

  def IADD
  [R <: Stack, T1 <: PType, T2 <: PType]
  (implicit t1: T1 => Int32Usable[T1], t2: T2 => Int32Usable[T2]):
  F[R ** T2 ** T1] => F[R ** PInt32] = f => {
    f.visitInsn(Opcodes.IADD)
    castF(f)
  }

  def LADD
  [R <: Stack]:
  F[R ** PInt64 ** PInt64] => F[R ** PInt64] = f => {
    f.visitInsn(Opcodes.LADD)
    castF(f)
  }

  def FADD
  [R <: Stack]:
  F[R ** PFloat32 ** PFloat32] => F[R ** PFloat32] = f => {
    f.visitInsn(Opcodes.FADD)
    castF(f)
  }

  def DADD
  [R <: Stack]:
  F[R ** PFloat64 ** PFloat64] => F[R ** PFloat64] = f => {
    f.visitInsn(Opcodes.DADD)
    castF(f)
  }

  def BigIntADD
  [R <: Stack]:
  F[R ** PReference[PBigInt] ** PReference[PBigInt]] => F[R ** PReference[PBigInt]] = f => {
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Java.BigInteger.internalName, "add",
      JvmName.getMethodDescriptor(List(JvmName.Java.BigInteger), JvmName.Java.BigInteger), false)
    castF(f)
  }

  def BoxInt8
  [R <: Stack]:
  F[R ** PInt8] => F[R ** PReference[PBoxedInt8]] = f => {
    val className = JvmName.Java.Byte
    f.visitMethodInsn(Opcodes.INVOKESTATIC, className.internalName, "valueOf", JvmName.getMethodDescriptor(RInt8, className))
    castF[R ** PReference[PBoxedInt8]](f)
  }

  def BoxInt16
  [R <: Stack]:
  F[R ** PInt16] => F[R ** PReference[PBoxedInt16]] = f => {
    val className = JvmName.Java.Short
    f.visitMethodInsn(Opcodes.INVOKESTATIC, className.internalName, "valueOf", JvmName.getMethodDescriptor(RInt16, className))
    castF[R ** PReference[PBoxedInt16]](f)
  }

  def BoxInt32
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PBoxedInt32]] = f => {
    val className = JvmName.Java.Integer
    f.visitMethodInsn(Opcodes.INVOKESTATIC, className.internalName, "valueOf", JvmName.getMethodDescriptor(RInt32, className))
    castF[R ** PReference[PBoxedInt32]](f)
  }

  def BoxBool
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PBoxedBool]] = f => {
    val className = JvmName.Java.Boolean
    f.visitMethodInsn(Opcodes.INVOKESTATIC, className.internalName, "valueOf", JvmName.getMethodDescriptor(RBool, className))
    castF[R ** PReference[PBoxedBool]](f)
  }

  def BoxInt64
  [R <: Stack]:
  F[R ** PInt64] => F[R ** PReference[PBoxedInt64]] = f => {
    val className = JvmName.Java.Long
    f.visitMethodInsn(Opcodes.INVOKESTATIC, className.internalName, "valueOf", JvmName.getMethodDescriptor(RInt64, className))
    castF[R ** PReference[PBoxedInt64]](f)
  }

  def BoxChar
  [R <: Stack]:
  F[R ** PChar] => F[R ** PReference[PBoxedChar]] = f => {
    val className = JvmName.Java.Character
    f.visitMethodInsn(Opcodes.INVOKESTATIC, className.internalName, "valueOf", JvmName.getMethodDescriptor(RChar, className))
    castF[R ** PReference[PBoxedChar]](f)
  }

  def BoxFloat32
  [R <: Stack]:
  F[R ** PFloat32] => F[R ** PReference[PBoxedFloat32]] = f => {
    val className = JvmName.Java.Float
    f.visitMethodInsn(Opcodes.INVOKESTATIC, className.internalName, "valueOf", JvmName.getMethodDescriptor(RFloat32, className))
    castF[R ** PReference[PBoxedFloat32]](f)
  }

  def BoxFloat64
  [R <: Stack]:
  F[R ** PFloat64] => F[R ** PReference[PBoxedFloat64]] = f => {
    val className = JvmName.Java.Double
    f.visitMethodInsn(Opcodes.INVOKESTATIC, className.internalName, "valueOf", JvmName.getMethodDescriptor(RFloat64, className))
    castF[R ** PReference[PBoxedFloat64]](f)
  }

  def UnboxInt8
  [R <: Stack]:
  F[R ** PReference[PBoxedInt8]] => F[R ** PInt8] = f => {
    val className = JvmName.Java.Byte
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, className.internalName, "byteValue", RInt8.nothingToThisDescriptor)
    castF[R ** PInt8](f)
  }

  def UnboxInt16
  [R <: Stack]:
  F[R ** PReference[PBoxedInt16]] => F[R ** PInt16] = f => {
    val className = JvmName.Java.Short
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, className.internalName, "shortValue", RInt16.nothingToThisDescriptor)
    castF[R ** PInt16](f)
  }

  def UnboxInt32
  [R <: Stack]:
  F[R ** PReference[PBoxedInt32]] => F[R ** PInt32] = f => {
    val className = JvmName.Java.Integer
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, className.internalName, "intValue", RInt32.nothingToThisDescriptor)
    castF[R ** PInt32](f)
  }

  def UnboxBool
  [R <: Stack]:
  F[R ** PReference[PBoxedBool]] => F[R ** PInt32] = f => {
    val className = JvmName.Java.Boolean
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, className.internalName, "booleanValue", RBool.nothingToThisDescriptor)
    castF[R ** PInt32](f)
  }

  def UnboxInt64
  [R <: Stack]:
  F[R ** PReference[PBoxedInt64]] => F[R ** PInt64] = f => {
    val className = JvmName.Java.Long
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, className.internalName, "longValue", RInt64.nothingToThisDescriptor)
    castF[R ** PInt64](f)
  }

  def UnboxChar
  [R <: Stack]:
  F[R ** PReference[PBoxedChar]] => F[R ** PChar] = f => {
    val className = JvmName.Java.Character
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, className.internalName, "charValue", RChar.nothingToThisDescriptor)
    castF[R ** PChar](f)
  }

  def UnboxFloat32
  [R <: Stack]:
  F[R ** PReference[PBoxedFloat32]] => F[R ** PFloat32] = f => {
    val className = JvmName.Java.Float
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, className.internalName, "floatValue", RFloat32.nothingToThisDescriptor)
    castF[R ** PFloat32](f)
  }

  def UnboxFloat64
  [R <: Stack]:
  F[R ** PReference[PBoxedFloat64]] => F[R ** PFloat64] = f => {
    val className = JvmName.Java.Double
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, className.internalName, "doubleValue", RFloat64.nothingToThisDescriptor)
    castF[R ** PFloat64](f)
  }

  def pushUnit
  [R <: Stack]:
  F[R] => F[R ** PReference[PUnit]] = {
    val unitType = RUnit.rType
    START[R] ~ GETSTATIC(unitType, GenUnitClass.InstanceFieldName, unitType, undoErasure = false)
  }

  def pushNull
  [R <: Stack, T <: PRefType]
  (tpe: RType[PReference[T]]):
  F[R] => F[R ** PReference[T]] = f => {
    f.visitInsn(Opcodes.ACONST_NULL)
    undoErasure(tpe, f.visitor)
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
    // TODO(JLS): it should be checked that i.e. pushInt8 does not hit the large cases
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
      case 0f => f.visitInsn(Opcodes.FCONST_0)
      case 1f => f.visitInsn(Opcodes.FCONST_1)
      case 2f => f.visitInsn(Opcodes.FCONST_2)
      case _ => f.visitLdcInsn(n)
    }
    castF(f)
  }

  def pushFloat64
  [R <: Stack]
  (n: Double):
  F[R] => F[R ** PFloat64] = f => {
    n match {
      case 0d => f.visitInsn(Opcodes.DCONST_0)
      case 1d => f.visitInsn(Opcodes.DCONST_1)
      case _ => f.visitLdcInsn(n)
    }
    castF(f)
  }

  def pushChar
  [R <: Stack]
  (c: scala.Char):
  F[R] => F[R ** PChar] = f => {
    compileInt(f.visitor, c)
    castF(f)
  }

  def pushString
  [R <: Stack]
  (s: String):
  F[R] => F[R ** PReference[PStr]] = f => {
    f.visitLdcInsn(s)
    castF(f)
  }

  // TODO(JLS): could be done with general object init method
  def pushBigInt
  [R <: Stack]
  (bi: java.math.BigInteger):
  F[R] => F[R ** PReference[PBigInt]] = f => {
    val className = JvmName.Java.BigInteger.internalName
    f.visitTypeInsn(Opcodes.NEW, className)
    f.visitInsn(Opcodes.DUP)
    f.visitLdcInsn(bi.toString)
    f.visitMethodInsn(Opcodes.INVOKESPECIAL, className, JvmName.constructorMethod,
      JvmName.getMethodDescriptor(List(JvmName.Java.String), None), false)
    castF(f)
  }

  def ALOAD
  [R <: Stack, T <: PRefType]
  (index: Int, tpe: RType[PReference[T]]):
  F[R] => F[R ** PReference[T]] = f => {
    f.visitVarInsn(Opcodes.ALOAD, index)
    undoErasure(tpe, f.visitor)
    castF(f)
  }

  def ALOAD
  [R <: Stack, T <: PRefType]
  (index: Int, name: JvmName, tag: Tag[T] = null):
  F[R] => F[R ** PReference[T]] = f => {
    f.visitVarInsn(Opcodes.ALOAD, index)
    undoErasure(name, f.visitor)
    castF(f)
  }

  def preInitALOAD
  [R <: Stack, T <: PRefType]
  (index: Int, tpe: RType[PReference[T]]):
  F[R] => F[R ** PReference[T]] = f => {
    f.visitVarInsn(Opcodes.ALOAD, index)
    castF(f)
  }

  def preInitALOAD
  [R <: Stack, T <: PRefType]
  (index: Int, tag: Tag[T] = null):
  F[R] => F[R ** PReference[T]] = f => {
    f.visitVarInsn(Opcodes.ALOAD, index)
    castF(f)
  }

  /**
    * Cannot be used for object initialization since the type is casted
    */
  def THISLOAD
  [R <: Stack, T <: PRefType]
  (tpe: RType[PReference[T]]):
  F[R] => F[R ** PReference[T]] =
    ALOAD(0, tpe)

  /**
    * Cannot be used for object initialization since the type is casted
    */
  def THISLOAD
  [R <: Stack, T <: PRefType]
  (name: JvmName, tag: Tag[T] = null):
  F[R] => F[R ** PReference[T]] =
    ALOAD(0, name)

  def THISINIT
  [R <: Stack]
  (superClass: JvmName):
  F[R] => F[R] = f => {
    f.visitVarInsn(Opcodes.ALOAD, 0)
    f.visitMethodInsn(Opcodes.INVOKESPECIAL, superClass.internalName, JvmName.constructorMethod, JvmName.nothingToVoid)
    castF(f)
  }

  def SUBTYPE
  [R <: Stack, T <: PRefType]:
  F[R ** PReference[T]] => F[R ** PReference[PAnyObject]] = f => {
    f.visitTypeInsn(Opcodes.CHECKCAST, RObject.internalName)
    f.asInstanceOf[F[R ** PReference[PAnyObject]]]
  }

  def ChannelSUBTYPE
  [R <: Stack, T <: PRefType]:
  F[R ** PReference[PChan[T]]] => F[R ** PReference[PChan[PAnyObject]]] =
    f => f.asInstanceOf[F[R ** PReference[PChan[PAnyObject]]]]

  // TODO(JLS): This needs to return both StackEnd (no code should follow) and R ** T (compileExp should push T on stack) (maybe stop flag type on F)
  def TAILCALL
  [R <: Stack, T <: PType]
  (arguments: List[ErasedAst.Expression[_ <: PType]], fnType: RArrow[T], lenv0: Map[Symbol.LabelSym, Label]):
  F[R ** PReference[PFunction[T]]] => F[R ** T] = {
    START[R ** PReference[PFunction[T]]] ~
      setArgs(fnType.jvmName, arguments, GenFunctionInterfaces.argFieldName, lenv0) ~
      AReturnNoEnd(tagOf[T])
  }

  private def AReturnNoEnd
  [R <: Stack, T <: PType]
  (t: Tag[T] = null):
  F[R ** PReference[PFunction[T]]] => F[R ** T] = f => {
    f.visitInsn(Opcodes.ARETURN)
    castF(f)
  }

  def unwind
  [R <: Stack, T <: PType]
  (fnType: RArrow[T]):
  F[R ** PReference[PFunction[T]]] => F[R ** T] =
    unwindCont(fnType.result)

  def unwindCont
  [R <: Stack, T <: PType]
  (resultType: RType[T]):
  F[R ** PReference[PFunction[T]]] => F[R ** T] = f => {
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, resultType.contName.internalName, GenContinuationInterfaces.UnwindMethodName,
      resultType.erasedType.nothingToThisDescriptor)
    undoErasure(resultType, f.visitor)
    castF(f)
  }

  def setArgs[R <: Stack, T <: PType]
  (className: JvmName, args: List[ErasedAst.Expression[_ <: PType]], fieldName: Int => String, lenv0: Map[Symbol.LabelSym, Label], t: Tag[T] = null):
  F[R ** PReference[PFunction[T]]] => F[R ** PReference[PFunction[T]]] =
    START[R ** PReference[PFunction[T]]] ~
      multiComposition(args.zipWithIndex) {
        case (exp, index) =>
          START[R ** PReference[PFunction[T]]] ~ DUP ~ compileExp(exp, lenv0) ~ setArg(className, fieldName(index), exp.tpe.erasedDescriptor)
      }

  private def setArg[R <: Stack, T1 <: PType, T2 <: PType]
  (className: JvmName, fieldName: String, erasedDescriptor: Descriptor, t1: Tag[T1] = null):
  F[R ** PReference[PFunction[T1]] ** T2] => F[R] = f => {
    f.visitFieldInsn(Opcodes.PUTFIELD, className.internalName, fieldName, erasedDescriptor)
    castF(f)
  }

  def CALL
  [R <: Stack, T <: PType]
  (arguments: List[ErasedAst.Expression[_ <: PType]], fnType: RArrow[T], lenv0: Map[Symbol.LabelSym, Label]):
  F[R ** PReference[PFunction[T]]] => F[R ** T] = {
    START[R ** PReference[PFunction[T]]] ~
      setArgs(fnType.jvmName, arguments, GenFunctionInterfaces.argFieldName, lenv0) ~
      unwind(fnType)
  }

  def CREATEDEF
  [R <: Stack, T <: PType]
  (defName: JvmName, fnName: JvmName, t: Tag[T] = null):
  F[R] => F[R ** PReference[PFunction[T]]] = {
    START[R] ~
      NEW(defName, tagOf[PFunction[T]]) ~
      DUP ~
      invokeSimpleConstructor(defName) ~
      (f => {
        undoErasure(fnName, f.visitor)
        f
      })
  }

  def CREATECLOSURE
  [R <: Stack, T <: PType]
  (freeVars: List[ErasedAst.FreeVar[_ <: PType]], cloName: JvmName, fnName: JvmName, lenv0: Map[Symbol.LabelSym, Label], t: Tag[T] = null):
  F[R] => F[R ** PReference[PFunction[T]]] =
    START[R] ~
      NEW(cloName, tagOf[PFunction[T]]) ~
      DUP ~
      invokeSimpleConstructor(cloName) ~
      setArgs(cloName, freeVars.map(f => ErasedAst.Expression.Var(f.sym, f.tpe, SourceLocation.Unknown)), GenClosureClasses.cloArgFieldName, lenv0, tagOf[T]) ~
      ((f: F[R ** PReference[PFunction[T]]]) => {
        undoErasure(fnName, f.visitor)
        f
      })
  // TODO(JLS): This added exp could maybe cause trouble

  def ILOAD
  [R <: Stack]
  (index: Int):
  F[R] => F[R ** PInt32] = f => {
    f.visitVarInsn(Opcodes.ILOAD, index)
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
    f.visitVarInsn(Opcodes.LLOAD, index)
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
    f.visitVarInsn(Opcodes.FLOAD, index)
    castF(f)
  }

  def DLOAD
  [R <: Stack]
  (index: Int):
  F[R] => F[R ** PFloat64] = f => {
    f.visitVarInsn(Opcodes.DLOAD, index)
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
    case RReference(_) => ALOAD(index, tpe)
  }

  def BALOAD
  [R <: Stack]:
  F[R ** PReference[PArray[PInt8]] ** PInt32] => F[R ** PInt8] = f => {
    f.visitInsn(Opcodes.BALOAD)
    castF(f)
  }

  def SALOAD
  [R <: Stack]:
  F[R ** PReference[PArray[PInt16]] ** PInt32] => F[R ** PInt16] = f => {
    f.visitInsn(Opcodes.SALOAD)
    castF(f)
  }

  def IALOAD
  [R <: Stack]:
  F[R ** PReference[PArray[PInt32]] ** PInt32] => F[R ** PInt32] = f => {
    f.visitInsn(Opcodes.IALOAD)
    castF(f)
  }

  def LALOAD
  [R <: Stack]:
  F[R ** PReference[PArray[PInt64]] ** PInt32] => F[R ** PInt64] = f => {
    f.visitInsn(Opcodes.LALOAD)
    castF(f)
  }

  def CALOAD
  [R <: Stack]:
  F[R ** PReference[PArray[PChar]] ** PInt32] => F[R ** PChar] = f => {
    f.visitInsn(Opcodes.CALOAD)
    castF(f)
  }

  def FALOAD
  [R <: Stack]:
  F[R ** PReference[PArray[PFloat32]] ** PInt32] => F[R ** PFloat32] = f => {
    f.visitInsn(Opcodes.FALOAD)
    castF(f)
  }

  def DALOAD
  [R <: Stack]:
  F[R ** PReference[PArray[PFloat64]] ** PInt32] => F[R ** PFloat64] = f => {
    f.visitInsn(Opcodes.DALOAD)
    castF(f)
  }

  def AALOAD
  [R <: Stack, T <: PRefType]
  (tpe: RType[PReference[T]]):
  F[R ** PReference[PArray[PReference[T]]] ** PInt32] => F[R ** PReference[T]] = f => {
    f.visitInsn(Opcodes.AALOAD)
    undoErasure(tpe, f.visitor)
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
    case RReference(_) => AALOAD(tpe)
  }

  def BASTORE
  [R <: Stack]:
  F[R ** PReference[PArray[PInt8]] ** PInt32 ** PInt8] => F[R] = f => {
    f.visitInsn(Opcodes.BASTORE)
    castF(f)
  }

  def SASTORE
  [R <: Stack]:
  F[R ** PReference[PArray[PInt16]] ** PInt32 ** PInt16] => F[R] = f => {
    f.visitInsn(Opcodes.SASTORE)
    castF(f)
  }

  def IASTORE
  [R <: Stack]:
  F[R ** PReference[PArray[PInt32]] ** PInt32 ** PInt32] => F[R] = f => {
    f.visitInsn(Opcodes.IASTORE)
    castF(f)
  }

  def LASTORE
  [R <: Stack]:
  F[R ** PReference[PArray[PInt64]] ** PInt32 ** PInt64] => F[R] = f => {
    f.visitInsn(Opcodes.LASTORE)
    castF(f)
  }

  def CASTORE
  [R <: Stack]:
  F[R ** PReference[PArray[PChar]] ** PInt32 ** PChar] => F[R] = f => {
    f.visitInsn(Opcodes.CASTORE)
    castF(f)
  }

  def FASTORE
  [R <: Stack]:
  F[R ** PReference[PArray[PFloat32]] ** PInt32 ** PFloat32] => F[R] = f => {
    f.visitInsn(Opcodes.FASTORE)
    castF(f)
  }

  def DASTORE
  [R <: Stack]:
  F[R ** PReference[PArray[PFloat64]] ** PInt32 ** PFloat64] => F[R] = f => {
    f.visitInsn(Opcodes.DASTORE)
    castF(f)
  }

  def AASTORE
  [R <: Stack, T <: PRefType]:
  F[R ** PReference[PArray[PReference[T]]] ** PInt32 ** PReference[T]] => F[R] = f => {
    f.visitInsn(Opcodes.AASTORE)
    castF(f)
  }

  def XASTORE
  [R <: Stack, T <: PType]
  (tpe: RType[T]):
  F[R ** PReference[PArray[T]] ** PInt32 ** T] => F[R] =
    tpe match {
      case RChar => CASTORE
      case RFloat32 => FASTORE
      case RFloat64 => DASTORE
      case RInt8 => BASTORE
      case RInt16 => SASTORE
      case RBool | RInt32 => IASTORE
      case RInt64 => LASTORE
      case RReference(_) => AASTORE
    }

  val symOffsetOffset = 1

  def ISTORE
  [R <: Stack, T <: PType]
  (sym: Symbol.VarSym)
  (implicit t: T => Int32Usable[T]):
  F[R ** T] => F[R] =
    ISTORE(sym.getStackOffset + symOffsetOffset)

  def ISTORE
  [R <: Stack, T <: PType]
  (index: Int)
  (implicit t: T => Int32Usable[T]):
  F[R ** T] => F[R] = f => {
    f.visitVarInsn(Opcodes.ISTORE, index)
    castF(f)
  }

  def LSTORE
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PInt64] => F[R] =
    LSTORE(sym.getStackOffset + symOffsetOffset)

  def LSTORE
  [R <: Stack]
  (index: Int):
  F[R ** PInt64] => F[R] = f => {
    f.visitVarInsn(Opcodes.LSTORE, index)
    castF(f)
  }

  def FSTORE
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PFloat32] => F[R] =
    FSTORE(sym.getStackOffset + symOffsetOffset)

  def FSTORE
  [R <: Stack]
  (index: Int):
  F[R ** PFloat32] => F[R] = f => {
    f.visitVarInsn(Opcodes.FSTORE, index)
    castF(f)
  }

  def DSTORE
  [R <: Stack]
  (sym: Symbol.VarSym):
  F[R ** PFloat64] => F[R] =
    DSTORE(sym.getStackOffset + symOffsetOffset)

  def DSTORE
  [R <: Stack]
  (index: Int):
  F[R ** PFloat64] => F[R] = f => {
    f.visitVarInsn(Opcodes.DSTORE, index)
    castF(f)
  }

  def ASTORE
  [R <: Stack, T <: PRefType]
  (sym: Symbol.VarSym):
  F[R ** PReference[T]] => F[R] =
    ASTORE(sym.getStackOffset + symOffsetOffset)

  def ASTORE
  [R <: Stack, T <: PRefType]
  (index: Int):
  F[R ** PReference[T]] => F[R] = f => {
    f.visitVarInsn(Opcodes.ASTORE, index)
    castF(f)
  }

  def XSTORE
  [R <: Stack, T <: PType]
  (sym: Symbol.VarSym, tpe: RType[T]):
  F[R ** T] => F[R] =
    tpe match {
      case RChar => ISTORE(sym)
      case RFloat32 => FSTORE(sym)
      case RFloat64 => DSTORE(sym)
      case RInt8 => ISTORE(sym)
      case RInt16 => ISTORE(sym)
      case RBool | RInt32 => ISTORE(sym)
      case RInt64 => LSTORE(sym)
      case RReference(_) => ASTORE(sym)
    }

  def XSTORE
  [R <: Stack, T <: PType]
  (index: Int, tpe: RType[T]):
  F[R ** T] => F[R] =
    tpe match {
      case RChar => ISTORE(index)
      case RFloat32 => FSTORE(index)
      case RFloat64 => DSTORE(index)
      case RInt8 => ISTORE(index)
      case RInt16 => ISTORE(index)
      case RBool | RInt32 => ISTORE(index)
      case RInt64 => LSTORE(index)
      case RReference(_) => ASTORE(index)
    }

  // TODO(JLS): bools are awkward with no PType, PBool
  def BOOLNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PInt32]]] = f => {
    f.visitIntInsn(Opcodes.NEWARRAY, Opcodes.T_BOOLEAN)
    castF(f)
  }

  def BNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PInt8]]] = f => {
    f.visitIntInsn(Opcodes.NEWARRAY, Opcodes.T_BYTE)
    castF(f)
  }

  def SNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PInt16]]] = f => {
    f.visitIntInsn(Opcodes.NEWARRAY, Opcodes.T_SHORT)
    castF(f)
  }

  def INEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PInt32]]] = f => {
    f.visitIntInsn(Opcodes.NEWARRAY, Opcodes.T_INT)
    castF(f)
  }

  def LNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PInt64]]] = f => {
    f.visitIntInsn(Opcodes.NEWARRAY, Opcodes.T_LONG)
    castF(f)
  }

  def CNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PChar]]] = f => {
    f.visitIntInsn(Opcodes.NEWARRAY, Opcodes.T_CHAR)
    castF(f)
  }

  def FNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PFloat32]]] = f => {
    f.visitIntInsn(Opcodes.NEWARRAY, Opcodes.T_FLOAT)
    castF(f)
  }

  def DNEWARRAY
  [R <: Stack]:
  F[R ** PInt32] => F[R ** PReference[PArray[PFloat64]]] = f => {
    f.visitIntInsn(Opcodes.NEWARRAY, Opcodes.T_DOUBLE)
    castF(f)
  }

  def ANEWARRAY
  [R <: Stack, T <: PRefType]
  (elmType: RType[PReference[T]]):
  F[R ** PInt32] => F[R ** PReference[PArray[PReference[T]]]] = f => {
    f.visitTypeInsn(Opcodes.ANEWARRAY, squeezeReference(elmType).internalName)
    castF(f)
  }

  // TODO(JLS): check multi dim arrays
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
  F[R ** PReference[PArray[T]] ** PInt32 ** PReference[PArray[T]] ** PInt32 ** PInt32] => F[R] = f => {
    val descriptor = JvmName.getMethodDescriptor(RObject :: RInt32 :: RObject :: RInt32 :: RInt32 :: Nil, None)
    f.visitMethodInsn(Opcodes.INVOKESTATIC, JvmName.Java.System.internalName, "arraycopy", descriptor)
    castF(f)
  }

  def arraysFill
  [R <: Stack, T <: PType]
  (elementType: RType[T]):
  F[R ** PReference[PArray[T]] ** T] => F[R] = f => {
    val descriptor = JvmName.getMethodDescriptor(RArray(elementType) :: elementType :: Nil, None)
    f.visitMethodInsn(Opcodes.INVOKESTATIC, JvmName.Java.Arrays.internalName, "fill", descriptor)
    castF(f)
  }

  def objectsEquals
  [R <: Stack, T1 <: PRefType, T2 <: PRefType]:
  F[R ** PReference[T2] ** PReference[T1]] => F[R ** PInt32] = f => {
    f.visitMethodInsn(Opcodes.INVOKESTATIC, JvmName.Java.Objects.internalName, "equals", JvmName.getMethodDescriptor(List(RObject, RObject), RBool))
    castF(f)
  }

  def objectsHash
  [R <: Stack, T <: PRefType]:
  F[R ** PReference[PArray[PReference[T]]]] => F[R ** PInt32] = f => {
    f.visitMethodInsn(Opcodes.INVOKESTATIC, JvmName.Java.Objects.internalName, "hash", JvmName.getMethodDescriptor(List(RArray(RReference(RObject))), RInt32))
    castF(f)
  }

  def arrayLength
  [R <: Stack, T <: PType]
  (tag: Tag[T] = null):
  F[R ** PReference[PArray[T]]] => F[R ** PInt32] = f => {
    f.visitInsn(Opcodes.ARRAYLENGTH)
    castF(f)
  }

  def getChannelValue
  [R <: Stack, T <: PRefType]
  (tpe: RType[PReference[T]]):
  F[R ** PReference[PChan[T]]] => F[R ** PReference[T]] = f => {
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Flix.Channel.internalName, "get", RObject.nothingToThisDescriptor)
    undoErasure(tpe, f.visitor)
    castF(f)
  }

  def putChannelValue
  [R <: Stack, T <: PRefType]
  (tag: Tag[T] = null):
  F[R ** PReference[PChan[T]] ** PReference[T]] => F[R] = f => {
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Flix.Channel.internalName, "put", RObject.thisToNothingDescriptor)
    castF(f)
  }

  def setRefValue
  [R <: Stack, T <: PType]
  (classType: RReference[PRef[T]], innerType: RType[T]):
  F[R ** PReference[PRef[T]] ** T] => F[R] = f => {
    f.visitFieldInsn(Opcodes.PUTFIELD, classType.internalName, GenRefClasses.ValueFieldName, innerType.erasedDescriptor)
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
