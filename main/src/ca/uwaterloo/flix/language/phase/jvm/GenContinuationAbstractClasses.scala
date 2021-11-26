/*
 * Copyright 2017 Magnus Madsen
 * Copyright 2021 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.ast.MonoType
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassWriter, Label}

/**
  * Generates bytecode for the continuation classes.
  */
object GenContinuationAbstractClasses {

  val ResultFieldName: String = "result"
  val InvokeMethodName: String = "invoke"
  val UnwindMethodName: String = "unwind"

  /**
    * Returns the set of continuation classes for the given set of types `ts`.
    */
  def gen(ts: Set[MonoType])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    ts.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, tpe@MonoType.Arrow(_, tresult)) =>
        // Case 1: The type constructor is an arrow.
        // Construct continuation class.
        val jvmType = JvmOps.getContinuationInterfaceType(tpe)
        val jvmName = jvmType.name
        val resultType = JvmOps.getErasedJvmType(tresult)
        val bytecode = genByteCode(jvmType, resultType)
        macc + (jvmName -> JvmClass(jvmName, bytecode))
      case (macc, _) =>
        // Case 2: The type constructor is a non-arrow.
        // Nothing to be done. Return the map.
        macc
    }
  }

  /**
    * Returns the bytecode for the given continuation class.
    */
  private def genByteCode(interfaceType: JvmType.Reference, resultType: JvmType)(implicit root: Root, flix: Flix): Array[Byte] = {

    // Pseudo code to generate:
    //
    // public abstract class Cont$Bool implements java.lang.Runnable {
    //   public abstract boolean result;
    //   public Cont$Bool() { ... }
    //   public abstract Cont$Bool invoke();
    //   public final boolean unwind();
    //   public final boolean run();
    // }
    //

    // Class visitor
    val visitor = AsmOps.mkClassWriter()

    // Class header
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_ABSTRACT,
      interfaceType.name.toInternalName, null, JvmName.Object.toInternalName, Array(JvmName.Runnable.toInternalName))

    // Class constructor
    genConstructor(visitor)

    // Result field
    visitor.visitField(ACC_PUBLIC + ACC_ABSTRACT, ResultFieldName,
      resultType.toDescriptor, null, null).visitEnd()

    // `invoke()` method
    visitor.visitMethod(ACC_PUBLIC + ACC_ABSTRACT, InvokeMethodName,
      AsmOps.getMethodDescriptor(Nil, interfaceType), null, null).visitEnd()

    // `unwind()` method
    genUnwindMethod(visitor, interfaceType, resultType)

    // `run()` method
    genRunMethod(visitor, interfaceType, resultType)

    visitor.visitEnd()
    visitor.toByteArray
  }

  private def genConstructor(visitor: ClassWriter): Unit = {
    val m = visitor.visitMethod(ACC_PUBLIC, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, null, null)

    m.visitVarInsn(ALOAD, 0)
    m.visitMethodInsn(INVOKESPECIAL, JvmName.Object.toInternalName, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, false)
    m.visitInsn(RETURN)

    m.visitMaxs(999, 999)
    m.visitEnd()
  }

  private def genUnwindMethod(visitor: ClassWriter, interfaceType: JvmType.Reference, erasedResultType: JvmType): Unit = {
    val m = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, UnwindMethodName, AsmOps.getMethodDescriptor(Nil, erasedResultType), null, null)
    m.visitCode()

    m.visitVarInsn(ALOAD, 0)
    m.visitVarInsn(ASTORE, 1)

    m.visitInsn(ACONST_NULL)
    m.visitVarInsn(ASTORE, 2)

    val loopStart = new Label()
    m.visitLabel(loopStart)
    m.visitVarInsn(ALOAD, 1)
    m.visitVarInsn(ASTORE, 2)
    m.visitVarInsn(ALOAD, 1)
    m.visitMethodInsn(INVOKEVIRTUAL, interfaceType.name.toInternalName, InvokeMethodName, AsmOps.getMethodDescriptor(Nil, interfaceType), false)
    m.visitVarInsn(ASTORE, 1)
    m.visitVarInsn(ALOAD, 1)
    m.visitJumpInsn(IFNONNULL, loopStart)

    m.visitVarInsn(ALOAD, 2)
    m.visitFieldInsn(GETFIELD, interfaceType.name.toInternalName, ResultFieldName, erasedResultType.toDescriptor)
    m.visitInsn(AsmOps.getReturnInstruction(erasedResultType))

    m.visitMaxs(999, 999)
    m.visitEnd()
  }

  private def genRunMethod(visitor: ClassWriter, interfaceType: JvmType.Reference, erasedResultType: JvmType): Unit = {
    val m = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "run", MethodDescriptor.NothingToVoid.toDescriptor, null, null)

    m.visitVarInsn(ALOAD, 0)
    m.visitMethodInsn(INVOKEVIRTUAL, interfaceType.name.toInternalName, UnwindMethodName, AsmOps.getMethodDescriptor(Nil, erasedResultType), false)
    if (AsmOps.getStackSize(erasedResultType) == 1) {
      m.visitInsn(POP)
    } else {
      m.visitInsn(POP2)
    }
    m.visitInsn(RETURN)

    m.visitMaxs(999, 999)
    m.visitEnd()
  }

}
