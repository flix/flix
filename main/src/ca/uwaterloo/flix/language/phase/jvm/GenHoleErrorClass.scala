/*
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
import org.objectweb.asm.{ClassWriter, Label}
import org.objectweb.asm.Opcodes._

object GenHoleErrorClass {

  val HoleFieldName: String = "hole"
  val LocationFieldName: String = "location"

  def gen()(implicit flix: Flix): Map[JvmName, JvmClass] = {
    val jvmName = JvmName.HoleError
    val bytecode = genByteCode(jvmName)
    Map(jvmName -> JvmClass(jvmName, bytecode))
  }

  private def genByteCode(name: JvmName)(implicit flix: Flix): Array[Byte] = {
    // class writer
    val visitor = AsmOps.mkClassWriter()

    // internal name of super
    val superClass = JvmName.FlixError

    // Initialize the visitor to create a class.
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, name.toInternalName, null, superClass.toInternalName, null)

    // Source of the class
    visitor.visitSource(name.toInternalName, null)

    genConstructor(name, superClass, visitor)
    genEquals(name, visitor)
    genHashCode(name, visitor)
    visitor.visitField(ACC_PUBLIC + ACC_FINAL, HoleFieldName, JvmName.String.toDescriptor, null, null).visitEnd()
    visitor.visitField(ACC_PUBLIC + ACC_FINAL, LocationFieldName, JvmName.ReifiedSourceLocation.toDescriptor, null, null).visitEnd()

    visitor.visitEnd()
    visitor.toByteArray
  }

  private def genConstructor(name: JvmName, superClass: JvmName, visitor: ClassWriter): Unit = {
    val stringToBuilderDescriptor = s"(${JvmName.String.toDescriptor})${JvmName.StringBuilder.toDescriptor}"
    val builderName = JvmName.StringBuilder.toInternalName

    val method = visitor.visitMethod(ACC_PUBLIC, "<init>", s"(${JvmName.String.toDescriptor}${JvmName.ReifiedSourceLocation.toDescriptor})${JvmType.Void.toDescriptor}", null, null)
    method.visitCode()

    method.visitVarInsn(ALOAD, 0)
    method.visitTypeInsn(NEW, builderName)
    method.visitInsn(DUP)
    method.visitMethodInsn(INVOKESPECIAL, builderName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    method.visitLdcInsn("Hole '")
    method.visitMethodInsn(INVOKEVIRTUAL, builderName, "append", stringToBuilderDescriptor, false)
    method.visitVarInsn(ALOAD, 1)
    method.visitMethodInsn(INVOKEVIRTUAL, builderName, "append", stringToBuilderDescriptor, false)
    method.visitLdcInsn("' at ")
    method.visitMethodInsn(INVOKEVIRTUAL, builderName, "append", stringToBuilderDescriptor, false)
    method.visitVarInsn(ALOAD, 2)
    method.visitMethodInsn(INVOKEVIRTUAL, JvmName.ReifiedSourceLocation.toInternalName, "toString", AsmOps.getMethodDescriptor(Nil, JvmType.String), false)
    method.visitMethodInsn(INVOKEVIRTUAL, builderName, "append", stringToBuilderDescriptor, false)
    method.visitMethodInsn(INVOKEVIRTUAL, builderName, "toString", AsmOps.getMethodDescriptor(Nil, JvmType.String), false)
    method.visitMethodInsn(INVOKESPECIAL, superClass.toInternalName, "<init>", AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Void), false)
    method.visitVarInsn(ALOAD, 0)
    method.visitVarInsn(ALOAD, 1)
    method.visitFieldInsn(PUTFIELD, name.toInternalName, HoleFieldName, JvmName.String.toDescriptor)
    method.visitVarInsn(ALOAD, 0)
    method.visitVarInsn(ALOAD, 2)
    method.visitFieldInsn(PUTFIELD, name.toInternalName, LocationFieldName, JvmName.ReifiedSourceLocation.toDescriptor)
    method.visitInsn(RETURN)

    method.visitMaxs(999, 999)
    method.visitEnd()
  }

  private def genEquals(name: JvmName, visitor: ClassWriter): Unit = {
    val method = visitor.visitMethod(ACC_PUBLIC, "equals", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.PrimBool), null, null)
    method.visitCode()

    method.visitVarInsn(ALOAD, 0)
    method.visitVarInsn(ALOAD, 1)
    val continue1 = new Label()
    method.visitJumpInsn(IF_ACMPNE, continue1)
    method.visitInsn(ICONST_1)
    method.visitInsn(IRETURN)

    method.visitLabel(continue1)
    method.visitVarInsn(ALOAD, 1)
    val returnFalse1 = new Label()
    method.visitJumpInsn(IFNULL, returnFalse1)
    method.visitVarInsn(ALOAD, 0)
    method.visitMethodInsn(INVOKEVIRTUAL, JvmName.Object.toInternalName, "getClass", "()Ljava/lang/Class;", false)
    method.visitVarInsn(ALOAD, 1)
    method.visitMethodInsn(INVOKEVIRTUAL, JvmName.Object.toInternalName, "getClass", "()Ljava/lang/Class;", false)
    val continue2 = new Label()
    method.visitJumpInsn(IF_ACMPEQ, continue2)

    method.visitLabel(returnFalse1)
    method.visitInsn(ICONST_0)
    method.visitInsn(IRETURN)

    method.visitLabel(continue2)
    method.visitVarInsn(ALOAD, 1)
    method.visitTypeInsn(CHECKCAST, name.toInternalName)
    method.visitVarInsn(ASTORE, 2)
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, name.toInternalName, HoleFieldName, JvmName.String.toDescriptor)
    method.visitVarInsn(ALOAD, 2)
    method.visitFieldInsn(GETFIELD, name.toInternalName, HoleFieldName, JvmName.String.toDescriptor)
    method.visitMethodInsn(INVOKESTATIC, JvmName.Objects.toInternalName, "equals", AsmOps.getMethodDescriptor(List(JvmType.Object, JvmType.Object), JvmType.PrimBool), false)
    val returnFalse2 = new Label()
    method.visitJumpInsn(IFEQ, returnFalse2)
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, name.toInternalName, LocationFieldName, JvmName.ReifiedSourceLocation.toDescriptor)
    method.visitVarInsn(ALOAD, 2)
    method.visitFieldInsn(GETFIELD, name.toInternalName, LocationFieldName, JvmName.ReifiedSourceLocation.toDescriptor)
    method.visitMethodInsn(INVOKESTATIC, JvmName.Objects.toInternalName, "equals", AsmOps.getMethodDescriptor(List(JvmType.Object, JvmType.Object), JvmType.PrimBool), false)
    method.visitJumpInsn(IFEQ, returnFalse2)
    method.visitInsn(ICONST_1)
    val iReturn = new Label()
    method.visitJumpInsn(GOTO, iReturn)

    method.visitLabel(returnFalse2)
    method.visitInsn(ICONST_0)

    method.visitLabel(iReturn)
    method.visitInsn(IRETURN)

    method.visitMaxs(999, 999)
    method.visitEnd()
  }

  private def genHashCode(name: JvmName, visitor: ClassWriter): Unit = {
    val method = visitor.visitMethod(ACC_PUBLIC, "hashCode", AsmOps.getMethodDescriptor(Nil, JvmType.PrimInt), null, null)
    method.visitCode()

    method.visitInsn(ICONST_2)
    method.visitTypeInsn(ANEWARRAY, JvmName.Object.toInternalName)
    method.visitInsn(DUP)
    method.visitInsn(ICONST_0)
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, name.toInternalName, HoleFieldName, JvmType.String.toDescriptor)
    method.visitInsn(AASTORE)
    method.visitInsn(DUP)
    method.visitInsn(ICONST_1)
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, name.toInternalName, LocationFieldName, JvmName.ReifiedSourceLocation.toDescriptor)
    method.visitInsn(AASTORE)
    method.visitMethodInsn(INVOKESTATIC, JvmName.Objects.toInternalName, "hash", s"([${JvmName.Object.toDescriptor})${JvmType.PrimInt.toDescriptor}", false)
    method.visitInsn(IRETURN)

    method.visitMaxs(999, 999)
    method.visitEnd()
  }

}
