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

object GenReifiedSourceLocationClass {

  val sourceFieldName: String = "source"
  val beginLineFieldName: String = "beginLine"
  val beginColFieldName: String = "beginCol"
  val endLineFieldName: String = "endLine"
  val endColFieldName: String = "endCol"
  val constructorDescriptor: String = AsmOps.getMethodDescriptor(List(JvmType.String, JvmType.PrimInt, JvmType.PrimInt, JvmType.PrimInt, JvmType.PrimInt), JvmType.Void)

  def gen()(implicit flix: Flix): Map[JvmName, JvmClass] = {
    val jvmName = JvmName.ReifiedSourceLocation
    val bytecode = genByteCode(jvmName)
    Map(jvmName -> JvmClass(jvmName, bytecode))
  }

  def genByteCode(name: JvmName)(implicit flix: Flix): Array[Byte] = {
    // class writer
    val visitor = AsmOps.mkClassWriter()

    // internal name of super
    val superClass = JvmName.Object

    // Initialize the visitor to create a class.
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, name.toInternalName, null, superClass.toInternalName, null)

    // Source of the class
    visitor.visitSource(name.toInternalName, null)

    def mkIntField(name: String): Unit = visitor.visitField(ACC_PUBLIC + ACC_FINAL, name, JvmType.PrimInt.toDescriptor, null, null).visitEnd()

    visitor.visitField(ACC_PUBLIC + ACC_FINAL, sourceFieldName, JvmType.String.toDescriptor, null, null).visitEnd()
    mkIntField(beginLineFieldName)
    mkIntField(beginColFieldName)
    mkIntField(endLineFieldName)
    mkIntField(endColFieldName)

    genConstructor(name, superClass, visitor)
    genEqualsMethod(name, visitor)
    genHashCode(name, visitor)
    genToString(name, visitor)

    visitor.visitEnd()
    visitor.toByteArray
  }

  def genToString(name: JvmName, visitor: ClassWriter): Unit = {
    val stringToBuilderDescriptor = s"(${JvmName.String.toDescriptor})${JvmName.StringBuilder.toDescriptor}"
    val intToBuilderDescriptor = s"(${JvmType.PrimInt.toDescriptor})${JvmName.StringBuilder.toDescriptor}"
    val builderName = JvmName.StringBuilder.toInternalName

    val method = visitor.visitMethod(ACC_PUBLIC, "toString", AsmOps.getMethodDescriptor(Nil, JvmType.String), null, null)
    method.visitCode()

    method.visitTypeInsn(NEW, builderName)
    method.visitInsn(DUP)
    method.visitMethodInsn(INVOKESPECIAL, builderName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, name.toInternalName, sourceFieldName, JvmName.String.toDescriptor)
    method.visitMethodInsn(INVOKEVIRTUAL, builderName, "append", stringToBuilderDescriptor, false)
    method.visitLdcInsn(":")
    method.visitMethodInsn(INVOKEVIRTUAL, builderName, "append", stringToBuilderDescriptor, false)
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, name.toInternalName, beginLineFieldName, JvmType.PrimInt.toDescriptor)
    method.visitMethodInsn(INVOKEVIRTUAL, builderName, "append", intToBuilderDescriptor, false)
    method.visitLdcInsn(":")
    method.visitMethodInsn(INVOKEVIRTUAL, builderName, "append", stringToBuilderDescriptor, false)
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, name.toInternalName, beginColFieldName, JvmType.PrimInt.toDescriptor)
    method.visitMethodInsn(INVOKEVIRTUAL, builderName, "append", intToBuilderDescriptor, false)
    method.visitMethodInsn(INVOKEVIRTUAL, builderName, "toString", AsmOps.getMethodDescriptor(Nil, JvmType.String), false)
    method.visitInsn(ARETURN)

    method.visitMaxs(999, 999)
    method.visitEnd()
  }

  def genConstructor(name: JvmName, superClass: JvmName, visitor: ClassWriter)(implicit flix: Flix): Unit = {
    val method = visitor.visitMethod(ACC_PUBLIC, "<init>", constructorDescriptor, null, null)
    method.visitCode()

    method.visitVarInsn(ALOAD, 0)
    method.visitMethodInsn(INVOKESPECIAL, superClass.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

    method.visitVarInsn(ALOAD, 0)
    method.visitVarInsn(ALOAD, 1)
    method.visitFieldInsn(PUTFIELD, name.toInternalName, sourceFieldName, JvmType.String.toDescriptor)

    method.visitVarInsn(ALOAD, 0)
    method.visitVarInsn(ILOAD, 2)
    method.visitFieldInsn(PUTFIELD, name.toInternalName, beginLineFieldName, JvmType.PrimInt.toDescriptor)

    method.visitVarInsn(ALOAD, 0)
    method.visitVarInsn(ILOAD, 3)
    method.visitFieldInsn(PUTFIELD, name.toInternalName, beginColFieldName, JvmType.PrimInt.toDescriptor)

    method.visitVarInsn(ALOAD, 0)
    method.visitVarInsn(ILOAD, 4)
    method.visitFieldInsn(PUTFIELD, name.toInternalName, endLineFieldName, JvmType.PrimInt.toDescriptor)

    method.visitVarInsn(ALOAD, 0)
    method.visitVarInsn(ILOAD, 5)
    method.visitFieldInsn(PUTFIELD, name.toInternalName, endColFieldName, JvmType.PrimInt.toDescriptor)

    method.visitInsn(RETURN)

    method.visitMaxs(999, 999)
    method.visitEnd()
  }

  def genHashCode(name: JvmName, visitor: ClassWriter): Unit = {
    val method = visitor.visitMethod(ACC_PUBLIC, "hashCode", AsmOps.getMethodDescriptor(Nil, JvmType.PrimInt), null, null)
    method.visitCode()

    method.visitInsn(ICONST_5)
    method.visitTypeInsn(ANEWARRAY, JvmName.Objects.toInternalName)
    method.visitInsn(DUP)
    method.visitInsn(ICONST_0)
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, name.toInternalName, sourceFieldName, JvmType.String.toDescriptor)
    method.visitInsn(AASTORE)
    method.visitInsn(DUP)
    method.visitInsn(ICONST_1)
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, name.toInternalName, beginLineFieldName, JvmType.PrimInt.toDescriptor)
    method.visitMethodInsn(INVOKESTATIC, JvmName.Integer.toInternalName, "valueOf", s"(${JvmType.PrimInt.toDescriptor})Ljava/lang/Integer;", false)
    method.visitInsn(AASTORE)
    method.visitInsn(DUP)
    method.visitInsn(ICONST_2)
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, name.toInternalName, beginColFieldName, JvmType.PrimInt.toDescriptor)
    method.visitMethodInsn(INVOKESTATIC, JvmName.Integer.toInternalName, "valueOf", s"(${JvmType.PrimInt.toDescriptor})Ljava/lang/Integer;", false)
    method.visitInsn(AASTORE)
    method.visitInsn(DUP)
    method.visitInsn(ICONST_3)
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, name.toInternalName, endLineFieldName, JvmType.PrimInt.toDescriptor)
    method.visitMethodInsn(INVOKESTATIC, JvmName.Integer.toInternalName, "valueOf", s"(${JvmType.PrimInt.toDescriptor})Ljava/lang/Integer;", false)
    method.visitInsn(AASTORE)
    method.visitInsn(DUP)
    method.visitInsn(ICONST_4)
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, name.toInternalName, endColFieldName, JvmType.PrimInt.toDescriptor)
    method.visitMethodInsn(INVOKESTATIC, JvmName.Integer.toInternalName, "valueOf", s"(${JvmType.PrimInt.toDescriptor})Ljava/lang/Integer;", false)
    method.visitInsn(AASTORE)
    method.visitMethodInsn(INVOKESTATIC, JvmName.Objects.toInternalName, "hash", s"([${JvmName.Object.toDescriptor})${JvmType.PrimInt.toDescriptor}", false)
    method.visitInsn(IRETURN)

    method.visitMaxs(999, 999)
    method.visitEnd()
  }

  def genEqualsMethod(name: JvmName, visitor: ClassWriter): Unit = {
    val method = visitor.visitMethod(ACC_PUBLIC, "equals", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.PrimBool), null, null)
    method.visitCode()

    method.visitVarInsn(ALOAD, 0)
    method.visitVarInsn(ALOAD, 1)
    val continue = new Label()
    method.visitJumpInsn(IF_ACMPNE, continue)
    method.visitInsn(ICONST_1)
    method.visitInsn(IRETURN)

    method.visitLabel(continue)
    method.visitVarInsn(ALOAD, 1)
    val returnFalse1 = new Label()
    method.visitJumpInsn(IFNULL, returnFalse1)
    method.visitVarInsn(ALOAD, 0)
    method.visitMethodInsn(INVOKEVIRTUAL, JvmName.Object.toInternalName, "getClass", "()Ljava/lang/Class;", false)
    method.visitVarInsn(ALOAD, 1)
    method.visitMethodInsn(INVOKEVIRTUAL, JvmName.Object.toInternalName, "getClass", "()Ljava/lang/Class;", false)
    val compareFields = new Label()
    method.visitJumpInsn(IF_ACMPEQ, compareFields)

    method.visitLabel(returnFalse1)
    method.visitInsn(ICONST_0)
    method.visitInsn(IRETURN)

    method.visitLabel(compareFields)
    method.visitVarInsn(ALOAD, 1)
    method.visitTypeInsn(CHECKCAST, name.toInternalName)
    method.visitVarInsn(ASTORE, 2)
    val returnFalse2 = new Label()
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, name.toInternalName, beginLineFieldName, JvmType.PrimInt.toDescriptor)
    method.visitVarInsn(ALOAD, 2)
    method.visitFieldInsn(GETFIELD, name.toInternalName, beginLineFieldName, JvmType.PrimInt.toDescriptor)
    method.visitJumpInsn(IF_ICMPNE, returnFalse2)
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, name.toInternalName, beginColFieldName, JvmType.PrimInt.toDescriptor)
    method.visitVarInsn(ALOAD, 2)
    method.visitFieldInsn(GETFIELD, name.toInternalName, beginColFieldName, JvmType.PrimInt.toDescriptor)
    method.visitJumpInsn(IF_ICMPNE, returnFalse2)
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, name.toInternalName, endLineFieldName, JvmType.PrimInt.toDescriptor)
    method.visitVarInsn(ALOAD, 2)
    method.visitFieldInsn(GETFIELD, name.toInternalName, endLineFieldName, JvmType.PrimInt.toDescriptor)
    method.visitJumpInsn(IF_ICMPNE, returnFalse2)
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, name.toInternalName, endColFieldName, JvmType.PrimInt.toDescriptor)
    method.visitVarInsn(ALOAD, 2)
    method.visitFieldInsn(GETFIELD, name.toInternalName, endColFieldName, JvmType.PrimInt.toDescriptor)
    method.visitJumpInsn(IF_ICMPNE, returnFalse2)
    method.visitVarInsn(ALOAD, 0)
    method.visitFieldInsn(GETFIELD, name.toInternalName, sourceFieldName, JvmType.String.toDescriptor)
    method.visitVarInsn(ALOAD, 2)
    method.visitFieldInsn(GETFIELD, name.toInternalName, sourceFieldName, JvmType.String.toDescriptor)
    method.visitMethodInsn(INVOKESTATIC, JvmName.Objects.toInternalName, "equals", AsmOps.getMethodDescriptor(List(JvmType.Object, JvmType.Object), JvmType.PrimBool), false)
    method.visitJumpInsn(IFEQ, returnFalse2)
    method.visitInsn(ICONST_1)
    val returnInt = new Label()
    method.visitJumpInsn(GOTO, returnInt)

    method.visitLabel(returnFalse2)
    method.visitInsn(ICONST_0)

    method.visitLabel(returnInt)
    method.visitInsn(IRETURN)

    method.visitMaxs(999, 999)
    method.visitEnd()
  }

}
