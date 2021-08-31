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
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

object GenUnitClass {

  val instanceFieldName = "INSTANCE"

  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    val jvmType = JvmType.Unit
    val jvmName = jvmType.name
    val bytecode = genByteCode(jvmType, jvmName)
    Map(jvmName -> JvmClass(jvmName, bytecode))
  }

  def genByteCode(jvmType: JvmType, name: JvmName)(implicit flix: Flix): Array[Byte] = {
    // class writer
    val visitor = AsmOps.mkClassWriter()

    // internal name of super
    val superClass = JvmName.Object.toInternalName

    // Initialize the visitor to create a class.
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, name.toInternalName, null, superClass, null)

    // Source of the class
    visitor.visitSource(name.toInternalName, null)

    // singleton instance
    visitor.visitField(ACC_STATIC + ACC_FINAL + ACC_PUBLIC, instanceFieldName, jvmType.toDescriptor, null, null).visitEnd()

    genStaticConstructor(jvmType, name, visitor)
    genConstructor(jvmType, name, visitor)

    visitor.visitEnd()
    visitor.toByteArray
  }

  def genStaticConstructor(jvmType: JvmType, name: JvmName, visitor: ClassWriter): Unit = {
    val methodVisitor = visitor.visitMethod(ACC_STATIC, "<clinit>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)
    methodVisitor.visitCode()
    methodVisitor.visitTypeInsn(NEW, name.toInternalName)
    methodVisitor.visitInsn(DUP)
    methodVisitor.visitMethodInsn(INVOKESPECIAL, name.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    methodVisitor.visitFieldInsn(PUTSTATIC, name.toInternalName, instanceFieldName, jvmType.toDescriptor)
    methodVisitor.visitInsn(RETURN)
    methodVisitor.visitMaxs(999, 999)
    methodVisitor.visitEnd()
  }

  def genConstructor(jvmType: JvmType, name: JvmName, visitor: ClassWriter): Unit = {
    val methodVisitor = visitor.visitMethod(ACC_PRIVATE, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)
    methodVisitor.visitCode()
    methodVisitor.visitVarInsn(ALOAD, 0)
    methodVisitor.visitMethodInsn(INVOKESPECIAL, JvmName.Object.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    methodVisitor.visitInsn(RETURN)
    methodVisitor.visitMaxs(999, 999)
    methodVisitor.visitEnd()
  }

}
