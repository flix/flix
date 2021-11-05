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
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

/**
  * A copy of this generated class has to be maintained at main/src/dev/flix/runtime/GlobalCounter.java.
  */
object GenGlobalCounterClass {

  val NewIdMethodName: String = "newId"
  val NewIdReturnType: JvmType = JvmType.PrimLong

  private val counterFieldName: String = "counter"

  def gen()(implicit flix: Flix): Map[JvmName, JvmClass] = {
    val className = JvmName.GlobalCounter
    val bytecode = genByteCode(className)
    Map(className -> JvmClass(className, bytecode))
  }

  private def genByteCode(name: JvmName)(implicit flix: Flix): Array[Byte] = {
    // class writer
    val visitor = AsmOps.mkClassWriter()

    // internal name of super
    val superClass = JvmName.Object

    // Initialize the visitor to create a class.
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, name.toInternalName, null, superClass.toInternalName, null)

    // Source of the class
    visitor.visitSource(name.toInternalName, null)

    genConstructor(superClass, visitor)
    genStaticConstructor(name, visitor)
    visitor.visitField(ACC_PRIVATE + ACC_FINAL + ACC_STATIC, counterFieldName, JvmName.AtomicLong.toDescriptor, null, null).visitEnd()
    genNewIdMethod(name, visitor)

    visitor.visitEnd()
    visitor.toByteArray
  }

  private def genConstructor(superClass: JvmName, visitor: ClassWriter): Unit = {
    val method = visitor.visitMethod(ACC_PRIVATE, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)
    method.visitCode()

    method.visitVarInsn(ALOAD, 0)
    method.visitMethodInsn(INVOKESPECIAL, superClass.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    method.visitInsn(RETURN)

    method.visitMaxs(999, 999)
    method.visitEnd()
  }

  private def genStaticConstructor(name: JvmName, visitor: ClassWriter): Unit = {
    val methodVisitor = visitor.visitMethod(ACC_STATIC, "<clinit>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)
    methodVisitor.visitCode()

    methodVisitor.visitTypeInsn(NEW, JvmName.AtomicLong.toInternalName)
    methodVisitor.visitInsn(DUP)
    methodVisitor.visitMethodInsn(INVOKESPECIAL, JvmName.AtomicLong.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
    methodVisitor.visitFieldInsn(PUTSTATIC, name.toInternalName, counterFieldName, JvmName.AtomicLong.toDescriptor)
    methodVisitor.visitInsn(RETURN)

    methodVisitor.visitMaxs(999, 999)
    methodVisitor.visitEnd()
  }

  private def genNewIdMethod(name: JvmName, visitor: ClassWriter)(implicit flix: Flix): Unit = {
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_STATIC + ACC_FINAL, NewIdMethodName, AsmOps.getMethodDescriptor(Nil, NewIdReturnType), null, null)
    method.visitCode()

    method.visitFieldInsn(GETSTATIC, name.toInternalName, counterFieldName, JvmName.AtomicLong.toDescriptor)
    method.visitMethodInsn(INVOKEVIRTUAL, JvmName.AtomicLong.toInternalName, "getAndIncrement", AsmOps.getMethodDescriptor(Nil, JvmType.PrimLong), false)
    method.visitInsn(AsmOps.getReturnInstruction(NewIdReturnType))

    method.visitMaxs(999, 999)
    method.visitEnd()
  }

}
