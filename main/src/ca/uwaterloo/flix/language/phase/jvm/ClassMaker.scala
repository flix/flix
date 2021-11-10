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

package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

class ClassMaker(visitor: ClassWriter) {
  private def makeField(fieldName: String, fieldType: JvmType, mod: Int): Unit = {
    val field = visitor.visitField(mod, fieldName, fieldType.toDescriptor, null, null)
    field.visitEnd()
  }

  def mkField(fieldName: String, fieldType: JvmType): Unit = {
    makeField(fieldName, fieldType, ACC_PUBLIC)
  }

  def mkStaticField(fieldName: String, fieldType: JvmType): Unit = {
    makeField(fieldName, fieldType, ACC_STATIC + ACC_FINAL + ACC_PUBLIC)
  }

  def mkPublicConstructor(f: BytecodeInstructions.Instruction, descriptor: String): Unit = {
    mkMethod(f, JvmName.ConstructorMethod, descriptor, ACC_PUBLIC)
  }

  def mkPrivateConstructor(f: BytecodeInstructions.Instruction, descriptor: String): Unit = {
    mkMethod(f, JvmName.ConstructorMethod, descriptor, ACC_PRIVATE)
  }

  def mkStaticConstructor(f: BytecodeInstructions.Instruction): Unit =
    mkMethod(f, JvmName.StaticConstructorMethod, JvmName.Descriptors.NothingToVoid, ACC_STATIC)

  private def mkMethod(f: BytecodeInstructions.Instruction, methodName: String, descriptor: String, mod: Int): Unit = {
    val methodVisitor = visitor.visitMethod(mod, methodName, descriptor, null, null)
    methodVisitor.visitCode()
    f(new BytecodeInstructions.F(methodVisitor))
    methodVisitor.visitMaxs(999, 999)
    methodVisitor.visitEnd()
  }

  def closeClassMaker: Array[Byte] = {
    visitor.visitEnd()
    visitor.toByteArray
  }
}

object ClassMaker {

  private def mkClassMaker(className: JvmName, superClass: JvmName, mod: Int, interfaces: List[JvmName])(implicit flix: Flix): ClassMaker = {
    val visitor = AsmOps.mkClassWriter()
    visitor.visit(AsmOps.JavaVersion, mod, className.toInternalName, null, superClass.toInternalName, interfaces.map(_.toInternalName).toArray)
    visitor.visitSource(className.toInternalName, null)
    new ClassMaker(visitor)
  }

  def mkClass(className: JvmName, superClass: JvmName = JvmName.Object, interfaces: List[JvmName] = Nil)(implicit flix: Flix): ClassMaker = {
    mkClassMaker(className, superClass, ACC_PUBLIC + ACC_FINAL, interfaces)
  }
}
