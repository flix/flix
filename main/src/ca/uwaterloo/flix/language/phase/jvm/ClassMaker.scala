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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker._
import org.objectweb.asm.{ClassWriter, Opcodes}

class ClassMaker(visitor: ClassWriter) {
  private def makeField(fieldName: String, fieldType: JvmType, v: Visibility, f: Finality, i: Instancing): Unit = {
    val modifier = ClassMaker.valueOf(v) + ClassMaker.valueOf(f) + ClassMaker.valueOf(i)
    val field = visitor.visitField(modifier, fieldName, fieldType.toDescriptor, null, null)
    field.visitEnd()
  }

  def mkField(fieldName: String, fieldType: JvmType, v: Visibility, f: Finality, i: Instancing): Unit = {
    makeField(fieldName, fieldType, v, f, i)
  }

  def mkConstructor(f: BytecodeInstructions.Instruction, descriptor: String, v: Visibility): Unit = {
    mkMethod(f, JvmName.ConstructorMethod, descriptor, v, Instanced)
  }

  def mkStaticConstructor(f: BytecodeInstructions.Instruction): Unit =
    mkMethod(f, JvmName.StaticConstructorMethod, JvmName.Descriptors.NothingToVoid, Default, Static)

  private def mkMethod(f: BytecodeInstructions.Instruction, methodName: String, descriptor: String, v: Visibility, i: Instancing): Unit = {
    val modifier = ClassMaker.valueOf(v) + ClassMaker.valueOf(i)
    val methodVisitor = visitor.visitMethod(modifier, methodName, descriptor, null, null)
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

  private def valueOf(v: Visibility): Int = v match {
    case Private => Opcodes.ACC_PRIVATE
    case Default => 0
    case Public => Opcodes.ACC_PUBLIC
  }

  private def valueOf(f: Finality): Int = f match {
    case Final => Opcodes.ACC_FINAL
    case Implementable => 0
  }

  private def valueOf(i: Instancing): Int = i match {
    case Static => Opcodes.ACC_STATIC
    case Instanced => 0
  }

  private def mkClassMaker(className: JvmName, v: Visibility, f: Finality, superClass: JvmName, interfaces: List[JvmName])(implicit flix: Flix): ClassMaker = {
    val visitor = AsmOps.mkClassWriter()
    val modifier = valueOf(f) + valueOf(v)
    visitor.visit(AsmOps.JavaVersion, modifier, className.toInternalName, null, superClass.toInternalName, interfaces.map(_.toInternalName).toArray)
    visitor.visitSource(className.toInternalName, null)
    new ClassMaker(visitor)
  }

  def mkClass(className: JvmName, v: Visibility, f: Finality, superClass: JvmName = JvmName.Object, interfaces: List[JvmName] = Nil)(implicit flix: Flix): ClassMaker = {
    mkClassMaker(className, v, f, superClass, interfaces)
  }

  sealed trait Visibility

  object Private extends Visibility

  object Default extends Visibility

  object Public extends Visibility


  sealed trait Finality

  object Final extends Finality

  object Implementable extends Finality


  sealed trait Instancing

  object Static extends Instancing

  object Instanced extends Instancing


  sealed trait Abstraction

  object Abstract extends Abstraction

  object Implemented extends Abstraction
}
