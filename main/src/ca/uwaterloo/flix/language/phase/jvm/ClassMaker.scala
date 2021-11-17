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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Finality._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Instancing._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker._
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import org.objectweb.asm.{ClassWriter, Opcodes}

class ClassMaker(visitor: ClassWriter) {
  private def makeField(fieldName: String, fieldType: JvmType, v: Visibility, f: Finality, i: Instancing): Unit = {
    val modifier = v.toInt + f.toInt + i.toInt
    val field = visitor.visitField(modifier, fieldName, fieldType.toDescriptor, null, null)
    field.visitEnd()
  }

  def mkField(fieldName: String, fieldType: JvmType, v: Visibility, f: Finality, i: Instancing): Unit = {
    makeField(fieldName, fieldType, v, f, i)
  }

  def mkConstructor(f: BytecodeInstructions.InstructionSet, descriptor: MethodDescriptor, v: Visibility): Unit = {
    mkMethod(f, JvmName.ConstructorMethod, descriptor, v, Implementable, Instanced)
  }

  def mkStaticConstructor(f: BytecodeInstructions.InstructionSet): Unit =
    mkMethod(f, JvmName.StaticConstructorMethod, MethodDescriptor.NothingToVoid, Default, Implementable, Static)

  def mkMethod(ins: BytecodeInstructions.InstructionSet, methodName: String, descriptor: MethodDescriptor, v: Visibility, f: Finality, i: Instancing): Unit = {
    val m = v.toInt + f.toInt + i.toInt
    val mv = visitor.visitMethod(m, methodName, descriptor.toString, null, null)
    mv.visitCode()
    ins(new BytecodeInstructions.F(mv))
    mv.visitMaxs(999, 999)
    mv.visitEnd()
  }

  def closeClassMaker: Array[Byte] = {
    visitor.visitEnd()
    visitor.toByteArray
  }
}

object ClassMaker {

  private def mkClassMaker(className: JvmName, v: Visibility, f: Finality, superClass: JvmName, interfaces: List[JvmName])(implicit flix: Flix): ClassMaker = {
    val cw = AsmOps.mkClassWriter()
    val m = f.toInt + v.toInt
    cw.visit(AsmOps.JavaVersion, m, className.toInternalName, null, superClass.toInternalName, interfaces.map(_.toInternalName).toArray)
    cw.visitSource(className.toInternalName, null)
    new ClassMaker(cw)
  }

  def mkClass(className: JvmName, v: Visibility, f: Finality, superClass: JvmName = JvmName.Object, interfaces: List[JvmName] = Nil)(implicit flix: Flix): ClassMaker = {
    mkClassMaker(className, v, f, superClass, interfaces)
  }

  sealed trait Visibility {
    def toInt: Int
  }

  object Visibility {
    case object Private extends Visibility {
      override val toInt: Int = Opcodes.ACC_PRIVATE
    }

    case object Default extends Visibility {
      override val toInt: Int = 0
    }

    case object Public extends Visibility {
      override val toInt: Int = Opcodes.ACC_PUBLIC
    }
  }


  sealed trait Finality {
    def toInt: Int
  }

  object Finality {
    case object Final extends Finality {
      override val toInt: Int = Opcodes.ACC_FINAL
    }

    case object Implementable extends Finality {
      override val toInt: Int = 0
    }
  }

  sealed trait Instancing {
    def toInt: Int
  }

  object Instancing {
    case object Static extends Instancing {
      override val toInt: Int = Opcodes.ACC_STATIC
    }

    case object Instanced extends Instancing {
      override val toInt: Int = 0
    }
  }


  sealed trait Abstraction {
    def toInt: Int
  }

  object Abstraction {
    case object Abstract extends Abstraction {
      override val toInt: Int = Opcodes.ACC_ABSTRACT
    }

    case object Implemented extends Abstraction {
      override val toInt: Int = 0
    }
  }
}
