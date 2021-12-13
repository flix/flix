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
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Abstract.{IsAbstract, NotAbstract}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Interface.{IsInterface, NotInterface}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Static._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker._
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import org.objectweb.asm.{ClassWriter, Opcodes}


// TODO: There are further things you can constrain, fx. final classes have implicitly final methods.
sealed trait ClassMaker {
  /**
    * Creates a static field.
    */
  def mkStaticField(fieldName: String, fieldType: BackendType, v: Visibility, f: Final): Unit = {
    makeField(fieldName, fieldType, v, f, IsStatic)
  }

  /**
    * Creates a static constructor.
    */
  def mkStaticConstructor(ins: InstructionSet): Unit =
    makeMethod(Some(ins), JvmName.StaticConstructorMethod, MethodDescriptor.NothingToVoid, IsDefault, NotFinal, IsStatic, NotAbstract)

  /**
    * Creates a static constructor.
    */
  def mkStaticMethod(ins: InstructionSet, methodName: String, d: MethodDescriptor, v: Visibility, f: Final): Unit = {
    makeMethod(Some(ins), methodName, d, v, f, IsStatic, NotAbstract)
  }

  /**
    * Closes the class maker.
    * This should be the last function called on the class maker.
    */
  def closeClassMaker: Array[Byte] = {
    visitor.visitEnd()
    visitor.toByteArray
  }

  protected val visitor: ClassWriter

  protected def makeField(fieldName: String, fieldType: BackendType, v: Visibility, f: Final, s: Static): Unit = {
    val m = v.toInt + f.toInt + s.toInt
    val field = visitor.visitField(m, fieldName, fieldType.toDescriptor, null, null)
    field.visitEnd()
  }

  protected def makeMethod(i: Option[InstructionSet], methodName: String, d: MethodDescriptor, v: Visibility, f: Final, s: Static, a: Abstract): Unit = {
    val m = v.toInt + f.toInt + s.toInt + a.toInt
    val mv = visitor.visitMethod(m, methodName, d.toDescriptor, null, null)
    i match {
      case None => ()
      case Some(ins) =>
        mv.visitCode()
        ins(new BytecodeInstructions.F(mv))
        mv.visitMaxs(999, 999)
    }
    mv.visitEnd()
  }

  protected def makeAbstractMethod(methodName: String, d: MethodDescriptor): Unit = {
    makeMethod(None, methodName, d, IsPublic, NotFinal, NotStatic, IsAbstract)
  }
}

object ClassMaker {
  class InstanceClassMaker(cw: ClassWriter) extends ClassMaker {
    protected val visitor: ClassWriter = cw

    def mkField(fieldName: String, fieldType: BackendType, v: Visibility, f: Final): Unit = {
      makeField(fieldName, fieldType, v, f, NotStatic)
    }

    def mkConstructor(ins: InstructionSet, d: MethodDescriptor, v: Visibility): Unit = {
      makeMethod(Some(ins), JvmName.ConstructorMethod, d, v, NotFinal, NotStatic, NotAbstract)
    }

    def mkObjectConstructor(v: Visibility): Unit = {
      val ins = thisLoad() ~ invokeConstructor(JvmName.Object, MethodDescriptor.NothingToVoid) ~ RETURN()
      mkConstructor(ins, MethodDescriptor.NothingToVoid, v)
    }

    def mkMethod(ins: InstructionSet, methodName: String, d: MethodDescriptor, v: Visibility, f: Final): Unit = {
      makeMethod(Some(ins), methodName, d, v, f, NotStatic, NotAbstract)
    }
  }

  class AbstractClassMaker(cw: ClassWriter) extends ClassMaker {
    protected val visitor: ClassWriter = cw

    def mkField(fieldName: String, fieldType: BackendType, v: Visibility, f: Final): Unit = {
      makeField(fieldName, fieldType, v, f, NotStatic)
    }

    def mkConstructor(ins: InstructionSet, d: MethodDescriptor, v: Visibility): Unit = {
      makeMethod(Some(ins), JvmName.ConstructorMethod, d, v, NotFinal, NotStatic, NotAbstract)
    }

    def mkObjectConstructor(v: Visibility): Unit = {
      val ins = thisLoad() ~ invokeConstructor(JvmName.Object, MethodDescriptor.NothingToVoid) ~ RETURN()
      mkConstructor(ins, MethodDescriptor.NothingToVoid, v)
    }

    def mkMethod(ins: InstructionSet, methodName: String, d: MethodDescriptor, v: Visibility, f: Final): Unit = {
      makeMethod(Some(ins), methodName, d, v, f, NotStatic, NotAbstract)
    }

    def mkAbstractMethod(methodName: String, d: MethodDescriptor): Unit = {
      makeAbstractMethod(methodName, d)
    }
  }

  class InterfaceMaker(cw: ClassWriter) extends ClassMaker {
    protected val visitor: ClassWriter = cw

    def mkAbstractMethod(methodName: String, d: MethodDescriptor): Unit = {
      makeAbstractMethod(methodName, d)
    }
  }

  def mkClass(className: JvmName, f: Final, superClass: JvmName = JvmName.Object, interfaces: List[JvmName] = Nil)(implicit flix: Flix): InstanceClassMaker = {
    new InstanceClassMaker(mkClassWriter(className, IsPublic, f, NotAbstract, NotInterface, superClass, interfaces))
  }

  def mkAbstractClass(className: JvmName, superClass: JvmName = JvmName.Object, interfaces: List[JvmName] = Nil)(implicit flix: Flix): AbstractClassMaker = {
    new AbstractClassMaker(mkClassWriter(className, IsPublic, NotFinal, IsAbstract, NotInterface, superClass, interfaces))
  }

  def mkInterface(interfaceName: JvmName, interfaces: List[JvmName] = Nil)(implicit flix: Flix): InterfaceMaker = {
    new InterfaceMaker(mkClassWriter(interfaceName, IsPublic, NotFinal, IsAbstract, IsInterface, JvmName.Object, interfaces))
  }

  private def mkClassWriter(name: JvmName, v: Visibility, f: Final, a: Abstract, i: Interface, superClass: JvmName, interfaces: List[JvmName])(implicit flix: Flix): ClassWriter = {
    val cw = AsmOps.mkClassWriter()
    val m = v.toInt + f.toInt + a.toInt + i.toInt
    cw.visit(AsmOps.JavaVersion, m, name.toInternalName, null, superClass.toInternalName, interfaces.map(_.toInternalName).toArray)
    cw.visitSource(name.toInternalName, null)
    cw
  }

  sealed trait Visibility {
    val toInt: Int = this match {
      case IsPrivate => Opcodes.ACC_PRIVATE
      case IsDefault => 0
      case IsPublic => Opcodes.ACC_PUBLIC
    }
  }

  object Visibility {
    case object IsPrivate extends Visibility

    case object IsDefault extends Visibility

    case object IsPublic extends Visibility
  }


  sealed trait Final {
    val toInt: Int = this match {
      case IsFinal => Opcodes.ACC_FINAL
      case NotFinal => 0
    }
  }

  object Final {
    case object IsFinal extends Final

    case object NotFinal extends Final
  }

  sealed trait Static {
    val toInt: Int = this match {
      case IsStatic => Opcodes.ACC_STATIC
      case NotStatic => 0
    }
  }

  object Static {
    case object IsStatic extends Static

    case object NotStatic extends Static
  }

  sealed trait Abstract {
    val toInt: Int = this match {
      case Abstract.IsAbstract => Opcodes.ACC_ABSTRACT
      case Abstract.NotAbstract => 0
    }
  }

  object Abstract {
    case object IsAbstract extends Abstract

    case object NotAbstract extends Abstract
  }

  sealed trait Interface {
    val toInt: Int = this match {
      case Interface.IsInterface => Opcodes.ACC_INTERFACE
      case Interface.NotInterface => 0
    }
  }

  object Interface {
    case object IsInterface extends Interface

    case object NotInterface extends Interface
  }

  sealed case class InstanceField(clazz: JvmName, name: String, tpe: BackendType) {
    def mkField(cm: InstanceClassMaker, v: Visibility, f: Final): Unit =
      cm.mkField(name, tpe, v, f)

    def mkField(cm: AbstractClassMaker, v: Visibility, f: Final): Unit =
      cm.mkField(name, tpe, v, f)

    def putField(): InstructionSet =
      PUTFIELD(clazz, name, tpe)

    def getField(): InstructionSet =
      GETFIELD(clazz, name, tpe)
  }
}
