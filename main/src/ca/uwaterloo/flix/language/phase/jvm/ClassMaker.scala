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

import ca.uwaterloo.flix.api.{CompilerConstants, Flix}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.*
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Abstract.{IsAbstract, NotAbstract}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.*
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Interface.{IsInterface, NotInterface}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Static.*
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.*
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.*
import ca.uwaterloo.flix.language.ast.shared.JvmAnnotation
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import org.objectweb.asm.{ClassWriter, MethodVisitor, Opcodes}


// TODO: There are further things you can constrain and assert, e.g. final classes have implicitly final methods.
sealed trait ClassMaker {
  def mkStaticConstructor(c: StaticConstructorMethod, ins: MethodVisitor => Unit): Unit = {
    makeMethod(Nil, Some(ins), c.name, c.d, IsDefault, NotFinal, IsStatic, NotAbstract)
  }

  /**
    * Closes the class maker.
    * This should be the last function called on the class maker.
    */
  def closeClassMaker(): Array[Byte] = {
    visitor.visitEnd()
    visitor.toByteArray
  }

  protected val visitor: ClassWriter

  protected def makeField(fieldName: String, fieldType: BackendType, v: Visibility, f: Final, vol: Volatility, s: Static): Unit = {
    val m = v.toInt + f.toInt + s.toInt + vol.toInt
    val field = visitor.visitField(m, fieldName, fieldType.toDescriptor, null, null)
    field.visitEnd()
  }

  def mkField(field: Field, v: Visibility, f: Final, vol: Volatility): Unit = field match {
    case InstanceField(_, name, tpe) => makeField(name, tpe, v, f, vol, NotStatic)
    case StaticField(_, name, tpe) => makeField(name, tpe, v, f, vol, IsStatic)
  }

  protected def makeMethod(ann: List[JvmAnnotation], i: Option[MethodVisitor => Unit], methodName: String, d: MethodDescriptor, v: Visibility, f: Final, s: Static, a: Abstract): Unit = {
    val m = v.toInt + f.toInt + s.toInt + a.toInt
    val mv = visitor.visitMethod(m, methodName, d.toDescriptor, null, null)
    for (a <- ann) {
      val descriptor = "L" + a.clazz.getName.replace('.', '/') + ";"
      val retention = a.clazz.getAnnotation(classOf[java.lang.annotation.Retention])
      val visible = retention != null && retention.value() == java.lang.annotation.RetentionPolicy.RUNTIME
      val av = mv.visitAnnotation(descriptor, visible)
      av.visitEnd()
    }
    i match {
      case None => ()
      case Some(ins) =>
        mv.visitCode()
        ins(mv)
        mv.visitMaxs(999, 999)
    }
    mv.visitEnd()
  }

  protected def makeAbstractMethod(methodName: String, d: MethodDescriptor): Unit = {
    makeMethod(Nil, None, methodName, d, IsPublic, NotFinal, NotStatic, IsAbstract)
  }
}

object ClassMaker {

  class InstanceClassMaker(cw: ClassWriter) extends ClassMaker {
    protected val visitor: ClassWriter = cw

    def mkStaticMethod(m: StaticMethod, v: Visibility, f: Final, ins: MethodVisitor => Unit): Unit = {
      makeMethod(Nil, Some(ins), m.name, m.d, v, f, IsStatic, NotAbstract)
    }

    def mkConstructor(c: ConstructorMethod, v: Visibility, ins: MethodVisitor => Unit): Unit = {
      makeMethod(Nil, Some(ins), JvmName.ConstructorMethod, c.d, v, NotFinal, NotStatic, NotAbstract)
    }

    def mkMethod(m: InstanceMethod, v: Visibility, f: Final, ins: MethodVisitor => Unit): Unit = {
      makeMethod(Nil, Some(ins), m.name, m.d, v, f, NotStatic, NotAbstract)
    }

    def mkMethod(ann: List[JvmAnnotation], m: InstanceMethod, v: Visibility, f: Final, ins: MethodVisitor => Unit): Unit = {
      makeMethod(ann, Some(ins), m.name, m.d, v, f, NotStatic, NotAbstract)
    }
  }

  class AbstractClassMaker(cw: ClassWriter) extends ClassMaker {
    protected val visitor: ClassWriter = cw

    def mkConstructor(c: ConstructorMethod, v: Visibility, ins: MethodVisitor => Unit): Unit = {
      makeMethod(Nil, Some(ins), c.name, c.d, v, NotFinal, NotStatic, NotAbstract)
    }

    def mkStaticMethod(m: StaticMethod, v: Visibility, f: Final, ins: MethodVisitor => Unit): Unit = {
      makeMethod(Nil, Some(ins), m.name, m.d, v, f, IsStatic, NotAbstract)
    }

    def mkMethod(m: InstanceMethod, v: Visibility, f: Final, ins: MethodVisitor => Unit): Unit = {
      makeMethod(Nil, Some(ins), m.name, m.d, v, f, NotStatic, NotAbstract)
    }

    def mkAbstractMethod(m: AbstractMethod): Unit = {
      makeAbstractMethod(m.name, m.d)
    }
  }

  class InterfaceMaker(cw: ClassWriter) extends ClassMaker {
    protected val visitor: ClassWriter = cw

    def mkInterfaceMethod(m: InterfaceMethod): Unit = {
      makeAbstractMethod(m.name, m.d)
    }

    def mkStaticInterfaceMethod(m: StaticInterfaceMethod, v: Visibility, f: Final, ins: MethodVisitor => Unit): Unit = {
      makeMethod(Nil, Some(ins), m.name, m.d, v, f, IsStatic, NotAbstract)
    }

    def mkDefaultMethod(m: DefaultMethod, v: Visibility, f: Final, ins: MethodVisitor => Unit): Unit = {
      makeMethod(Nil, Some(ins), m.name, m.d, v, f, NotStatic, NotAbstract)
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
    cw.visit(CompilerConstants.JvmTargetVersion, m, name.toInternalName, null, superClass.toInternalName, interfaces.map(_.toInternalName).toArray)
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

  sealed trait Volatility {
    val toInt: Int = this match {
      case IsVolatile => Opcodes.ACC_VOLATILE
      case NotVolatile => 0
    }
  }

  object Volatility {
    case object IsVolatile extends Volatility

    case object NotVolatile extends Volatility
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

  sealed trait Field {
    def clazz: JvmName

    def name: String

    def tpe: BackendType
  }

  sealed case class InstanceField(clazz: JvmName, name: String, tpe: BackendType) extends Field

  sealed case class StaticField(clazz: JvmName, name: String, tpe: BackendType) extends Field

  sealed trait Method {
    def clazz: JvmName

    def name: String

    def d: MethodDescriptor
  }

  sealed case class ConstructorMethod(clazz: JvmName, args: List[BackendType]) extends Method {
    override def name: String = JvmName.ConstructorMethod

    override def d: MethodDescriptor = MethodDescriptor(args, VoidableType.Void)
  }

  case class StaticConstructorMethod(clazz: JvmName) extends Method {
    override def name: String = JvmName.StaticConstructorMethod

    override def d: MethodDescriptor = MethodDescriptor.NothingToVoid
  }

  sealed case class InstanceMethod(clazz: JvmName, name: String, d: MethodDescriptor) extends Method {
    def implementation(clazz: JvmName): InstanceMethod = InstanceMethod(clazz, name, d)
  }

  sealed case class DefaultMethod(clazz: JvmName, name: String, d: MethodDescriptor) extends Method

  sealed case class InterfaceMethod(clazz: JvmName, name: String, d: MethodDescriptor) extends Method {
    def implementation(clazz: JvmName): InstanceMethod = InstanceMethod(clazz, name, d)
  }

  sealed case class AbstractMethod(clazz: JvmName, name: String, d: MethodDescriptor) extends Method {
    def implementation(clazz: JvmName): InstanceMethod = InstanceMethod(clazz, name, d)
  }

  sealed case class StaticMethod(clazz: JvmName, name: String, d: MethodDescriptor) extends Method

  sealed case class StaticInterfaceMethod(clazz: JvmName, name: String, d: MethodDescriptor) extends Method

}
