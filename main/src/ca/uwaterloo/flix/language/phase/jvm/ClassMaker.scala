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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.*
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Abstract.{IsAbstract, NotAbstract}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.*
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Interface.{IsInterface, NotInterface}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Static.*
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.*
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.*
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor
import org.objectweb.asm.{ClassWriter, MethodVisitor, Opcodes}


// TODO: There are further things you can constrain and assert, e.g. final classes have implicitly final methods.
sealed trait ClassMaker {
  def mkStaticConstructor(c: StaticConstructorMethod, ins: MethodVisitor => Unit): Unit = {
    makeMethod(Some(ins), c.name, c.d, IsDefault, NotFinal, IsStatic, NotAbstract)
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

  protected def makeMethod(i: Option[MethodVisitor => Unit], methodName: String, d: MethodDescriptor, v: Visibility, f: Final, s: Static, a: Abstract): Unit = {
    val m = v.toInt + f.toInt + s.toInt + a.toInt
    val mv = visitor.visitMethod(m, methodName, d.toDescriptor, null, null)
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
    makeMethod(None, methodName, d, IsPublic, NotFinal, NotStatic, IsAbstract)
  }
}

object ClassMaker {

  class InstanceClassMaker(cw: ClassWriter) extends ClassMaker {
    protected val visitor: ClassWriter = cw

    def mkStaticMethod(m: StaticMethod, v: Visibility, f: Final, ins: MethodVisitor => Unit): Unit = {
      makeMethod(Some(ins), m.name, m.d, v, f, IsStatic, NotAbstract)
    }

    def mkConstructor(c: ConstructorMethod, v: Visibility, ins: MethodVisitor => Unit): Unit = {
      makeMethod(Some(ins), JvmName.ConstructorMethod, c.d, v, NotFinal, NotStatic, NotAbstract)
    }

    def mkMethod(m: InstanceMethod, v: Visibility, f: Final, ins: MethodVisitor => Unit): Unit = {
      makeMethod(Some(ins), m.name, m.d, v, f, NotStatic, NotAbstract)
    }
  }

  class AbstractClassMaker(cw: ClassWriter) extends ClassMaker {
    protected val visitor: ClassWriter = cw

    def mkConstructor(c: ConstructorMethod, v: Visibility, ins: MethodVisitor => Unit): Unit = {
      makeMethod(Some(ins), c.name, c.d, v, NotFinal, NotStatic, NotAbstract)
    }

    def mkStaticMethod(m: StaticMethod, v: Visibility, f: Final, ins: MethodVisitor => Unit): Unit = {
      makeMethod(Some(ins), m.name, m.d, v, f, IsStatic, NotAbstract)
    }

    def mkMethod(m: InstanceMethod, v: Visibility, f: Final, ins: MethodVisitor => Unit): Unit = {
      makeMethod(Some(ins), m.name, m.d, v, f, NotStatic, NotAbstract)
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
      makeMethod(Some(ins), m.name, m.d, v, f, IsStatic, NotAbstract)
    }

    def mkDefaultMethod(m: DefaultMethod, v: Visibility, f: Final, ins: MethodVisitor => Unit): Unit = {
      makeMethod(Some(ins), m.name, m.d, v, f, NotStatic, NotAbstract)
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

  // Constants.

  object Arrays {

    def BoolArrToString: StaticMethod =
      StaticMethod(JvmName.Arrays, "toString", mkDescriptor(BackendType.Array(BackendType.Bool))(BackendType.String))

    def CharArrToString: StaticMethod =
      StaticMethod(JvmName.Arrays, "toString", mkDescriptor(BackendType.Array(BackendType.Char))(BackendType.String))

    def Int8ArrToString: StaticMethod =
      StaticMethod(JvmName.Arrays, "toString", mkDescriptor(BackendType.Array(BackendType.Int8))(BackendType.String))

    def Int16ArrToString: StaticMethod =
      StaticMethod(JvmName.Arrays, "toString", mkDescriptor(BackendType.Array(BackendType.Int16))(BackendType.String))

    def Int32ArrToString: StaticMethod =
      StaticMethod(JvmName.Arrays, "toString", mkDescriptor(BackendType.Array(BackendType.Int32))(BackendType.String))

    def Int64ArrToString: StaticMethod =
      StaticMethod(JvmName.Arrays, "toString", mkDescriptor(BackendType.Array(BackendType.Int64))(BackendType.String))

    def Float32ArrToString: StaticMethod =
      StaticMethod(JvmName.Arrays, "toString", mkDescriptor(BackendType.Array(BackendType.Float32))(BackendType.String))

    def Float64ArrToString: StaticMethod =
      StaticMethod(JvmName.Arrays, "toString", mkDescriptor(BackendType.Array(BackendType.Float64))(BackendType.String))

    def DeepToString: StaticMethod =
      StaticMethod(JvmName.Arrays, "deepToString", mkDescriptor(BackendType.Array(BackendType.Object))(BackendType.String))

  }

  object Iterator {

    def HasNextMethod: InterfaceMethod =
      InterfaceMethod(JvmName.Iterator, "hasNext", mkDescriptor()(BackendType.Bool))

    def NextMethod: InterfaceMethod =
      InterfaceMethod(JvmName.Iterator, "next", mkDescriptor()(BackendType.Object))

  }

  object LambdaMetafactory {
    def MetafactoryMethod: StaticMethod =
      StaticMethod(JvmName.LambdaMetafactory, "metafactory", mkDescriptor(JvmName.MethodHandles$Lookup.toTpe, BackendType.String, JvmName.MethodType.toTpe, JvmName.MethodType.toTpe, JvmName.MethodHandle.toTpe, JvmName.MethodType.toTpe)(JvmName.CallSite.toTpe))
  }

  object LinkedList {

    def AddFirstMethod: InstanceMethod =
      InstanceMethod(JvmName.LinkedList, "addFirst", mkDescriptor(BackendType.Object)(VoidableType.Void))

    def IteratorMethod: InstanceMethod =
      InstanceMethod(JvmName.LinkedList, "iterator", mkDescriptor()(JvmName.Iterator.toTpe))

  }

  object Object {

    def Constructor: ConstructorMethod = ConstructorMethod(JvmName.Object, Nil)

    def EqualsMethod: InstanceMethod =
      InstanceMethod(JvmName.Object, "equals", mkDescriptor(BackendType.Object)(BackendType.Bool))

    def ToStringMethod: InstanceMethod =
      InstanceMethod(JvmName.Object, "toString", mkDescriptor()(BackendType.String))

  }

  object ReentrantLock {

    def Constructor: ConstructorMethod = ConstructorMethod(JvmName.ReentrantLock, Nil)

    def UnlockMethod: InstanceMethod = InstanceMethod(JvmName.ReentrantLock, "unlock", MethodDescriptor.NothingToVoid)

    def LockInterruptiblyMethod: InstanceMethod =
      InstanceMethod(JvmName.ReentrantLock, "lockInterruptibly", MethodDescriptor.NothingToVoid)

  }

  object Regex {
    def CompileMethod: StaticMethod =
      StaticMethod(JvmName.Regex, "compile", mkDescriptor(BackendType.String)(JvmName.Regex.toTpe))
  }

  object Runnable {
    def RunMethod: InterfaceMethod = InterfaceMethod(JvmName.Runnable, "run", MethodDescriptor.NothingToVoid)
  }

  object String {
    def Concat: InstanceMethod =
      InstanceMethod(JvmName.String, "concat", mkDescriptor(BackendType.String)(BackendType.String))
  }

  object StringBuilder {

    def Constructor: ConstructorMethod = ConstructorMethod(JvmName.StringBuilder, Nil)

    def AppendStringMethod: InstanceMethod =
      InstanceMethod(JvmName.StringBuilder, "append", mkDescriptor(BackendType.String)(JvmName.StringBuilder.toTpe))

    def AppendInt32Method: InstanceMethod =
      InstanceMethod(JvmName.StringBuilder, "append", mkDescriptor(BackendType.Int32)(JvmName.StringBuilder.toTpe))

  }

  object Thread {

    def StartMethod: InstanceMethod =
      InstanceMethod(JvmName.Thread, "start", MethodDescriptor.NothingToVoid)

    def JoinMethod: InstanceMethod =
      InstanceMethod(JvmName.Thread, "join", MethodDescriptor.NothingToVoid)

    def CurrentThreadMethod: StaticMethod =
      StaticMethod(JvmName.Thread, "currentThread", mkDescriptor()(JvmName.Thread.toTpe))

    def InterruptMethod: InstanceMethod =
      InstanceMethod(JvmName.Thread, "interrupt", MethodDescriptor.NothingToVoid)

    def SetUncaughtExceptionHandlerMethod: InstanceMethod =
      InstanceMethod(JvmName.Thread, "setUncaughtExceptionHandler", mkDescriptor(JvmName.Thread$UncaughtExceptionHandler.toTpe)(VoidableType.Void))

    def OfVirtualMethod: StaticMethod =
      StaticMethod(JvmName.Thread, "ofVirtual", mkDescriptor()(JvmName.Thread$Builder$OfVirtual.toTpe))
  }

  object ThreadBuilderOfVirtual {
    def UnstartedMethod: InterfaceMethod =
      InterfaceMethod(JvmName.Thread$Builder$OfVirtual, "unstarted", mkDescriptor(JvmName.Runnable.toTpe)(JvmName.Thread.toTpe))
  }

  object ThreadUncaughtExceptionHandler {
    def UncaughtExceptionMethod: InstanceMethod =
      InstanceMethod(JvmName.Thread$UncaughtExceptionHandler, "uncaughtException", mkDescriptor(JvmName.Thread.toTpe, JvmName.Throwable.toTpe)(VoidableType.Void))
  }

}
