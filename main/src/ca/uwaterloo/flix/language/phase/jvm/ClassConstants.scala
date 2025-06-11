package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceMethod, InterfaceMethod, StaticMethod}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor
import org.objectweb.asm.MethodVisitor

object ClassConstants {

  // Flix Constants.

  object FlixError {

    def Constructor: ConstructorMethod = ConstructorMethod(JvmName.FlixError, List(BackendType.String))

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkAbstractClass(JvmName.FlixError, JvmName.Error)
      cm.mkConstructor(Constructor, IsPublic, constructorIns(_))
      cm.closeClassMaker()
    }

    private def constructorIns(implicit mv: MethodVisitor): Unit = {
      import BytecodeInstructions.*
      thisLoad()
      ALOAD(1)
      invokeConstructor(JvmName.Error, mkDescriptor(BackendType.String)(VoidableType.Void))
      RETURN()
    }

  }

  // Java Constants.

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

  object BigDecimal {
    def Constructor: ConstructorMethod = ClassMaker.ConstructorMethod(JvmName.BigDecimal, List(BackendType.String))
  }

  object BigInteger {
    def Constructor: ConstructorMethod = ClassMaker.ConstructorMethod(JvmName.BigInteger, List(BackendType.String))
  }

  object ConcurrentLinkedQueue {

    def AddMethod: InstanceMethod =
      InstanceMethod(JvmName.ConcurrentLinkedQueue, "add", mkDescriptor(BackendType.Object)(BackendType.Bool))

    def PollMethod: InstanceMethod =
      InstanceMethod(JvmName.ConcurrentLinkedQueue, "poll", mkDescriptor()(BackendType.Object))

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

    def CurrentThreadMethod: StaticMethod =
      StaticMethod(JvmName.Thread, "currentThread", mkDescriptor()(JvmName.Thread.toTpe))

    def InterruptMethod: InstanceMethod =
      InstanceMethod(JvmName.Thread, "interrupt", MethodDescriptor.NothingToVoid)

    def JoinMethod: InstanceMethod =
      InstanceMethod(JvmName.Thread, "join", MethodDescriptor.NothingToVoid)

    def OfVirtualMethod: StaticMethod =
      StaticMethod(JvmName.Thread, "ofVirtual", mkDescriptor()(JvmName.Thread$Builder$OfVirtual.toTpe))

    def SetUncaughtExceptionHandlerMethod: InstanceMethod =
      InstanceMethod(JvmName.Thread, "setUncaughtExceptionHandler", mkDescriptor(JvmName.Thread$UncaughtExceptionHandler.toTpe)(VoidableType.Void))

    def StartMethod: InstanceMethod =
      InstanceMethod(JvmName.Thread, "start", MethodDescriptor.NothingToVoid)

    def StartVirtualThreadMethod: StaticMethod =
      ClassMaker.StaticMethod(JvmName.Thread, "startVirtualThread", MethodDescriptor.mkDescriptor(JvmName.Runnable.toTpe)(JvmName.Thread.toTpe))

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
