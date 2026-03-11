/*
 *  Copyright 2025 Jonathan Lindegaard Starup
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceMethod, InterfaceMethod, StaticMethod}
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor
import org.objectweb.asm.MethodVisitor

object ClassConstants {

  // Flix Constants.

  object FlixError {

    val Constructor: ConstructorMethod = ConstructorMethod(JvmName.FlixError, List(BackendType.String))

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

    val BoolArrToString: StaticMethod =
      StaticMethod(JvmName.Arrays, "toString", mkDescriptor(BackendType.Array(BackendType.Bool))(BackendType.String))

    val CharArrToString: StaticMethod =
      StaticMethod(JvmName.Arrays, "toString", mkDescriptor(BackendType.Array(BackendType.Char))(BackendType.String))

    val Int8ArrToString: StaticMethod =
      StaticMethod(JvmName.Arrays, "toString", mkDescriptor(BackendType.Array(BackendType.Int8))(BackendType.String))

    val Int16ArrToString: StaticMethod =
      StaticMethod(JvmName.Arrays, "toString", mkDescriptor(BackendType.Array(BackendType.Int16))(BackendType.String))

    val Int32ArrToString: StaticMethod =
      StaticMethod(JvmName.Arrays, "toString", mkDescriptor(BackendType.Array(BackendType.Int32))(BackendType.String))

    val Int64ArrToString: StaticMethod =
      StaticMethod(JvmName.Arrays, "toString", mkDescriptor(BackendType.Array(BackendType.Int64))(BackendType.String))

    val Float32ArrToString: StaticMethod =
      StaticMethod(JvmName.Arrays, "toString", mkDescriptor(BackendType.Array(BackendType.Float32))(BackendType.String))

    val Float64ArrToString: StaticMethod =
      StaticMethod(JvmName.Arrays, "toString", mkDescriptor(BackendType.Array(BackendType.Float64))(BackendType.String))

    val DeepToString: StaticMethod =
      StaticMethod(JvmName.Arrays, "deepToString", mkDescriptor(BackendType.Array(BackendType.Object))(BackendType.String))

  }

  object BigDecimal {
    val Constructor: ConstructorMethod = ClassMaker.ConstructorMethod(JvmName.BigDecimal, List(BackendType.String))
  }

  object BigInteger {
    val Constructor: ConstructorMethod = ClassMaker.ConstructorMethod(JvmName.BigInteger, List(BackendType.String))
  }

  object ConcurrentLinkedQueue {

    val AddMethod: InstanceMethod =
      InstanceMethod(JvmName.ConcurrentLinkedQueue, "add", mkDescriptor(BackendType.Object)(BackendType.Bool))

    val PollMethod: InstanceMethod =
      InstanceMethod(JvmName.ConcurrentLinkedQueue, "poll", mkDescriptor()(BackendType.Object))

  }

  object Iterator {

    val HasNextMethod: InterfaceMethod =
      InterfaceMethod(JvmName.Iterator, "hasNext", mkDescriptor()(BackendType.Bool))

    val NextMethod: InterfaceMethod =
      InterfaceMethod(JvmName.Iterator, "next", mkDescriptor()(BackendType.Object))

  }

  object LambdaMetafactory {
    val MetafactoryMethod: StaticMethod =
      StaticMethod(JvmName.LambdaMetafactory, "metafactory", mkDescriptor(JvmName.MethodHandles$Lookup.toTpe, BackendType.String, JvmName.MethodType.toTpe, JvmName.MethodType.toTpe, JvmName.MethodHandle.toTpe, JvmName.MethodType.toTpe)(JvmName.CallSite.toTpe))
  }

  object LinkedList {

    val AddFirstMethod: InstanceMethod =
      InstanceMethod(JvmName.LinkedList, "addFirst", mkDescriptor(BackendType.Object)(VoidableType.Void))

    val IteratorMethod: InstanceMethod =
      InstanceMethod(JvmName.LinkedList, "iterator", mkDescriptor()(JvmName.Iterator.toTpe))

  }

  object Object {

    val Constructor: ConstructorMethod = ConstructorMethod(JvmName.Object, Nil)

    val EqualsMethod: InstanceMethod =
      InstanceMethod(JvmName.Object, "equals", mkDescriptor(BackendType.Object)(BackendType.Bool))

    val ToStringMethod: InstanceMethod =
      InstanceMethod(JvmName.Object, "toString", mkDescriptor()(BackendType.String))

  }

  object ReentrantLock {

    val Constructor: ConstructorMethod = ConstructorMethod(JvmName.ReentrantLock, Nil)

    val UnlockMethod: InstanceMethod = InstanceMethod(JvmName.ReentrantLock, "unlock", MethodDescriptor.NothingToVoid)

    val LockInterruptiblyMethod: InstanceMethod =
      InstanceMethod(JvmName.ReentrantLock, "lockInterruptibly", MethodDescriptor.NothingToVoid)

  }

  object Regex {
    val CompileMethod: StaticMethod =
      StaticMethod(JvmName.Regex, "compile", mkDescriptor(BackendType.String)(JvmName.Regex.toTpe))
  }

  object Runnable {
    val RunMethod: InterfaceMethod = InterfaceMethod(JvmName.Runnable, "run", MethodDescriptor.NothingToVoid)
  }

  object String {
    val Concat: InstanceMethod =
      InstanceMethod(JvmName.String, "concat", mkDescriptor(BackendType.String)(BackendType.String))
  }

  object StringBuilder {

    val Constructor: ConstructorMethod = ConstructorMethod(JvmName.StringBuilder, Nil)

    val AppendStringMethod: InstanceMethod =
      InstanceMethod(JvmName.StringBuilder, "append", mkDescriptor(BackendType.String)(JvmName.StringBuilder.toTpe))

    val AppendInt32Method: InstanceMethod =
      InstanceMethod(JvmName.StringBuilder, "append", mkDescriptor(BackendType.Int32)(JvmName.StringBuilder.toTpe))

  }

  object Thread {

    val CurrentThreadMethod: StaticMethod =
      StaticMethod(JvmName.Thread, "currentThread", mkDescriptor()(JvmName.Thread.toTpe))

    val InterruptMethod: InstanceMethod =
      InstanceMethod(JvmName.Thread, "interrupt", MethodDescriptor.NothingToVoid)

    val JoinMethod: InstanceMethod =
      InstanceMethod(JvmName.Thread, "join", MethodDescriptor.NothingToVoid)

    val OfVirtualMethod: StaticMethod =
      StaticMethod(JvmName.Thread, "ofVirtual", mkDescriptor()(JvmName.Thread$Builder$OfVirtual.toTpe))

    val SetUncaughtExceptionHandlerMethod: InstanceMethod =
      InstanceMethod(JvmName.Thread, "setUncaughtExceptionHandler", mkDescriptor(JvmName.Thread$UncaughtExceptionHandler.toTpe)(VoidableType.Void))

    val StartMethod: InstanceMethod =
      InstanceMethod(JvmName.Thread, "start", MethodDescriptor.NothingToVoid)

    val StartVirtualThreadMethod: StaticMethod =
      ClassMaker.StaticMethod(JvmName.Thread, "startVirtualThread", MethodDescriptor.mkDescriptor(JvmName.Runnable.toTpe)(JvmName.Thread.toTpe))

  }

  object ThreadBuilderOfVirtual {
    val UnstartedMethod: InterfaceMethod =
      InterfaceMethod(JvmName.Thread$Builder$OfVirtual, "unstarted", mkDescriptor(JvmName.Runnable.toTpe)(JvmName.Thread.toTpe))
  }

  object ThreadUncaughtExceptionHandler {
    val UncaughtExceptionMethod: InstanceMethod =
      InstanceMethod(JvmName.Thread$UncaughtExceptionHandler, "uncaughtException", mkDescriptor(JvmName.Thread.toTpe, JvmName.Throwable.toTpe)(VoidableType.Void))
  }

}
