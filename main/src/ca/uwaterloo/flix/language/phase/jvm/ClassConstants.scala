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
import ca.uwaterloo.flix.language.phase.jvm.BackendType.RichClassDesc
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceMethod, InterfaceMethod, StaticMethod}
import ca.uwaterloo.flix.language.phase.jvm.MethodDescriptor.mkDescriptor
import org.objectweb.asm.MethodVisitor

object ClassConstants {

  // Flix Constants.

  object FlixError {

    val Constructor: ConstructorMethod = ConstructorMethod(JvmName.FlixError.toClassDesc, List(BackendType.String))

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkAbstractClass(JvmName.FlixError.toClassDesc, JavaClasses.Error)
      cm.mkConstructor(Constructor, IsPublic, constructorIns(_))
      cm.closeClassMaker()
    }

    private def constructorIns(implicit mv: MethodVisitor): Unit = {
      import BytecodeInstructions.*
      thisLoad()
      ALOAD(1)
      invokeConstructor(JavaClasses.Error, mkDescriptor(BackendType.String)(VoidableType.Void))
      RETURN()
    }

  }

  // Java Constants.

  object BigDecimal {
    val Constructor: ConstructorMethod = ClassMaker.ConstructorMethod(JavaClasses.BigDecimal, List(BackendType.String))
  }

  object BigInteger {
    val Constructor: ConstructorMethod = ClassMaker.ConstructorMethod(JavaClasses.BigInteger, List(BackendType.String))
  }

  object ConcurrentLinkedQueue {

    val AddMethod: InstanceMethod =
      InstanceMethod(JavaClasses.ConcurrentLinkedQueue, "add", mkDescriptor(BackendType.Object)(BackendType.Bool))

    val PollMethod: InstanceMethod =
      InstanceMethod(JavaClasses.ConcurrentLinkedQueue, "poll", mkDescriptor()(BackendType.Object))

  }

  object Iterator {

    val HasNextMethod: InterfaceMethod =
      InterfaceMethod(JavaClasses.Iterator, "hasNext", mkDescriptor()(BackendType.Bool))

    val NextMethod: InterfaceMethod =
      InterfaceMethod(JavaClasses.Iterator, "next", mkDescriptor()(BackendType.Object))

  }

  object LambdaMetafactory {
    val MetafactoryMethod: StaticMethod =
      StaticMethod(JavaClasses.LambdaMetafactory, "metafactory", mkDescriptor(JavaClasses.MethodHandles$Lookup.toTpe, BackendType.String, JavaClasses.MethodType.toTpe, JavaClasses.MethodType.toTpe, JavaClasses.MethodHandle.toTpe, JavaClasses.MethodType.toTpe)(JavaClasses.CallSite.toTpe))
  }

  object LinkedList {

    val AddFirstMethod: InstanceMethod =
      InstanceMethod(JavaClasses.LinkedList, "addFirst", mkDescriptor(BackendType.Object)(VoidableType.Void))

    val IteratorMethod: InstanceMethod =
      InstanceMethod(JavaClasses.LinkedList, "iterator", mkDescriptor()(JavaClasses.Iterator.toTpe))

  }

  object Object {

    val Constructor: ConstructorMethod = ConstructorMethod(JavaClasses.Object, Nil)

    val EqualsMethod: InstanceMethod =
      InstanceMethod(JavaClasses.Object, "equals", mkDescriptor(BackendType.Object)(BackendType.Bool))

    val ToStringMethod: InstanceMethod =
      InstanceMethod(JavaClasses.Object, "toString", mkDescriptor()(BackendType.String))

  }

  object ReentrantLock {

    val Constructor: ConstructorMethod = ConstructorMethod(JavaClasses.ReentrantLock, Nil)

    val UnlockMethod: InstanceMethod = InstanceMethod(JavaClasses.ReentrantLock, "unlock", MethodDescriptor.NothingToVoid)

    val LockInterruptiblyMethod: InstanceMethod =
      InstanceMethod(JavaClasses.ReentrantLock, "lockInterruptibly", MethodDescriptor.NothingToVoid)

  }

  object Regex {
    val CompileMethod: StaticMethod =
      StaticMethod(JavaClasses.Regex, "compile", mkDescriptor(BackendType.String)(JavaClasses.Regex.toTpe))
  }

  object Runnable {
    val RunMethod: InterfaceMethod = InterfaceMethod(JavaClasses.Runnable, "run", MethodDescriptor.NothingToVoid)
  }

  object StringBuilder {

    val Constructor: ConstructorMethod = ConstructorMethod(JavaClasses.StringBuilder, Nil)

    val AppendStringMethod: InstanceMethod =
      InstanceMethod(JavaClasses.StringBuilder, "append", mkDescriptor(BackendType.String)(JavaClasses.StringBuilder.toTpe))

    val AppendInt32Method: InstanceMethod =
      InstanceMethod(JavaClasses.StringBuilder, "append", mkDescriptor(BackendType.Int32)(JavaClasses.StringBuilder.toTpe))

  }

  object Thread {

    val CurrentThreadMethod: StaticMethod =
      StaticMethod(JavaClasses.Thread, "currentThread", mkDescriptor()(JavaClasses.Thread.toTpe))

    val InterruptMethod: InstanceMethod =
      InstanceMethod(JavaClasses.Thread, "interrupt", MethodDescriptor.NothingToVoid)

    val JoinMethod: InstanceMethod =
      InstanceMethod(JavaClasses.Thread, "join", MethodDescriptor.NothingToVoid)

    val OfVirtualMethod: StaticMethod =
      StaticMethod(JavaClasses.Thread, "ofVirtual", mkDescriptor()(JavaClasses.Thread$Builder$OfVirtual.toTpe))

    val SetUncaughtExceptionHandlerMethod: InstanceMethod =
      InstanceMethod(JavaClasses.Thread, "setUncaughtExceptionHandler", mkDescriptor(JavaClasses.Thread$UncaughtExceptionHandler.toTpe)(VoidableType.Void))

    val StartMethod: InstanceMethod =
      InstanceMethod(JavaClasses.Thread, "start", MethodDescriptor.NothingToVoid)

    val StartVirtualThreadMethod: StaticMethod =
      ClassMaker.StaticMethod(JavaClasses.Thread, "startVirtualThread", MethodDescriptor.mkDescriptor(JavaClasses.Runnable.toTpe)(JavaClasses.Thread.toTpe))

  }

  object ThreadBuilderOfVirtual {
    val UnstartedMethod: InterfaceMethod =
      InterfaceMethod(JavaClasses.Thread$Builder$OfVirtual, "unstarted", mkDescriptor(JavaClasses.Runnable.toTpe)(JavaClasses.Thread.toTpe))
  }

  object ThreadUncaughtExceptionHandler {
    val UncaughtExceptionMethod: InstanceMethod =
      InstanceMethod(JavaClasses.Thread$UncaughtExceptionHandler, "uncaughtException", mkDescriptor(JavaClasses.Thread.toTpe, JavaClasses.Throwable.toTpe)(VoidableType.Void))
  }

}
