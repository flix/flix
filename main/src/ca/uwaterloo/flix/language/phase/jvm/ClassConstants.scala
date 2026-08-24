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
import ca.uwaterloo.flix.language.phase.jvm.MethodDescriptor.mkDescriptor
import org.objectweb.asm.MethodVisitor

object ClassConstants {

  // Flix Constants.

  object FlixError {

    val Constructor: ConstructorMethod = ConstructorMethod(JvmName.FlixError.toClassDesc, List(BackendType.String))

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkAbstractClass(JvmName.FlixError.toClassDesc, JvmName.Error.toClassDesc)
      cm.mkConstructor(Constructor, IsPublic, constructorIns(_))
      cm.closeClassMaker()
    }

    private def constructorIns(implicit mv: MethodVisitor): Unit = {
      import BytecodeInstructions.*
      thisLoad()
      ALOAD(1)
      invokeConstructor(JvmName.Error.toClassDesc, mkDescriptor(BackendType.String)(VoidableType.Void))
      RETURN()
    }

  }

  // Java Constants.

  object BigDecimal {
    val Constructor: ConstructorMethod = ClassMaker.ConstructorMethod(JvmName.BigDecimal.toClassDesc, List(BackendType.String))
  }

  object BigInteger {
    val Constructor: ConstructorMethod = ClassMaker.ConstructorMethod(JvmName.BigInteger.toClassDesc, List(BackendType.String))
  }

  object ConcurrentLinkedQueue {

    val AddMethod: InstanceMethod =
      InstanceMethod(JvmName.ConcurrentLinkedQueue.toClassDesc, "add", mkDescriptor(BackendType.Object)(BackendType.Bool))

    val PollMethod: InstanceMethod =
      InstanceMethod(JvmName.ConcurrentLinkedQueue.toClassDesc, "poll", mkDescriptor()(BackendType.Object))

  }

  object Iterator {

    val HasNextMethod: InterfaceMethod =
      InterfaceMethod(JvmName.Iterator.toClassDesc, "hasNext", mkDescriptor()(BackendType.Bool))

    val NextMethod: InterfaceMethod =
      InterfaceMethod(JvmName.Iterator.toClassDesc, "next", mkDescriptor()(BackendType.Object))

  }

  object LambdaMetafactory {
    val MetafactoryMethod: StaticMethod =
      StaticMethod(JvmName.LambdaMetafactory.toClassDesc, "metafactory", mkDescriptor(JvmName.MethodHandles$Lookup.toTpe, BackendType.String, JvmName.MethodType.toTpe, JvmName.MethodType.toTpe, JvmName.MethodHandle.toTpe, JvmName.MethodType.toTpe)(JvmName.CallSite.toTpe))
  }

  object LinkedList {

    val AddFirstMethod: InstanceMethod =
      InstanceMethod(JvmName.LinkedList.toClassDesc, "addFirst", mkDescriptor(BackendType.Object)(VoidableType.Void))

    val IteratorMethod: InstanceMethod =
      InstanceMethod(JvmName.LinkedList.toClassDesc, "iterator", mkDescriptor()(JvmName.Iterator.toTpe))

  }

  object Object {

    val Constructor: ConstructorMethod = ConstructorMethod(JvmName.Object.toClassDesc, Nil)

    val EqualsMethod: InstanceMethod =
      InstanceMethod(JvmName.Object.toClassDesc, "equals", mkDescriptor(BackendType.Object)(BackendType.Bool))

    val ToStringMethod: InstanceMethod =
      InstanceMethod(JvmName.Object.toClassDesc, "toString", mkDescriptor()(BackendType.String))

  }

  object ReentrantLock {

    val Constructor: ConstructorMethod = ConstructorMethod(JvmName.ReentrantLock.toClassDesc, Nil)

    val UnlockMethod: InstanceMethod = InstanceMethod(JvmName.ReentrantLock.toClassDesc, "unlock", MethodDescriptor.NothingToVoid)

    val LockInterruptiblyMethod: InstanceMethod =
      InstanceMethod(JvmName.ReentrantLock.toClassDesc, "lockInterruptibly", MethodDescriptor.NothingToVoid)

  }

  object Regex {
    val CompileMethod: StaticMethod =
      StaticMethod(JvmName.Regex.toClassDesc, "compile", mkDescriptor(BackendType.String)(JvmName.Regex.toTpe))
  }

  object Runnable {
    val RunMethod: InterfaceMethod = InterfaceMethod(JvmName.Runnable.toClassDesc, "run", MethodDescriptor.NothingToVoid)
  }

  object StringBuilder {

    val Constructor: ConstructorMethod = ConstructorMethod(JvmName.StringBuilder.toClassDesc, Nil)

    val AppendStringMethod: InstanceMethod =
      InstanceMethod(JvmName.StringBuilder.toClassDesc, "append", mkDescriptor(BackendType.String)(JvmName.StringBuilder.toTpe))

    val AppendInt32Method: InstanceMethod =
      InstanceMethod(JvmName.StringBuilder.toClassDesc, "append", mkDescriptor(BackendType.Int32)(JvmName.StringBuilder.toTpe))

  }

  object Thread {

    val CurrentThreadMethod: StaticMethod =
      StaticMethod(JvmName.Thread.toClassDesc, "currentThread", mkDescriptor()(JvmName.Thread.toTpe))

    val InterruptMethod: InstanceMethod =
      InstanceMethod(JvmName.Thread.toClassDesc, "interrupt", MethodDescriptor.NothingToVoid)

    val JoinMethod: InstanceMethod =
      InstanceMethod(JvmName.Thread.toClassDesc, "join", MethodDescriptor.NothingToVoid)

    val OfVirtualMethod: StaticMethod =
      StaticMethod(JvmName.Thread.toClassDesc, "ofVirtual", mkDescriptor()(JvmName.Thread$Builder$OfVirtual.toTpe))

    val SetUncaughtExceptionHandlerMethod: InstanceMethod =
      InstanceMethod(JvmName.Thread.toClassDesc, "setUncaughtExceptionHandler", mkDescriptor(JvmName.Thread$UncaughtExceptionHandler.toTpe)(VoidableType.Void))

    val StartMethod: InstanceMethod =
      InstanceMethod(JvmName.Thread.toClassDesc, "start", MethodDescriptor.NothingToVoid)

    val StartVirtualThreadMethod: StaticMethod =
      ClassMaker.StaticMethod(JvmName.Thread.toClassDesc, "startVirtualThread", MethodDescriptor.mkDescriptor(JvmName.Runnable.toTpe)(JvmName.Thread.toTpe))

  }

  object ThreadBuilderOfVirtual {
    val UnstartedMethod: InterfaceMethod =
      InterfaceMethod(JvmName.Thread$Builder$OfVirtual.toClassDesc, "unstarted", mkDescriptor(JvmName.Runnable.toTpe)(JvmName.Thread.toTpe))
  }

  object ThreadUncaughtExceptionHandler {
    val UncaughtExceptionMethod: InstanceMethod =
      InstanceMethod(JvmName.Thread$UncaughtExceptionHandler.toClassDesc, "uncaughtException", mkDescriptor(JvmName.Thread.toTpe, JvmName.Throwable.toTpe)(VoidableType.Void))
  }

}
