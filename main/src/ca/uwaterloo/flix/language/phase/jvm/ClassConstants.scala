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
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.MethodTypeDescs.{mkDescriptor, mkVoidDescriptor}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc
import java.lang.constant.ConstantDescs.{CD_boolean, CD_int}

object ClassConstants {

  // Flix Constants.

  object FlixError {

    val Desc: ClassDesc = Mangle.mkDesc(Mangle.DevFlixRuntime, Mangle.mkClassName("FlixError"))

    val Constructor: ConstructorMethod = ConstructorMethod(Desc, List(JavaClasses.String))

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkAbstractClass(Desc, JavaClasses.Error)
      cm.mkConstructor(Constructor, IsPublic, constructorIns(_))
      cm.closeClassMaker()
    }

    private def constructorIns(implicit mv: MethodVisitor): Unit = {
      thisLoad()
      ALOAD(1)
      invokeConstructor(JavaClasses.Error, mkVoidDescriptor(JavaClasses.String))
      RETURN()
    }

  }

  // Java Constants.

  object BigDecimal {
    val Constructor: ConstructorMethod = ClassMaker.ConstructorMethod(JavaClasses.BigDecimal, List(JavaClasses.String))
  }

  object BigInteger {
    val Constructor: ConstructorMethod = ClassMaker.ConstructorMethod(JavaClasses.BigInteger, List(JavaClasses.String))
  }

  object ConcurrentLinkedQueue {

    val AddMethod: InstanceMethod =
      InstanceMethod(JavaClasses.ConcurrentLinkedQueue, "add", mkDescriptor(JavaClasses.Object)(CD_boolean))

    val PollMethod: InstanceMethod =
      InstanceMethod(JavaClasses.ConcurrentLinkedQueue, "poll", mkDescriptor()(JavaClasses.Object))

  }

  object Iterator {

    val HasNextMethod: InterfaceMethod =
      InterfaceMethod(JavaClasses.Iterator, "hasNext", mkDescriptor()(CD_boolean))

    val NextMethod: InterfaceMethod =
      InterfaceMethod(JavaClasses.Iterator, "next", mkDescriptor()(JavaClasses.Object))

  }

  object LambdaMetafactory {
    val MetafactoryMethod: StaticMethod =
      StaticMethod(JavaClasses.LambdaMetafactory, "metafactory", mkDescriptor(JavaClasses.MethodHandles$Lookup, JavaClasses.String, JavaClasses.MethodType, JavaClasses.MethodType, JavaClasses.MethodHandle, JavaClasses.MethodType)(JavaClasses.CallSite))
  }

  object LinkedList {

    val AddFirstMethod: InstanceMethod =
      InstanceMethod(JavaClasses.LinkedList, "addFirst", mkVoidDescriptor(JavaClasses.Object))

    val IteratorMethod: InstanceMethod =
      InstanceMethod(JavaClasses.LinkedList, "iterator", mkDescriptor()(JavaClasses.Iterator))

  }

  object Object {

    val Constructor: ConstructorMethod = ConstructorMethod(JavaClasses.Object, Nil)

    val EqualsMethod: InstanceMethod =
      InstanceMethod(JavaClasses.Object, "equals", mkDescriptor(JavaClasses.Object)(CD_boolean))

    val ToStringMethod: InstanceMethod =
      InstanceMethod(JavaClasses.Object, "toString", mkDescriptor()(JavaClasses.String))

  }

  object ReentrantLock {

    val Constructor: ConstructorMethod = ConstructorMethod(JavaClasses.ReentrantLock, Nil)

    val UnlockMethod: InstanceMethod = InstanceMethod(JavaClasses.ReentrantLock, "unlock", MethodTypeDescs.NothingToVoid)

    val LockInterruptiblyMethod: InstanceMethod =
      InstanceMethod(JavaClasses.ReentrantLock, "lockInterruptibly", MethodTypeDescs.NothingToVoid)

  }

  object Regex {
    val CompileMethod: StaticMethod =
      StaticMethod(JavaClasses.Regex, "compile", mkDescriptor(JavaClasses.String)(JavaClasses.Regex))
  }

  object Runnable {
    val RunMethod: InterfaceMethod = InterfaceMethod(JavaClasses.Runnable, "run", MethodTypeDescs.NothingToVoid)
  }

  object StringBuilder {

    val Constructor: ConstructorMethod = ConstructorMethod(JavaClasses.StringBuilder, Nil)

    val AppendStringMethod: InstanceMethod =
      InstanceMethod(JavaClasses.StringBuilder, "append", mkDescriptor(JavaClasses.String)(JavaClasses.StringBuilder))

    val AppendInt32Method: InstanceMethod =
      InstanceMethod(JavaClasses.StringBuilder, "append", mkDescriptor(CD_int)(JavaClasses.StringBuilder))

  }

  object Thread {

    val CurrentThreadMethod: StaticMethod =
      StaticMethod(JavaClasses.Thread, "currentThread", mkDescriptor()(JavaClasses.Thread))

    val InterruptMethod: InstanceMethod =
      InstanceMethod(JavaClasses.Thread, "interrupt", MethodTypeDescs.NothingToVoid)

    val JoinMethod: InstanceMethod =
      InstanceMethod(JavaClasses.Thread, "join", MethodTypeDescs.NothingToVoid)

    val OfVirtualMethod: StaticMethod =
      StaticMethod(JavaClasses.Thread, "ofVirtual", mkDescriptor()(JavaClasses.Thread$Builder$OfVirtual))

    val SetUncaughtExceptionHandlerMethod: InstanceMethod =
      InstanceMethod(JavaClasses.Thread, "setUncaughtExceptionHandler", mkVoidDescriptor(JavaClasses.Thread$UncaughtExceptionHandler))

    val StartMethod: InstanceMethod =
      InstanceMethod(JavaClasses.Thread, "start", MethodTypeDescs.NothingToVoid)

    val StartVirtualThreadMethod: StaticMethod =
      ClassMaker.StaticMethod(JavaClasses.Thread, "startVirtualThread", mkDescriptor(JavaClasses.Runnable)(JavaClasses.Thread))

  }

  object ThreadBuilderOfVirtual {
    val UnstartedMethod: InterfaceMethod =
      InterfaceMethod(JavaClasses.Thread$Builder$OfVirtual, "unstarted", mkDescriptor(JavaClasses.Runnable)(JavaClasses.Thread))
  }

  object ThreadUncaughtExceptionHandler {
    val UncaughtExceptionMethod: InstanceMethod =
      InstanceMethod(JavaClasses.Thread$UncaughtExceptionHandler, "uncaughtException", mkVoidDescriptor(JavaClasses.Thread, JavaClasses.Throwable))
  }

}
