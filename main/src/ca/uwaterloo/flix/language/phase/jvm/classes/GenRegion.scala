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

package ca.uwaterloo.flix.language.phase.jvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.{IsPrivate, IsPublic}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.{IsVolatile, NotVolatile}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceField, InstanceMethod, mkClass}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{DevFlixRuntime, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.MethodTypeDescs.mkVoidDescriptor
import ca.uwaterloo.flix.language.phase.jvm.{ClassConstants, JavaClasses, Mangle, MethodTypeDescs}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/**
  * The `Region` class, which tracks a region's spawned threads and its exit handlers, and
  * relays exceptions from child threads back to the thread that opened the region.
  */
object GenRegion {

  val desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("Region"))


  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = mkClass(this.desc, IsFinal)

    cm.mkField(ThreadsField, IsPrivate, IsFinal, NotVolatile)
    cm.mkField(RegionThreadField, IsPrivate, IsFinal, NotVolatile)
    cm.mkField(ChildExceptionField, IsPrivate, NotFinal, IsVolatile)
    cm.mkField(OnExitField, IsPrivate, IsFinal, NotVolatile)

    cm.mkConstructor(Constructor, IsPublic, constructorIns(_))

    cm.mkMethod(Nil, SpawnMethod, IsPublic, IsFinal, spawnIns(_))
    cm.mkMethod(Nil, ExitMethod, IsPublic, IsFinal, exitIns(_))
    cm.mkMethod(Nil, ReportChildExceptionMethod, IsPublic, IsFinal, reportChildExceptionIns(_))
    cm.mkMethod(Nil, ReThrowChildExceptionMethod, IsPublic, IsFinal, reThrowChildExceptionIns(_))
    cm.mkMethod(Nil, RunOnExitMethod, IsPublic, IsFinal, runOnExitIns(_))

    cm.closeClassMaker()
  }

  // private final ConcurrentLinkedQueue<Thread> threads = new ConcurrentLinkedQueue<Thread>();
  private def ThreadsField: InstanceField = InstanceField(this.desc, "threads", JavaClasses.ConcurrentLinkedQueue)

  // private final LinkedList<Runnable> onExit = new LinkedList<Runnable>();
  private def OnExitField: InstanceField = InstanceField(this.desc, "onExit", JavaClasses.LinkedList)

  // private final Thread regionThread = Thread.currentThread();
  private def RegionThreadField: InstanceField = InstanceField(this.desc, "regionThread", JavaClasses.Thread)

  // private volatile Throwable childException = null;
  private def ChildExceptionField: InstanceField = InstanceField(this.desc, "childException", JavaClasses.Throwable)

  def Constructor: ConstructorMethod = ConstructorMethod(this.desc, Nil)

  private def constructorIns(implicit mv: MethodVisitor): Unit = {
    thisLoad()
    INVOKESPECIAL(ClassConstants.Object.Constructor)
    thisLoad()
    NEW(JavaClasses.ConcurrentLinkedQueue)
    DUP()
    invokeConstructor(JavaClasses.ConcurrentLinkedQueue, MethodTypeDescs.NothingToVoid)
    PUTFIELD(ThreadsField)
    thisLoad()
    INVOKESTATIC(ClassConstants.Thread.CurrentThreadMethod)
    PUTFIELD(RegionThreadField)
    thisLoad()
    ACONST_NULL()
    PUTFIELD(ChildExceptionField)
    thisLoad()
    NEW(JavaClasses.LinkedList)
    DUP()
    invokeConstructor(JavaClasses.LinkedList, MethodTypeDescs.NothingToVoid)
    PUTFIELD(OnExitField)
    RETURN()
  }

  // final public void spawn(Runnable r) {
  //   Thread t = new Thread(r);
  //   t.setUncaughtExceptionHandler(new UncaughtExceptionHandler(this));
  //   t.start();
  //   threads.add(t);
  // }
  def SpawnMethod: InstanceMethod = InstanceMethod(this.desc, "spawn", mkVoidDescriptor(JavaClasses.Runnable))

  private def spawnIns(implicit mv: MethodVisitor): Unit = {
    INVOKESTATIC(ClassConstants.Thread.OfVirtualMethod)
    ALOAD(1)
    INVOKEINTERFACE(ClassConstants.ThreadBuilderOfVirtual.UnstartedMethod)
    storeWithName(2, JavaClasses.Thread) { thread =>
      thread.load()
      NEW(GenUncaughtExceptionHandler.desc)
      DUP()
      thisLoad()
      invokeConstructor(GenUncaughtExceptionHandler.desc, mkVoidDescriptor(GenRegion.desc))
      INVOKEVIRTUAL(ClassConstants.Thread.SetUncaughtExceptionHandlerMethod)
      thread.load()
      INVOKEVIRTUAL(ClassConstants.Thread.StartMethod)
      thisLoad()
      GETFIELD(ThreadsField)
      thread.load()
      INVOKEVIRTUAL(ClassConstants.ConcurrentLinkedQueue.AddMethod)
      POP()
      RETURN()
    }
  }

  // final public void exit() throws InterruptedException {
  //   Thread t;
  //   while ((t = threads.poll()) != null)
  //     t.join();
  //   for (Runnable r: onExit)
  //     r.run();
  // }
  def ExitMethod: InstanceMethod = InstanceMethod(this.desc, "exit", MethodTypeDescs.NothingToVoid)

  private def exitIns(implicit mv: MethodVisitor): Unit = {
    withName(1, JavaClasses.Thread) { t =>
      whileLoop(Condition.NONNULL) {
        thisLoad()
        GETFIELD(ThreadsField)
        INVOKEVIRTUAL(ClassConstants.ConcurrentLinkedQueue.PollMethod)
        CHECKCAST(JavaClasses.Thread)
        DUP()
        t.store()
      } {
        t.load()
        INVOKEVIRTUAL(ClassConstants.Thread.JoinMethod)
      }
      withName(2, JavaClasses.Iterator) { i =>
        thisLoad()
        GETFIELD(OnExitField)
        INVOKEVIRTUAL(ClassConstants.LinkedList.IteratorMethod)
        i.store()
        whileLoop(Condition.NE) {
          i.load()
          INVOKEINTERFACE(ClassConstants.Iterator.HasNextMethod)
        } {
          i.load()
          INVOKEINTERFACE(ClassConstants.Iterator.NextMethod)
          CHECKCAST(JavaClasses.Runnable)
          INVOKEINTERFACE(ClassConstants.Runnable.RunMethod)
        }
      }
      RETURN()
    }
  }

  // final public void reportChildException(Throwable e) {
  //   childException = e;
  //   regionThread.interrupt();
  // }
  def ReportChildExceptionMethod: InstanceMethod = InstanceMethod(this.desc, "reportChildException", mkVoidDescriptor(JavaClasses.Throwable))

  private def reportChildExceptionIns(implicit mv: MethodVisitor): Unit = {
    thisLoad()
    ALOAD(1)
    PUTFIELD(ChildExceptionField)
    thisLoad()
    GETFIELD(RegionThreadField)
    INVOKEVIRTUAL(ClassConstants.Thread.InterruptMethod)
    RETURN()
  }

  // final public void reThrowChildException() throws Throwable {
  //   if (childException != null)
  //     throw childException;
  // }
  def ReThrowChildExceptionMethod: InstanceMethod = InstanceMethod(this.desc, "reThrowChildException", MethodTypeDescs.NothingToVoid)

  private def reThrowChildExceptionIns(implicit mv: MethodVisitor): Unit = {
    thisLoad()
    GETFIELD(ChildExceptionField)
    ifCondition(Condition.NONNULL) {
      thisLoad()
      GETFIELD(ChildExceptionField)
      ATHROW()
    }
    RETURN()
  }

  // final public void runOnExit(Runnable r) {
  //   onExit.addFirst(r);
  // }
  private def RunOnExitMethod: InstanceMethod = InstanceMethod(this.desc, "runOnExit", mkVoidDescriptor(JavaClasses.Runnable))

  private def runOnExitIns(implicit mv: MethodVisitor): Unit = {
    thisLoad()
    GETFIELD(OnExitField)
    ALOAD(1)
    INVOKEVIRTUAL(ClassConstants.LinkedList.AddFirstMethod)
    RETURN()
  }

}
