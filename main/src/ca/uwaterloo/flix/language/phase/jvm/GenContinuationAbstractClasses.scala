/*
 * Copyright 2017 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.{NothingToVoid, mkDescriptor}

/**
  * Generates bytecode for the continuation classes.
  */
object GenContinuationAbstractClasses {

  val ResultFieldName: String = "result"
  val InvokeMethodName: String = "invoke"
  val UnwindMethodName: String = "unwind"

  /**
    * Returns the set of continuation classes for the given set of types `ts`.
    */
  def gen(ts: Iterable[BackendObjType.Arrow])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    ts.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, arrowType) =>
        macc + (arrowType.continuation -> JvmClass(arrowType.continuation, genByteCode(arrowType)))
    }
  }

  /**
    * Returns the bytecode for the given continuation class.
    */
  private def genByteCode(arrowType: BackendObjType.Arrow)(implicit root: Root, flix: Flix): Array[Byte] = {

    // Pseudo code to generate:
    //
    // public abstract class Cont$Bool implements java.lang.Runnable {
    //   public "abstract" boolean result;
    //   public Cont$Bool() { ... }
    //   public abstract Cont$Bool invoke();
    //   public final boolean unwind();
    //   public final boolean run();
    // }
    //

    val cm = ClassMaker.mkAbstractClass(arrowType.continuation, interfaces = List(JvmName.Runnable))
    cm.mkObjectConstructor(IsPublic)
    // essentially an abstract field
    cm.mkField(ResultFieldName, arrowType.result, IsPublic, NotFinal)
    cm.mkAbstractMethod(InvokeMethodName, mkDescriptor()(arrowType.continuation.toObjTpe.toTpe))
    cm.mkMethod(genUnwindMethod(arrowType), UnwindMethodName, mkDescriptor()(arrowType.result), IsPublic, IsFinal)
    cm.mkMethod(genRunMethod(arrowType), "run", NothingToVoid, IsPublic, IsFinal)

    cm.closeClassMaker
  }

  private def genUnwindMethod(arrowType: BackendObjType.Arrow): InstructionSet =
    loadThis() ~ storeWithName(1, arrowType.continuation.toObjTpe.toTpe) { currentCont =>
      pushNull() ~ storeWithName(2, arrowType.continuation.toObjTpe.toTpe) { previousCont =>
        doWhile(Condition.NonNull) {
          currentCont.load() ~
            previousCont.store() ~
            currentCont.load() ~
            INVOKEVIRTUAL(arrowType.continuation, InvokeMethodName, mkDescriptor()(arrowType.continuation.toObjTpe.toTpe)) ~
            DUP() ~
            currentCont.store()
        } ~
          previousCont.load() ~
          GETFIELD(arrowType.continuation, ResultFieldName, arrowType.result) ~
          xReturn(arrowType.result)
      }
    }

  private def genRunMethod(arrowType: BackendObjType.Arrow): InstructionSet =
    loadThis() ~
      INVOKEVIRTUAL(arrowType.continuation, UnwindMethodName, mkDescriptor()(arrowType.result)) ~
      xPop(arrowType.result) ~
      RETURN()
}
