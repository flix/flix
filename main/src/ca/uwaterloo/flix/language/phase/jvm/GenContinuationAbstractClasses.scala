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

  /**
    * Returns the set of continuation classes for the given set of types `ts`.
    */
  def gen(conts: Iterable[BackendObjType.Continuation])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    conts.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, contType) =>
        macc + (contType.jvmName -> JvmClass(contType.jvmName, genByteCode(contType)))
    }
  }

  /**
    * Returns the bytecode for the given continuation class.
    */
  private def genByteCode(contType: BackendObjType.Continuation)(implicit root: Root, flix: Flix): Array[Byte] = {

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

    val cm = ClassMaker.mkAbstractClass(contType.jvmName, interfaces = List(JvmName.Runnable))
    cm.mkObjectConstructor(IsPublic)
    // essentially an abstract field
    cm.mkField(contType.ResultField)
    contType.InvokeMethod.mkAbstractMethod(cm)
    contType.UnwindMethod.mkMethod(cm, genUnwindMethod(contType), IsPublic, IsFinal)
    cm.mkMethod(genRunMethod(contType), "run", NothingToVoid, IsPublic, IsFinal)

    cm.closeClassMaker()
  }

  private def genUnwindMethod(contType: BackendObjType.Continuation): InstructionSet =
    thisLoad() ~ storeWithName(1, contType.toTpe) { currentCont =>
      pushNull() ~ storeWithName(2, contType.toTpe) { previousCont =>
        doWhile(Condition.NONNULL) {
          currentCont.load() ~
            previousCont.store() ~
            currentCont.load() ~
            contType.InvokeMethod.invoke() ~
            DUP() ~
            currentCont.store()
        } ~
          previousCont.load() ~
          GETFIELD(contType.ResultField) ~
          xReturn(contType.result)
      }
    }

  private def genRunMethod(contType: BackendObjType.Continuation): InstructionSet =
    thisLoad() ~
      contType.UnwindMethod.invoke() ~
      xPop(contType.result) ~
      RETURN()
}
