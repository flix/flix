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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.NotVolatile
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceField, mkClass}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{DevFlixRuntime, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.{ClassConstants, JavaClasses, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/**
  * A non-empty [[GenResumption]]: one handler's worth of the continuation, and the
  * resumption to continue with once it is done.
  */
object GenResumptionCons {

  val desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("ResumptionCons"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = mkClass(this.desc, IsFinal, interfaces = List(GenResumption.desc))

    cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))

    cm.mkField(SymField, IsPublic, NotFinal, NotVolatile)
    cm.mkField(HandlerField, IsPublic, NotFinal, NotVolatile)
    cm.mkField(FramesField, IsPublic, NotFinal, NotVolatile)
    cm.mkField(TailField, IsPublic, NotFinal, NotVolatile)

    cm.mkMethod(Nil, GenResumption.RewindMethod.implementation(this.desc), IsPublic, IsFinal, rewindIns(_))

    cm.closeClassMaker()
  }

  def Constructor: ConstructorMethod = ConstructorMethod(this.desc, Nil)

  def SymField: InstanceField = InstanceField(this.desc, "sym", JavaClasses.String)

  def HandlerField: InstanceField = InstanceField(this.desc, "handler", GenHandler.desc)

  def FramesField: InstanceField = InstanceField(this.desc, "frames", GenFrames.desc)

  def TailField: InstanceField = InstanceField(this.desc, "tail", GenResumption.desc)

  private def rewindIns(implicit mv: MethodVisitor): Unit = {
    withName(1, GenValue.desc) { v =>
      thisLoad()
      GETFIELD(SymField)
      thisLoad()
      GETFIELD(HandlerField)
      thisLoad()
      GETFIELD(FramesField)
      // () -> tail.rewind(v)
      thisLoad()
      GETFIELD(TailField)
      v.load()
      mkStaticLambda(GenThunk.InvokeMethod, GenResumption.StaticRewindMethod, drop = 0)
      mkStaticLambda(GenThunk.InvokeMethod, GenHandler.InstallHandlerMethod, drop = 0)
      xReturn(GenThunk.desc)
    }
  }

}
