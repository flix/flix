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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceField, InstanceMethod, mkClass}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{DevFlixRuntime, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.{ClassConstants, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/** A non-empty [[GenFrames]] stack: a head [[GenFrame]] and the rest of the stack. */
object GenFramesCons {

  /** The JVM class descriptor for the generated `FramesCons` class. */
  val Desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("FramesCons"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = mkClass(this.Desc, IsFinal, interfaces = List(GenFrames.Desc))

    cm.mkField(HeadField, IsPublic, NotFinal, NotVolatile)
    cm.mkField(TailField, IsPublic, NotFinal, NotVolatile)
    cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
    cm.mkMethod(Nil, PushMethod, IsPublic, IsFinal, GenFrames.pushImplementation(_))
    cm.mkMethod(Nil, GenFrames.ReverseOntoMethod.implementation(this.Desc), IsPublic, IsFinal, reverseOntoIns(_))

    cm.closeClassMaker()
  }

  def HeadField: InstanceField = InstanceField(this.Desc, "head", GenFrame.Desc)

  def TailField: InstanceField = InstanceField(this.Desc, "tail", GenFrames.Desc)

  def Constructor: ConstructorMethod = ConstructorMethod(this.Desc, Nil)

  private def PushMethod: InstanceMethod = GenFrames.PushMethod.implementation(this.Desc)

  private def reverseOntoIns(implicit mv: MethodVisitor): Unit = {
    withName(1, GenFrames.Desc) { rest =>
      thisLoad()
      GETFIELD(TailField)
      NEW(GenFramesCons.Desc)
      DUP()
      INVOKESPECIAL(GenFramesCons.Constructor)
      DUP()
      thisLoad()
      GETFIELD(HeadField)
      PUTFIELD(HeadField)
      DUP()
      rest.load()
      PUTFIELD(TailField)
      INVOKEINTERFACE(GenFrames.ReverseOntoMethod)
      xReturn(GenFrames.Desc)
    }
  }

}
