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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{InterfaceMethod, mkInterface}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{DevFlixRuntime, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.MethodTypeDescs.mkDescriptor
import ca.uwaterloo.flix.language.phase.jvm.Mangle
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/**
  * The `Frames` interface, a stack of [[GenFrame]]s, implemented by [[GenFramesCons]] and
  * [[GenFramesNil]].
  */
object GenFrames {

  /** The JVM class descriptor for the generated `Frames` class. */
  val Desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("Frames"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = mkInterface(this.Desc)

    cm.mkInterfaceMethod(PushMethod)
    cm.mkInterfaceMethod(ReverseOntoMethod)

    cm.closeClassMaker()
  }

  def PushMethod: InterfaceMethod = InterfaceMethod(this.Desc, "push", mkDescriptor(GenFrame.Desc)(GenFrames.Desc))

  def ReverseOntoMethod: InterfaceMethod = InterfaceMethod(this.Desc, "reverseOnto", mkDescriptor(GenFrames.Desc)(GenFrames.Desc))

  def pushImplementation(implicit mv: MethodVisitor): Unit = {
    withName(1, GenFrame.Desc) { frame =>
      NEW(GenFramesCons.Desc)
      DUP()
      INVOKESPECIAL(GenFramesCons.Constructor)
      DUP()
      frame.load()
      PUTFIELD(GenFramesCons.HeadField)
      DUP()
      thisLoad()
      PUTFIELD(GenFramesCons.TailField)
      xReturn(GenFramesCons.Desc)
    }
  }

}
