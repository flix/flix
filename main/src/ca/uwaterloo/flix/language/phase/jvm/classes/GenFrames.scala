/*
 * Copyright 2021 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
