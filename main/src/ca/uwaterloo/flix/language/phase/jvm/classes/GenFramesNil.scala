/*
 * Copyright 2021 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.jvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.IsFinal
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceMethod, mkClass}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{DevFlixRuntime, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.{ClassConstants, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/** The empty [[GenFrames]] stack. */
object GenFramesNil {

  /** The JVM class descriptor for the generated `FramesNil` class. */
  val Desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("FramesNil"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = mkClass(this.Desc, IsFinal, interfaces = List(GenFrames.Desc))

    cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
    cm.mkMethod(Nil, PushMethod, IsPublic, IsFinal, GenFrames.pushImplementation(_))
    cm.mkMethod(Nil, GenFrames.ReverseOntoMethod.implementation(this.Desc), IsPublic, IsFinal, reverseOntoIns(_))

    cm.closeClassMaker()
  }

  def Constructor: ConstructorMethod = ConstructorMethod(this.Desc, Nil)

  def PushMethod: InstanceMethod = GenFrames.PushMethod.implementation(this.Desc)

  private def reverseOntoIns(implicit mv: MethodVisitor): Unit = {
    withName(1, GenFrames.Desc) { rest =>
      rest.load()
      xReturn(rest.tpe)
    }
  }

}
