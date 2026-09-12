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

import java.lang.constant.ClassDesc

/**
  * The `EffectCall` interface: an effect operation applied to a handler and a resumption.
  */
object GenEffectCall {

  /** The JVM class descriptor for the generated `EffectCall` class. */
  val Desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("EffectCall"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = mkInterface(this.Desc)
    cm.mkInterfaceMethod(ApplyMethod)
    cm.closeClassMaker()
  }

  def ApplyMethod: InterfaceMethod = InterfaceMethod(this.Desc, "apply", mkDescriptor(GenHandler.Desc, GenResumption.Desc)(GenResult.Desc))

}
