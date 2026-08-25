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

import java.lang.constant.ClassDesc

/**
  * The `EffectCall` interface: an effect operation applied to a handler and a resumption.
  */
object GenEffectCall {

  val desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("EffectCall"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = mkInterface(this.desc)
    cm.mkInterfaceMethod(ApplyMethod)
    cm.closeClassMaker()
  }

  def ApplyMethod: InterfaceMethod = InterfaceMethod(this.desc, "apply", mkDescriptor(GenHandler.desc, GenResumption.desc)(GenResult.desc))

}
