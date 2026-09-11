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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.NotFinal
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{InterfaceMethod, StaticInterfaceMethod, mkInterface}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{DevFlixRuntime, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.MethodTypeDescs.mkDescriptor
import ca.uwaterloo.flix.language.phase.jvm.Mangle
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/**
  * The `Resumption` interface, the continuation of a suspended computation, implemented by
  * [[GenResumptionCons]] and [[GenResumptionNil]].
  */
object GenResumption {

  /** The JVM class descriptor for the generated `Resumption` class. */
  val Desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("Resumption"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = mkInterface(this.Desc)
    cm.mkInterfaceMethod(RewindMethod)
    cm.mkStaticInterfaceMethod(StaticRewindMethod, IsPublic, NotFinal, staticRewindIns(_))
    cm.closeClassMaker()
  }

  def RewindMethod: InterfaceMethod = InterfaceMethod(this.Desc, "rewind", mkDescriptor(GenValue.Desc)(GenResult.Desc))

  def StaticRewindMethod: StaticInterfaceMethod = StaticInterfaceMethod(this.Desc, "staticRewind", mkDescriptor(GenResumption.Desc, GenValue.Desc)(GenResult.Desc))

  private def staticRewindIns(implicit mv: MethodVisitor): Unit = {
    withName(0, GenResumption.Desc) { resumption =>
      withName(1, GenValue.Desc) { v =>
        resumption.load()
        v.load()
        INVOKEINTERFACE(GenResumption.RewindMethod)
        ARETURN()
      }
    }
  }

}
