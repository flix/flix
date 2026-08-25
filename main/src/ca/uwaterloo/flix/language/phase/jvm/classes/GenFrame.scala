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

/** Frame is really just java.util.Function<Value, Result> * */
object GenFrame {

  val desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("Frame"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = mkInterface(this.desc)

    cm.mkInterfaceMethod(ApplyMethod)
    cm.mkStaticInterfaceMethod(StaticApplyMethod, IsPublic, NotFinal, staticApplyIns(_))

    cm.closeClassMaker()
  }

  def ApplyMethod: InterfaceMethod = InterfaceMethod(this.desc, "applyFrame", mkDescriptor(GenValue.desc)(GenResult.desc))

  def StaticApplyMethod: StaticInterfaceMethod = StaticInterfaceMethod(
    this.desc,
    "applyFrameStatic",
    mkDescriptor(GenFrame.desc, GenValue.desc)(GenResult.desc)
  )

  private def staticApplyIns(implicit mv: MethodVisitor): Unit = {
    withName(0, GenFrame.desc) { fun =>
      withName(1, GenValue.desc) { resumeArg =>
        fun.load()
        resumeArg.load()
        INVOKEINTERFACE(GenFrame.ApplyMethod)
        ARETURN()
      }
    }
  }

}
