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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.IsFinal
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, mkClass}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{DevFlixRuntime, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.{ClassConstants, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/** The empty [[GenResumption]]: rewinding it just yields the value it is given. */
object GenResumptionNil {

  val desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("ResumptionNil"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = mkClass(this.desc, IsFinal, interfaces = List(GenResumption.desc))

    cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
    cm.mkMethod(Nil, GenResumption.RewindMethod.implementation(this.desc), IsPublic, IsFinal, rewindIns(_))

    cm.closeClassMaker()
  }

  def Constructor: ConstructorMethod = ConstructorMethod(this.desc, Nil)

  private def rewindIns(implicit mv: MethodVisitor): Unit = {
    withName(1, GenValue.desc) { v =>
      v.load()
      xReturn(v.tpe)
    }
  }

}
