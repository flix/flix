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
  * The `Suspension` class, a [[GenResult]] holding a computation stopped by an effect
  * operation, together with the frames to resume once the operation is handled.
  */
object GenSuspension {

  val desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("Suspension"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = mkClass(this.desc, IsFinal, interfaces = List(GenResult.desc))

    cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
    cm.mkField(EffSymField, IsPublic, NotFinal, NotVolatile)
    cm.mkField(EffOpField, IsPublic, NotFinal, NotVolatile)
    cm.mkField(PrefixField, IsPublic, NotFinal, NotVolatile)
    cm.mkField(ResumptionField, IsPublic, NotFinal, NotVolatile)

    cm.closeClassMaker()
  }

  def Constructor: ConstructorMethod = ConstructorMethod(this.desc, Nil)

  def EffSymField: InstanceField = InstanceField(this.desc, "effSym", JavaClasses.String)

  def EffOpField: InstanceField = InstanceField(this.desc, "effOp", GenEffectCall.desc)

  def PrefixField: InstanceField = InstanceField(this.desc, "prefix", GenFrames.desc)

  def ResumptionField: InstanceField = InstanceField(this.desc, "resumption", GenResumption.desc)

}
