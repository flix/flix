/*
 * Copyright 2021 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.jvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.jvm.JavaClasses
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.NotVolatile
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceField, mkClass}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{DevFlixRuntime, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.{ClassConstants, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/**
  * The `Suspension` class, a [[GenResult]] holding a computation stopped by an effect
  * operation, together with the frames to resume once the operation is handled.
  */
object GenSuspension {

  /** The JVM class descriptor for the generated `Suspension` class. */
  val Desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("Suspension"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = mkClass(this.Desc, IsFinal, interfaces = List(GenResult.Desc))

    cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
    cm.mkField(EffSymField, IsPublic, NotFinal, NotVolatile)
    cm.mkField(EffOpField, IsPublic, NotFinal, NotVolatile)
    cm.mkField(PrefixField, IsPublic, NotFinal, NotVolatile)
    cm.mkField(ResumptionField, IsPublic, NotFinal, NotVolatile)

    cm.closeClassMaker()
  }

  def Constructor: ConstructorMethod = ConstructorMethod(this.Desc, Nil)

  def EffSymField: InstanceField = InstanceField(this.Desc, "effSym", JavaClasses.String)

  def EffOpField: InstanceField = InstanceField(this.Desc, "effOp", GenEffectCall.Desc)

  def PrefixField: InstanceField = InstanceField(this.Desc, "prefix", GenFrames.Desc)

  def ResumptionField: InstanceField = InstanceField(this.Desc, "resumption", GenResumption.Desc)

}
