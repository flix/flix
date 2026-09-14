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
  * A non-empty [[GenResumption]]: one handler's worth of the continuation, and the
  * resumption to continue with once it is done.
  */
object GenResumptionCons {

  /** The JVM class descriptor for the generated `ResumptionCons` class. */
  val Desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("ResumptionCons"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = mkClass(this.Desc, IsFinal, interfaces = List(GenResumption.Desc))

    cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))

    cm.mkField(SymField, IsPublic, NotFinal, NotVolatile)
    cm.mkField(HandlerField, IsPublic, NotFinal, NotVolatile)
    cm.mkField(FramesField, IsPublic, NotFinal, NotVolatile)
    cm.mkField(TailField, IsPublic, NotFinal, NotVolatile)

    cm.mkMethod(Nil, GenResumption.RewindMethod.implementation(this.Desc), IsPublic, IsFinal, rewindIns(_))

    cm.closeClassMaker()
  }

  def Constructor: ConstructorMethod = ConstructorMethod(this.Desc, Nil)

  def SymField: InstanceField = InstanceField(this.Desc, "sym", JavaClasses.String)

  def HandlerField: InstanceField = InstanceField(this.Desc, "handler", GenHandler.Desc)

  def FramesField: InstanceField = InstanceField(this.Desc, "frames", GenFrames.Desc)

  def TailField: InstanceField = InstanceField(this.Desc, "tail", GenResumption.Desc)

  private def rewindIns(implicit mv: MethodVisitor): Unit = {
    withName(1, GenValue.Desc) { v =>
      thisLoad()
      GETFIELD(SymField)
      thisLoad()
      GETFIELD(HandlerField)
      thisLoad()
      GETFIELD(FramesField)
      // () -> tail.rewind(v)
      thisLoad()
      GETFIELD(TailField)
      v.load()
      mkStaticLambda(GenThunk.InvokeMethod, GenResumption.StaticRewindMethod, drop = 0)
      mkStaticLambda(GenThunk.InvokeMethod, GenHandler.InstallHandlerMethod, drop = 0)
      xReturn(GenThunk.Desc)
    }
  }

}
