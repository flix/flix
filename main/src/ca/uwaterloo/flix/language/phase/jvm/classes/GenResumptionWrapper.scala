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

import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.{IsPrivate, IsPublic}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.NotVolatile
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, ConstructorMethodName, InstanceField, InstanceMethod, mkClass}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{DevFlixRuntime, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.MethodTypeDescs.mkDescriptor
import ca.uwaterloo.flix.language.phase.jvm.{BackendObjType, Mangle, MethodTypeDescs}
import ca.uwaterloo.flix.util.ClassDescs
import org.objectweb.asm.{Label, MethodVisitor, Opcodes}

import java.lang.constant.ClassDesc
import java.lang.constant.ConstantDescs.{CD_Object, CD_boolean}

/**
  * The class that presents a [[GenResumption]] as a one-argument Flix function, so that a
  * handler can call the continuation like any other closure.
  *
  * `tpe` is the erased type the resumption is resumed with.
  */
object GenResumptionWrapper {

  def desc(tpe: ClassDesc): ClassDesc =
    mkDesc(DevFlixRuntime, Mangle.mkClassName("ResumptionWrapper", Mangle.erasedName(tpe)))

  // tpe -> Result
  private def superClass(tpe: ClassDesc): BackendObjType.AbstractArrow =
    BackendObjType.AbstractArrow(List(tpe), CD_Object)

  def genByteCode(tpe: ClassDesc)(implicit flix: Flix): Array[Byte] = {
    val cm = mkClass(desc(tpe), IsFinal, superClass(tpe).desc)
    cm.mkConstructor(Constructor(tpe), IsPublic, constructorIns(tpe)(_))
    cm.mkField(ResumptionField(tpe), IsPrivate, IsFinal, NotVolatile)
    cm.mkMethod(Nil, InvokeMethod(tpe), IsPublic, NotFinal, invokeIns(tpe)(_))
    cm.mkMethod(Nil, UniqueMethod(tpe), IsPublic, NotFinal, uniqueIns(_))
    cm.closeClassMaker()
  }

  def Constructor(tpe: ClassDesc): ConstructorMethod = ConstructorMethod(desc(tpe), List(GenResumption.desc))

  private def constructorIns(tpe: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    withName(1, GenResumption.desc) { resumption =>
      thisLoad()
      INVOKESPECIAL(superClass(tpe).desc, ConstructorMethodName, MethodTypeDescs.NothingToVoid)
      thisLoad()
      resumption.load()
      PUTFIELD(ResumptionField(tpe))
      RETURN()
    }
  }

  def ResumptionField(tpe: ClassDesc): InstanceField = InstanceField(desc(tpe), "resumption", GenResumption.desc)

  def InvokeMethod(tpe: ClassDesc): InstanceMethod = GenThunk.InvokeMethod.implementation(desc(tpe))

  private def invokeIns(tpe: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    thisLoad()
    GETFIELD(ResumptionField(tpe))
    tpe match {
      case CD_boolean =>
        // Use cached Value.TRUE / Value.FALSE singletons
        thisLoad()
        mv.visitFieldInsn(Opcodes.GETFIELD, ClassDescs.internalNameOf(desc(tpe)), "arg0", tpe.descriptorString())
        val falseLabel = new Label()
        val doneLabel = new Label()
        mv.visitJumpInsn(Opcodes.IFEQ, falseLabel)
        GETSTATIC(GenValue.TrueField)
        mv.visitJumpInsn(Opcodes.GOTO, doneLabel)
        mv.visitLabel(falseLabel)
        GETSTATIC(GenValue.FalseField)
        mv.visitLabel(doneLabel)
      case _ =>
        NEW(GenValue.desc)
        DUP()
        INVOKESPECIAL(GenValue.Constructor)
        DUP()
        thisLoad()
        mv.visitFieldInsn(Opcodes.GETFIELD, ClassDescs.internalNameOf(desc(tpe)), "arg0", tpe.descriptorString())
        PUTFIELD(GenValue.fieldFromType(tpe))
    }
    INVOKEINTERFACE(GenResumption.RewindMethod)
    xReturn(GenResult.desc)
  }

  private def UniqueMethod(tpe: ClassDesc): InstanceMethod =
    InstanceMethod(desc(tpe), "getUniqueThreadClosure", mkDescriptor()(superClass(tpe).desc))

  private def uniqueIns(implicit mv: MethodVisitor): Unit = {
    thisLoad()
    ARETURN()
  }

}
