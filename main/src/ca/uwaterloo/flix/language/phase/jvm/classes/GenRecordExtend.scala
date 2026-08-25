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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceField, InstanceMethod}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.Branch.{FalseBranch, TrueBranch}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{RootPackage, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.{BackendObjType, ClassConstants, ClassMaker, JavaClasses, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/**
  * The record extension class `RecordExtend$Obj`, which holds one label of a record and
  * the rest of the record.
  *
  * `value` is the erased type of the label's value, so there is one class per erased
  * value type rather than one per record shape.
  */
object GenRecordExtend {

  def desc(value: ClassDesc): ClassDesc =
    mkDesc(RootPackage, Mangle.mkClassName("RecordExtend", Mangle.erasedName(value)))

  def genByteCode(value: ClassDesc)(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(desc(value), IsFinal, interfaces = List(BackendObjType.Record.desc))

    cm.mkConstructor(Constructor(value), IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
    cm.mkField(LabelField(value), IsPublic, NotFinal, NotVolatile)
    cm.mkField(ValueField(value), IsPublic, NotFinal, NotVolatile)
    cm.mkField(RestField(value), IsPublic, NotFinal, NotVolatile)
    cm.mkMethod(Nil, BackendObjType.Record.LookupFieldMethod.implementation(desc(value)), IsPublic, IsFinal, lookupFieldIns(value)(_))
    cm.mkMethod(Nil, RestrictFieldMethod(value), IsPublic, IsFinal, restrictFieldIns(value)(_))

    cm.closeClassMaker()
  }

  def Constructor(value: ClassDesc): ConstructorMethod = ConstructorMethod(desc(value), Nil)

  def LabelField(value: ClassDesc): InstanceField = InstanceField(desc(value), "label", JavaClasses.String)

  def ValueField(value: ClassDesc): InstanceField = InstanceField(desc(value), "value", value)

  def RestField(value: ClassDesc): InstanceField = InstanceField(desc(value), "rest", BackendObjType.Record.desc)

  private def lookupFieldIns(value: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    caseOnLabelEquality(value) {
      case TrueBranch =>
        thisLoad()
        ARETURN()
      case FalseBranch =>
        thisLoad()
        GETFIELD(RestField(value))
        ALOAD(1)
        INVOKEINTERFACE(BackendObjType.Record.LookupFieldMethod)
        ARETURN()
    }
  }

  def RestrictFieldMethod(value: ClassDesc): InstanceMethod =
    BackendObjType.Record.RestrictFieldMethod.implementation(desc(value))

  private def restrictFieldIns(value: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    caseOnLabelEquality(value) {
      case TrueBranch =>
        thisLoad()
        GETFIELD(RestField(value))
        ARETURN()
      case FalseBranch =>
        NEW(desc(value))
        DUP()
        INVOKESPECIAL(Constructor(value))
        DUP()
        thisLoad()
        GETFIELD(LabelField(value))
        PUTFIELD(LabelField(value))
        DUP()
        thisLoad()
        GETFIELD(ValueField(value))
        PUTFIELD(ValueField(value))
        DUP() // get the new restricted rest to put
        thisLoad()
        GETFIELD(RestField(value))
        ALOAD(1)
        INVOKEINTERFACE(BackendObjType.Record.RestrictFieldMethod)
        PUTFIELD(RestField(value)) // put the rest field and return
        ARETURN()
    }
  }

  /**
    * Compares the label of `this`and `ALOAD(1)` and executes the designated branch.
    */
  private def caseOnLabelEquality(value: ClassDesc)(cases: Branch => Unit)(implicit mv: MethodVisitor): Unit = {
    thisLoad()
    GETFIELD(LabelField(value))
    ALOAD(1)
    INVOKEVIRTUAL(ClassConstants.Object.EqualsMethod)
    branch(Condition.Bool)(cases)
  }

}
