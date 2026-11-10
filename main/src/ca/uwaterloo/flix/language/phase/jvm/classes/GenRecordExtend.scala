/*
 * Copyright 2021 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
import ca.uwaterloo.flix.language.phase.jvm.{ClassMaker, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/**
  * The record extension class `RecordExtend$Obj`, which holds one label of a record and
  * the rest of the record.
  *
  * `value` is the erased type of the label's value, so there is one class per erased
  * value type rather than one per record shape.
  *
  * The label and the rest of the record are the fields of [[GenRecord]]; the class only
  * adds the value.
  */
object GenRecordExtend {

  def desc(value: ClassDesc): ClassDesc =
    mkDesc(RootPackage, Mangle.mkClassName("RecordExtend", Mangle.erasedName(value)))

  def genByteCode(value: ClassDesc)(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(desc(value), IsFinal, superClass = GenRecord.Desc)

    cm.mkConstructor(Constructor(value), IsPublic, nullarySuperConstructor(GenRecord.Constructor)(_))
    cm.mkField(ValueField(value), IsPublic, NotFinal, NotVolatile)
    cm.mkMethod(Nil, RestrictFieldMethod(value), IsPublic, IsFinal, restrictFieldIns(value)(_))

    cm.closeClassMaker()
  }

  def Constructor(value: ClassDesc): ConstructorMethod = ConstructorMethod(desc(value), Nil)

  def ValueField(value: ClassDesc): InstanceField = InstanceField(desc(value), "value", value)

  private def RestrictFieldMethod(value: ClassDesc): InstanceMethod =
    GenRecord.RestrictFieldMethod.implementation(desc(value))

  private def restrictFieldIns(value: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    caseOnLabelEquality {
      case TrueBranch =>
        thisLoad()
        GETFIELD(GenRecord.RestField)
        ARETURN()
      case FalseBranch =>
        NEW(desc(value))
        DUP()
        INVOKESPECIAL(Constructor(value))
        DUP()
        // The label is copied by reference, so the copy holds the same constant pool string.
        thisLoad()
        GETFIELD(GenRecord.LabelField)
        PUTFIELD(GenRecord.LabelField)
        DUP()
        thisLoad()
        GETFIELD(ValueField(value))
        PUTFIELD(ValueField(value))
        DUP() // get the new restricted rest to put
        thisLoad()
        GETFIELD(GenRecord.RestField)
        ALOAD(1)
        INVOKEVIRTUAL(GenRecord.RestrictFieldMethod)
        PUTFIELD(GenRecord.RestField) // put the rest field and return
        ARETURN()
    }
  }

  /**
    * Compares the label of `this` and `ALOAD(1)` and executes the designated branch.
    *
    * The labels are compared by reference, not with `equals`. This is sound because every
    * label is loaded from the constant pool with an `ldc` instruction, and the JVM interns
    * such strings: equal labels are the same object, whichever class loaded them.
    *
    * The labels are loaded by `RecordSelect`, `RecordExtend`, and `RecordRestrict` in
    * [[ca.uwaterloo.flix.language.phase.jvm.GenExpression]]. A label that is built at
    * runtime -- and not interned -- would never be found.
    */
  private def caseOnLabelEquality(cases: Branch => Unit)(implicit mv: MethodVisitor): Unit = {
    thisLoad()
    GETFIELD(GenRecord.LabelField)
    ALOAD(1)
    branch(Condition.ACMPEQ)(cases)
  }

}
