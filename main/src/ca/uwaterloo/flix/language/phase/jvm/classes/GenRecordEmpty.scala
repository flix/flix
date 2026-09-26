/*
 * Copyright 2021 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.jvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.IsFinal
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.NotVolatile
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceMethod, StaticConstructorMethod, StaticField}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{RootPackage, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.{ClassConstants, ClassMaker, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/** The empty record, a singleton implementing [[GenRecord]]. */
object GenRecordEmpty {

  /** The JVM class descriptor for the generated `RecordEmpty` class. */
  val Desc: ClassDesc = mkDesc(RootPackage, Mangle.mkClassName("RecordEmpty"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(this.Desc, IsFinal, interfaces = List(GenRecord.Desc))

    cm.mkStaticConstructor(StaticConstructorMethod(this.Desc), singletonStaticConstructor(Constructor, SingletonField)(_))
    cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
    cm.mkField(SingletonField, IsPublic, IsFinal, NotVolatile)
    cm.mkMethod(Nil, LookupFieldMethod, IsPublic, IsFinal, throwUnsupportedExc(_))
    cm.mkMethod(Nil, RestrictFieldMethod, IsPublic, IsFinal, throwUnsupportedExc(_))

    cm.closeClassMaker()
  }

  private def Constructor: ConstructorMethod = ConstructorMethod(this.Desc, Nil)

  def SingletonField: StaticField = StaticField(this.Desc, "INSTANCE", this.Desc)

  private def LookupFieldMethod: InstanceMethod = GenRecord.LookupFieldMethod.implementation(this.Desc)

  private def RestrictFieldMethod: InstanceMethod = GenRecord.RestrictFieldMethod.implementation(this.Desc)

  private def throwUnsupportedExc(implicit mv: MethodVisitor): Unit = {
    throwUnsupportedOperationException(
      s"${GenRecord.LookupFieldMethod.name} method shouldn't be called")
  }

}
