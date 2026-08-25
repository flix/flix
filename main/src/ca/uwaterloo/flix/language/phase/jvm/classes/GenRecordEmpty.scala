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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.NotVolatile
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceMethod, StaticConstructorMethod, StaticField}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{RootPackage, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.{ClassConstants, ClassMaker, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/** The empty record, a singleton implementing [[GenRecord]]. */
object GenRecordEmpty {

  val desc: ClassDesc = mkDesc(RootPackage, Mangle.mkClassName("RecordEmpty"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(this.desc, IsFinal, interfaces = List(GenRecord.desc))

    cm.mkStaticConstructor(StaticConstructorMethod(this.desc), singletonStaticConstructor(Constructor, SingletonField)(_))
    cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
    cm.mkField(SingletonField, IsPublic, IsFinal, NotVolatile)
    cm.mkMethod(Nil, LookupFieldMethod, IsPublic, IsFinal, throwUnsupportedExc(_))
    cm.mkMethod(Nil, RestrictFieldMethod, IsPublic, IsFinal, throwUnsupportedExc(_))

    cm.closeClassMaker()
  }

  def Constructor: ConstructorMethod = ConstructorMethod(this.desc, Nil)

  def SingletonField: StaticField = StaticField(this.desc, "INSTANCE", this.desc)

  private def LookupFieldMethod: InstanceMethod = GenRecord.LookupFieldMethod.implementation(this.desc)

  private def RestrictFieldMethod: InstanceMethod = GenRecord.RestrictFieldMethod.implementation(this.desc)

  private def throwUnsupportedExc(implicit mv: MethodVisitor): Unit = {
    throwUnsupportedOperationException(
      s"${GenRecord.LookupFieldMethod.name} method shouldn't be called")
  }

}
