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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, StaticConstructorMethod, StaticField}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{RootPackage, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.{BackendObjType, ClassMaker, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/**
  * The class of a nullary enum case, e.g. `Color$Red` for `case Red` of `enum Color`.
  *
  * A nullary case carries no values, so the class has a single instance held in
  * [[SingletonField]].
  */
object GenNullaryTag {

  def desc(enumName: String, name: String): ClassDesc =
    mkDesc(RootPackage, Mangle.mkClassName(enumName, name))

  def genByteCode(enumName: String, name: String, ordinal: Int)(implicit flix: Flix): Array[Byte] = {
    val d = desc(enumName, name)
    val cm = ClassMaker.mkClass(d, IsFinal, superClass = BackendObjType.Tagged.desc)

    cm.mkStaticConstructor(StaticConstructorMethod(d), singletonStaticConstructor(Constructor(enumName, name), SingletonField(enumName, name))(_))
    cm.mkField(SingletonField(enumName, name), IsPublic, IsFinal, NotVolatile)
    cm.mkConstructor(Constructor(enumName, name), IsPublic, constructorIns(ordinal)(_))

    cm.closeClassMaker()
  }

  def SingletonField(enumName: String, name: String): StaticField = {
    val d = desc(enumName, name)
    StaticField(d, "singleton", d)
  }

  def Constructor(enumName: String, name: String): ConstructorMethod =
    ConstructorMethod(desc(enumName, name), Nil)

  /** `[] --> return` */
  private def constructorIns(ordinal: Int)(implicit mv: MethodVisitor): Unit = {
    thisLoad()
    INVOKESPECIAL(BackendObjType.Tagged.Constructor)
    thisLoad()
    pushInt(ordinal)
    PUTFIELD(BackendObjType.Tagged.OrdinalField)
    RETURN()
  }

}
