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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, StaticConstructorMethod, StaticField, mkClass}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{DevFlixRuntime, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.{ClassConstants, Mangle}

import java.lang.constant.ClassDesc

/** The `Unit` class, whose single instance is the Flix unit value. */
object GenUnit {

  /** The JVM class descriptor for the generated `Unit` class. */
  val Desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("Unit"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = mkClass(this.Desc, IsFinal)

    cm.mkStaticConstructor(StaticConstructorMethod(this.Desc), singletonStaticConstructor(Constructor, SingletonField)(_))
    cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
    cm.mkField(SingletonField, IsPublic, IsFinal, NotVolatile)

    cm.closeClassMaker()
  }

  def Constructor: ConstructorMethod = ConstructorMethod(this.Desc, Nil)

  def SingletonField: StaticField = StaticField(this.Desc, "INSTANCE", this.Desc)


}
