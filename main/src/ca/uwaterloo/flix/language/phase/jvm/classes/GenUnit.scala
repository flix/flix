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

  private def Constructor: ConstructorMethod = ConstructorMethod(this.Desc, Nil)

  def SingletonField: StaticField = StaticField(this.Desc, "INSTANCE", this.Desc)


}
