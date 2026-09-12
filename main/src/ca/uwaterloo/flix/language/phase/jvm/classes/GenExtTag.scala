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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceField}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{RootPackage, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.{ClassMaker, Mangle}

import java.lang.constant.ClassDesc

/**
  * The class of an extensible tag, e.g. `ExtTag$Obj$Int32` for a tag carrying a reference
  * and an `Int32`.
  *
  * `elms` are the erased types of the values the tag carries; the class is shared by every
  * tag with that erased shape, and the tag is identified at runtime by
  * `ExtTagged.NameField`.
  */
object GenExtTag {

  def desc(elms: List[ClassDesc]): ClassDesc =
    mkDesc(RootPackage, Mangle.mkClassName("ExtTag", elms.map(Mangle.erasedName)))

  def genByteCode(elms: List[ClassDesc])(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(desc(elms), IsFinal, superClass = GenExtTagged.Desc)

    cm.mkConstructor(Constructor(elms), IsPublic, nullarySuperConstructor(GenExtTagged.Constructor)(_))
    elms.indices.foreach(i => cm.mkField(IndexField(elms, i), IsPublic, NotFinal, NotVolatile))

    cm.closeClassMaker()
  }

  def NameField: InstanceField = GenExtTagged.NameField

  def IndexField(elms: List[ClassDesc], i: Int): InstanceField = InstanceField(desc(elms), s"v$i", elms(i))

  def Constructor(elms: List[ClassDesc]): ConstructorMethod = ConstructorMethod(desc(elms), Nil)

}
