/*
 * Copyright 2021 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.jvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.NotVolatile
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceField}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{RootPackage, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.{ClassMaker, Mangle}
import ca.uwaterloo.flix.util.InternalCompilerException

import java.lang.constant.ClassDesc

/**
  * The class of a non-nullary enum case, e.g. `Tag$Obj$Int32` for a case carrying a
  * reference and an `Int32`.
  *
  * `elms` are the erased types of the values the case carries; the class is shared by
  * every case with that erased shape, and the case is identified at runtime by
  * `Tagged.OrdinalField`.
  */
object GenTag {

  def desc(elms: List[ClassDesc]): ClassDesc =
    mkDesc(RootPackage, Mangle.mkClassName("Tag", elms.map(Mangle.erasedName)))

  def genByteCode(elms: List[ClassDesc])(implicit flix: Flix): Array[Byte] = {
    if (elms.isEmpty) throw InternalCompilerException(s"Unexpected nullary Tag type", SourceLocation.Unknown)
    val cm = ClassMaker.mkClass(desc(elms), IsFinal, superClass = GenTagged.Desc)

    cm.mkConstructor(Constructor(elms), IsPublic, nullarySuperConstructor(GenTagged.Constructor)(_))
    elms.indices.foreach(i => cm.mkField(IndexField(elms, i), IsPublic, NotFinal, NotVolatile))

    cm.closeClassMaker()
  }

  def OrdinalField: InstanceField = GenTagged.OrdinalField

  def IndexField(elms: List[ClassDesc], i: Int): InstanceField = InstanceField(desc(elms), s"v$i", elms(i))

  def Constructor(elms: List[ClassDesc]): ConstructorMethod = ConstructorMethod(desc(elms), Nil)

}
