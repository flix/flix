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
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.NotVolatile
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceField}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{RootPackage, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.{BackendObjType, ClassMaker, Mangle}
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
    val cm = ClassMaker.mkClass(desc(elms), IsFinal, superClass = BackendObjType.Tagged.desc)

    cm.mkConstructor(Constructor(elms), IsPublic, nullarySuperConstructor(BackendObjType.Tagged.Constructor)(_))
    elms.indices.foreach(i => cm.mkField(IndexField(elms, i), IsPublic, NotFinal, NotVolatile))

    cm.closeClassMaker()
  }

  def OrdinalField: InstanceField = BackendObjType.Tagged.OrdinalField

  def IndexField(elms: List[ClassDesc], i: Int): InstanceField = InstanceField(desc(elms), s"v$i", elms(i))

  def Constructor(elms: List[ClassDesc]): ConstructorMethod = ConstructorMethod(desc(elms), Nil)

}
