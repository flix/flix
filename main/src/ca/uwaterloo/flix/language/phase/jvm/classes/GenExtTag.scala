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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceField}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{RootPackage, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.{BackendObjType, ClassMaker, Mangle}

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
    val cm = ClassMaker.mkClass(desc(elms), IsFinal, superClass = GenExtTagged.desc)

    cm.mkConstructor(Constructor(elms), IsPublic, nullarySuperConstructor(GenExtTagged.Constructor)(_))
    elms.indices.foreach(i => cm.mkField(IndexField(elms, i), IsPublic, NotFinal, NotVolatile))

    cm.closeClassMaker()
  }

  def NameField: InstanceField = GenExtTagged.NameField

  def IndexField(elms: List[ClassDesc], i: Int): InstanceField = InstanceField(desc(elms), s"v$i", elms(i))

  def Constructor(elms: List[ClassDesc]): ConstructorMethod = ConstructorMethod(desc(elms), Nil)

}
