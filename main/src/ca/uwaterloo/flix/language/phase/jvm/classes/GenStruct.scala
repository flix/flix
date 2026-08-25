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
import ca.uwaterloo.flix.language.phase.jvm.{ClassConstants, ClassMaker, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/** The class of a Flix struct, with one field per erased element type. */
object GenStruct {

  def desc(elms: List[ClassDesc]): ClassDesc =
    mkDesc(RootPackage, Mangle.mkClassName("Struct", elms.map(Mangle.erasedName)))


  def genByteCode(elms: List[ClassDesc])(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(desc(elms), IsFinal)

    elms.indices.foreach(i => cm.mkField(IndexField(elms, i), IsPublic, NotFinal, NotVolatile))
    cm.mkConstructor(Constructor(elms), IsPublic, constructorIns(elms)(_))

    cm.closeClassMaker()
  }

  def IndexField(elms: List[ClassDesc], i: Int): InstanceField = InstanceField(desc(elms), s"field$i", elms(i))

  def Constructor(elms: List[ClassDesc]): ConstructorMethod = ConstructorMethod(desc(elms), elms)

  private def constructorIns(elms: List[ClassDesc])(implicit mv: MethodVisitor): Unit = {
    withNames(1, elms) { case (_, variables) =>
      thisLoad()
      // super()
      DUP()
      INVOKESPECIAL(ClassConstants.Object.Constructor)
      // this.field$i = var$j
      // fields are numbered consecutively while variables skip indices based
      // on their stack size
      for ((elm, i) <- variables.zipWithIndex) {
        DUP()
        elm.load()
        PUTFIELD(IndexField(elms, i))
      }
      RETURN()
    }
  }


}
