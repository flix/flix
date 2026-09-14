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
import ca.uwaterloo.flix.language.phase.jvm.{ClassConstants, ClassMaker, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/** The class of a Flix tuple, with one field per erased element type. */
object GenTuple {

  def desc(elms: List[ClassDesc]): ClassDesc =
    mkDesc(RootPackage, Mangle.mkClassName("Tuple", elms.map(Mangle.erasedName)))


  def genByteCode(elms: List[ClassDesc])(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(desc(elms), IsFinal)

    elms.indices.foreach(i => cm.mkField(IndexField(elms, i), IsPublic, NotFinal, NotVolatile))
    cm.mkConstructor(Constructor(elms), IsPublic, constructorIns(elms)(_))

    cm.closeClassMaker()
  }

  def IndexField(elms: List[ClassDesc], i: Int): InstanceField = InstanceField(desc(elms), s"field$i", elms(i))

  def Constructor(elms: List[ClassDesc]): ConstructorMethod = ConstructorMethod(desc(elms), elms)

  /** `[] --> return` */
  private def constructorIns(elms: List[ClassDesc])(implicit mv: MethodVisitor): Unit =
    withNames(1, elms) { case (_, variables) =>
      thisLoad()
      // super()
      DUP()
      INVOKESPECIAL(ClassConstants.Object.Constructor)
      // this.field$i = var$j
      for ((elm, i) <- variables.zipWithIndex) {
        DUP()
        elm.load()
        PUTFIELD(IndexField(elms, i))
      }
      RETURN()
    }


}
