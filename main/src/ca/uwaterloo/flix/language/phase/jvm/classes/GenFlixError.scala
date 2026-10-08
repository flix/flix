/*
 *  Copyright 2025 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.jvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.jvm.JavaClasses
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.ConstructorMethod
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{DevFlixRuntime, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.MethodTypeDescs.mkVoidDescriptor
import ca.uwaterloo.flix.language.phase.jvm.{ClassMaker, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/**
  * The abstract `FlixError` class, which the generated `HoleError`, `MatchError`, `CastError`,
  * and `UnhandledEffectError` classes extend.
  */
object GenFlixError {

  /** The JVM class descriptor for the generated `FlixError` class. */
  val Desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("FlixError"))

  val Constructor: ConstructorMethod = ConstructorMethod(Desc, List(JavaClasses.String))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkAbstractClass(Desc, JavaClasses.Error)
    cm.mkConstructor(Constructor, IsPublic, constructorIns(_))
    cm.closeClassMaker()
  }

  private def constructorIns(implicit mv: MethodVisitor): Unit = {
    thisLoad()
    ALOAD(1)
    invokeConstructor(JavaClasses.Error, mkVoidDescriptor(JavaClasses.String))
    RETURN()
  }

}
