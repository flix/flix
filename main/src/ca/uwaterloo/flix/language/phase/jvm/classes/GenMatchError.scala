/*
 * Copyright 2021 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.jvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.jvm.JavaClasses
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.IsFinal
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.NotVolatile
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceField}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{DevFlixRuntime, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.{ClassConstants, ClassMaker, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/**
  * The `MatchError` class, which is thrown when a pattern match is non-exhaustive.
  */
object GenMatchError {

  /** The JVM class descriptor for the generated `MatchError` class. */
  val Desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("MatchError"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(this.Desc, IsFinal, superClass = GenFlixError.Desc)

    cm.mkConstructor(Constructor, IsPublic, constructorIns(_))
    // This field allows external equality checking.
    cm.mkField(LocationField, IsPublic, IsFinal, NotVolatile)

    cm.closeClassMaker()
  }

  private def LocationField: InstanceField = InstanceField(this.Desc, "location", GenReifiedSourceLocation.Desc)

  def Constructor: ConstructorMethod = ConstructorMethod(this.Desc, List(GenReifiedSourceLocation.Desc))

  private def constructorIns(implicit mv: MethodVisitor): Unit = {
    thisLoad()
    NEW(JavaClasses.StringBuilder)
    DUP()
    INVOKESPECIAL(ClassConstants.StringBuilder.Constructor)
    pushString("Non-exhaustive match at ")
    INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
    ALOAD(1)
    INVOKEVIRTUAL(ClassConstants.Object.ToStringMethod)
    INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
    INVOKEVIRTUAL(ClassConstants.Object.ToStringMethod)
    INVOKESPECIAL(GenFlixError.Constructor)
    // save argument locally
    thisLoad()
    ALOAD(1)
    PUTFIELD(this.LocationField)
    RETURN()
  }

}
