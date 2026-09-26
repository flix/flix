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
  * The `UnhandledEffectError` class, which is thrown when an effect operation is called
  * without a corresponding handler.
  */
object GenUnhandledEffectError {

  /** The JVM class descriptor for the generated `UnhandledEffectError` class. */
  val Desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("UnhandledEffectError"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(this.Desc, IsFinal, superClass = GenFlixError.Desc)

    cm.mkConstructor(Constructor, IsPublic, constructorIns(_))
    // This field allows external equality checking.
    cm.mkField(EffectNameField, IsPublic, IsFinal, NotVolatile)
    cm.mkField(LocationField, IsPublic, IsFinal, NotVolatile)

    cm.closeClassMaker()
  }

  private def EffectNameField: InstanceField = InstanceField(this.Desc, "effectName", JavaClasses.String)

  private def LocationField: InstanceField = InstanceField(this.Desc, "location", GenReifiedSourceLocation.Desc)

  def Constructor: ConstructorMethod =
    ConstructorMethod(this.Desc, List(GenSuspension.Desc, JavaClasses.String, GenReifiedSourceLocation.Desc))

  private def constructorIns(implicit mv: MethodVisitor): Unit = {
    withName(1, GenSuspension.Desc)(suspension => withName(2, JavaClasses.String)(info => withName(3, GenReifiedSourceLocation.Desc)(loc => {
      def appendString(): Unit = INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)

      thisLoad()
      NEW(JavaClasses.StringBuilder)
      DUP()
      INVOKESPECIAL(ClassConstants.StringBuilder.Constructor)
      pushString("Unhandled effect '")
      appendString()
      suspension.load()
      GETFIELD(GenSuspension.EffSymField)
      appendString()
      pushString("' (")
      appendString()
      info.load()
      appendString()
      pushString(") at ")
      appendString()
      loc.load()
      INVOKEVIRTUAL(ClassConstants.Object.ToStringMethod)
      appendString()
      INVOKEVIRTUAL(ClassConstants.Object.ToStringMethod)
      INVOKESPECIAL(GenFlixError.Constructor)
      // save arguments locally
      thisLoad()
      suspension.load()
      GETFIELD(GenSuspension.EffSymField)
      PUTFIELD(EffectNameField)
      thisLoad()
      loc.load()
      PUTFIELD(LocationField)
      RETURN()
    })))
  }

}
