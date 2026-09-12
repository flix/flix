/*
 * Copyright 2021 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.jvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.jvm.JavaClasses
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.NotFinal
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.NotVolatile
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceField}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{RootPackage, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.{ClassConstants, ClassMaker, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/** The abstract base class of every extensible tag class, carrying the tag's name. */
object GenExtTagged {

  /** The JVM class descriptor for the generated `ExtTagged` class. */
  val Desc: ClassDesc = mkDesc(RootPackage, Mangle.mkClassName("ExtTagged"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkAbstractClass(this.Desc)

    cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))

    cm.mkField(NameField, IsPublic, NotFinal, NotVolatile)

    cm.closeClassMaker()
  }

  def NameField: InstanceField = InstanceField(this.Desc, "tag", JavaClasses.String)

  def Constructor: ConstructorMethod = ConstructorMethod(this.Desc, Nil)

  /** [...] -> [..., tagName] */
  def mkTagName(name: String)(implicit mv: MethodVisitor): Unit = pushString(Mangle.mangle(name))

  /** [..., tagName1, tagName2] --> [..., tagName1 == tagName2] */
  def eqTagName()(implicit mv: MethodVisitor): Unit = {
    // ACMP is okay since tag strings are loaded through ldc instructions
    ifConditionElse(Condition.ACMPEQ)(pushBool(true))(pushBool(false))
  }

}
