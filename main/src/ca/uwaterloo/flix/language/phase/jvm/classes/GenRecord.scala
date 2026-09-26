/*
 * Copyright 2021 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.jvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.jvm.JavaClasses
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.InterfaceMethod
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{RootPackage, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.MethodTypeDescs.mkDescriptor
import ca.uwaterloo.flix.language.phase.jvm.{ClassMaker, Mangle}

import java.lang.constant.ClassDesc

/** The `Record` interface, implemented by [[GenRecordEmpty]] and [[GenRecordExtend]]. */
object GenRecord {

  /** The JVM class descriptor for the generated `Record` class. */
  val Desc: ClassDesc = mkDesc(RootPackage, Mangle.mkClassName("Record"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkInterface(this.Desc)

    cm.mkInterfaceMethod(LookupFieldMethod)
    cm.mkInterfaceMethod(RestrictFieldMethod)

    cm.closeClassMaker()
  }

  def LookupFieldMethod: InterfaceMethod = InterfaceMethod(this.Desc, "lookupField",
    mkDescriptor(JavaClasses.String)(this.Desc))

  def RestrictFieldMethod: InterfaceMethod = InterfaceMethod(this.Desc, "restrictField",
    mkDescriptor(JavaClasses.String)(this.Desc))

}
