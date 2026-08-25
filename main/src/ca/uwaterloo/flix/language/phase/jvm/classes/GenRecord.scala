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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.InterfaceMethod
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{RootPackage, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.MethodTypeDescs.mkDescriptor
import ca.uwaterloo.flix.language.phase.jvm.{ClassMaker, JavaClasses, Mangle}

import java.lang.constant.ClassDesc

/** The `Record` interface, implemented by [[GenRecordEmpty]] and [[GenRecordExtend]]. */
object GenRecord {

  val desc: ClassDesc = mkDesc(RootPackage, Mangle.mkClassName("Record"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkInterface(this.desc)

    cm.mkInterfaceMethod(LookupFieldMethod)
    cm.mkInterfaceMethod(RestrictFieldMethod)

    cm.closeClassMaker()
  }

  def LookupFieldMethod: InterfaceMethod = InterfaceMethod(this.desc, "lookupField",
    mkDescriptor(JavaClasses.String)(this.desc))

  def RestrictFieldMethod: InterfaceMethod = InterfaceMethod(this.desc, "restrictField",
    mkDescriptor(JavaClasses.String)(this.desc))

}
