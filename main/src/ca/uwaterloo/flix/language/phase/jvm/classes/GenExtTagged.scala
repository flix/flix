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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.NotFinal
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.NotVolatile
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceField}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{RootPackage, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.{ClassConstants, ClassMaker, JavaClasses, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/** The abstract base class of every extensible tag class, carrying the tag's name. */
object GenExtTagged {

  val desc: ClassDesc = mkDesc(RootPackage, Mangle.mkClassName("ExtTagged"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkAbstractClass(this.desc)

    cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))

    cm.mkField(NameField, IsPublic, NotFinal, NotVolatile)

    cm.closeClassMaker()
  }

  def NameField: InstanceField = InstanceField(this.desc, "tag", JavaClasses.String)

  def Constructor: ConstructorMethod = ConstructorMethod(this.desc, Nil)

  /** [...] -> [..., tagName] */
  def mkTagName(name: String)(implicit mv: MethodVisitor): Unit = pushString(Mangle.mangle(name))

  /** [..., tagName1, tagName2] --> [..., tagName1 == tagName2] */
  def eqTagName()(implicit mv: MethodVisitor): Unit = {
    // ACMP is okay since tag strings are loaded through ldc instructions
    ifConditionElse(Condition.ACMPEQ)(pushBool(true))(pushBool(false))
  }

}
