/*
 *  Copyright 2025 Jonathan Lindegaard Starup
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
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
