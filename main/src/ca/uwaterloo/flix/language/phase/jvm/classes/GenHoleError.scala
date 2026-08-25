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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.IsFinal
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.NotVolatile
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceField}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{DevFlixRuntime, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.{ClassConstants, ClassMaker, JavaClasses, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/**
  * The `HoleError` class, which is thrown when a hole expression is evaluated.
  */
object GenHoleError {

  val desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("HoleError"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(this.desc, IsFinal, ClassConstants.FlixError.Desc)

    cm.mkConstructor(Constructor, IsPublic, constructorIns(_))
    // These fields allow external equality checking.
    cm.mkField(HoleField, IsPublic, IsFinal, NotVolatile)
    cm.mkField(LocationField, IsPublic, IsFinal, NotVolatile)

    cm.closeClassMaker()
  }

  private def HoleField: InstanceField = InstanceField(this.desc, "hole", JavaClasses.String)

  private def LocationField: InstanceField = InstanceField(this.desc, "location", GenReifiedSourceLocation.desc)

  def Constructor: ConstructorMethod = ConstructorMethod(this.desc, List(JavaClasses.String, GenReifiedSourceLocation.desc))

  private def constructorIns(implicit mv: MethodVisitor): Unit = {
    withName(1, JavaClasses.String) { hole =>
      withName(2, GenReifiedSourceLocation.desc) { loc =>
        thisLoad()
        // create an error msg
        NEW(JavaClasses.StringBuilder)
        DUP()
        INVOKESPECIAL(ClassConstants.StringBuilder.Constructor)
        pushString("Hole '")
        INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
        hole.load()
        INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
        pushString("' at ")
        INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
        loc.load()
        INVOKEVIRTUAL(ClassConstants.Object.ToStringMethod)
        INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
        INVOKEVIRTUAL(ClassConstants.Object.ToStringMethod)
        INVOKESPECIAL(ClassConstants.FlixError.Constructor)
        // save the arguments locally
        thisLoad()
        hole.load()
        PUTFIELD(HoleField)
        thisLoad()
        loc.load()
        PUTFIELD(LocationField)
        RETURN()
      }
    }
  }

}
