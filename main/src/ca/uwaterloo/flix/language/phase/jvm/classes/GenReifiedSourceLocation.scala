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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.NotVolatile
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceField, InstanceMethod}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{DevFlixRuntime, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.{ClassConstants, ClassMaker, JavaClasses, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc
import java.lang.constant.ConstantDescs.CD_int

/**
  * The `ReifiedSourceLocation` class, which is a runtime representation of a source location.
  */
object GenReifiedSourceLocation {

  val desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("ReifiedSourceLocation"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(this.desc, IsFinal)

    cm.mkConstructor(Constructor, IsPublic, constructorIns(_))

    cm.mkField(SourceField, IsPublic, IsFinal, NotVolatile)
    cm.mkField(BeginLineField, IsPublic, IsFinal, NotVolatile)
    cm.mkField(BeginColField, IsPublic, IsFinal, NotVolatile)
    cm.mkField(EndLineField, IsPublic, IsFinal, NotVolatile)
    cm.mkField(EndColField, IsPublic, IsFinal, NotVolatile)

    cm.mkMethod(Nil, ToStringMethod, IsPublic, NotFinal, toStringIns(_))

    cm.closeClassMaker()
  }

  def Constructor: ConstructorMethod = ConstructorMethod(
    this.desc, List(JavaClasses.String, CD_int, CD_int, CD_int, CD_int)
  )

  private def constructorIns(implicit mv: MethodVisitor): Unit = {
    thisLoad()
    INVOKESPECIAL(ClassConstants.Object.Constructor)
    thisLoad()
    ALOAD(1)
    PUTFIELD(SourceField)
    thisLoad()
    ILOAD(2)
    PUTFIELD(BeginLineField)
    thisLoad()
    ILOAD(3)
    PUTFIELD(BeginColField)
    thisLoad()
    ILOAD(4)
    PUTFIELD(EndLineField)
    thisLoad()
    ILOAD(5)
    PUTFIELD(EndColField)
    RETURN()
  }

  private def SourceField: InstanceField =
    InstanceField(this.desc, "source", JavaClasses.String)

  private def BeginLineField: InstanceField =
    InstanceField(this.desc, "beginLine", CD_int)

  private def BeginColField: InstanceField =
    InstanceField(this.desc, "beginCol", CD_int)

  private def EndLineField: InstanceField =
    InstanceField(this.desc, "endLine", CD_int)

  private def EndColField: InstanceField =
    InstanceField(this.desc, "endCol", CD_int)

  private def ToStringMethod: InstanceMethod = ClassConstants.Object.ToStringMethod.implementation(this.desc)

  private def toStringIns(implicit mv: MethodVisitor): Unit = {
    // create string builder
    NEW(JavaClasses.StringBuilder)
    DUP()
    INVOKESPECIAL(ClassConstants.StringBuilder.Constructor)
    // build string
    thisLoad()
    GETFIELD(SourceField)
    INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
    pushString(":")
    INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
    thisLoad()
    GETFIELD(BeginLineField)
    INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendInt32Method)
    pushString(":")
    INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendStringMethod)
    thisLoad()
    GETFIELD(BeginColField)
    INVOKEVIRTUAL(ClassConstants.StringBuilder.AppendInt32Method)
    // create the string
    INVOKEVIRTUAL(ClassConstants.Object.ToStringMethod)
    ARETURN()
  }

}
