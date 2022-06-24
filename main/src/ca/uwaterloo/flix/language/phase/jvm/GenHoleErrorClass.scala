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

package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.InstanceField
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor

object GenHoleErrorClass {

  private val holeField: InstanceField =
    InstanceField(JvmName.HoleError, IsPublic, IsFinal, "hole", BackendObjType.String.toTpe)
  private val locationField: InstanceField =
    InstanceField(JvmName.HoleError, IsPublic, IsFinal, "location", BackendObjType.ReifiedSourceLocation.toTpe)

  def gen()(implicit flix: Flix): Map[JvmName, JvmClass] = {
    Map(JvmName.HoleError -> JvmClass(JvmName.HoleError, genByteCode()))
  }

  private def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(JvmName.HoleError, IsFinal, BackendObjType.FlixError.jvmName)

    cm.mkConstructor(genConstructor(), mkDescriptor(BackendObjType.String.toTpe, BackendObjType.ReifiedSourceLocation.toTpe)(VoidableType.Void), IsPublic)
    cm.mkMethod(BackendObjType.JavaObject.EqualsMethod.implementation(JvmName.HoleError, Some(genEqualsMethod())))
    cm.mkMethod(BackendObjType.JavaObject.HashcodeMethod.implementation(JvmName.HoleError, Some(genHashCodeMethod())))
    cm.mkField(holeField)
    cm.mkField(locationField)

    cm.closeClassMaker()
  }

  private def genConstructor(): InstructionSet = {
    def stringBuilderAppend(): InstructionSet = INVOKEVIRTUAL(JvmName.StringBuilder, "append",
      mkDescriptor(BackendObjType.String.toTpe)(JvmName.StringBuilder.toTpe))

    withName(1, BackendObjType.String.toTpe) { hole =>
      withName(2, BackendObjType.ReifiedSourceLocation.toTpe) { loc =>
        thisLoad() ~
          // create an error msg
          NEW(JvmName.StringBuilder) ~
          DUP() ~
          invokeConstructor(JvmName.StringBuilder) ~
          pushString("Hole '") ~ stringBuilderAppend() ~
          hole.load() ~ stringBuilderAppend() ~
          pushString("' at ") ~ stringBuilderAppend() ~
          loc.load() ~ INVOKEVIRTUAL(BackendObjType.JavaObject.ToStringMethod) ~ stringBuilderAppend() ~
          INVOKEVIRTUAL(BackendObjType.JavaObject.ToStringMethod) ~
          INVOKESPECIAL(BackendObjType.FlixError.Constructor) ~
          // save the arguments locally
          thisLoad() ~ hole.load() ~ PUTFIELD(holeField) ~
          thisLoad() ~ loc.load() ~ PUTFIELD(locationField) ~
          RETURN()
      }
    }
  }

  private def genEqualsMethod(): InstructionSet = {
    def objectEquals(): InstructionSet = INVOKESTATIC(JvmName.Objects, "equals",
      mkDescriptor(BackendObjType.JavaObject.toTpe, BackendObjType.JavaObject.toTpe)(BackendType.Bool))

    withName(1, BackendObjType.JavaObject.toTpe) { other =>
      // check exact equality
      thisLoad() ~ other.load() ~
        ifTrue(Condition.ACMPEQ)(pushBool(true) ~ IRETURN()) ~
        // check for null
        other.load() ~
        ifTrue(Condition.NULL)(pushBool(false) ~ IRETURN()) ~
        // check for class equality
        thisLoad() ~
        INVOKEVIRTUAL(BackendObjType.JavaObject.GetClassMethod) ~
        other.load() ~
        INVOKEVIRTUAL(BackendObjType.JavaObject.GetClassMethod) ~
        ifTrue(Condition.ACMPNE)(pushBool(false) ~ IRETURN()) ~
        // cast the other obj
        other.load() ~ CHECKCAST(JvmName.HoleError) ~
        storeWithName(2, JvmName.HoleError.toTpe) { otherHoleError =>
          // compare the hole field
          thisLoad() ~ GETFIELD(holeField) ~
            otherHoleError.load() ~ GETFIELD(holeField) ~
            objectEquals() ~
            ifTrue(Condition.EQ)(pushBool(false) ~ IRETURN()) ~
            // compare the location field
            thisLoad() ~ GETFIELD(locationField) ~
            otherHoleError.load() ~ GETFIELD(locationField) ~
            objectEquals() ~
            IRETURN()
        }
    }
  }

  private def genHashCodeMethod(): InstructionSet =
    ICONST_2() ~
      ANEWARRAY(BackendObjType.JavaObject.jvmName) ~
      // store hole
      DUP() ~
      ICONST_0() ~
      thisLoad() ~ GETFIELD(holeField) ~
      AASTORE() ~
      // store location
      DUP() ~
      ICONST_1() ~
      thisLoad() ~ GETFIELD(locationField) ~
      AASTORE() ~
      // hash the array
      INVOKESTATIC(JvmName.Objects, "hash", mkDescriptor(BackendType.Array(BackendObjType.JavaObject.toTpe))(BackendType.Int32)) ~
      IRETURN()
}
