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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Finality._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Instancing._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility._
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor
import org.objectweb.asm.Opcodes

object GenMatchErrorClass {

  val LocationFieldName: String = "location"

  /**
    * Creates a subclass of `dev.flix.runtime.FlixError` with a
    * `dev.flix.runtime.ReifiedSourceLocation` and a string prefix of the message.
    * Includes equals and hashCode methods.
    */
  def gen()(implicit flix: Flix): Map[JvmName, JvmClass] = {
    Map(JvmName.MatchError -> JvmClass(JvmName.MatchError, genByteCode()))
  }

  private def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(JvmName.MatchError, Public, Final, superClass = JvmName.FlixError)

    cm.mkConstructor(genConstructor(), mkDescriptor(JvmType.ReifiedSourceLocation)(JvmType.Void), Public)
    cm.mkMethod(genEquals(), "equals", mkDescriptor(JvmType.Object)(JvmType.PrimBool), Public, Implementable, Instanced)
    cm.mkMethod(genHashCode(), "hashCode", mkDescriptor()(JvmType.PrimInt), Public, Implementable, Instanced)
    cm.mkField(LocationFieldName, JvmType.ReifiedSourceLocation, Public, Final, Instanced)

    cm.closeClassMaker
  }

  private def genConstructor(): InstructionSet = {
    val stringBuilderDescriptor = mkDescriptor(JvmType.String)(JvmType.StringBuilder)
    ALOAD(0) ~
      NEW(JvmName.StringBuilder) ~
      DUP() ~
      invokeConstructor(JvmName.StringBuilder) ~
      pushString("Non-exhaustive match at ") ~
      INVOKEVIRTUAL(JvmName.StringBuilder, "append", stringBuilderDescriptor) ~
      ALOAD(1) ~
      INVOKEVIRTUAL(JvmName.ReifiedSourceLocation, "toString", mkDescriptor()(JvmType.String)) ~
      INVOKEVIRTUAL(JvmName.StringBuilder, "append", stringBuilderDescriptor) ~
      INVOKEVIRTUAL(JvmName.StringBuilder, "toString", mkDescriptor()(JvmType.String)) ~
      invokeConstructor(JvmName.FlixError, mkDescriptor(JvmType.String)(JvmType.Void)) ~
      ALOAD(0) ~
      ALOAD(1) ~
      PUTFIELD(JvmName.MatchError, LocationFieldName, JvmType.ReifiedSourceLocation) ~
      RETURN()
  }

  private def genEquals(): InstructionSet = {
    ALOAD(0) ~
      ALOAD(1) ~
      IF_ACMPNE {
        ALOAD(1) ~
          IFNULL {
            pushBool(false) ~ IRETURN()
          } {
            ALOAD(0) ~
              INVOKEVIRTUAL(JvmName.Object, "getClass", MethodDescriptor(Nil, JvmType.Class)) ~
              ALOAD(1) ~
              INVOKEVIRTUAL(JvmName.Object, "getClass", MethodDescriptor(Nil, JvmType.Class)) ~
              IF_ACMPNE {
                ALOAD(1) ~
                  CHECKCAST(JvmName.MatchError) ~
                  ASTORE(2) ~
                  ALOAD(0) ~
                  GETFIELD(JvmName.MatchError, LocationFieldName, JvmType.ReifiedSourceLocation) ~
                  ALOAD(2) ~
                  GETFIELD(JvmName.MatchError, LocationFieldName, JvmType.ReifiedSourceLocation) ~
                  INVOKESTATIC(JvmName.Objects, "equals", MethodDescriptor(List(JvmType.Object, JvmType.Object), JvmType.PrimBool)) ~
                  IRETURN()
              } {
                pushBool(false) ~ IRETURN()
              }
          }
      } {
        pushBool(true) ~ IRETURN()
      }
  }

  private def genHashCode(): InstructionSet = {
    ICONST_1() ~
      ANEWARRAY(JvmName.Object) ~
      DUP() ~
      ICONST_0() ~
      ALOAD(0) ~
      GETFIELD(JvmName.MatchError, LocationFieldName, JvmType.ReifiedSourceLocation) ~
      AASTORE() ~
      cheat(_.visitMethodInsn(Opcodes.INVOKESTATIC, JvmName.Objects.toInternalName, "hash", s"([${JvmName.Object.toDescriptor})${JvmType.PrimInt.toDescriptor}", false)) ~
      IRETURN()
  }
}
