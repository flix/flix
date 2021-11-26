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
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions.Branch.{FalseBranch, TrueBranch}
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Finality._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Instancing._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility._
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor

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

    cm.mkConstructor(genConstructor(), mkDescriptor(JvmName.ReifiedSourceLocation.toObjTpe.toTpe)(VoidableType.Void), Public)

    // TODO: Are these ever used?
    cm.mkMethod(genEquals(), "equals", mkDescriptor(JvmName.Object.toObjTpe.toTpe)(BackendType.Bool), Public, NonFinal, NonStatic)
    cm.mkMethod(genHashCode(), "hashCode", mkDescriptor()(BackendType.Int32), Public, NonFinal, NonStatic)
    cm.mkField(LocationFieldName, JvmName.ReifiedSourceLocation.toObjTpe.toTpe, Public, Final, NonStatic)

    cm.closeClassMaker
  }

  private def genConstructor(): InstructionSet = {
    val stringBuilderDescriptor = mkDescriptor(BackendObjType.String.toTpe)(JvmName.StringBuilder.toObjTpe.toTpe)
    loadThis() ~
      NEW(JvmName.StringBuilder) ~
      DUP() ~
      invokeConstructor(JvmName.StringBuilder) ~
      pushString("Non-exhaustive match at ") ~
      INVOKEVIRTUAL(JvmName.StringBuilder, "append", stringBuilderDescriptor) ~
      ALOAD(1) ~
      INVOKEVIRTUAL(JvmName.ReifiedSourceLocation, "toString", mkDescriptor()(BackendObjType.String.toTpe)) ~
      INVOKEVIRTUAL(JvmName.StringBuilder, "append", stringBuilderDescriptor) ~
      INVOKEVIRTUAL(JvmName.StringBuilder, "toString", mkDescriptor()(BackendObjType.String.toTpe)) ~
      invokeConstructor(JvmName.FlixError, mkDescriptor(BackendObjType.String.toTpe)(VoidableType.Void)) ~
      loadThis() ~
      ALOAD(1) ~
      PUTFIELD(JvmName.MatchError, LocationFieldName, JvmName.ReifiedSourceLocation.toObjTpe.toTpe) ~
      RETURN()
  }

  private def genEquals(): InstructionSet =
    loadThis() ~
      ALOAD(1) ~
      IF_ACMPNE {
        case TrueBranch =>
          ALOAD(1) ~
            IFNULL {
              case TrueBranch =>
                pushBool(false) ~ IRETURN()
              case FalseBranch =>
                loadThis() ~
                  INVOKEVIRTUAL(JvmName.Object, "getClass", mkDescriptor()(JvmName.Class.toObjTpe.toTpe)) ~
                  ALOAD(1) ~
                  INVOKEVIRTUAL(JvmName.Object, "getClass", mkDescriptor()(JvmName.Class.toObjTpe.toTpe)) ~
                  IF_ACMPNE {
                    case TrueBranch =>
                      ALOAD(1) ~
                        CHECKCAST(JvmName.MatchError) ~
                        ASTORE(2) ~
                        loadThis() ~
                        GETFIELD(JvmName.MatchError, LocationFieldName, JvmName.ReifiedSourceLocation.toObjTpe.toTpe) ~
                        ALOAD(2) ~
                        GETFIELD(JvmName.MatchError, LocationFieldName, JvmName.ReifiedSourceLocation.toObjTpe.toTpe) ~
                        INVOKESTATIC(JvmName.Objects, "equals", mkDescriptor(JvmName.Object.toObjTpe.toTpe, JvmName.Object.toObjTpe.toTpe)(BackendType.Bool)) ~
                        IRETURN()
                    case FalseBranch =>
                      pushBool(false) ~ IRETURN()
                  }
            }
        case FalseBranch =>
          pushBool(true) ~ IRETURN()
      }

  private def genHashCode(): InstructionSet =
    ICONST_1() ~
      ANEWARRAY(JvmName.Object) ~
      DUP() ~
      ICONST_0() ~
      loadThis() ~
      GETFIELD(JvmName.MatchError, LocationFieldName, JvmName.ReifiedSourceLocation.toObjTpe.toTpe) ~
      AASTORE() ~
      INVOKESTATIC(JvmName.Object, "hash", mkDescriptor(BackendType.Array(JvmName.Object.toObjTpe.toTpe))(BackendType.Int32)) ~
      IRETURN()
}
