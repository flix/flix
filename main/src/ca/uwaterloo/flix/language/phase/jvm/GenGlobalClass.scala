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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.StaticField
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility._
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import org.objectweb.asm.Opcodes

/**
  * A copy of this generated class has to be maintained at main/src/dev/flix/runtime/Global.java.
  */
object GenGlobalClass {

  private val newIdMethodName: String = "newId"
  private val counterField: StaticField =
    StaticField(JvmName.Global, "counter", JvmName.AtomicLong.toTpe)

  private val getArgsMethodName: String = "getArgs"
  private val setArgsMethodName: String = "setArgs"
  private val argsField: StaticField = StaticField(JvmName.Global, "args",
    BackendType.Array(BackendObjType.String.toTpe))

  def gen()(implicit flix: Flix): Map[JvmName, JvmClass] = {
    Map(JvmName.Global -> JvmClass(JvmName.Global, genByteCode()))
  }

  private def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(JvmName.Global, IsFinal)

    cm.mkObjectConstructor(IsPrivate)
    cm.mkStaticConstructor(genStaticConstructor())
    counterField.mkStaticField(cm, IsPrivate, IsFinal)
    cm.mkStaticMethod(genNewIdMethod(), newIdMethodName,
      MethodDescriptor(Nil, BackendType.Int64),
      IsPublic, IsFinal)

    argsField.mkStaticField(cm, IsPrivate, NotFinal)
    val stringArrayType = BackendType.Array(BackendObjType.String.toTpe)
    cm.mkStaticMethod(genGetArgsMethod(), getArgsMethodName,
      MethodDescriptor(Nil, stringArrayType),
      IsPublic, IsFinal)
    cm.mkStaticMethod(genSetArgsMethod(), setArgsMethodName,
      MethodDescriptor(List(stringArrayType), VoidableType.Void),
      IsPublic, IsFinal)
    cm.closeClassMaker()
  }

  private def genStaticConstructor(): InstructionSet =
    NEW(JvmName.AtomicLong) ~
      DUP() ~ invokeConstructor(JvmName.AtomicLong) ~
      counterField.putStaticField() ~
      ACONST_NULL() ~
      argsField.putStaticField() ~
      RETURN()

  private def genNewIdMethod()(implicit flix: Flix): InstructionSet =
    counterField.getStaticField() ~
      INVOKEVIRTUAL(JvmName.AtomicLong, "getAndIncrement",
        MethodDescriptor(Nil, BackendType.Int64)) ~
      LRETURN()

  private def genGetArgsMethod()(implicit flix: Flix): InstructionSet = {
    def arrayCopy(): InstructionSet = (f: F) => {
      f.visitMethodInstruction(Opcodes.INVOKESTATIC, JvmName.System, "arraycopy",
        MethodDescriptor(List(
          JvmName.Object.toTpe,
          BackendType.Int32,
          JvmName.Object.toTpe,
          BackendType.Int32,
          BackendType.Int32
        ), VoidableType.Void))
      f
    }

    argsField.getStaticField() ~
      ARRAYLENGTH() ~
      ANEWARRAY(BackendObjType.String.jvmName) ~
      ASTORE(0) ~
      // the new array is now created, now to copy the args
      argsField.getStaticField() ~
      ICONST_0() ~
      ALOAD(0) ~
      ICONST_0() ~
      argsField.getStaticField() ~ ARRAYLENGTH() ~
      arrayCopy() ~
      ALOAD(0) ~
      ARETURN()
  }


  private def genSetArgsMethod()(implicit flix: Flix): InstructionSet =
    ALOAD(0) ~ argsField.putStaticField() ~ RETURN()

}
