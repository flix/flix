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
import ca.uwaterloo.flix.language.phase.jvm.BackendObjType.Global
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility._
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import org.objectweb.asm.Opcodes

/**
  * A copy of this generated class has to be maintained at main/src/dev/flix/runtime/Global.java.
  */
object GenGlobalClass {

  def gen()(implicit flix: Flix): Map[JvmName, JvmClass] = {
    val name = Global.jvmName
    Map(name -> JvmClass(name, genByteCode()))
  }

  private def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(Global.jvmName, IsFinal)

    cm.mkObjectConstructor(IsPrivate)
    cm.mkStaticConstructor(genStaticConstructor())
    cm.mkField(Global.CounterField)
    cm.mkStaticMethod(genNewIdMethod(), Global.NewIdMethod.name,
      Global.NewIdMethod.d, IsPublic, IsFinal)

    cm.mkField(Global.ArgsField)
    cm.mkStaticMethod(genGetArgsMethod(), Global.GetArgsMethod.name,
      Global.GetArgsMethod.d, IsPublic, IsFinal)
    cm.mkStaticMethod(genSetArgsMethod(), Global.SetArgsMethod.name,
      Global.SetArgsMethod.d, IsPublic, IsFinal)
    cm.closeClassMaker()
  }

  private def genStaticConstructor(): InstructionSet =
    NEW(JvmName.AtomicLong) ~
      DUP() ~ invokeConstructor(JvmName.AtomicLong) ~
      PUTSTATIC(Global.CounterField) ~
      ICONST_0() ~
      ANEWARRAY(BackendObjType.String.jvmName) ~
      PUTSTATIC(Global.ArgsField) ~
      RETURN()

  private def genNewIdMethod()(implicit flix: Flix): InstructionSet =
    GETSTATIC(Global.CounterField) ~
      INVOKEVIRTUAL(JvmName.AtomicLong, "getAndIncrement",
        MethodDescriptor(Nil, BackendType.Int64)) ~
      LRETURN()

  private def arrayCopy(): InstructionSet = (f: F) => {
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

  private def genGetArgsMethod()(implicit flix: Flix): InstructionSet =
    GETSTATIC(Global.ArgsField) ~
      ARRAYLENGTH() ~
      ANEWARRAY(BackendObjType.String.jvmName) ~
      ASTORE(0) ~
      // the new array is now created, now to copy the args
      GETSTATIC(Global.ArgsField) ~
      ICONST_0() ~
      ALOAD(0) ~
      ICONST_0() ~
      GETSTATIC(Global.ArgsField) ~ ARRAYLENGTH() ~
      arrayCopy() ~
      ALOAD(0) ~
      ARETURN()


  private def genSetArgsMethod()(implicit flix: Flix): InstructionSet =
    ALOAD(0) ~
      ARRAYLENGTH() ~
      ANEWARRAY(BackendObjType.String.jvmName) ~
      ASTORE(1) ~
      ALOAD(0) ~
      ICONST_0() ~
      ALOAD(1) ~
      ICONST_0() ~
      ALOAD(0) ~ ARRAYLENGTH() ~
      arrayCopy() ~
      ALOAD(1) ~ PUTSTATIC(Global.ArgsField) ~ RETURN()

}
