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

/**
  * A copy of this generated class has to be maintained at main/src/dev/flix/runtime/GlobalCounter.java.
  */
object GenGlobalCounterClass {

  private val NewIdMethodName: String = "newId"
  private val counterField: StaticField =
    StaticField(JvmName.GlobalCounter, "counter", JvmName.AtomicLong.toTpe)

  def gen()(implicit flix: Flix): Map[JvmName, JvmClass] = {
    Map(JvmName.GlobalCounter -> JvmClass(JvmName.GlobalCounter, genByteCode()))
  }

  private def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(JvmName.GlobalCounter, IsFinal)

    cm.mkObjectConstructor(IsPrivate)
    cm.mkStaticConstructor(genStaticConstructor())
    counterField.mkStaticField(cm, IsPublic, IsFinal)
    cm.mkStaticMethod(genNewIdMethod(), NewIdMethodName, MethodDescriptor(Nil, BackendType.Int64), IsPublic, IsFinal)

    cm.closeClassMaker()
  }

  private def genStaticConstructor(): InstructionSet =
    NEW(JvmName.AtomicLong) ~
      DUP() ~ invokeConstructor(JvmName.AtomicLong) ~
      counterField.putStaticField() ~
      RETURN()

  private def genNewIdMethod()(implicit flix: Flix): InstructionSet =
    counterField.getStaticField() ~
      INVOKEVIRTUAL(JvmName.AtomicLong, "getAndIncrement", MethodDescriptor(Nil, BackendType.Int64)) ~
      LRETURN()

}
