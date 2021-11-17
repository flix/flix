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

/**
  * A copy of this generated class has to be maintained at main/src/dev/flix/runtime/GlobalCounter.java.
  */
object GenGlobalCounterClass {

  val NewIdMethodName: String = "newId"

  private val counterFieldName: String = "counter"

  def gen()(implicit flix: Flix): Map[JvmName, JvmClass] = {
    Map(JvmName.GlobalCounter -> JvmClass(JvmName.GlobalCounter, genByteCode()))
  }

  private def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(JvmName.GlobalCounter, Public, Final)

    cm.mkConstructor(genConstructor(), JvmName.MethodDescriptor.NothingToVoid, Private)
    cm.mkStaticConstructor(genStaticConstructor())
    cm.mkField(counterFieldName, JvmType.AtomicLong, Private, Final, Static)
    cm.mkMethod(genNewIdMethod(), NewIdMethodName, MethodDescriptor(Nil, JvmType.PrimLong), Public, Final, Static)

    cm.closeClassMaker
  }

  private def genConstructor(): InstructionSet = {
    ALOAD(0) ~
      invokeConstructor(JvmName.Object) ~
      RETURN()
  }

  private def genStaticConstructor(): InstructionSet = {
    NEW(JvmName.AtomicLong) ~
      DUP() ~
      invokeConstructor(JvmName.AtomicLong) ~
      PUTSTATIC(JvmName.AtomicLong, counterFieldName, JvmType.AtomicLong) ~
      RETURN()
  }

  private def genNewIdMethod()(implicit flix: Flix): InstructionSet = {
    GETSTATIC(JvmName.GlobalCounter, counterFieldName, JvmType.AtomicLong) ~
      INVOKEVIRTUAL(JvmName.AtomicLong, "getAndIncrement", MethodDescriptor(Nil, JvmType.PrimLong)) ~
      LRETURN()
  }

}
