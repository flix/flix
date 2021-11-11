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
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker._

object GenUnitClass {

  val InstanceFieldName = "INSTANCE"

  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    val unitType = JvmType.Unit
    val unitName = unitType.name
    val bytecode = genByteCode()
    Map(unitName -> JvmClass(unitName, bytecode))
  }

  private def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = mkClass(JvmType.Unit.name, Public, Final)

    // Singleton instance
    cm.mkField(InstanceFieldName, JvmType.Unit, Public, Final, Static)

    cm.mkStaticConstructor(genStaticConstructor())
    cm.mkConstructor(genConstructor(), JvmName.Descriptors.NothingToVoid, Private)

    cm.closeClassMaker
  }

  private def genStaticConstructor(): Instruction = {
    NEW(JvmType.Unit.name) ~
      DUP ~
      InvokeSimpleConstructor(JvmType.Unit.name) ~
      PUTSTATIC(JvmType.Unit.name, InstanceFieldName, JvmType.Unit) ~
      RETURN
  }

  private def genConstructor(): Instruction = {
    ALOAD(0) ~
      InvokeSimpleConstructor(JvmName.Object) ~
      RETURN
  }

}
