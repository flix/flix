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

object GenUnitClass {

  val InstanceFieldName = "INSTANCE"

  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    val unitType = JvmType.Unit
    val unitName = unitType.name
    val bytecode = genByteCode(unitType, unitName)
    Map(unitName -> JvmClass(unitName, bytecode))
  }

  private def genByteCode(unitType: JvmType, unitName: JvmName)(implicit flix: Flix): Array[Byte] = {
    val classMaker = ClassMaker.mkClass(unitName)

    // Singleton instance
    classMaker.mkStaticField(InstanceFieldName, unitType)

    classMaker.mkStaticConstructor(genStaticConstructor(unitType, unitName))
    classMaker.mkPrivateConstructor(genConstructor(), JvmName.Descriptors.NothingToVoid)

    classMaker.closeClassMaker
  }

  private def genStaticConstructor(unitType: JvmType, unitName: JvmName): Instruction = {
    NEW(unitName) ~
      DUP ~
      InvokeSimpleConstructor(unitName) ~
      PUTSTATIC(unitName, InstanceFieldName, unitType) ~
      RETURN
  }

  private def genConstructor(): Instruction = {
    ALOAD(0) ~
      InvokeSimpleConstructor(JvmName.Object) ~
      RETURN
  }

}
