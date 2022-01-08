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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor

object GenFlixErrorClass {

  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    Map(JvmName.FlixError -> JvmClass(JvmName.FlixError, genByteCode()))
  }

  private def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkAbstractClass(JvmName.FlixError, JvmName.Error)

    cm.mkConstructor(genConstructor(), mkDescriptor(BackendObjType.String.toTpe)(VoidableType.Void), IsPublic)

    cm.closeClassMaker()
  }

  private def genConstructor()(implicit flix: Flix): InstructionSet =
    thisLoad() ~
      ALOAD(1) ~
      invokeConstructor(JvmName.Error, mkDescriptor(BackendObjType.String.toTpe)(VoidableType.Void)) ~
      RETURN()
}
