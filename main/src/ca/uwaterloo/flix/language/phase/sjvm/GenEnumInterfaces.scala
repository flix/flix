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

package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.ast.{ErasedAst, Symbol}
import ca.uwaterloo.flix.util.ParOps

object GenEnumInterfaces {

  def gen(enums: Map[Symbol.EnumSym, ErasedAst.Enum])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    ParOps.parAgg(enums, Map[JvmName, JvmClass]())({
      case (macc, (_, enum)) =>
        val name = squeezeReference(enum.tpeDeprecated).jvmName
        macc + (name -> JvmClass(name, genByteCode(name)))
    }, _ ++ _)
  }

  private def genByteCode(name: JvmName)(implicit root: Root, flix: Flix): Array[Byte] = {
    ClassMaker.mkInterface(name).closeClassMaker
  }

}
