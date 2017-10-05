/*
 * Copyright 2017 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.ExecutableAst.Root

object GenTagClasses {

  /**
    * Returns the set of tuple interfaces for the given set of types `ts`.
    */
  def gen(ts: Set[TagInfo])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    ts.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, tag) =>
        val jvmType = JvmOps.getTagClassType(tag)
        val jvmName = jvmType.name
        val bytecode = genByteCode(tag)
        macc + (jvmName -> JvmClass(jvmName, bytecode))
    }
  }

  /**
    * Returns the bytecode for the given tag.
    */
  private def genByteCode(tag: TagInfo): Array[Byte] = {
    List(0xCA.toByte, 0xFE.toByte, 0xBA.toByte, 0xBE.toByte).toArray
  }

}
