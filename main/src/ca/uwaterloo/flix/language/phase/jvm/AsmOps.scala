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

import org.objectweb.asm.ClassWriter

object AsmOps {

  /**
    * Returns a freshly created class writer object.
    *
    * The object is constructed to compute stack map frames automatically.
    */
  def mkClassWriter(): ClassWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES) {
    override def getCommonSuperClass(tpe1: String, tpe2: String): String = {
      JvmName.Object.toInternalName
    }
  }

}
