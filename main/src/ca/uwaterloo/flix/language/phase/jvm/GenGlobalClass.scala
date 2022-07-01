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

/**
  * OBS: A copy of this generated class has to be maintained at main/src/dev/flix/runtime/Global.java.
  */
object GenGlobalClass {
  def gen()(implicit flix: Flix): Map[JvmName, JvmClass] = {
    Map(Global.jvmName -> JvmClass(Global.jvmName, Global.genByteCode()))
  }
}
