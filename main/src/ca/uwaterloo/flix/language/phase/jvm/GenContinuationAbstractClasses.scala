/*
 * Copyright 2017 Magnus Madsen
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

object GenContinuationAbstractClasses {
  def gen(conts: Iterable[BackendObjType.Continuation])(implicit flix: Flix): Map[JvmName, JvmClass] = {
    conts.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, contType) =>
        macc + (contType.jvmName -> JvmClass(contType.jvmName, contType.genByteCode()))
    }
  }
}
