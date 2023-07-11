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
import ca.uwaterloo.flix.language.ast.ReducedAst.Root

object GenFunctionAbstractClasses {
  def gen(arrows: Iterable[BackendObjType.Arrow])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    arrows.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, arrow) =>
        macc + (arrow.jvmName -> JvmClass(arrow.jvmName, arrow.genByteCode()))
    }
  }
}
