/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.PType._
import ca.uwaterloo.flix.language.ast.{ErasedAst, PType, RType, Symbol}

object GenDefClasses {

  def gen(defs: Map[Symbol.DefnSym, ErasedAst.Def[PReference[PFunction]]])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    defs.foldLeft(Map[JvmName, JvmClass]()) {
      case (macc, (sym, defn)) =>
        val defnName: JvmName = ???
        macc + (defnName -> JvmClass(defnName, ???))
    }
  }

}
