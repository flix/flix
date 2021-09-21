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
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.{ErasedAst, PType}
import ca.uwaterloo.flix.util.ParOps

object GenEnumInterfaces {

  def gen(enums: Map[Symbol.EnumSym, ErasedAst.Enum[_ <: PType]])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
//    ParOps.parAgg(defs, Map[JvmName, JvmClass]())({
//      case (macc, (sym, defn)) =>
//        val functionType = squeezeFunction(squeezeReference(defn.tpe)).asInstanceOf[RArrow[PType]]
//        val defnCasted = defn.asInstanceOf[ErasedAst.Def[PType]]
//        macc + (sym.defName -> JvmClass(sym.defName, genByteCode(defnCasted, sym.defName, functionType)))
//    }, _ ++ _)
    ???
  }

}
