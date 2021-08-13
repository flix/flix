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
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.ast.{ErasedAst, PType, RType, Symbol}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.language.phase.sjvm.ClassMaker.Mod
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._
import ca.uwaterloo.flix.util.ParOps
import org.objectweb.asm.Opcodes

object GenTagClasses {

    def gen[A](a: A): A = a
//  def gen(ts: Set[TagInfo])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
//    // TODO(JLS): impl
//    ParOps.parAgg(defs, Map[JvmName, JvmClass]())({
//      case (macc, (sym, defn)) =>
//        if (SjvmOps.nonLaw(defn) && nonClosureFunctions.contains(sym)) {
//          val functionType = squeezeFunction(squeezeReference(defn.tpe))
//          macc + (sym.defName -> JvmClass(sym.defName, genByteCode(defn, sym.defName, functionType)))
//        } else macc
//    }, _ ++ _)
//  }

}
