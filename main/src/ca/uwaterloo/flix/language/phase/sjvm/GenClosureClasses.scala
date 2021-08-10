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
import ca.uwaterloo.flix.util.ParOps

object GenClosureClasses {

  def gen(closures: Set[ClosureInfo])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {

    ParOps.parAgg(closures, Map.empty[JvmName, JvmClass])({
      case (macc, closure) =>
        val cloName = closure.sym.cloName
        val bytecode = genByteCode(root.defs(closure.sym), cloName, squeezeFunction(squeezeReference(closure.tpe)))
        macc + (cloName -> JvmClass(cloName, bytecode))
    }, _ ++ _)

  }

  private def genByteCode(defn: ErasedAst.Def, defName: JvmName, functionType: RArrow)(implicit root: Root, flix: Flix): Array[Byte] = {
    ???
  }

  def genInvokeFunction[T <: PType](defn: ErasedAst.Def, functionBody: ErasedAst.Expression[T], defName: JvmName, functionType: RArrow): F[StackNil] => F[StackEnd] = {
    ???
  }

  def magicReversePutField[R <: Stack, T <: PType](className: JvmName, resultType: RType[T]): F[R ** T ** PReference[PAnyObject]] => F[R] = f => {
    ???
  }

  def magicStoreArg[R <: Stack, T <: PType](index: Int, tpe: RType[T], defName: JvmName, sym: Symbol.VarSym): F[R] => F[R] = {
    ???
  }

}