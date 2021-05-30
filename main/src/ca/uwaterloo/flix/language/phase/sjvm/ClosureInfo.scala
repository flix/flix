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

import ca.uwaterloo.flix.language.ast.ErasedAst.FreeVar
import ca.uwaterloo.flix.language.ast.PRefType.PAnyObject
import ca.uwaterloo.flix.language.ast.PType.PReference
import ca.uwaterloo.flix.language.ast.{PRefType, PType, RType, Symbol}

/**
  * Meta information about a closure.
  */
case class ClosureInfo(sym: Symbol.DefnSym, freeVars: List[FreeVar], tpe: RType[PReference[PAnyObject]]) {
  /**
    * Returns the hash code of `this` closure info.
    */
  override def hashCode(): Int = 7 * sym.hashCode + 11 * freeVars.hashCode()

  /**
    * Returns `true` if the given `obj` is the same closure info as `this`.
    */
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: ClosureInfo => this.sym == that.sym && this.freeVars == that.freeVars
    case _ => false
  }
}
