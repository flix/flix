/*
 * Copyright 2024 Matthew Lutze
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
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.Symbol

case class Scope(syms: List[Symbol.KindedTypeVarSym]) {

  /**
    * Returns the scope corresponding to the given region sym, nested inside the current region.
    */
  def enter(sym: Symbol.KindedTypeVarSym): Scope = Scope(sym :: syms)

  /**
    * Returns true iff `this` scope is outside of `that` scope.
    */
  def isOutside(that: Scope): Boolean = {
    // In principle, we should check that `this.syms` is a suffix of `that.syms`,
    // but checking length is sufficient.

    this.syms.length < that.syms.length
  }
}

object Scope {

  /**
    * The scope that is not inside any region.
    */
  // TODO LEVELS is declaration level higher?
  val Top: Scope = Scope(Nil)

}
