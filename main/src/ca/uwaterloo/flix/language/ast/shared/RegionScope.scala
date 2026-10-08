/*
 * Copyright 2024 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.Symbol

case class RegionScope(syms: List[Symbol.RegionSym]) {

  /**
    * Returns the scope corresponding to the given region sym, nested inside the current region.
    */
  def enter(sym: Symbol.RegionSym): RegionScope = RegionScope(sym :: syms)

  /**
    * Returns true iff `this` scope is outside of `that` scope.
    */
  def isOutside(that: RegionScope): Boolean = {
    // In principle, we should check that `this.syms` is a suffix of `that.syms`,
    // but checking length is sufficient.

    this.syms.length < that.syms.length
  }
}

object RegionScope {

  /**
    * The scope that is not inside any region.
    */
  // TODO LEVELS is declaration level higher?
  val Top: RegionScope = RegionScope(Nil)

}
