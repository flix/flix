/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.language.ast.Symbol

sealed trait PackageModules {
  def contains(sym: Symbol.ModuleSym): Boolean = this match {
    case PackageModules.All => true
    case PackageModules.Selected(included) => included.contains(sym)
  }
}

object PackageModules {

  case object All extends PackageModules

  case class Selected(included: Set[Symbol.ModuleSym]) extends PackageModules

}
