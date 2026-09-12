/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.Name


object Stratification {
  /**
    * Represents the empty stratification.
    */
  val empty: Stratification = Stratification(Map.empty)
}

/**
  * Represents a stratification that maps every predicate symbol to its stratum.
  */
case class Stratification(m: Map[Name.Pred, Int])
