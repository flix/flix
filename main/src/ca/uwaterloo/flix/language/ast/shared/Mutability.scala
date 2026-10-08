/*
 * Copyright 2025 Casper Dalgaard Nielsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

/**
  * Represents whether a struct is mutable.
  */
sealed trait Mutability

object Mutability {

  /**
    * The struct is immutable.
    */
  case object Immutable extends Mutability

  /**
    * The struct is mutable.
    */
  case object Mutable extends Mutability

}
