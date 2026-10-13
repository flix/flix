/*
 * Copyright 2024 Hogler Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

/**
  * A common super-type for the polarity of an atom.
  */
sealed trait Polarity

object Polarity {

  /**
    * The atom is positive.
    */
  case object Positive extends Polarity

  /**
    * The atom is negative.
    */
  case object Negative extends Polarity

}
