/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

/**
  * A common super-type that represents whether a formal parameter is structurally decreasing.
  */
sealed trait Decreasing

object Decreasing {
  /**
    * The parameter is not observed as structurally decreasing.
    */
  case object NonDecreasing extends Decreasing

  /**
    * The parameter is a strict substructure at a recursive call site.
    */
  case object StrictlyDecreasing extends Decreasing
}
