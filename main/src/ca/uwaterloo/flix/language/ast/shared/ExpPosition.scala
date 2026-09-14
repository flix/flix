/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared


/**
  * A common super-type that represents an expression position (tail position or not).
  */
sealed trait ExpPosition

object ExpPosition {
  /**
    * Represents an expression in tail position.
    */
  case object Tail extends ExpPosition

  /**
    * Represents an expression in non-tail position.
    */
  case object NonTail extends ExpPosition
}
