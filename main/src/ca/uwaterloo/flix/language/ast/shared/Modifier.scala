/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

/**
  * A common super-type for modifiers.
  */
sealed trait Modifier

object Modifier {

  /**
    * The mutable modifier.
    */
  case object Mutable extends Modifier

  /**
    * The public modifier.
    */
  case object Public extends Modifier

  /**
    * The redefinition modifier.
    */
  case object Redef extends Modifier

  /**
    * The sealed modifier.
    */
  case object Sealed extends Modifier

  /**
    * The synthetic modifier.
    */
  case object Synthetic extends Modifier

}
