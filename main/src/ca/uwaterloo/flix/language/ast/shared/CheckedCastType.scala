/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

/**
  * A common supertype for casts.
  */
sealed trait CheckedCastType

object CheckedCastType {

  /**
    * Represents a checked type cast.
    */
  case object TypeCast extends CheckedCastType

  /**
    * Represents a checked effect cast.
    */
  case object EffectCast extends CheckedCastType

}
