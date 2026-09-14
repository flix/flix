/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

/**
  * Represents the way a variable is bound.
  */
sealed trait BoundBy

object BoundBy {

  /**
    * Represents a variable that is bound by a formal parameter.
    */
  case object FormalParam extends BoundBy

  /**
    * Represents a variable that is bound by a let-binding.
    */
  case object Let extends BoundBy

  /**
    * Represents a variable that is bound by a pattern.
    */
  case object Pattern extends BoundBy

  /**
    * Represents a variable that is bound by a select.
    */
  case object SelectRule extends BoundBy

  /**
    * Represents a variable that is bound by a catch rule.
    */
  case object CatchRule extends BoundBy

  /**
    * Represents a variable that is bound by a constraint.
    */
  case object Constraint extends BoundBy

  /**
    * Represents a variable that is bound by a local def.
    */
  case object LocalDef extends BoundBy
}
