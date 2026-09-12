/*
 * Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

/**
  * A common super-type for the fixity of an atom.
  */
sealed trait Fixity

object Fixity {

  /**
    * The atom is loose (it does not have to be fully materialized before it can be used).
    */
  case object Loose extends Fixity

  /**
    * The atom is fixed (it must be fully materialized before it can be used).
    */
  case object Fixed extends Fixity

}
