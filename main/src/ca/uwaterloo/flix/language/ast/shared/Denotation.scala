/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

/**
  * A common super-type for the denotation of an atom.
  */
sealed trait Denotation

object Denotation {

  /**
    * The atom has a relational denotation.
    */
  case object Relational extends Denotation

  /**
    * The atom has a latticenal denotation.
    */
  case object Latticenal extends Denotation

}
