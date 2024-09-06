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
