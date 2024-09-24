package ca.uwaterloo.flix.language.ast

/** A common super-type that captures the rigidity of a type variable. */
sealed trait Rigidity

object Rigidity {

  /** Denotes a type variable that is flexible, i.e. can be unified with other variables and types. */
  case object Flexible extends Rigidity

  /** Denotes a type variable that is rigid, i.e. cannot be unified with anything other than itself. */
  case object Rigid extends Rigidity
}
