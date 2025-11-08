package ca.uwaterloo.flix.language.ast.shared

sealed trait Mutable

/**
  * Represents whether a struct is mutable
  */
object Mutable {

  /**
    * The struct is immutable.
    */
  case object Immutable extends Mutable

  /**
    * The struct is mutable.
    */
  case object Mutable extends Mutable

}
