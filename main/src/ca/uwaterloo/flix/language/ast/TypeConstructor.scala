package ca.uwaterloo.flix.language.ast

/**
  * Representation of type constructors.
  */
sealed trait TypeConstructor {
  def kind: Kind
}

object TypeConstructor {

  /**
    * Represents the Float32 type constructor.
    */
  case object Float32 extends TypeConstructor {
    def kind: Kind = Kind.Star
  }


}