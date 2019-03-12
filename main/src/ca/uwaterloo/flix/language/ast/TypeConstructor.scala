package ca.uwaterloo.flix.language.ast

/**
  * Representation of type constructors.
  */
sealed trait TypeConstructor {
  def kind: Kind
}

object TypeConstructor {

  /**
    * A type constructor that represent the Unit type.
    */
  case object Unit extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent the type of 32-bit floating point numbers.
    */
  case object Float32 extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent the type of 64-bit floating point numbers.
    */
  case object Float64 extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent the type of 8-bit integers.
    */
  case object Int8 extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent the type of 16-bit integers.
    */
  case object Int16 extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent the type of 32-bit integers.
    */
  case object Int32 extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

}
