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
    * A type constructor that represent the Bool type.
    */
  case object Bool extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent the Char type.
    */
  case object Char extends TypeConstructor {
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

  /**
    * A type constructor that represent the type of 64-bit integers.
    */
  case object Int64 extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent the type of arbitrary-precision integers.
    */
  case object BigInt extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent the type of strings.
    */
  case object Str extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent the type of arrays.
    */
  case object Array extends TypeConstructor {
    /**
      * The shape of an array is Array[t].
      */
    def kind: Kind = Kind.Star -> Kind.Star
  }

  /**
    * A type constructor that represent the type of channels.
    */
  case object Channel extends TypeConstructor {
    /**
      * The shape of a channel is Channel[t].
      */
    def kind: Kind = Kind.Star -> Kind.Star
  }

  /**
    * A type constructor that represent the type of enums.
    */
  case class Enum(sym: Symbol.EnumSym, kind: Kind) extends TypeConstructor

  /**
    * A type constructor that represent the type of JVM classes.
    */
  case class Native(clazz: Class[_]) extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent the type of references.
    */
  case object Ref extends TypeConstructor {
    /**
      * The shape of a reference is Ref[t].
      */
    def kind: Kind = Kind.Star -> Kind.Star
  }

  /**
    * A type constructor that represent the type of tuples.
    */
  case class Tuple(l: Int) extends TypeConstructor {
    /**
      * The shape of a tuple is (t1, ..., tn).
      */
    def kind: Kind = ??? // TODO
  }

  /**
    * A type constructor that represent the type of vectors.
    */
  case object Vector extends TypeConstructor {
    /**
      * The shape of a vector is Array[t;n].
      */
    def kind: Kind = (Kind.Star -> Kind.Nat) -> Kind.Star
  }

  /**
    * A type constructor that represents a relation.
    *
    * @param sym the symbol of the relation.
    */
  case class Relation(sym: Symbol.RelSym) extends TypeConstructor {
    def kind: Kind = Kind.Star -> Kind.Star
  }

  /**
    * A type constructor that represents a lattice.
    *
    * @param sym the symbol of the lattice.
    */
  case class Lattice(sym: Symbol.LatSym) extends TypeConstructor {
    def kind: Kind = Kind.Star -> Kind.Star
  }

}
