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
    def kind: Kind = Kind.Square
  }

  /**
    * A type constructor that represent the Null type.
    */
  case object Null extends TypeConstructor {
    def kind: Kind = Kind.Square
  }

  /**
    * A type constructor that represent the Bool type.
    */
  case object Bool extends TypeConstructor {
    def kind: Kind = Kind.Square
  }

  /**
    * A type constructor that represent the Char type.
    */
  case object Char extends TypeConstructor {
    def kind: Kind = Kind.Square
  }

  /**
    * A type constructor that represent the type of 32-bit floating point numbers.
    */
  case object Float32 extends TypeConstructor {
    def kind: Kind = Kind.Square
  }

  /**
    * A type constructor that represent the type of 64-bit floating point numbers.
    */
  case object Float64 extends TypeConstructor {
    def kind: Kind = Kind.Square
  }

  /**
    * A type constructor that represent the type of 8-bit integers.
    */
  case object Int8 extends TypeConstructor {
    def kind: Kind = Kind.Square
  }

  /**
    * A type constructor that represent the type of 16-bit integers.
    */
  case object Int16 extends TypeConstructor {
    def kind: Kind = Kind.Square
  }

  /**
    * A type constructor that represent the type of 32-bit integers.
    */
  case object Int32 extends TypeConstructor {
    def kind: Kind = Kind.Square
  }

  /**
    * A type constructor that represent the type of 64-bit integers.
    */
  case object Int64 extends TypeConstructor {
    def kind: Kind = Kind.Square
  }

  /**
    * A type constructor that represent the type of arbitrary-precision integers.
    */
  case object BigInt extends TypeConstructor {
    def kind: Kind = Kind.Square
  }

  /**
    * A type constructor that represent the type of strings.
    */
  case object Str extends TypeConstructor {
    def kind: Kind = Kind.Square
  }

  /**
    * A type constructor that represents the type of functions.
    */
  case class Arrow(arity: Int) extends TypeConstructor {
    def kind: Kind = Kind.Bool ->: Kind.mkArrow(arity)
  }

  /**
    * A type constructor that represents the type of empty records.
    */
  case object RecordEmpty extends TypeConstructor {
    def kind: Kind = Kind.Record
  }

  /**
    * A type constructor that represents the type of extended records.
    */
  case class RecordExtend(field: Name.Field) extends TypeConstructor {
    /**
      * The shape of an extended record is { field: type | rest }
      */
    def kind: Kind = Kind.Star ->: Kind.Record ->: Kind.Record
  }

  /**
    * A type constructor that represents the type of empty schemas.
    */
  case object SchemaEmpty extends TypeConstructor {
    def kind: Kind = Kind.Schema
  }

  /**
    * A type constructor that represents the type of extended schemas.
    */
  case class SchemaExtend(pred: Name.Pred) extends TypeConstructor {
    /**
      * The shape of an extended schema is { name: type | rest }
      */
    def kind: Kind = Kind.Star ->: Kind.Schema ->: Kind.Schema
  }

  /**
    * A type constructor that represent the type of arrays.
    */
  case object Array extends TypeConstructor {
    /**
      * The shape of an array is Array[t].
      */
    def kind: Kind = Kind.Star ->: Kind.Square
  }

  /**
    * A type constructor that represent the type of channels.
    */
  case object Channel extends TypeConstructor {
    /**
      * The shape of a channel is Channel[t].
      */
    def kind: Kind = Kind.Star ->: Kind.Square
  }

  /**
    * A type constructor that represent the type of lazy expressions.
    */
  case object Lazy extends TypeConstructor {
    /**
      * The shape of lazy is Lazy[t].
      */
    def kind: Kind = Kind.Star ->: Kind.Square
  }

  /**
    * A type constructor that represent the type of tags.
    */
  case class Tag(sym: Symbol.EnumSym, tag: Name.Tag) extends TypeConstructor {
    /**
      * The shape of a tag is "like" a function `caseType` -> (`resultType`) -> *.
      */
    def kind: Kind = Kind.Star ->: Kind.Star ->: Kind.Square
  }

  /**
    * A type constructor that represent the type of enums.
    */
  case class Enum(sym: Symbol.EnumSym, kind: Kind) extends TypeConstructor

  /**
    * A type constructor that represent the type of JVM classes.
    */
  case class Native(clazz: Class[_]) extends TypeConstructor {
    def kind: Kind = Kind.Square
  }

  /**
    * A type constructor that represent the type of scoped references.
    */
  case object ScopedRef extends TypeConstructor {
    /**
      * The shape of a reference is `ScopedRef[t, l]`.
      */
    def kind: Kind = Kind.Star ->: Kind.Bool ->: Kind.Square
    // MATT ???
  }

  /**
    * A type constructor that represent the type of tuples.
    */
  case class Tuple(l: Int) extends TypeConstructor {
    /**
      * The shape of a tuple is (t1, ..., tn).
      */
    def kind: Kind = Kind.mkArrow(l)
  }

  /**
    * A type constructor for relations.
    */
  case object Relation extends TypeConstructor {
    def kind: Kind = Kind.Star ->: Kind.Square
  }

  /**
    * A type constructor for lattices.
    */
  case object Lattice extends TypeConstructor {
    def kind: Kind = Kind.Star ->: Kind.Square
  }

  /**
    * A type constructor that represent the Boolean True.
    */
  case object True extends TypeConstructor {
    def kind: Kind = Kind.Bool
  }

  /**
    * A type constructor that represents the Boolean False.
    */
  case object False extends TypeConstructor {
    def kind: Kind = Kind.Bool
  }

  /**
    * A type constructor that represents the negation of an effect.
    */
  case object Not extends TypeConstructor {
    def kind: Kind = Kind.Bool ->: Kind.Bool
  }

  /**
    * A type constructor that represents the conjunction of two effects.
    */
  case object And extends TypeConstructor {
    def kind: Kind = Kind.Bool ->: Kind.Bool ->: Kind.Bool
  }

  /**
    * A type constructor that represents the disjunction of two effects.
    */
  case object Or extends TypeConstructor {
    def kind: Kind = Kind.Bool ->: Kind.Bool ->: Kind.Bool
  }

  /**
    * A type constructor that represent the type of regions.
    */
  case object Region extends TypeConstructor {
    /**
      * The shape of a region is Region[l].
      */
    def kind: Kind = Kind.Bool ->: Kind.Star
  }

  /**
    * A type constructor adding locality to square types.
    */
  case object Star extends TypeConstructor {
    /**
      * The shape of a Star type is Star[l, t]
      */
    def kind: Kind = Kind.Bool ->: Kind.Square ->: Kind.Star
  }

}
