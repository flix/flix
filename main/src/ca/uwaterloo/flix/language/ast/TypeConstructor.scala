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
    def kind: Kind = Kind.Star ->: Kind.Star
  }

  /**
    * A type constructor that represent the type of channels.
    */
  case object Channel extends TypeConstructor {
    /**
      * The shape of a channel is Channel[t].
      */
    def kind: Kind = Kind.Star ->: Kind.Star
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
    def kind: Kind = Kind.Star ->: Kind.Star
  }

  /**
    * A type constructor that represent the type of tuples.
    */
  case class Tuple(l: Int) extends TypeConstructor {
    /**
      * The shape of a tuple is (t1, ..., tn).
      */
    def kind: Kind =
      if (l == 0)
        Kind.Star
      else
        Kind.Arrow(List.fill(l)(Kind.Star), Kind.Star)
  }

  /**
    * A type constructor that represent the type of vectors.
    */
  case object Vector extends TypeConstructor {
    /**
      * The shape of a vector is Array[t;n].
      */
    def kind: Kind = Kind.Star ->: Kind.Nat ->: Kind.Star
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
  case class RecordExtend(label: String) extends TypeConstructor {
    /**
      * The shape of an extended record is Record[t;r]
      */
    def kind: Kind = Kind.Star ->: Kind.Record ->: Kind.Record
  }

  // MATT docs
  case object SchemaEmpty extends TypeConstructor {
    def kind: Kind = Kind.Schema
  }

  // MATT docs
  case class SchemaExtend(sym: String) extends TypeConstructor {
    // MATT docs
    def kind: Kind = Kind.Predicate ->: Kind.Schema ->: Kind.Schema
  }

  /**
    * A type constructor for relations.
    */
  case object Relation extends TypeConstructor {
    def kind: Kind = Kind.Star ->: Kind.Predicate
  }

  /**
    * A type constructor for lattices.
    */
  case object Lattice extends TypeConstructor {
    def kind: Kind = Kind.Star ->: Kind.Predicate
  }

  /**
    * A type constructor that represents a pure effect.
    *
    * Pure represents TRUE in the Boolean algebra.
    */
  case object Pure extends TypeConstructor {
    def kind: Kind = Kind.Effect
  }

  /**
    * A type constructor that represents an impure effect.
    *
    * Impure represents FALSE in the Boolean algebra.
    */
  case object Impure extends TypeConstructor {
    def kind: Kind = Kind.Effect
  }

  /**
    * A type constructor that represents the negation of an effect.
    */
  case object Not extends TypeConstructor {
    def kind: Kind = Kind.Effect ->: Kind.Effect
  }

  /**
    * A type constructor that represents the conjunction of two effects.
    */
  case object And extends TypeConstructor {
    def kind: Kind = Kind.Effect ->: Kind.Effect ->: Kind.Effect
  }

  /**
    * A type constructor that represents the disjunction of two effects.
    */
  case object Or extends TypeConstructor {
    def kind: Kind = Kind.Effect ->: Kind.Effect ->: Kind.Effect
  }

  /**
    * Create an enum type constructor with the given arity.
    */
  def mkEnumCst(sym: Symbol.EnumSym, arity: Int): Enum = {
    val kind = Seq.fill(arity + 1)(Kind.Star: Kind).reduceRight((k, acc) => k ->: acc)
    Enum(sym, kind)
  }
}
