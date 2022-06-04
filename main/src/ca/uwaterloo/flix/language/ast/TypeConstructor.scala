package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.Ast.{EliminatedBy, IntroducedBy}
import ca.uwaterloo.flix.language.phase.{Kinder, Resolver, Typer}
import ca.uwaterloo.flix.util.InternalCompilerException

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
    * A type constructor that represent the Null type.
    */
  case object Null extends TypeConstructor {
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
    * A type constructor that represents the type of functions.
    */
  case class Arrow(arity: Int) extends TypeConstructor {
    def kind: Kind = Kind.Bool ->: Kind.mkArrow(arity)
  }

  /**
    * A type constructor that represents the type of empty record rows.
    */
  case object RecordRowEmpty extends TypeConstructor {
    def kind: Kind = Kind.RecordRow
  }

  /**
    * A type constructor that represents the type of extended record rows.
    */
  case class RecordRowExtend(field: Name.Field) extends TypeConstructor {
    /**
      * The shape of an extended record is { field :: type | rest }
      */
    def kind: Kind = Kind.Star ->: Kind.RecordRow ->: Kind.RecordRow
  }

  /**
    * A type constructor that represents the type of records.
    */
  case object Record extends TypeConstructor {
    /**
      * The shape of a record constructor is Record[row]
      */
    def kind: Kind = Kind.RecordRow ->: Kind.Star
  }

  /**
    * A type constructor that represents the type of empty schema rows.
    */
  case object SchemaRowEmpty extends TypeConstructor {
    def kind: Kind = Kind.SchemaRow
  }

  /**
    * A type constructor that represents the type of extended schema rows.
    */
  case class SchemaRowExtend(pred: Name.Pred) extends TypeConstructor {
    /**
      * The shape of an extended schema is { name :: type | rest }
      */
    def kind: Kind = Kind.Predicate ->: Kind.SchemaRow ->: Kind.SchemaRow
  }

  /**
    * A type constructor that represents the type of schemas.
    */
  case object Schema extends TypeConstructor {
    /**
      * The shape of a schema constructor is Schema[row]
      */
    def kind: Kind = Kind.SchemaRow ->: Kind.Star
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
    * A type constructor that represent the type of lazy expressions.
    */
  case object Lazy extends TypeConstructor {
    /**
      * The shape of lazy is Lazy[t].
      */
    def kind: Kind = Kind.Star ->: Kind.Star
  }

  /**
    * A type constructor that represent the type of tags.
    */
  @EliminatedBy(Typer.getClass)
  case class Tag(sym: Symbol.EnumSym, tag: Name.Tag) extends TypeConstructor {
    /**
      * The shape of a tag is "like" a function `caseType` -> (`resultType`) -> *.
      */
    def kind: Kind = Kind.Star ->: Kind.Star ->: Kind.Star
  }

  /**
    * A type constructor that represents the type of enums.
    */
  @IntroducedBy(Kinder.getClass)
  case class KindedEnum(sym: Symbol.EnumSym, kind: Kind) extends TypeConstructor

  /**
    * An unkinded type constructor that represents the type of enums.
    */
  @EliminatedBy(Kinder.getClass)
  case class UnkindedEnum(sym: Symbol.EnumSym) extends TypeConstructor {
    override def kind: Kind = throw InternalCompilerException("Attempt to access kind of unkinded type constructor")
  }

  /**
    * A type alias that has not yet been applied.
    *
    * Only exists temporarily in the Resolver.
    */
  @EliminatedBy(Resolver.getClass)
  case class UnappliedAlias(sym: Symbol.TypeAliasSym) extends TypeConstructor {
    override def kind: Kind = throw InternalCompilerException("Attempt to access kind of unkinded type constructor")
  }

  /**
    * A type constructor that represent the type of JVM classes.
    */
  case class Native(clazz: Class[_]) extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent the type of scoped arrays.
    */
  case object Array extends TypeConstructor {
    /**
      * The shape of a reference is `ScopedArray[t, l]`.
      */
    def kind: Kind = Kind.Star ->: Kind.Bool ->: Kind.Star
  }

  /**
    * A type constructor that represent the type of scoped references.
    */
  case object ScopedRef extends TypeConstructor {
    /**
      * The shape of a reference is `ScopedRef[t, l]`.
      */
    def kind: Kind = Kind.Star ->: Kind.Bool ->: Kind.Star
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
    def kind: Kind = Kind.Star ->: Kind.Predicate
  }

  /**
    * A type constructor for lattices.
    */
  case object Lattice extends TypeConstructor {
    def kind: Kind = Kind.Star ->: Kind.Predicate
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
    * A type constructor that represents the complement of an effect set.
    */
  case object Complement extends TypeConstructor {
    def kind: Kind = Kind.Effect ->: Kind.Effect
  }

  /**
    * A type constructor that represents the union of two effect sets.
    */
  case object Union extends TypeConstructor {
    def kind: Kind = Kind.Effect ->: Kind.Effect ->: Kind.Effect
  }

  /**
    * A type constructor that represents the intersection of two effect sets.
    */
  case object Intersection extends TypeConstructor {
    def kind: Kind = Kind.Effect ->: Kind.Effect ->: Kind.Effect
  }

  /**
    * A type constructor that represents the difference of two effect sets.
    */
  case object Difference extends TypeConstructor {
    def kind: Kind = Kind.Effect ->: Kind.Effect ->: Kind.Effect
  }

  /**
    * A type constructor that represents a single effect.
    */
  case class Effect(sym: Symbol.EffectSym) extends TypeConstructor {
    def kind: Kind = Kind.Effect
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

}
