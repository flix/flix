package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.api.Flix

// MATT license
// MATT docs
sealed trait UnkindedType

object UnkindedType {
  case class Cst(cst: Constructor, loc: SourceLocation)

  case class Apply(t1: UnkindedType, t2: UnkindedType)

  case class Lambda(t1: UnkindedType.Var, t2: UnkindedType)

  case class Var(id: Int, text: Option[String] = None)

  // MATT docs
  def freshVar(text: Option[String] = None)(implicit flix: Flix): UnkindedType.Var = {
    Var(flix.genSym.freshId(), text)
  }

  trait Constructor

  object Constructor {

    /**
      * A type constructor that represent the Unit type.
      */
    case object Unit extends Constructor

    /**
      * A type constructor that represent the Null type.
      */
    case object Null extends Constructor

    /**
      * A type constructor that represent the Bool type.
      */
    case object Bool extends Constructor

    /**
      * A type constructor that represent the Char type.
      */
    case object Char extends Constructor

    /**
      * A type constructor that represent the type of 32-bit floating point numbers.
      */
    case object Float32 extends Constructor

    /**
      * A type constructor that represent the type of 64-bit floating point numbers.
      */
    case object Float64 extends Constructor

    /**
      * A type constructor that represent the type of 8-bit integers.
      */
    case object Int8 extends Constructor

    /**
      * A type constructor that represent the type of 16-bit integers.
      */
    case object Int16 extends Constructor

    /**
      * A type constructor that represent the type of 32-bit integers.
      */
    case object Int32 extends Constructor

    /**
      * A type constructor that represent the type of 64-bit integers.
      */
    case object Int64 extends Constructor

    /**
      * A type constructor that represent the type of arbitrary-precision integers.
      */
    case object BigInt extends Constructor

    /**
      * A type constructor that represent the type of strings.
      */
    case object Str extends Constructor

    /**
      * A type constructor that represents the type of functions.
      */
    case class Arrow(arity: Int) extends Constructor

    /**
      * A type constructor that represents the type of empty records.
      */
    case object RecordEmpty extends Constructor

    /**
      * A type constructor that represents the type of extended records.
      */
    case class RecordExtend(field: Name.Field) extends Constructor

    /**
      * A type constructor that represents the type of empty schemas.
      */
    case object SchemaEmpty extends Constructor

    /**
      * A type constructor that represents the type of extended schemas.
      */
    case class SchemaExtend(pred: Name.Pred) extends Constructor

    /**
      * A type constructor that represent the type of arrays.
      */
    case object Array extends Constructor

    /**
      * A type constructor that represent the type of channels.
      */
    case object Channel extends Constructor

    /**
      * A type constructor that represent the type of lazy expressions.
      */
    case object Lazy extends Constructor

    /**
      * A type constructor that represent the type of tags.
      */
    case class Tag(sym: Symbol.EnumSym, tag: Name.Tag) extends Constructor

    /**
      * A type constructor that represent the type of enums.
      */
    case class Enum(sym: Symbol.EnumSym) extends Constructor

    /**
      * A type constructor that represent the type of JVM classes.
      */
    case class Native(clazz: Class[_]) extends Constructor

    /**
      * A type constructor that represent the type of references.
      */
    case object Ref extends Constructor

    /**
      * A type constructor that represent the type of tuples.
      */
    case class Tuple(l: Int) extends Constructor

    /**
      * A type constructor for relations.
      */
    case object Relation extends Constructor

    /**
      * A type constructor for lattices.
      */
    case object Lattice extends Constructor

    /**
      * A type constructor that represent the Boolean True.
      */
    case object True extends Constructor

    /**
      * A type constructor that represents the Boolean False.
      */
    case object False extends Constructor

    /**
      * A type constructor that represents the negation of an effect.
      */
    case object Not extends Constructor

    /**
      * A type constructor that represents the conjunction of two effects.
      */
    case object And extends Constructor

    /**
      * A type constructor that represents the disjunction of two effects.
      */
    case object Or extends Constructor
  }
}