package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.util.InternalCompilerException

/**
  * Representation of mono types.
  */
sealed trait MonoType {

  /**
    * Returns the type constructor of `this` type.
    *
    * For example,
    *
    * Celsius                       =>      Celsius
    * Option[Int]                   =>      Option
    * Arrow[Bool, Char]             =>      Arrow
    * Tuple[Bool, Int]              =>      Tuple
    * Result[Bool, Int]             =>      Result
    * Result[Bool][Int]             =>      Result
    * Option[Result[Bool, Int]]     =>      Option
    */
  def typeConstructor: MonoType = this match {
    case MonoType.Apply(t1, _) => t1.typeConstructor
    case _ => this
  }

  /**
    * Returns the type arguments of `this` type.
    *
    * For example,
    *
    * Celsius                       =>      Nil
    * Option[Int]                   =>      Int :: Nil
    * Arrow[Bool, Char]             =>      Bool :: Char :: Nil
    * Tuple[Bool, Int]              =>      Bool :: Int :: Nil
    * Result[Bool, Int]             =>      Bool :: Int :: Nil
    * Result[Bool][Int]             =>      Bool :: Int :: Nil
    * Option[Result[Bool, Int]]     =>      Result[Bool, Int] :: Nil
    */
  def typeArguments: List[MonoType] = this match {
    case MonoType.Apply(tpe1, tpe2) => tpe1.typeArguments ::: tpe2 :: Nil
    case _ => Nil
  }


  /**
    * Returns `true` if `this` type is an array type.
    */
  def isArray: Boolean = typeConstructor match {
    case MonoType.Array => true
    case _ => false
  }

  /**
    * Returns `true` if `this` type is an arrow type.
    */
  def isArrow: Boolean = typeConstructor match {
    case MonoType.Arrow(l) => true
    case _ => false
  }

  /**
    * Returns `true` if `this` type is an enum type.
    */
  def isEnum: Boolean = typeConstructor match {
    case MonoType.Enum(sym, kind) => true
    case _ => false
  }

  /**
    * Returns `true` if `this` type is a tuple type.
    */
  def isTuple: Boolean = typeConstructor match {
    case MonoType.Tuple(l) => true
    case _ => false
  }

  /**
    * Returns `true` if `this` type is a reference type.
    */
  def isRef: Boolean = typeConstructor match {
    case MonoType.Ref => true
    case _ => false
  }

}

object MonoType {

  /////////////////////////////////////////////////////////////////////////////
  // MonoTypes                                                                   //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * A type variable expression.
    */
  case class Var(id: Int, kind: Kind) extends MonoType {
    /**
      * The optional textual name of `this` type variable.
      */
    private var text: Option[String] = None

    /**
      * Optionally returns the textual name of `this` type variable.
      */
    def getText: Option[String] = text

    /**
      * Sets the textual name of `this` type variable.
      */
    def setText(s: String): Unit = {
      text = Some(s)
    }

    /**
      * Returns `true` if `this` type variable is equal to `o`.
      */
    override def equals(o: scala.Any): Boolean = o match {
      case that: Var => this.id == that.id
      case _ => false
    }

    /**
      * Returns the hash code of `this` type variable.
      */
    override def hashCode(): Int = id
  }

  /**
    * A type constructor that represents the unit value.
    */
  case object Unit extends MonoType {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent boolean values.
    */
  case object Bool extends MonoType {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent character values.
    */
  case object Char extends MonoType {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent 32-bit floating point numbers.
    */
  case object Float32 extends MonoType {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent 64-bit floating point numbers.
    */
  case object Float64 extends MonoType {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent 8-bit signed integers.
    */
  case object Int8 extends MonoType {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent 16-bit signed integers.
    */
  case object Int16 extends MonoType {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent 32-bit signed integers.
    */
  case object Int32 extends MonoType {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent 64-bit signed integers.
    */
  case object Int64 extends MonoType {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent arbitrary-precision integers.
    */
  case object BigInt extends MonoType {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent strings.
    */
  case object Str extends MonoType {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent channels.
    */
  case object Channel extends MonoType {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent arrays.
    */
  case object Array extends MonoType {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent vectors.
    */
  case object Vector extends MonoType {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent native objects.
    */
  case class Native(clazz: Class[_]) extends MonoType {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represents references.
    */
  case object Ref extends MonoType {
    def kind: Kind = Kind.Star
  }

  /**
    * A type expression that represents functions.
    */
  case class Arrow(length: Int) extends MonoType {
    def kind: Kind = Kind.Arrow((0 until length).map(_ => Kind.Star).toList, Kind.Star)
  }

  /**
    * A type constructor that represents enums.
    *
    * @param sym  the symbol of the enum.
    * @param kind the kind of the enum.
    */
  case class Enum(sym: Symbol.EnumSym, kind: Kind) extends MonoType

  /**
    * A type constructor that represents a relation with attributes of the given types.
    *
    * @param sym  the symbol of the relation.
    * @param attr the attribute types of the relation.
    * @param kind the kind of the relation.
    */
  case class Relation(sym: Symbol.RelSym, attr: List[MonoType], kind: Kind) extends MonoType

  /**
    * A type constructor that represents a lattice with attributes of the given types.
    *
    * @param sym  the symbol of the lattice.
    * @param attr the attribute types of the relation.
    * @param kind the kind of the lattice.
    */
  case class Lattice(sym: Symbol.LatSym, attr: List[MonoType], kind: Kind) extends MonoType

  /**
    * A type constructor that represents a schema.
    *
    * @param m the types of the predicate symbols in the system.
    */
  case class Schema(m: Map[Symbol.PredSym, MonoType]) extends MonoType {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represents tuples of the given `length`.
    */
  case class Tuple(length: Int) extends MonoType {
    def kind: Kind = Kind.Arrow((0 until length).map(_ => Kind.Star).toList, Kind.Star)
  }

  /**
    * A type constructor that represents the empty record type.
    */
  case object RecordEmpty extends MonoType {
    def kind: Kind = ??? // TODO
  }

  /**
    * A type constructor that represents a record extension type.
    */
  case class RecordExtend(label: String, value: MonoType, rest: MonoType) extends MonoType {
    def kind: Kind = ??? // TODO
  }

  /**
    * A type constructor that represents zero.
    */
  case object Zero extends MonoType {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represents the successor of a type.
    */
  case class Succ(len: Int, t: MonoType) extends MonoType {
    def kind: Kind = Kind.Star
  }

  /**
    * A type expression that a type application tpe1[tpe2].
    */
  case class Apply(tpe1: MonoType, tpe2: MonoType) extends MonoType

  /**
    * Return the inner type of the channel
    *
    * For example given Channel[Int] return Int.
    */
  def getChannelInnerMonoType(tpe: MonoType): MonoType = {
    tpe match {
      case MonoType.Apply(MonoType.Channel, t) => t
      case _ => throw InternalCompilerException(s"Excepted channel type. Actual type: '$tpe' ")
    }
  }

  /**
    * Return the inner type of the array or vector
    *
    * For example given Array[Int] return Int,
    * and given Vector[Int, 5] return Int.
    */
  def getArrayInnerMonoType(tpe: MonoType): MonoType = {
    tpe match {
      case MonoType.Apply(MonoType.Array, t) => t
      case MonoType.Apply(MonoType.Apply(MonoType.Vector, t), _) => t
      case _ => throw InternalCompilerException(s"Excepted array or vector type. Actual type: '$tpe' ")
    }
  }

  /**
    * Constructs the arrow type A -> B.
    */
  def mkArrow(a: MonoType, b: MonoType): MonoType = MonoType.Apply(MonoType.Apply(MonoType.Arrow(2), a), b)

  /**
    * Constructs the arrow type A_1 -> .. -> A_n -> B.
    */
  def mkArrow(as: List[MonoType], b: MonoType): MonoType = {
    as.foldRight(b)(mkArrow)
  }

}
