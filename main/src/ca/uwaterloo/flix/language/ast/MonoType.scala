package ca.uwaterloo.flix.language.ast

/**
  * Representation of mono types.
  */
sealed trait MonoType {

  @deprecated("will be removed", "0.5")
  def typeConstructor: MonoType = this match {
    case MonoType.Array(_) => this
    case MonoType.Enum(_, _) => this
    case MonoType.Channel(_) => this
    case MonoType.Ref(_) => this
    case MonoType.Apply(t1, _) => t1.typeConstructor
    case _ => this
  }

  @deprecated("will be removed", "0.5")
  def typeArguments: List[MonoType] = this match {
    case MonoType.Array(tpe) => List(tpe)
    case MonoType.Channel(tpe) => List(tpe)
    case MonoType.Enum(_, targs) => targs
    case MonoType.Ref(tpe) => List(tpe)
    case MonoType.Apply(tpe1, tpe2) => tpe1.typeArguments ::: tpe2 :: Nil
    case _ => Nil
  }

  @deprecated("will be removed", "0.5")
  def isArrow: Boolean = typeConstructor match {
    case MonoType.Arrow(l) => true
    case _ => false
  }

  @deprecated("will be removed", "0.5")
  def isTuple: Boolean = typeConstructor match {
    case MonoType.Tuple(l) => true
    case _ => false
  }

}

object MonoType {

  ///
  /// Primitive Types.
  ///

  case object Unit extends MonoType

  case object Bool extends MonoType

  case object Char extends MonoType

  case object Float32 extends MonoType

  case object Float64 extends MonoType

  case object Int8 extends MonoType

  case object Int16 extends MonoType

  case object Int32 extends MonoType

  case object Int64 extends MonoType

  case object BigInt extends MonoType

  case object Str extends MonoType

  ///
  /// Compound Types.
  ///

  // TODO: Order like in type.

  case class Array(tpe: MonoType) extends MonoType

  case class Channel(tpe: MonoType) extends MonoType

  case class Enum(sym: Symbol.EnumSym, args: List[MonoType]) extends MonoType // TODO: We want elms here?

  case class Ref(tpe: MonoType) extends MonoType

  case object RecordEmpty extends MonoType

  case class RecordExtend(label: String, value: MonoType, rest: MonoType) extends MonoType

  case class Relation(sym: Symbol.RelSym, attr: List[MonoType], kind: Kind) extends MonoType

  case class Lattice(sym: Symbol.LatSym, attr: List[MonoType], kind: Kind) extends MonoType

  case class Schema(m: Map[Symbol.PredSym, MonoType]) extends MonoType

  case class Native(clazz: Class[_]) extends MonoType


  /**
    * A type expression that represents functions.
    */
  case class Arrow(length: Int) extends MonoType {
    def kind: Kind = Kind.Arrow((0 until length).map(_ => Kind.Star).toList, Kind.Star)
  }

  /**
    * A type constructor that represents tuples of the given `length`.
    */
  case class Tuple(length: Int) extends MonoType {
    def kind: Kind = Kind.Arrow((0 until length).map(_ => Kind.Star).toList, Kind.Star)
  }

  /**
    * A type expression that a type application tpe1[tpe2].
    */
  case class Apply(tpe1: MonoType, tpe2: MonoType) extends MonoType


  @deprecated("will be removed", "0.5")
  case class Var(id: Int, kind: Kind) extends MonoType {
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

  @deprecated("will be removed", "0.5")
  def mkArrow(a: MonoType, b: MonoType): MonoType = MonoType.Apply(MonoType.Apply(MonoType.Arrow(2), a), b)

  @deprecated("will be removed", "0.5")
  def mkArrow(as: List[MonoType], b: MonoType): MonoType = {
    as.foldRight(b)(mkArrow)
  }

}
