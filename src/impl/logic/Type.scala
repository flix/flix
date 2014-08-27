package impl.logic

sealed trait Type {

  /**
   * Returns the "right-most" or "result" type of a the type.
   */
  def resultType: Type = this match {
    case Type.Function(typ1, typ2) => typ2.resultType
    case _ => this
  }

  /**
   * Returns `true` iff the type is a set type or a function type which ultimately results in a set type.
   */
  def isSetMap: Boolean = this match {
    case Type.Set(typ) => true
    case Type.Function(typ1, typ2) => typ2.isSetMap
    case _ => false
  }

  /**
   * Returns `true` iff the type is a lat type or a function type which ultimately results in a lat type.
   */
  def isLatMap: Boolean = this match {
    case Type.Lat(typ) => true
    case Type.Function(typ1, typ2) => typ2.isLatMap
    case _ => false
  }

}

object Type {

  /**
   * The type of the Unit value.
   */
  case object Unit extends Type

  /**
   * The type of booleans.
   */
  case object Bool extends Type

  /**
   * The type of integers.
   */
  case object Int extends Type

  /**
   * The type of strings.
   */
  case object Str extends Type

  /**
   * The type of functions.
   */
  case class Function(typ1: Type, typ2: Type) extends Type

  /**
   * The type of tagged types.
   */
  case class Tagged(name: Symbol.NamedSymbol, typ: Type) extends Type

  /**
   * The type of sums.
   */
  case class Sum(ts: List[Type]) extends Type

  /**
   * The type of sets.
   */
  case class Set(typ1: Type) extends Type

  /**
   * The type of lattices.
   */
  case class Lat(typ1: Type) extends Type

  /**
   * The type of 2-tuples.
   */
  case class Tuple2(typ1: Type, typ2: Type) extends Type

  /**
   * The type of 3-tuples.
   */
  case class Tuple3(typ1: Type, typ2: Type, typ3: Type) extends Type

  /**
   * The type of 4-tuples.
   */
  case class Tuple4(t1: Type, typ2: Type, typ3: Type, typ4: Type) extends Type

  /**
   * The type of 5-tuples.
   */
  case class Tuple5(t1: Type, typ2: Type, typ3: Type, typ4: Type, typ5: Type) extends Type

}
