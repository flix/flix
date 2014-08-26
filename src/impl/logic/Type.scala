package impl.logic

sealed trait Type {

  def isSetMap: Boolean = ???

  def isLatMap: Boolean = ???

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
