package impl.logic

sealed trait Type

object Type {

  /**
   * The type of variants.
   */
  case class Variant(t: IndexedSeq[Type]) extends Type

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
  case object String extends Type

  /**
   * The type of null-ary constructors.
   */
  case class Constructor0(name: Symbol.NamedSymbol) extends Type

  /**
   * The type of 1-ary constructors.
   */
  case class Constructor1(name: Symbol.NamedSymbol, t1: Type) extends Type

  /**
   * The type of 2-ary constructors.
   */
  case class Constructor2(name: Symbol.NamedSymbol, t1: Type, t2: Type) extends Type

  /**
   * The type of 3-ary constructors.
   */
  case class Constructor3(name: Symbol.NamedSymbol, t1: Type, t2: Type, t3: Type) extends Type

  /**
   * The type of 4-ary constructors.
   */
  case class Constructor4(name: Symbol.NamedSymbol, t1: Type, t2: Type, t3: Type, t4: Type) extends Type

  /**
   * The type of 5-ary constructors.
   */
  case class Constructor5(name: Symbol.NamedSymbol, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type) extends Type

}
