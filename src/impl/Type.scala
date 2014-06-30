package impl

import impl.logic.Value

sealed trait Type

object Type {

  // TODO: Bounded vs. Unbounded types?
  // TODO: Allow nested relations/maps???
  // TODO: Replace nominal by Constructor0

  /**
   * The type of named atoms.
   */
  case class Nominal(name: Symbol) extends Type

  /**
   * The type of booleans.
   */
  case object Boolean extends Type

  /**
   * The type of integers.
   */
  case object Integer extends Type

  /**
   * The type of strings.
   */
  case object String extends Type

  /**
   * The type of variants.
   */
  case class Variant(t: Map[Symbol, Type]) extends Type

  /**
   * The type of lattices.
   */
  case class Lattice(elms: Type, bot: Value, order: Set[HornClause], join: Set[HornClause]) extends Type

  /**
   * The type of null-ary constructors.
   */
  case class Constructor0(name: Symbol) extends Type

  /**
   * The type of 1-ary constructors.
   */
  case class Constructor1(name: Symbol, t1: Type) extends Type

  /**
   * The type of 2-ary constructors.
   */
  case class Constructor2(name: Symbol, t1: Type, t2: Type) extends Type

  /**
   * The type of 3-ary constructors.
   */
  case class Constructor3(name: Symbol, t1: Type, t2: Type, t3: Type) extends Type

  /**
   * The type of 4-ary constructors.
   */
  case class Constructor4(name: Symbol, t1: Type, t2: Type, t3: Type, t4: Type) extends Type

  /**
   * The type of 5-ary constructors.
   */
  case class Constructor5(name: Symbol, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type) extends Type

}
