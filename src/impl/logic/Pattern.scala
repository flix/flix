package impl.logic

sealed trait Pattern

object Pattern {

  /**
   * A variable pattern.
   */
  case class Var(s: Symbol.VariableSymbol) extends Pattern

  /**
   * A tagged pattern.
   */
  case class Tagged()

  /**
   * A 2-tuple pattern.
   */
  case class Tuple2(p1: Pattern, p2: Pattern) extends Pattern

  /**
   * A 3-tuple pattern.
   */
  case class Tuple3(p1: Pattern, p2: Pattern, p3: Pattern) extends Pattern

  /**
   * A 4-tuple pattern.
   */
  case class Tuple4(p1: Pattern, p2: Pattern, p3: Pattern, p4: Pattern) extends Pattern

  /**
   * A 5-tuple pattern.
   */
  case class Tuple5(p1: Pattern, p2: Pattern, p3: Pattern, p4: Pattern, p5: Pattern) extends Pattern

}
