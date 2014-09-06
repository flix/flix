package impl.logic

sealed trait Pattern

object Pattern {

  /**
   * A wildcard pattern.
   */
  case object Wildcard extends Pattern

  /**
   * A variable pattern.
   */
  case class Var(s: Symbol.VariableSymbol) extends Pattern

  /**
   * The Unit pattern.
   */
  case object Unit extends Pattern

  /**
   * A boolean pattern.
   */
  case class Bool(b: scala.Boolean) extends Pattern

  /**
   * An integer pattern.
   */
  case class Int(i: scala.Int) extends Pattern

  /**
   * A string pattern.
   */
  case class Str(s: java.lang.String) extends Pattern

  /**
   * A tagged pattern.
   */
  case class Tagged(name: Symbol.NamedSymbol, p: Pattern) extends Pattern

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
