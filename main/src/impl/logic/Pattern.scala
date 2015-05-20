package impl.logic

sealed trait Pattern {
  /**
   * Returns all free variables in the pattern.
   */
  def freeVars: Set[Symbol.VariableSymbol] = this match {
    case Pattern.Wildcard => Set.empty
    case Pattern.Var(x) => Set(x)
    case Pattern.Unit => Set.empty
    case Pattern.Bool(b) => Set.empty
    case Pattern.Int(i) => Set.empty
    case Pattern.Str(s) => Set.empty
    case Pattern.Tag(n, p) => p.freeVars
    case Pattern.Tuple2(p1, p2) => p1.freeVars ++ p2.freeVars
    case Pattern.Tuple3(p1, p2, p3) => p1.freeVars ++ p2.freeVars ++ p3.freeVars
    case Pattern.Tuple4(p1, p2, p3, p4) => p1.freeVars ++ p2.freeVars ++ p3.freeVars ++ p4.freeVars
    case Pattern.Tuple5(p1, p2, p3, p4, p5) => p1.freeVars ++ p2.freeVars ++ p3.freeVars ++ p4.freeVars ++ p5.freeVars
  }
}

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
  case class Tag(name: Symbol.NamedSymbol, p: Pattern) extends Pattern

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
