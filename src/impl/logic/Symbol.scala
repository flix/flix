package impl.logic

trait Symbol

object Symbol {

  /**
   * A predicate symbol.
   */
  case class PredicateSymbol(s: String) extends Symbol

  /**
   * A named symbol.
   */
  case class NamedSymbol(s: String) extends Symbol

  /**
   * A variable symbol.
   */
  case class VariableSymbol(s: String) extends Symbol

}
