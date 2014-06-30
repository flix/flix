package impl.logic

trait Symbol

object Symbol {

  /**
   * A predicate symbol.
   */
  case class PredicateSymbol(s: String)

  /**
   * A variable symbol
   */
  case class VariableSymbol(s: String)

}
