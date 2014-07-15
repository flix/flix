package impl.logic

sealed trait Symbol

object Symbol {

  /**
   * A function symbol.
   */
  case class FunctionSymbol(s: String) extends Symbol

  /**
   * A predicate symbol.
   */
  case class PredicateSymbol(s: String) extends Symbol

  /**
   * A lattice symbol.
   */
  case class LatticeSymbol(s: String) extends Symbol

  /**
   * A named symbol.
   */
  case class NamedSymbol(s: String) extends Symbol

  /**
   * A variable symbol.
   */
  case class VariableSymbol(s: String) extends Symbol

}
