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

  /**
   * An internal counter.
   */
  private var Counter: Int = 0;

  /**
   * Returns a fresh function symbol.
   */
  def freshVariableSymbol(prefix: String): VariableSymbol = {
    Counter += 1
    VariableSymbol(prefix + Counter)
  }

}
