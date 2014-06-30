package syntax

import impl.logic.{PredicateSymbol, VariableSymbol}

/**
 * Embedded DSL syntax for symbols.
 */
object Symbols {

  /**
   * Implicitely converts a string to a predicate symbol.
   */
  implicit def string2predicate(s: String): PredicateSymbol = PredicateSymbol(s)

  /**
   * Implicitely converts a string to a variable symbol.
   */
  implicit def string2variable(s: String): VariableSymbol = VariableSymbol(s)

  implicit class String2Symbol(s: String) {
    /**
     * Explicitely converts a string to a predicate symbol.
     */
    def p: PredicateSymbol = PredicateSymbol(s)

    /**
     * Explicitely converts a string to a variable symbol.
     */
    def v: VariableSymbol = VariableSymbol(s)
  }

}
