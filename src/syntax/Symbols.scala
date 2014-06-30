package syntax

import impl.logic.Symbol._

import scala.language.implicitConversions

/**
 * Embedded DSL syntax for symbols.
 */
object Symbols {

  /**
   * Implicitely converts a string to a predicate symbol.
   */
  implicit def string2predicate(s: String): PredicateSymbol = PredicateSymbol(s)

  /**
   * Implicitely converts a string to a named symbol.
   */
  implicit def string2named(s: String): NamedSymbol = NamedSymbol(s)

  /**
   * Implicitely converts a string to a variable symbol.
   */
  implicit def string2variable(s: String): VariableSymbol = VariableSymbol(s)

  implicit class String2Symbol(s: String) {
    /**
     * Explicitely converts a string to a predicate symbol.
     */
    def asP: PredicateSymbol = PredicateSymbol(s)

    /**
     * Explicitely converts a string to a named symbol.
     */
    def asN: NamedSymbol = NamedSymbol(s)

    /**
     * Explicitely converts a string to a variable symbol.
     */
    def asVar: VariableSymbol = VariableSymbol(s)
  }

}
