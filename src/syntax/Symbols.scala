package syntax

import impl.logic.Symbol
import impl.logic.Symbol._

import scala.language.implicitConversions

/**
 * Embedded DSL syntax for symbols.
 */
object Symbols {

  /**
   * Rich Symbols.
   */
  implicit class RichSymbol(s: Symbol) {
    def fmt: String = s match {
      case Symbol.FunctionSymbol(x) => x
      case Symbol.PredicateSymbol(x) => x
      case Symbol.NamedSymbol(x) => x
      case Symbol.TypeSymbol(x) => x
      case Symbol.VariableSymbol(x) => x
    }
  }

  /**
   * Implicitely converts a string to a function symbol.
   */
  implicit def string2function(s: String): FunctionSymbol = FunctionSymbol(s)

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

}
