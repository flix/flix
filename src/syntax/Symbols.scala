package syntax

import impl.logic.{PredicateSymbol, VariableSymbol}

class Symbols {

  implicit class SymbolOps(s: String) {
    def p: PredicateSymbol = ???

    def v: VariableSymbol = ???
  }

}
