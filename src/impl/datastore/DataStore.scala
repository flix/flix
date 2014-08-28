package impl.datastore

import impl.logic.{Predicate, Value}
import impl.logic.Symbol

trait DataStore {
  def newFact(p: Predicate, env0: Map[Symbol.VariableSymbol, Value]): Boolean

  def query(p: Predicate, env0: Map[Symbol.VariableSymbol, Value]): List[Map[Symbol.VariableSymbol, Value]]
}
