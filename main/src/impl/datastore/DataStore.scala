package impl.datastore

import impl.logic.{Predicate, Value}

trait DataStore {

  /**
   * Adds the given ground predicate to the datastore.
   */
  def store(p: Predicate.GroundPredicate): Unit

  /**
   * Returns the single value which matches the init of the predicate terms.
   */
  def lookup(p: Predicate.GroundPredicate): Option[Value]

  /**
   * Returns a list of  value lists which may satisfy the predicate.
   */
  def query(p: Predicate): List[List[Value]]

  /**
   * Prints all values in the datastore.
   */
  def output(): Unit

  /**
   * Outputs a list of values of the unary predicate with name predName.
   */
  def dumpPred(predName: String): List[Value]
}
