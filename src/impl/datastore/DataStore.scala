package impl.datastore

import impl.logic.{Predicate, Value}

trait DataStore {

  /**
   * Adds the given ground predicate to the datastore.
   */
  def store(p: Predicate.GroundPredicate): Unit

  /**
   * TODO: DOC
   */
  def lookup(p: Predicate.GroundPredicate): Option[Value]

  /**
   * TODO: DOC
   */
  def query(p: Predicate): List[List[Value]]

  /**
   * Prints all values in the datastore.
   */
  def output(): Unit

}
