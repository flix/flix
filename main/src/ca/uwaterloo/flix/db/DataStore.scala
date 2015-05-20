package ca.uwaterloo.flix.db

import scala.collection.mutable

class DataStore[V] {

  type Column = Int
  type Row = Array[V]

  val datastore = mutable.Set.empty[Row]
  val indexes = mutable.Map.empty[Set[Column], Set[Row]]

  def insert: Unit = ???

  def update: Unit = ???

  def query: Unit = ???

}
