package util

import scala.collection.mutable

class Relation1[K, V] extends Relation[K, V] {

  val m0 = mutable.Set.empty[V]

  def get(i: Int, k: K): Relation[K, V] = ???

  def put(v: V): Boolean = {
    if (m0 contains v) {
      return false
    }
    m0 += v
    true
  }

  def tuples: Set[V] = m0.toSet
}
