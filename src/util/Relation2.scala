package util

import scala.collection.mutable

class Relation2[K, V] extends Relation[K, V] {

  val m0 = mutable.Map.empty[K, Relation1[K, V]]
  val m1 = mutable.Map.empty[K, Relation1[K, V]]

  def get(i: Int, t: K): Relation1[K, V] = i match {
    case 0 => m0.getOrElseUpdate(t, new Relation1[K, V])
    case 1 => m1.getOrElseUpdate(t, new Relation1[K, V])
    case _ => throw new RuntimeException()
  }
  
  def put(k0: K, k1: K, v: V): Boolean = {
    m0.getOrElseUpdate(k0, new Relation1[K, V]).put(v)
    m1.getOrElseUpdate(k1, new Relation1[K, V]).put(v)
  }

  def tuples: Set[V] = {
    val r = mutable.Set.empty[V]
    for ((k, v) <- m0) {
      r ++= v.tuples
    }
    r.toSet
  }
}
