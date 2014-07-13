package util

import scala.collection.mutable

class Relation3[K, V] extends Relation[K, V] {

  val m0 = mutable.Map.empty[K, Relation2[K, V]]
  val m1 = mutable.Map.empty[K, Relation2[K, V]]
  val m2 = mutable.Map.empty[K, Relation2[K, V]]

  def get(i: Int, t: K): Relation2[K, V] = i match {
    case 0 => m0.getOrElseUpdate(t, new Relation2[K, V])
    case 1 => m1.getOrElseUpdate(t, new Relation2[K, V])
    case 2 => m2.getOrElseUpdate(t, new Relation2[K, V])
    case _ => throw new RuntimeException()
  }

  def put(k0: K, k1: K, k2: K, v: V): Boolean = {
    m0.getOrElseUpdate(k0, new Relation2[K, V]).put(k1, k2, v)
    m1.getOrElseUpdate(k1, new Relation2[K, V]).put(k0, k2, v)
    m2.getOrElseUpdate(k2, new Relation2[K, V]).put(k0, k1, v)
  }

  def tuples: Set[V] = {
    val r = mutable.Set.empty[V]
    for ((k, v) <- m0) {
      r ++= v.tuples
    }
    r.toSet
  }

}


