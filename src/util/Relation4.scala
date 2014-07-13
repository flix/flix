package util

import scala.collection.mutable

class Relation4[K, V]() extends Relation[K, V] {

  val m0 = mutable.Map.empty[K, Relation3[K, V]]
  val m1 = mutable.Map.empty[K, Relation3[K, V]]
  val m2 = mutable.Map.empty[K, Relation3[K, V]]
  val m3 = mutable.Map.empty[K, Relation3[K, V]]

  def get(i: Int, t: K): Relation3[K, V] = i match {
    case 0 => m0.getOrElseUpdate(t, new Relation3[K, V])
    case 1 => m1.getOrElseUpdate(t, new Relation3[K, V])
    case 2 => m2.getOrElseUpdate(t, new Relation3[K, V])
    case 3 => m3.getOrElseUpdate(t, new Relation3[K, V])
    case _ => throw new RuntimeException()
  }

  def put(k1: K, k2: K, k3: K, k4: K, v: V): Boolean = {
    m0.getOrElseUpdate(k1, new Relation3[K, V]).put(k2, k3, k4, v)
    m1.getOrElseUpdate(k2, new Relation3[K, V]).put(k1, k3, k4, v)
    m2.getOrElseUpdate(k3, new Relation3[K, V]).put(k1, k2, k4, v)
    m3.getOrElseUpdate(k4, new Relation3[K, V]).put(k1, k2, k3, v)
  }

  def tuples: Set[V] = {
    val r = mutable.Set.empty[V]
    for ((k, v) <- m0) {
      r ++= v.tuples
    }
    r.toSet
  }

}
