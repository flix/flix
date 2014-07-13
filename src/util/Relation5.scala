package util

import scala.collection.mutable

class Relation5[K, V]() extends Relation[K, V] {

  val m0 = mutable.Map.empty[K, Relation4[K, V]]
  val m1 = mutable.Map.empty[K, Relation4[K, V]]
  val m2 = mutable.Map.empty[K, Relation4[K, V]]
  val m3 = mutable.Map.empty[K, Relation4[K, V]]
  val m4 = mutable.Map.empty[K, Relation4[K, V]]

  def get(i: Int, t: K): Relation4[K, V] = i match {
    case 0 => m0.getOrElseUpdate(t, new Relation4[K, V])
    case 1 => m1.getOrElseUpdate(t, new Relation4[K, V])
    case 2 => m2.getOrElseUpdate(t, new Relation4[K, V])
    case 3 => m3.getOrElseUpdate(t, new Relation4[K, V])
    case 4 => m4.getOrElseUpdate(t, new Relation4[K, V])
    case _ => throw new RuntimeException()
  }

  def put(k1: K, k2: K, k3: K, k4: K, k5: K, v: V): Boolean = {
    m0.getOrElseUpdate(k1, new Relation4[K, V]).put(k2, k3, k4, k5, v)
    m1.getOrElseUpdate(k2, new Relation4[K, V]).put(k1, k3, k4, k5, v)
    m2.getOrElseUpdate(k3, new Relation4[K, V]).put(k1, k2, k4, k5, v)
    m3.getOrElseUpdate(k4, new Relation4[K, V]).put(k1, k2, k3, k5, v)
    m4.getOrElseUpdate(k5, new Relation4[K, V]).put(k1, k2, k3, k4, v)
  }

  def tuples: Set[V] = {
    val r = mutable.Set.empty[V]
    for ((k, v) <- m0) {
      r ++= v.tuples
    }
    r.toSet
  }

}
