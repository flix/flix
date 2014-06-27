package impl.util.collection.mutable

import scala.collection.mutable

/**
 * A mutable map with one key backed by a [[scala.collection.mutable.Map]].
 */
final class HashMap1[K1, V](m: mutable.Map[K1, V] = mutable.Map.empty[K1, V]) extends Map1[K1, V] {
  def keys: Set[K1] = m.keySet.toSet

  def get(k1: K1): Option[V] = m.get(k1)

  def put(k1: K1, v: V): Unit = {
    m.put(k1, v)
  }

  def getOrElse(k1: K1)(default: => V): V = m.getOrElse(k1, default)

  def remove(k1: K1): Unit = {
    m.remove(k1)
  }

  def getOrElsePut(k1: K1, v: V): V = get(k1) match {
    case None => put(k1, v); v;
    case Some(v2) => v2
  }

  def foreach[U](f: ((K1, V)) => U): Unit = {
    for ((k1, v) <- m) {
      f(k1, v);
    }
  }
}
