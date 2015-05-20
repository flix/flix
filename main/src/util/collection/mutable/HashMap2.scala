package util.collection.mutable

import scala.collection.{immutable, mutable}

/**
 * A util.collection.mutable map with two keys backed by a [[scala.collection.mutable.Map]].
 */
final class HashMap2[K1, K2, V](m: mutable.Map[K1, mutable.Map[K2, V]] = mutable.Map.empty[K1, mutable.Map[K2, V]]) extends Map2[K1, K2, V] {
  def keys: Set[K1] = m.keySet.toSet;

  def keys2: Set[(K1, K2)] = map {
    case (k1, k2, v) => (k1, k2);
  }.toSet

  def getOrElse(k1: K1, k2: K2)(default: => V): V = m.get(k1) match {
    case None => default;
    case Some(m2) => m2.getOrElse(k2, default);
  }

  def getOrElsePut(k1: K1, k2: K2, v: V): V = get(k1, k2) match {
    case None => put(k1, k2, v); v;
    case Some(v2) => v2;
  }

  def get(k1: K1, k2: K2): Option[V] = m.get(k1) match {
    case None => None;
    case Some(m2) => m2.get(k2);
  }

  def get(k1: K1): Map[K2, V] = m.get(k1) match {
    case None => immutable.Map.empty[K2, V];
    case Some(m2) => m2.toMap;
  }

  def put(k1: K1, k2: K2, v: V): Unit = m.get(k1) match {
    case None => m.put(k1, mutable.Map(k2 -> v));
    case Some(m2) => m2.put(k2, v);
  }

  def remove(k1: K1, k2: K2): Unit = m.get(k1) match {
    case None => // nop
    case Some(m2) => m2.remove(k2);
  }

  def foreach[U](f: ((K1, K2, V)) => U): Unit = {
    for ((k1, m2) <- m; (k2, v) <- m2) {
      f(k1, k2, v);
    }
  }
}
