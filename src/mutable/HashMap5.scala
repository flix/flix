package mutable

import scala.collection.{immutable, mutable}

/**
 * A mutable map with five keys backed by [[scala.collection.mutable.Map]] and [[Map4]].
 */
final class HashMap5[K1, K2, K3, K4, K5, V](m: mutable.Map[K1, Map4[K2, K3, K4, K5, V]] = mutable.Map.empty[K1, Map4[K2, K3, K4, K5, V]]) extends Map5[K1, K2, K3, K4, K5, V] {
  def keys: Set[K1] = m.keySet.toSet;

  def keys2: Set[(K1, K2)] = map {
    case (k1, k2, k3, k4, k5, v) => (k1, k2);
  }.toSet

  def keys3: Set[(K1, K2, K3)] = map {
    case (k1, k2, k3, k4, k5, v) => (k1, k2, k3);
  }.toSet

  def keys4: Set[(K1, K2, K3, K4)] = map {
    case (k1, k2, k3, k4, k5, v) => (k1, k2, k3, k4);
  }.toSet

  def keys5: Set[(K1, K2, K3, K4, K5)] = map {
    case (k1, k2, k3, k4, k5, v) => (k1, k2, k3, k4, k5);
  }.toSet

  def getOrElse(k1: K1, k2: K2, k3: K3, k4: K4, k5: K5)(default: => V): V = m.get(k1) match {
    case None => default;
    case Some(m2) => m2.getOrElse(k2, k3, k4, k5)(default);
  }

  def getOrElsePut(k1: K1, k2: K2, k3: K3, k4: K4, k5: K5, v: V): V = get(k1, k2, k3, k4, k5) match {
    case None => put(k1, k2, k3, k4, k5, v); v;
    case Some(v2) => v2;
  }

  def get(k1: K1, k2: K2, k3: K3, k4: K4, k5: K5): Option[V] = m.get(k1) match {
    case None => None;
    case Some(m2) => m2.get(k2, k3, k4, k5);
  }

  def get(k1: K1, k2: K2, k3: K3, k4: K4): Map[K5, V] = m.get(k1) match {
    case None => immutable.Map.empty[K5, V];
    case Some(m2) => m2.get(k2, k3, k4);
  }

  def get(k1: K1, k2: K2, k3: K3): Map2[K4, K5, V] = m.get(k1) match {
    case None => Map2.empty[K4, K5, V];
    case Some(m2) => m2.get(k2, k3);
  }

  def get(k1: K1, k2: K2): Map3[K3, K4, K5, V] = m.get(k1) match {
    case None => Map3.empty[K3, K4, K5, V];
    case Some(m2) => m2.get(k2);
  }

  def get(k1: K1): Map4[K2, K3, K4, K5, V] = m.get(k1) match {
    case None => Map4.empty[K2, K3, K4, K5, V];
    case Some(m2) => m2;
  }

  def put(k1: K1, k2: K2, k3: K3, k4: K4, k5: K5, v: V): Unit = {
    m.get(k1) match {
      case None => {
        val m2 = Map4.empty[K2, K3, K4, K5, V];
        m2.put(k2, k3, k4, k5, v);
        m.put(k1, m2);
      }
      case Some(m2) => m2.put(k2, k3, k4, k5, v);
    }
  }

  def remove(k1: K1, k2: K2, k3: K3, k4: K4, k5: K5): Unit = {
    m.get(k1) match {
      case None => // nop
      case Some(m2) => m2.remove(k2, k3, k4, k5);
    }
  }

  def foreach[U](f: ((K1, K2, K3, K4, K5, V)) => U): Unit = {
    for ((k1, m2) <- m) {
      m2 foreach {
        case (k2, k3, k4, k5, v) => f(k1, k2, k3, k4, k5, v);
      }
    }
  }

}
