package util.collection.mutable

/**
 * A util.collection.mutable multi map with two keys backed by a [[Map2]].
 */
final class HashMultiMap2[K1, K2, V](m: Map1[K1, MultiMap1[K2, V]] = Map1.empty[K1, MultiMap1[K2, V]]) extends MultiMap2[K1, K2, V] {
  def keys: Traversable[K1] = m.keys;

  def get(k1: K1, k2: K2): Set[V] = m.get(k1) match {
    case None => Set.empty
    case Some(m2) => m2.get(k2)
  }

  def get(k1: K1): MultiMap1[K2, V] = m.get(k1).getOrElse {
    val m2 = MultiMap1.empty[K2, V]
    m.put(k1, m2)
    m2
  }

  def has(k1: K1, k2: K2, v: V): Boolean = m.get(k1) match {
    case None => false
    case Some(m2) => m2.has(k2, v)
  }

  def hasNot(k1: K1, k2: K2, v: V): Boolean = !has(k1, k2, v);

  def put(k1: K1, k2: K2, v: V): Boolean = m.get(k1) match {
    case None =>
      val m2 = MultiMap1.empty[K2, V]
      m2.put(k2, v)
      m.put(k1, m2)
      true
    case Some(m2) => m2.put(k2, v)
  }

  def remove(k1: K1, k2: K2, v: V): Unit = m.get(k1) match {
    case None => // nop
    case Some(m2) => m2.remove(k2, v)
  }

  def tuples: Traversable[(K1, K2, V)] =
    for ((k1, m2) <- m; (k2, vs) <- m2; v <- vs)
    yield (k1, k2, v);

  def foreach[U](f: ((K1, K2, Set[V])) => U) {
    for ((k1, m2) <- m; (k2, vs) <- m2) {
      f(k1, k2, vs)
    }
  }
}
