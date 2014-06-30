package util.collection.mutable

/**
 * A util.collection.mutable multi map with two keys backed by a [[Map2]].
 */
final class HashMultiMap2[K1, K2, V](m: Map2[K1, K2, Set[V]] = Map2.empty[K1, K2, Set[V]]) extends MultiMap2[K1, K2, V] {
  def keys: Traversable[K1] = m.keys;

  def get(k1: K1, k2: K2): Set[V] = m.getOrElse(k1, k2)(Set.empty[V]);

  def has(k1: K1, k2: K2, v: V): Boolean = m.get(k1, k2).exists(_ contains v);

  def hasNot(k1: K1, k2: K2, v: V): Boolean = !has(k1, k2, v);

  def put(k1: K1, k2: K2, v: V): Boolean = {
    val vs = get(k1, k2)
    if (!(vs contains v)) {
      m.put(k1, k2, vs + v)
      return true
    }
    false
  }

  def remove(k1: K1, k2: K2, v: V): Unit = {
    m.put(k1, k2, get(k1, k2) - v);
  }

  def tuples: Traversable[(K1, K2, V)] =
    for ((k1, k2, vs) <- m.toList; v <- vs)
    yield (k1, k2, v);

  def foreach[U](f: ((K1, K2, Set[V])) => U) {
    m.foreach(f);
  }
}
