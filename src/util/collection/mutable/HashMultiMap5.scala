package util.collection.mutable

/**
 * A util.collection.mutable multi map with five keys backed by a [[Map5]].
 */
final class HashMultiMap5[K1, K2, K3, K4, K5, V](m: Map5[K1, K2, K3, K4, K5, Set[V]] = Map5.empty[K1, K2, K3, K4, K5, Set[V]]) extends MultiMap5[K1, K2, K3, K4, K5, V] {
  def keys: Traversable[K1] = m.keys;

  def get(k1: K1, k2: K2, k3: K3, k4: K4, k5: K5): Set[V] = m.getOrElse(k1, k2, k3, k4, k5)(Set.empty[V]);

  def has(k1: K1, k2: K2, k3: K3, k4: K4, k5: K5, v: V): Boolean = m.get(k1, k2, k3, k4, k5).exists(_ contains v);

  def hasNot(k1: K1, k2: K2, k3: K3, k4: K4, k5: K5, v: V): Boolean = !has(k1, k2, k3, k4, k5, v);

  def put(k1: K1, k2: K2, k3: K3, k4: K4, k5: K5, v: V): Boolean = {
    val vs = get(k1, k2, k3, k4, k5)
    if (!(vs contains v)) {
      m.put(k1, k2, k3, k4, k5, vs + v)
      return true
    }
    false
  }

  def remove(k1: K1, k2: K2, k3: K3, k4: K4, k5: K5, v: V): Unit = {
    m.put(k1, k2, k3, k4, k5, get(k1, k2, k3, k4, k5) - v);
  }

  def tuples: Traversable[(K1, K2, K3, K4, K5, V)] =
    for ((k1, k2, k3, k4, k5, vs) <- m.toList; v <- vs)
    yield (k1, k2, k3, k4, k5, v);

  def foreach[U](f: ((K1, K2, K3, K4, K5, Set[V])) => U) {
    m.foreach(f);
  }
}
