package util.collection.mutable

/**
 * A util.collection.mutable multi map with four keys backed by a [[Map4]].
 */
final class HashMultiMap4[K1, K2, K3, K4, V](m: Map4[K1, K2, K3, K4, Set[V]] = Map4.empty[K1, K2, K3, K4, Set[V]]) extends MultiMap4[K1, K2, K3, K4, V] {
  def keys: Traversable[K1] = m.keys;

  def get(k1: K1, k2: K2, k3: K3, k4: K4): Set[V] = m.getOrElse(k1, k2, k3, k4)(Set.empty[V]);

  def has(k1: K1, k2: K2, k3: K3, k4: K4, v: V): Boolean = m.get(k1, k2, k3, k4).exists(_ contains v);

  def hasNot(k1: K1, k2: K2, k3: K3, k4: K4, v: V): Boolean = !has(k1, k2, k3, k4, v);

  def put(k1: K1, k2: K2, k3: K3, k4: K4, v: V): Boolean = {
    val vs = get(k1, k2, k3, k4)
    if (!(vs contains v)) {
      m.put(k1, k2, k3, k4, vs + v);
      return true
    }
    false
  }

  def remove(k1: K1, k2: K2, k3: K3, k4: K4, v: V): Unit = {
    m.put(k1, k2, k3, k4, get(k1, k2, k3, k4) - v);
  }

  def tuples: Traversable[(K1, K2, K3, K4, V)] =
    for ((k1, k2, k3, k4, vs) <- m.toList; v <- vs)
    yield (k1, k2, k3, k4, v);

  def foreach[U](f: ((K1, K2, K3, K4, Set[V])) => U) {
    m.foreach(f);
  }
}
