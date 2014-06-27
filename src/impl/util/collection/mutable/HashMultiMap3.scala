package impl.util.collection.mutable

/**
 * A mutable multi map with three keys backed by a [[Map3]].
 */
final class HashMultiMap3[K1, K2, K3, V](m: Map3[K1, K2, K3, Set[V]] = Map3.empty[K1, K2, K3, Set[V]]) extends MultiMap3[K1, K2, K3, V] {
  def keys: Traversable[K1] = m.keys;

  def get(k1: K1, k2: K2, k3: K3): Set[V] = m.getOrElse(k1, k2, k3)(Set.empty[V]);

  def has(k1: K1, k2: K2, k3: K3, v: V): Boolean = m.get(k1, k2, k3).exists(_ contains v);

  def hasNot(k1: K1, k2: K2, k3: K3, v: V): Boolean = !has(k1, k2, k3, v);

  def put(k1: K1, k2: K2, k3: K3, v: V): Boolean = {
    val vs = get(k1, k2, k3)
    if (!(vs contains v)) {
      m.put(k1, k2, k3, vs + v);
      return true
    }
    false
  }

  def remove(k1: K1, k2: K2, k3: K3, v: V): Unit = {
    m.put(k1, k2, k3, get(k1, k2, k3) - v);
  }

  def tuples: Traversable[(K1, K2, K3, V)] =
    for ((k1, k2, k3, vs) <- m.toList; v <- vs)
    yield (k1, k2, k3, v);

  def foreach[U](f: ((K1, K2, K3, Set[V])) => U) {
    m.foreach(f);
  }
}
