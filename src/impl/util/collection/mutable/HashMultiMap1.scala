package impl.util.collection.mutable

/**
 * A mutable multi map with one key backed by a [[java.util.HashMap]].
 */
final class HashMultiMap1[K1, V](m: Map1[K1, Set[V]] = Map1.empty[K1, Set[V]]) extends MultiMap1[K1, V] {
  def keys: Traversable[K1] = m.keys;

  def has(k1: K1, v: V): Boolean = m.get(k1).exists(_ contains v);

  def hasNot(k1: K1, v: V): Boolean = !has(k1, v);

  def get(k: K1): Set[V] = m.getOrElse(k)(Set.empty);

  def put(k: K1, v: V): Unit = {
    m.put(k, get(k) + v)
  }

  def remove(k1: K1, v: V): Unit = m.get(k1) match {
    case None => // nop
    case Some(vs) => m.put(k1, vs - v);
  }

  def tuples: Traversable[(K1, V)] = {
    for ((k, vs) <- m.toList; v <- vs)
    yield (k, v);
  }

  def foreach[U](f: ((K1, Set[V])) => U) {
    m.foreach(f);
  }
}

