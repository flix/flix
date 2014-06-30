package util.collection.mutable

/**
 * Companion object for classes implementing [[MultiMap2]].
 */
object MultiMap2 {
  def empty[K1, K2, V]: MultiMap2[K1, K2, V] = new HashMultiMap2[K1, K2, V]();
}

/**
 * Interface for util.collection.mutable multi maps with two keys.
 */
trait MultiMap2[K1, K2, V] extends Traversable[(K1, K2, Set[V])] {
  def keys: Traversable[K1];
  def get(k1: K1): MultiMap1[K2, V];
  def get(k1: K1, k2: K2): Set[V];
  def has(k1: K1, k2: K2, v: V): Boolean;
  def hasNot(k1: K1, k2: K2, v: V): Boolean;
  def put(k1: K1, k2: K2, v: V): Boolean;
  def remove(k1: K1, k2: K2, v: V): Unit;
  def tuples: Traversable[(K1, K2, V)];
}
