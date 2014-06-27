package impl.util.collection.mutable

/**
 * Companion object for classes implementing [[MultiMap3]].
 */
object MultiMap3 {
  def empty[K1, K2, K3, V] = new HashMultiMap3[K1, K2, K3, V]();
}

/**
 * Interface for mutable multi maps with three keys.
 */
trait MultiMap3[K1, K2, K3, V] extends Traversable[(K1, K2, K3, Set[V])] {
  def keys: Traversable[K1];
  def get(k1: K1, k2: K2, k3: K3): Set[V];
  def has(k1: K1, k2: K2, k3: K3, v: V): Boolean;
  def hasNot(k1: K1, k2: K2, k3: K3, v: V): Boolean;
  def put(k1: K1, k2: K2, k3: K3, v: V): Unit;
  def remove(k1: K1, k2: K2, k3: K3, v: V): Unit;
  def tuples: Traversable[(K1, K2, K3, V)];
}
