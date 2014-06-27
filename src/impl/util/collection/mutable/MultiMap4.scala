package impl.util.collection.mutable

/**
 * Companion object for classes implementing [[MultiMap4]].
 */
object MultiMap4 {
  def empty[K1, K2, K3, K4, V]: MultiMap4[K1, K2, K3, K4, V] = new HashMultiMap4[K1, K2, K3, K4, V]();
}

/**
 * Interface for mutable multi maps with four keys.
 */
trait MultiMap4[K1, K2, K3, K4, V] extends Traversable[(K1, K2, K3, K4, Set[V])] {
  def keys: Traversable[K1];
  def get(k1: K1, k2: K2, k3: K3, k4: K4): Set[V];
  def has(k1: K1, k2: K2, k3: K3, k4: K4, v: V): Boolean;
  def hasNot(k1: K1, k2: K2, k3: K3, k4: K4, v: V): Boolean;
  def put(k1: K1, k2: K2, k3: K3, k4: K4, v: V): Boolean;
  def remove(k1: K1, k2: K2, k3: K3, k4: K4, v: V): Unit;
  def tuples: Traversable[(K1, K2, K3, K4, V)];
}