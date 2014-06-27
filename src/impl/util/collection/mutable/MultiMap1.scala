package impl.util.collection.mutable

/**
 * Companion object for classes implementing [[MultiMap1]].
 */
object MultiMap1 {
  def empty[K1, V]: MultiMap1[K1, V] = new HashMultiMap1[K1, V]();
}

/**
 * Interface for mutable multi map with one key.
 */
trait MultiMap1[K1, V] extends Traversable[(K1, Set[V])] {
  def keys: Traversable[K1];
  def has(k1: K1, v: V): Boolean;
  def hasNot(k1: K1, v: V): Boolean;
  def get(k1: K1): Set[V];
  def put(k1: K1, v: V): Boolean;
  def remove(k1: K1, v: V): Unit;
  def tuples: Traversable[(K1, V)];
}
