package impl.util.collection.mutable

/**
 * Companion object for classes implementing [[Map2]].
 */
object Map2 {
  def empty[K1, K2, V]: HashMap2[K1, K2, V] = new HashMap2();
}

/**
 * Interface for mutable maps with two keys.
 */
trait Map2[K1, K2, V] extends Traversable[(K1, K2, V)] {
  def keys: Set[K1];

  def keys2: Set[(K1, K2)];

  def getOrElse(k1: K1, k2: K2)(default: => V): V;

  def getOrElsePut(k1: K1, k2: K2, v: V): V;

  def get(k1: K1, k2: K2): Option[V];

  def get(k1: K1): Map[K2, V];

  def put(k1: K1, k2: K2, v: V);

  def remove(k1: K1, k2: K2);
}