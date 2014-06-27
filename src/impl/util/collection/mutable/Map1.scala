package impl.util.collection.mutable

/**
 * Companion object for classes implementing [[Map1]].
 */
object Map1 {
  def empty[K1, K2, V]: HashMap1[K1, V] = new HashMap1();
}

/**
 * Interface for mutable maps with two keys.
 */
trait Map1[K1, V] extends Traversable[(K1, V)] {
  def keys: Set[K1];

  def getOrElse(k1: K1)(default: => V): V;

  def getOrElsePut(k1: K1, v: V): V;

  def get(k1: K1): Option[V];

  def put(k1: K1, v: V);

  def remove(k1: K1);
}