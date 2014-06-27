package impl.util.collection.mutable

/**
 * Companion object for classes implementing [[Map3]].
 */
object Map3 {
  def empty[K1, K2, K3, V]: Map3[K1, K2, K3, V] = new HashMap3();
}

/**
 * Interface for mutable maps with three keys.
 */
trait Map3[K1, K2, K3, V] extends Traversable[(K1, K2, K3, V)] {
  def keys: Set[K1];

  def keys2: Set[(K1, K2)];

  def keys3: Set[(K1, K2, K3)];

  def getOrElse(k1: K1, k2: K2, k3: K3)(default: => V): V;

  def getOrElsePut(k1: K1, k2: K2, k3: K3, v: V): V;

  def get(k1: K1, k2: K2, k3: K3): Option[V];

  def get(k1: K1, k2: K2): Map[K3, V];

  def get(k1: K1): Map2[K2, K3, V];

  def put(k1: K1, k2: K2, k3: K3, v: V);

  def remove(k1: K1, k2: K2, k3: K3);
}

