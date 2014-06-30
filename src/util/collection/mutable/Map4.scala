package util.collection.mutable


/**
 * Companion object for classes implementing [Map4]].
 */
object Map4 {
  def empty[K1, K2, K3, K4, V]: Map4[K1, K2, K3, K4, V] = new HashMap4();
}

/**
 * Interface for util.collection.mutable maps with four keys.
 */
trait Map4[K1, K2, K3, K4, V] extends Traversable[(K1, K2, K3, K4, V)] {
  def keys: Set[K1];

  def keys2: Set[(K1, K2)];

  def keys3: Set[(K1, K2, K3)];

  def keys4: Set[(K1, K2, K3, K4)];

  def getOrElse(k1: K1, k2: K2, k3: K3, k4: K4)(default: => V): V;

  def getOrElsePut(k1: K1, k2: K2, k3: K3, k4: K4, v: V): V;

  def get(k1: K1, k2: K2, k3: K3, k4: K4): Option[V];

  def get(k1: K1, k2: K2, k3: K3): Map[K4, V];

  def get(k1: K1, k2: K2): Map2[K3, K4, V];

  def get(k1: K1): Map3[K2, K3, K4, V];

  def put(k1: K1, k2: K2, k3: K3, k4: K4, v: V);

  def remove(k1: K1, k2: K2, k3: K3, k4: K4);
}
