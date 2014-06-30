package util.collection.mutable

/**
 * Companion object for classes implementing [[Map5]].
 */
object Map5 {
  def empty[K1, K2, K3, K4, K5, V]: Map5[K1, K2, K3, K4, K5, V] = new HashMap5();
}

/**
 * Interface for immutable and util.collection.mutable maps with five keys.
 */
trait Map5[K1, K2, K3, K4, K5, V] extends Traversable[(K1, K2, K3, K4, K5, V)] {
  def keys: Set[K1];

  def keys2: Set[(K1, K2)];

  def keys3: Set[(K1, K2, K3)];

  def keys4: Set[(K1, K2, K3, K4)];

  def keys5: Set[(K1, K2, K3, K4, K5)];

  def getOrElse(k1: K1, k2: K2, k3: K3, k4: K4, k5: K5)(default: => V): V;

  def getOrElsePut(k1: K1, k2: K2, k3: K3, k4: K4, k5: K5, v: V): V;

  def get(k1: K1, k2: K2, k3: K3, k4: K4, k5: K5): Option[V];

  def get(k1: K1, k2: K2, k3: K3, k4: K4): Map[K5, V];

  def get(k1: K1, k2: K2, k3: K3): Map2[K4, K5, V];

  def get(k1: K1, k2: K2): Map3[K3, K4, K5, V];

  def get(k1: K1): Map4[K2, K3, K4, K5, V];

  def put(k1: K1, k2: K2, k3: K3, k4: K4, k5: K5, v: V);

  def remove(k1: K1, k2: K2, k3: K3, k4: K4, k5: K5);
}
