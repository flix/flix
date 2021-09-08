package ca.uwaterloo.flix.util.collection

/**
  * Companion object for the [[MultiMap]] class.
  */
object MultiMap {
  /**
    * Returns the empty multi map.
    */
  def empty[K, V]: MultiMap[K, V] = MultiMap(Map.empty)

  /**
    * Returns a singleton multi map with a mapping from `k` to `v`.
    */
  def singleton[K, V](k: K, v: V): MultiMap[K, V] = empty + (k, v)
}

/**
  * Represents a map from keys of type `K` to sets of values of type `V`.
  */
case class MultiMap[K, V](m: Map[K, Set[V]]) {

  /**
    * Returns `true` is the map is empty, `false` if not.
    */
  def isEmpty: Boolean = m.isEmpty

  /**
    * Returns `true` if the map contains `k`, `false` if not.
    */
  def contains(k: K): Boolean = m.contains(k)

  /**
    * Returns the size of the map.
    */
  def size: Int = m.size

  /**
    * Optionally returns the set of values that the key `k` maps to.
    */
  def get(k: K): Option[Set[V]] = m.get(k)

  /**
    * Returns the set of values that the key `k` maps to.
    */
  def apply(k: K): Set[V] = m.getOrElse(k, Set.empty)

  /**
    * Returns `this` multi map extended with an additional mapping from `k` to `v`.
    */
  def +(k: K, v: V): MultiMap[K, V] = {
    val s = m.getOrElse(k, Set.empty)
    MultiMap(m + (k -> (s + v)))
  }

  /**
    * Returns `this` multi map extended with additional mappings from `k`to the values in `vs`.
    */
  def +(k: K, vs: Set[V]): MultiMap[K, V] = {
    val s = m.getOrElse(k, Set.empty)
    MultiMap(m + (k -> (s ++ vs)))
  }

  /**
    * Returns `this` multi map extended with all mappings in `that` multi mapping.
    */
  def ++(that: MultiMap[K, V]): MultiMap[K, V] = {
    that.m.foldLeft(this) {
      case (macc, (k, vs)) => macc + (k, vs)
    }
  }

  /**
    * Returns `this` multi map with mappings from `k` removed.
    */
  def -(k: K): MultiMap[K, V] = {
    if (m.contains(k)) MultiMap(m.removed(k))
    else this
  }

  /**
    * Returns `this` multi map with mappings from `ks` removed.
    */
  def --(ks: Iterable[K]): MultiMap[K, V] = {
    if (ks.isEmpty) this
    else MultiMap(m.removedAll(ks))
  }

}
