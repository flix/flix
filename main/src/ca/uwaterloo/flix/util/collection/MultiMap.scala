package ca.uwaterloo.flix.util.collection

/**
  * Companion object for the [[MultiMap]] class.
  */
object MultiMap {
  /**
    * Returns the empty multi map.
    */
  def empty[K, V]: MultiMap[K, V] = MultiMap(Map.empty)
}

/**
  * Represents a map from keys of type `K` to sets of values of type `V`.
  */
case class MultiMap[K, V](m: Map[K, Set[V]]) {

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

}
