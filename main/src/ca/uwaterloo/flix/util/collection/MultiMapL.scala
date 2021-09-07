/*
 * Copyright 2021 Jonathan Lindegaard Starup
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.util.collection

/**
 * Companion object for the [[MultiMapL]] class.
 */
object MultiMapL {
  /**
   * Returns the empty multi map.
   */
  def empty[K, V]: MultiMapL[K, V] = MultiMapL(Map.empty)

  /**
   * Returns a singleton multi map with a mapping from `k` to `v`.
   */
  def singleton[K, V](k: K, v: V): MultiMapL[K, V] = empty + (k, v)
}

/**
 * Represents a map from keys of type `K` to list of values of type `V`.
 */
case class MultiMapL[K, V](m: Map[K, List[V]]) {

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
   * Optionally returns the list of values that the key `k` maps to.
   */
  def get(k: K): Option[List[V]] = m.get(k)

  /**
   * Returns the list of values that the key `k` maps to.
   */
  def apply(k: K): List[V] = m.getOrElse(k, List.empty)

  /**
   * Returns `this` multi map extended with an additional mapping from `k` to `v`.
   */
  def +(k: K, v: V): MultiMapL[K, V] = {
    val s = m.getOrElse(k, List.empty)
    MultiMapL(m + (k -> s.prepended(v)))
  }

  /**
   * Returns `this` multi map extended with additional mappings from `k`to the values in `vs`.
   */
  def +(k: K, vs: List[V]): MultiMapL[K, V] = {
    val s = m.getOrElse(k, List.empty)
    MultiMapL(m + (k -> (vs ++ s)))
  }

  /**
   * Returns `this` multi map extended with all mappings in `that` multi mapping.
   */
  def ++(that: MultiMapL[K, V]): MultiMapL[K, V] = {
    that.m.foldLeft(this) {
      case (macc, (k, vs)) => macc + (k, vs)
    }
  }

  /**
   * Returns `this` multi map with mappings from `k` removed.
   */
  def -(k: K): MultiMapL[K, V] = {
    if (m.contains(k)) MultiMapL(m.removed(k))
    else this
  }

  /**
   * Returns `this` multi map with mappings from `ks` removed.
   */
  def --(ks: Iterable[K]): MultiMapL[K, V] = {
    if (ks.isEmpty) this
    else MultiMapL(m.removedAll(ks))
  }

}
