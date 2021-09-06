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

object ListMap {
  /**
   * Returns the empty multi map.
   */
  def empty[K, V]: ListMap[K, V] = ListMap(Map.empty)

  /**
   * Returns a singleton multi map with a mapping from `k` to `v`.
   */
  def singleton[K, V](k: K, v: V): ListMap[K, V] = empty + (k, v)
}

/**
 * Represents a map from keys of type `K` to sets of values of type `V`.
 */
case class ListMap[K, V](m: Map[K, List[V]]) {

  /**
   * Optionally returns the set of values that the key `k` maps to.
   */
  def get(k: K): Option[List[V]] = m.get(k)

  /**
   * Returns the set of values that the key `k` maps to.
   */
  def apply(k: K): List[V] = m.getOrElse(k, List.empty)

  /**
   * Returns `this` multi map extended with an additional mapping from `k` to `v`.
   */
  def +(k: K, v: V): ListMap[K, V] = {
    val s = m.getOrElse(k, List.empty)
    ListMap(m + (k -> (s :+ v)))
  }

  /**
   * Returns `this` multi map extended with additional mappings from `k`to the values in `vs`.
   */
  def +(k: K, vs: List[V]): ListMap[K, V] = {
    val s = m.getOrElse(k, List.empty)
    ListMap(m + (k -> (s ++ vs)))
  }

  /**
   * Returns `this` multi map extended with all mappings in `that` multi mapping.
   */
  def ++(that: ListMap[K, V]): ListMap[K, V] = {
    that.m.foldLeft(this) {
      case (macc, (k, vs)) => macc + (k, vs)
    }
  }

}