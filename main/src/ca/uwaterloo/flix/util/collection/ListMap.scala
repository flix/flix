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

import java.util.Locale.FilteringMode
import scala.collection.MapOps

/**
  * Companion object for the [[ListMap]] class.
  */
object ListMap {
  /**
    * Returns the empty list map.
    */
  def empty[K, V]: ListMap[K, V] = ListMap(Map.empty)

  /**
    * Returns a singleton list map with a mapping from `k` to `v`.
    */
  def singleton[K, V](k: K, v: V): ListMap[K, V] = empty + (k -> v)

  /**
    * Creates a ListMap from the given iterable.
    */
  def from[K, V](it: Iterable[(K, V)]): ListMap[K, V] = {
    it.iterator.foldLeft(ListMap.empty[K, V]) {
      case (acc, (k, v)) => acc + (k, v)
    }
  }
}

/**
  * Represents a map from keys of type `K` to list of values of type `V`.
  * When a new mapping from `k` is added, it is prepended onto the list.
  */
case class ListMap[K, V](m: Map[K, List[V]]) {

  /**
    * Optionally returns the list of values that the key `k` maps to.
    */
  def get(k: K): Option[List[V]] = m.get(k)

  /**
    * Returns the list of values that the key `k` maps to.
    */
  def apply(k: K): List[V] = m.getOrElse(k, List.empty)

  /**
    * Returns all the values in the list map in a single iterable.
    */
  def values: Iterable[V] = m.values.flatten

  /**
    * Returns all the keys in the list map.
    */
  def keys: Iterable[K] = m.keys

  /**
    * Applies `f` to each of the mappings in the list map.
    */
  def flatMap[A](f: ((K, List[V])) => Iterable[A]): Iterable[A] = m.flatMap(f)

  /**
    * Returns the mapped list in the list map, if any.
    * Otherwise, returns the default list.
    */
  def getOrElse(k: K, default: List[V]): List[V] = m.getOrElse(k, default)

  /**
    * Required for pattern-matching in for-patterns.
    * Just call withFilter on the underlying map.
    */
  def withFilter(p: ((K, List[V])) => Boolean): MapOps.WithFilter[K, List[V], Iterable, Map] = m.withFilter(p)

  /**
    * Returns `this` list map extended with an additional mapping from `k` to `v`.
    */
  def +(kv: (K, V)): ListMap[K, V] = {
    val (k, v) = kv
    val l = m.getOrElse(k, List.empty)
    ListMap(m + (k -> (v :: l)))
  }

  /**
    * Returns `this` list map extended with additional mappings from `k`to the values in `vs`.
    */
  def ++(kvs: (K, List[V])): ListMap[K, V] = {
    val (k, vs) = kvs
    val l = m.getOrElse(k, List.empty)
    ListMap(m + (k -> (vs ++ l)))
  }

  /**
    * Returns `this` list map extended with all mappings in `that` list mapping.
    */
  def ++(that: ListMap[K, V]): ListMap[K, V] = {
    that.m.foldLeft(this) {
      case (macc, (k, vs)) => macc ++ (k -> vs)
    }
  }

  /**
    * Returns `this` list map without the mapping for `k`.
    */
  def -(k: K): ListMap[K, V] = ListMap(m - k)

  /**
    * Returns ´this´ list map without the mappings for ´ks´.
    */
  def --(ks: Iterable[K]): ListMap[K, V] = ListMap(m -- ks)
}
