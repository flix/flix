/*
 * Copyright 2022 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.util.collection

object MapOps {

  /**
    * Applies `f` to each of the values in the given map.
    *
    * Replaces the deprecated [[Map.mapValues]].
    */
  def mapValues[K, V1, V2](m: Map[K, V1])(f: V1 => V2): Map[K, V2] = {
    m.map {
      case (k, v) => (k, f(v))
    }
  }

  /**
    * Applies `f` to each of the keys and values in the given map.
    */
  def mapValuesWithKey[K, V1, V2](m: Map[K, V1])(f: (K, V1) => V2): Map[K, V2] = {
    m.map {
      case (k, v) => (k, f(k, v))
    }
  }

  /**
    * Applies `f` to each of the values in the given map, keeping only entries that return `Some(...)`.
    */
  def filterMapValues[K, V1, V2](m: Map[K, V1])(f: V1 => Option[V2]): Map[K, V2] = {
    m.flatMap {
      case (k, v1) => f(v1) match {
        case None => None
        case Some(v2) => Some((k, v2))
      }
    }
  }

  /**
    * Combines the two maps with the given function.
    *
    * If a key is present in only one of the maps, then that map's value is used.
    * If a key is present in both maps, the function is used to combine the respective values.
    */
  def unionWith[K, V](m1: Map[K, V], m2: Map[K, V])(f: (V, V) => V): Map[K, V] = {
    val keys = m1.keySet ++ m2.keySet
    keys.map {
      case key =>
        (m1.get(key), m2.get(key)) match {
          case (None, None) => throw new AssertionError("unexpected unknown key")
          case (Some(value), None) => key -> value
          case (None, Some(value)) => key -> value
          case (Some(value1), Some(value2)) => key -> f(value1, value2)
        }
    }.toMap
  }
}
