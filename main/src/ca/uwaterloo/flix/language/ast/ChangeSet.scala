/*
 * Copyright 2021 Magnus Madsen
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
package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.{DependencyGraph, Input}
import ca.uwaterloo.flix.util.collection.ListMap

import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap}
import scala.jdk.CollectionConverters.{ConcurrentMapHasAsScala, MapHasAsJava}

sealed trait ChangeSet {

  /**
    * Returns a new change set with `i` marked as changed.
    *
    * Note: The input `i` is always marked as dirty itself.
    */
  def markChanged(i: Input, dg: DependencyGraph): ChangeSet = this match {
    case ChangeSet.Everything => ChangeSet.Dirty(dg.dirty(i))
    case ChangeSet.Dirty(s) => ChangeSet.Dirty(s ++ dg.dirty(i))
  }

  /**
    * Returns two maps: `stale` and `fresh` according to the given `newMap` and `oldMap`.
    *
    * A fresh key is one that can be reused.
    * A stale key is one that must be re-compiled.
    * A key that is neither fresh nor stale can be deleted.
    *
    * An entry is fresh if it is in `oldMap` and it is not dirty (i.e. has not changed).
    * An entry is stale if it is not fresh and it is in `newMap`.
    *
    * Note that the union of stale and fresh does not have to equal `newMap` or `oldMap`.
    * This happens if a key is deleted. Then it does not occur `newMap` but it occurs in `oldMap`.
    * However, it is neither fresh nor stale. It should simply be forgotten.
    */
  def partition[K <: Sourceable, V1, V2](newMap: Map[K, V1], oldMap: Map[K, V2]): (Map[K, V1], Map[K, V2]) = this match {
    case ChangeSet.Everything =>
      (newMap, Map.empty)

    case ChangeSet.Dirty(dirty) =>
      val fresh = oldMap.filter(kv => !dirty.contains(kv._1.src.input)).filter(kv => newMap.contains(kv._1))
      val stale = newMap.filter(kv => !fresh.contains(kv._1))

      (stale, fresh)
  }

  /**
    * Returns two maps: `stale` and `fresh` according to the given `newMap` and `oldMap`.
    *
    * A fresh key is one that can be reused.
    * A stale key is one that must be re-compiled.
    * A key that is neither fresh nor stale can be deleted.
    *
    * An entry is fresh if it is in `oldMap` and it is not dirty (i.e. has not changed).
    * An entry is stale if it is not fresh and it is in `newMap`.
    *
    * Note that the union of stale and fresh does not have to equal `newMap` or `oldMap`.
    * This happens if a key is deleted. Then it does not occur `newMap` but it occurs in `oldMap`.
    * However, it is neither fresh nor stale. It should simply be forgotten.
    */
  def partition[K <: Sourceable, V1, V2](newMap: ConcurrentMap[K, V1], oldMap: ConcurrentMap[K, V2]): (ConcurrentMap[K, V1], ConcurrentMap[K, V2]) = {
    val (stale, fresh) = partition(newMap.asScala.toMap, oldMap.asScala.toMap)
    (new ConcurrentHashMap(stale.asJava), new ConcurrentHashMap(fresh.asJava))
  }

  /**
    * Returns two maps: `stale` and `fresh` according to the given `newMap` and `oldMap`, the value of the map is a list of Sourceable.
    *
    * A fresh item in the value list is one that can be reused.
    * A stale item in the value list is one that must be re-compiled.
    * An item that is neither fresh nor stale can be deleted.
    * Actually, all fresh and stale items will be in the newMap
    *
    * An item from newMap is fresh if it is also in `oldMap`, and it is not dirty (i.e. has not changed).
    * Otherwise, it is stale.
    *
    * The type of the value in the map must be the same, since when checking stale, we need to check if the value is in the fresh map.
    *
    * @param newMap  the new map
    * @param oldMap  the old map
    * @param eq      the equality function for the value
    */
  def partitionOnValues[K, V <: Sourceable](newMap: ListMap[K, V], oldMap: ListMap[K, V], eq: (V, V) => Boolean): (ListMap[K, V], ListMap[K, V]) = this match {
    case ChangeSet.Everything =>
      (newMap, ListMap.empty)

    case ChangeSet.Dirty(dirty) =>
      newMap.foldLeft((ListMap.empty[K, V], ListMap.empty[K, V])){ case ((stale, fresh), (k, v)) =>
        if (oldMap.get(k).exists(v2 => eq(v, v2)) && !dirty.contains(v.src.input))
          (stale, fresh + (k -> v))
        else
          (stale + (k -> v), fresh)
      }
  }


  /**
    * Updates the stale part of the map with the given function `f`.
    */
  def updateStaleValues[K <: Sourceable, V1, V2](newMap: Map[K, V1], oldMap: Map[K, V2])(f: Map[K, V1] => Map[K, V2]): Map[K, V2] = {
    val (stale, fresh) = partition(newMap, oldMap)
    fresh ++ f(stale)
  }

  /**
    * Updates the stale part of the list map with the given function `f`.
    *
    * @param newMap  the new map
    * @param oldMap  the old map
    * @param eq      the equality function for the value
    */
  def updateStaleValueLists[K, V <: Sourceable](newMap: ListMap[K, V], oldMap: ListMap[K, V], eq: (V, V) => Boolean)(f: ListMap[K, V] => ListMap[K, V]): ListMap[K, V] = {
    val (stale, fresh) = partitionOnValues(newMap, oldMap, eq)
    fresh ++ f(stale)
  }
}

object ChangeSet {

  /**
    * Represents a change set where everything is dirty (used for a complete re-compilation).
    */
  case object Everything extends ChangeSet

  /**
    * Represents a change set where everything in `s` is dirty (must be recompiled).
    */
  case class Dirty(s: Set[Input]) extends ChangeSet

}

