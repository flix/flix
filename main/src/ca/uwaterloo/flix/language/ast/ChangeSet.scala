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

import ca.uwaterloo.flix.language.ast.shared.Input

sealed trait ChangeSet {

  /**
    * Returns a new change set with `i` marked as changed.
    */
  def markChanged(i: Input): ChangeSet = this match {
    case ChangeSet.Everything => ChangeSet.Changes(Set(i))
    case ChangeSet.Changes(s) => ChangeSet.Changes(s + i)
  }

  /**
    * Returns two maps: `stale` and `fresh` according to the given `newMap` and `oldMap`.
    *
    * A fresh key is one that can be reused.
    * A stale key is one that must be re-compiled.
    * A key that is neither fresh nor stale can be deleted.
    *
    * An entry is fresh if it is in `oldMap` and has a stable source location.
    * An entry is stale if it is not fresh and it is in `newMap`.
    *
    * Note that the union of stale and fresh does not have to equal `newMap` or `oldMap`.
    * This happens if a key is deleted. Then it does not occur `newMap` but it occurs in `oldMap`.
    * However, it is neither fresh nor stale. It should simply be forgotten.
    */
  def partition[K <: Sourceable, V1, V2](newMap: Map[K, V1], oldMap: Map[K, V2]): (Map[K, V1], Map[K, V2]) = this match {
    case ChangeSet.Everything =>
      (newMap, Map.empty)

    case ChangeSet.Changes(_) =>
      // Note: At the moment we don't use the change set.
      // We simply consider whether a source is stable.
      val fresh = oldMap.filter(_._1.src.stable).filter(kv => newMap.contains(kv._1))
      val stale = newMap.filter(kv => !fresh.contains(kv._1))

      (stale, fresh)
  }

}

object ChangeSet {

  /**
    * Represents a change set where everything is changed (used for a complete re-compilation).
    */
  case object Everything extends ChangeSet

  /**
    * Represents the set `s` of changed sources.
    */
  case class Changes(s: Set[Input]) extends ChangeSet

}

