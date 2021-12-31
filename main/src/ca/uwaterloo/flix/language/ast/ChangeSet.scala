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

sealed trait ChangeSet {

  /**
    * Returns a new change set with `src` marked as changed.
    */
  def markChanged(src: Ast.Source): ChangeSet = this match {
    case ChangeSet.Everything => ChangeSet.Changes(Set(src))
    case ChangeSet.Changes(s) => ChangeSet.Changes(s + src)
  }

  /**
    * Returns two maps: `stale` and `fresh` according to the given `newMap` and `oldMap`.
    *
    * A fresh key is one that can be reused. A stale key is one that must be re-compiled.
    * A key that is neither fresh nor stale can be deleted.
    *
    * An entry is stale if it is (a) stale w.r.t. the current change set and `oldMap`.
    * An entry is fresh if it is (a) not stale and (b) occurs in `newMap`.
    *
    * Note that the union of stale and fresh does not have to equal `newMap` or `oldMap`.
    * This happens if a key is deleted. Then it does not occur `newMap` but it occurs in `oldMap`.
    * However, it is neither fresh nor stale. It should simply be forgotten.
    */
  def partition[K <: Locatable, V1, V2](newMap: Map[K, V1], oldMap: Map[K, V2]): (Map[K, V1], Map[K, V2]) = {
    val stale = newMap.filter(kv => isStale(kv._1, oldMap))
    val fresh = (oldMap -- stale.keySet).filter(kv => newMap.contains(kv._1))
    (stale, fresh)
  }

  /**
    * Returns `true` if the given `key` is stale w.r.t. the current change set and `oldMap`.
    *
    * A key is stale if either (a) everything has changed, (b) it is new (i.e. not in `oldMap`),
    * (c) it is in the change set, or (d) if its source location is unknown.
    */
  private def isStale[K <: Locatable, V](key: K, oldMap: Map[K, V]): Boolean = this match {
    case ChangeSet.Everything => true
    case ChangeSet.Changes(s) =>
      !oldMap.contains(key) || s.contains(key.loc.source) || key.loc == SourceLocation.Unknown
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
  case class Changes(s: Set[Ast.Source]) extends ChangeSet

}

