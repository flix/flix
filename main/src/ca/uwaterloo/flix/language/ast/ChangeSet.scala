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
    * A fresh element is one that can be reused. A stale element is one that must be re-compiled.
    * A element that is neither fresh nor stale can be deleted.
    *
    * An entry is stale if it is (a) stale w.r.t. the current change set and `oldMap`.
    * An entry is fresh if it is (a) not stale and (b) occurs in `newMap`.
    *
    * Note that the union of stale and fresh does not have to equal `newMap` or `oldMap`.
    * This happens if an element is deleted. Then it does not occur `newMap` but it occurs in `oldMap`.
    * However, it is neither fresh nor stale. It should simply be forgotten.
    */
  def partition[V1, V2](newMap: Map[Symbol.DefnSym, V1], oldMap: Map[Symbol.DefnSym, V2]): (Map[Symbol.DefnSym, V1], Map[Symbol.DefnSym, V2]) = {
    val stale = newMap.filter(kv => isStale(kv._1, oldMap))
    val fresh = (oldMap -- stale.keySet).filter(kv => newMap.contains(kv._1))
    (stale, fresh)
  }

  /**
    * Returns `true` if the given symbol `sym` is stale w.r.t. the current change set and `oldMap`.
    */
  private def isStale[V](sym: Symbol.DefnSym, oldMap: Map[Symbol.DefnSym, V]): Boolean = this match {
    case ChangeSet.Everything => true
    case ChangeSet.Changes(s) => !oldMap.contains(sym) || s.contains(sym.loc.source)
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

