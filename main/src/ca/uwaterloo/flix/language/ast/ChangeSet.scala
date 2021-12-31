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
    * Returns `true` if the given symbol `sym` is stale.
    */
  def isStale[V](sym: Symbol.DefnSym, oldRoot: Map[Symbol.DefnSym, V]): Boolean = this match {
    case ChangeSet.Everything => true
    case ChangeSet.Changes(s) => !oldRoot.contains(sym) || s.contains(sym.loc.source)
  }

}

object ChangeSet {

  /**
    * Represents a change set where everything is changed (i.e. used for a complete recompile).
    */
  case object Everything extends ChangeSet

  /**
    * Represents the set `s` of changed sources.
    */
  case class Changes(s: Set[Ast.Source]) extends ChangeSet

}

