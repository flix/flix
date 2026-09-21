/*
 * Copyright 2026 Magnus Madsen
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
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.phase.Lexer

object Mountpoint {

  /** A letter followed by letters, digits, and underscores. */
  private val Valid = "[A-Za-z][A-Za-z0-9_]*".r

  /**
    * Returns `s` as a mountpoint, if it can be written before `::`.
    *
    * A keyword cannot: the lexer reads `type::` as `type` and not as a mount.
    */
  def mkMountpoint(s: String): Option[Mountpoint] =
    if (Valid.matches(s) && !Lexer.isKeyword(s)) Some(Mountpoint(s)) else None

  /**
    * Returns the mount of a dependency on `id` that declares none: the name of its repository, if
    * that is a mountpoint. The name is never folded into one, so e.g. `tic-tac-toe` has none.
    */
  def ofRepoName(id: PackageId): Option[Mountpoint] =
    mkMountpoint(id.name)

}

/**
  * The name a dependency is reached under, written before `::` in a use, e.g. `use flixball::Board`.
  */
case class Mountpoint(name: String) extends Ordered[Mountpoint] {
  override def compare(that: Mountpoint): Int = this.name.compare(that.name)

  override def toString: String = name
}
