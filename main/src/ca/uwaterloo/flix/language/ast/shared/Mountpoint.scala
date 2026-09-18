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

object Mountpoint {

  /** An uppercase letter followed by letters, digits, and underscores. */
  private val Valid = "[A-Z][A-Za-z0-9_]*".r

  /** Returns `s` as a mountpoint, if it can name a top-level module. */
  def mkMountpoint(s: String): Option[Mountpoint] =
    if (Valid.matches(s)) Some(Mountpoint(s)) else None

  /**
    * Returns the mountpoint to suggest for a project named `s`.
    *
    * The parts of the name are capitalized and joined, so `tic-tac-toe` suggests `TicTacToe`. A
    * name that yields nothing a module can be called, such as one beginning with a digit, suggests
    * a placeholder instead, because the suggestion is only ever printed in an error.
    */
  def ofProjectName(s: String): Mountpoint =
    mkMountpoint(s.split("[-_]").filter(_.nonEmpty).map(_.capitalize).mkString).getOrElse(Placeholder)

  /** The mountpoint suggested for a project whose name cannot give one. */
  private val Placeholder: Mountpoint = Mountpoint("ModuleName")

}

/**
  * The name of the top-level module a dependency is visible under.
  */
case class Mountpoint(name: String) extends Ordered[Mountpoint] {
  override def compare(that: Mountpoint): Int = this.name.compare(that.name)

  override def toString: String = name
}
