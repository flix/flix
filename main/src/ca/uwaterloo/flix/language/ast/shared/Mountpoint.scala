/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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

}

/**
  * The name a dependency is reached under, written before `::` in a use, e.g. `use flixball::Board`.
  */
case class Mountpoint(name: String) extends Ordered[Mountpoint] {
  override def compare(that: Mountpoint): Int = this.name.compare(that.name)

  override def toString: String = name
}
