/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.SourceLocation

/**
  * Documentation.
  *
  * @param lines the lines of the comments.
  * @param loc   the source location of the text.
  */
case class Doc(lines: List[String], loc: SourceLocation) {
  def text: String = lines.
    dropWhile(_.isBlank).
    mkString("\n")

  /**
    * Returns a string representation that hides the internals.
    */
  override def toString: String = "Doc(...)"
}
