/*
 * Copyright 2025 Casper Dalgaard Nielsen
 *                Adam Yasser Tallouzi
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.ast.shared

sealed trait SolveMode

object SolveMode {
  case object Default extends SolveMode
  case object WithProvenance extends SolveMode
}
