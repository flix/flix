/*
 * Copyright 2024 Andreas Stenbæk Larsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.tools.pkg

sealed trait RepositoryError

object RepositoryError {
  case class UnsupportedRepositoryError(s: String) extends RepositoryError
}
