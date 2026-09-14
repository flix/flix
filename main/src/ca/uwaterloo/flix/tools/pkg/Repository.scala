/*
 * Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}

sealed trait Repository

object Repository {

  /**
   * Convert a [[String]] into a [[Repository]].
   */
  def mkRepository(s: String): Result[Repository, RepositoryError] = s match {
    case "github" => Ok(Repository.GitHub)
    case _ => Err(RepositoryError.UnsupportedRepositoryError(s))
  }

  /**
   * A GitHub repository.
   */
  case object GitHub extends Repository
}
