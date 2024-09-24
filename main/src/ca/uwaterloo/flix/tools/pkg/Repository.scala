/*
 * Copyright 2024 Magnus Madsen
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
package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}

sealed trait Repository

object Repository {

  /** Convert a [[String]] into a [[Repository]]. */
  def mkRepository(s: String): Result[Repository, RepositoryError] = s match {
    case "github" => Ok(Repository.GitHub)
    case _ => Err(RepositoryError.UnsupportedRepositoryError(s))
  }

  /** A GitHub repository. */
  case object GitHub extends Repository
}
