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

/**
  * A host that Flix packages are published to.
  */
sealed trait Repository

object Repository {

  /** Returns `s` as a repository, if it names one Flix can download a package from. */
  def mkRepository(s: String): Option[Repository] = s match {
    case "github" => Some(Repository.GitHub)
    case _ => None
  }

  /** A GitHub repository. */
  case object GitHub extends Repository {
    override def toString: String = "github"
  }

}
