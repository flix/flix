/*
 * Copyright 2023 Anna Blume Jakobsen
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

sealed trait PackageError

object PackageError {
  case class VersionDoesNotExist(msg: String) extends PackageError

  case class InvalidProjectName(msg: String) extends PackageError

  case class NoReleasesFound(msg: String) extends PackageError

  case class ProjectNotFound(msg: String) extends PackageError

  case class JsonError(msg: String) extends PackageError

  case class DownloadError(msg: String) extends PackageError

}
