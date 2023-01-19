/*
 * Copyright 2023 Magnus Madsen
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

import java.nio.file.Path

sealed trait ManifestError

object ManifestError {

  case class ManifestNotFoundAt(pathString: String) extends ManifestError

  case class MissingRequiredProperty(path: Path, msg: String) extends ManifestError

  case class RequiredPropertyHasWrongType(path: Path, msg: String) extends ManifestError

  case class VersionHasWrongLength(path: Path, msg: String) extends ManifestError

  case class VersionNumberWrong(path: Path, msg: String) extends ManifestError

  case class MavenDependencyFormatError(path: Path, msg: String) extends ManifestError

  case class DependencyFormatError(path: Path, msg: String) extends ManifestError

  case class AuthorNameError(path: Path, msg: String) extends ManifestError

  case class ManifestParseError(path: Path, msg: String) extends ManifestError

  case class IOError(path: Path) extends ManifestError

  case class UnableToDownload(msg: String) extends ManifestError

  /// ...

}
