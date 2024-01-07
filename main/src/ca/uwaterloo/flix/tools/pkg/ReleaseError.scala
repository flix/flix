/*
 * Copyright 2024 Holger Dal Mogensen
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

import ca.uwaterloo.flix.tools.pkg.github.GitHub
import ca.uwaterloo.flix.util.Formatter

sealed trait ReleaseError {
  /**
    * Returns a human-readable and formatted string representation of this error.
    */
  def message(f: Formatter): String
}

object ReleaseError {
  case object MissingManifest extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         | Cannot create a release without a `flix.toml` file.
         |""".stripMargin
  }

  case object NoLinkedProject extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         | Cannot create a release without the `package.github` option in `flix.toml`.
         |""".stripMargin
  }

  case object MissingApiKey extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         | Cannot create a release without the `--github-key` option.
         |""".stripMargin
  }

  case object Cancelled extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         | Release cancelled.
         |""".stripMargin
  }

  case object NetworkError extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         | Cannot reach GitHub at the current moment.
         |""".stripMargin
  }

  case object InvalidApiKeyError extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         | The API-key is not valid or does not have the necessary permissions.
         |""".stripMargin
  }

  case class InvalidProject(project: GitHub.Project) extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         | The GitHub repository does not exist:
         |  ${f.red(project.toString)}
         |""".stripMargin
  }

  case class AlreadyExists(project: GitHub.Project, version: SemVer) extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         | Release with version $version already exists.
         |""".stripMargin
  }

  case class Unexpected(code: Int, message: String) extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         | GitHub failed with an unexpected response:
         |  $code: $message
         |""".stripMargin
  }

  case class JsonError(json: String) extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         | GitHub returned JSON in an unexpected format:
         |  ${f.cyan(json)}
         |""".stripMargin
  }
}
