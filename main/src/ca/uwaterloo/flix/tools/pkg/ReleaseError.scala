/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
  case object Cancelled extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         |Release cancelled.
         |""".stripMargin
  }

  case object InvalidApiKeyError extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         |The API-key is not valid or does not have the necessary permissions.
         |""".stripMargin
  }

  case object MissingApiKey extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         |Cannot create a release without a GitHub token.
         |This can be passed via:
         |- The --github-token command line option.
         |- A file named .GITHUB_TOKEN in the project's root.
         |- The GITHUB_TOKEN environment variable.
         |""".stripMargin
  }

  case object MissingManifest extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         |Cannot create a release without a `flix.toml` file.
         |""".stripMargin
  }

  case object MissingRepository extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         |Cannot create a release without the `package.repository` option in `flix.toml`.
         |""".stripMargin
  }

  case object NetworkError extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         |Cannot reach GitHub at the current moment.
         |""".stripMargin
  }

  case class ReleaseAlreadyExists(project: GitHub.Project, version: SemVer) extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         |Release with version $version already exists.
         |""".stripMargin
  }

  case class RepositoryNotFound(project: GitHub.Project) extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         |The GitHub repository does not exist:
         | ${f.red(project.toString)}
         |""".stripMargin
  }

  case class UnexpectedResponseCode(code: Int, message: String) extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         |GitHub failed with an unexpected response:
         | $code: $message
         |""".stripMargin
  }

  case class UnexpectedResponseJson(json: String) extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         |GitHub returned JSON in an unexpected format:
         | ${f.cyan(json)}
         |""".stripMargin
  }
}
