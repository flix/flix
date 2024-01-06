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
  case object MissingManifestError extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         | Cannot create a release without a `flix.toml` file.
         |""".stripMargin
  }

  case object NoLinkedProjectError extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         | Cannot create a release without the `package.github` option in `flix.toml`.
         |""".stripMargin
  }

  case object MissingApiKeyError extends ReleaseError {
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

  case class ProjectInaccessible(project: GitHub.Project) extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         | Could not access GitHub repository: $project
         | This can be caused by:
         |   - Project name being misspelled
         |   - API-key being invalid
         |""".stripMargin
  }
}
