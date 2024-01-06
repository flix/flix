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

  case class UnknownResponse(code: Int, message: String) extends ReleaseError {
    override def message(f: Formatter): String =
      s"""
         | GitHub failed with an unknown response:
         |  $code: $message
         |""".stripMargin
  }
}
