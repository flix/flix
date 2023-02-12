package ca.uwaterloo.flix.api

import java.nio.file.Path

sealed trait BootstrapError

object BootstrapError {
  case class ManifestParseError(p: Path, msg: String) extends BootstrapError

  case class FlixPackageError(msg: String) extends BootstrapError

  case class MavenPackageError(msg: String) extends BootstrapError
}
