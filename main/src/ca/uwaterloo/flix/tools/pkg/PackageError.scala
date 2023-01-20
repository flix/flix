package ca.uwaterloo.flix.tools.pkg

sealed trait PackageError

object PackageError {
  case class VersionDoesNotExist(msg: String) extends PackageError

  case class NoReleasesFound(msg: String) extends PackageError

}
