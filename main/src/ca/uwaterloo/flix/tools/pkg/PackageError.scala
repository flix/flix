package ca.uwaterloo.flix.tools.pkg

sealed trait PackageError

object PackageError {

  case class MissingPackage() extends PackageError

}
