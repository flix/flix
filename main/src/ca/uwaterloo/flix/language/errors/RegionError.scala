package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationMessage

/**
  * A common super-type for region errors.
  */
sealed trait RegionError extends CompilationMessage {
  val kind = "Region Error"
}

object RegionError {

  // TODO

}
