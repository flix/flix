package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationError

/**
  * A common super-type for trivial errors.
  */
sealed trait TrivialError extends CompilationError {
  val kind = "Trivial Error"
}

object TrivialError {

  // TODO

}
