package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationError

/**
  * A common super-type for scope errors.
  */
sealed trait ScopeError extends CompilationError {
  val kind = "Scope Error"
}

object ScopeError {
  // MATT
}
