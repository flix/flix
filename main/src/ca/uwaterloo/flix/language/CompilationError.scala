package ca.uwaterloo.flix.language

/**
  * A common super-type for compilation errors.
  */
trait CompilationError {
  /**
    * The error message.
    */
  def message: String
}
