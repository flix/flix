package ca.uwaterloo.flix.api

/**
  * A common super-type for all Flix compilation and run-time errors.
  */
trait FlixError extends Exception {

  /**
    * Returns a human readable string representation of the error.
    */
  def message: String

}
