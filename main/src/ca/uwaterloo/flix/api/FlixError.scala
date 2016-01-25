package ca.uwaterloo.flix.api

/**
  * A common super-type for all Flix compilation and runtime errors.
  */
trait FlixError {
  /**
    * Returns a human readable string representation of the error.
    */
  def format: String
}