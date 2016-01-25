package ca.uwaterloo.flix.api

/**
  * A common super-type for all Flix compilation and runtime errors.
  */
trait FlixError {
  /**
    * Returns a human readable string representation of the error.
    */
  // TODO: Why is this called so?
  // TODO: Should this extend runtime exception?
  def format: String
}