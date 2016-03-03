package ca.uwaterloo.flix.api

/**
  * An exception thrown to indicate a Flix program exception.
  */
abstract class FlixException(msg: String) extends RuntimeException(msg)