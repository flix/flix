package ca.uwaterloo.flix.util

/**
  * An exception thrown to indicate an internal compiler error.
  *
  * This exception should never be thrown.
  *
  * @param message the error message.
  */
case class InternalCompilerException(message: String) extends RuntimeException(message)
