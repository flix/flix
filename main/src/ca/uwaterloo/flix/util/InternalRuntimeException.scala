package ca.uwaterloo.flix.util

/**
  * An exception thrown to indicate an internal runtime error.
  *
  * This exception should never be thrown.
  *
  * @param message the error message.
  */
case class InternalRuntimeException(message: String) extends RuntimeException(message)
