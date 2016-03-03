package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.language.ast.SourceLocation

/**
  * An exception thrown to indicate a user specified exception.
  *
  * @param msg the error message.
  * @param loc the source location of the user specified exception.
  */
case class UserException(msg: String, loc: SourceLocation) extends FlixException(msg)