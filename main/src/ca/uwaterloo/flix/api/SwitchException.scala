package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.language.ast.SourceLocation

/**
  * An exception thrown to indicate an incomplete switch error.
  *
  * @param msg the error message.
  * @param loc the source location of the incomplete switch.
  */
case class SwitchException(msg: String, loc: SourceLocation) extends FlixException(msg)