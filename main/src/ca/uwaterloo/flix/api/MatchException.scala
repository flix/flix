package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.language.ast.SourceLocation

/**
  * An exception thrown to indicate an incomplete match error.
  *
  * @param msg the error message.
  * @param loc the source location of the incomplete match.
  */
case class MatchException(msg: String, loc: SourceLocation) extends FlixException(msg)