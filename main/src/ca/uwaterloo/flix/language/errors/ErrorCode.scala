package ca.uwaterloo.flix.language.errors

sealed trait ErrorCode {
  import ErrorCode.*
  def id: Int = this match {
    case ConstructorNotFound => 47261
    case Another1            => 68572
    case Another2            => 57285
    case Another3            => 28547
    case Another4            => 86726
    case Another5            => 57294
  }
}

object ErrorCode {

  case object ConstructorNotFound extends ErrorCode

  case object Another1 extends ErrorCode

  case object Another2 extends ErrorCode

  case object Another3 extends ErrorCode

  case object Another4 extends ErrorCode

  case object Another5 extends ErrorCode

}
