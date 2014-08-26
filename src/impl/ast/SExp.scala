package impl.ast

trait SExp

object SExp {

  case class Unit(token: String) extends SExp

  case class Bool(token: String) extends SExp

  case class Int(token: String) extends SExp

  case class Str(token: String) extends SExp

  case class Keyword(token: String) extends SExp

  case class Var(token: String) extends SExp

  case class Name(token: String) extends SExp

  case class Label(token: String) extends SExp

  case class Lst(xs: List[SExp]) extends SExp

}
