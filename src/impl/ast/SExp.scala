package impl.ast

trait SExp

object SExp {

  case class Keyword(s: String) extends SExp

  case class Variable(x: java.lang.String) extends SExp

  case class Lst(xs: List[SExp]) extends SExp

}

trait Literal

object Literal {
  case class Name(s: java.lang.String) extends SExp

  case class Bool(b: scala.Boolean) extends SExp

  case class Int(i: scala.Int) extends SExp

  case class Str(s: java.lang.String) extends SExp
}
