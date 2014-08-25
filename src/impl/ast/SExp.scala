package impl.ast

trait SExp

object SExp {
  case class Variable(x: java.lang.String) extends SExp

  case class Name(s: java.lang.String) extends SExp

  case class Lst(xs: List[SExp]) extends SExp

}

trait Literal

object Literal {
  case class Bool(b: scala.Boolean) extends SExp

  case class Int(i: scala.Int) extends SExp

  case class Str(s: java.lang.String) extends SExp
}

trait Keyword extends SExp

object Keyword {

  case class DefType(s: String) extends Keyword

  case object DefBot extends Keyword

  case object DefLeq extends Keyword

  case object DefLub extends Keyword

  case object DefFun extends Keyword

  case object Rule extends Keyword

  case object Let extends Keyword

  case object Case extends Keyword

  case object Match extends Keyword

  case object Equal extends Keyword

  case object NotEqual extends Keyword

}