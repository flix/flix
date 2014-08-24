package impl.ast

trait SExp

object SExp {
  case class Str(s: String) extends SExp
  case class Lst(xs: List[SExp]) extends SExp
}
