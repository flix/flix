package impl.ast

trait SExp

object SExp {

  /**
   * Literals
   */
  case class Bool(b: scala.Boolean) extends SExp

  case class Int(i: scala.Int) extends SExp

  case class Str(s: java.lang.String) extends SExp


  case class Name(s: java.lang.String) extends SExp

  case class Lst(xs: List[SExp]) extends SExp

}

trait Keyword extends SExp

object Keyword {

  case object DefType extends SExp

  case object DefBot extends SExp

  case object DefLeq extends SExp

  case object DefLub extends SExp

  case object DefFun extends SExp

  case object Rule extends SExp

  case object Let extends SExp

  case object Case extends SExp

  case object Matcg extends SExp

}