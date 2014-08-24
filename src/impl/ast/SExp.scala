package impl.ast

trait SExp {
  def head: String
  def body: List[SExp]
}



