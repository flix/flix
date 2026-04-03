package ca.uwaterloo.flix.tools.fmt

import scala.annotation.tailrec

sealed trait Doc {
  def <>(right: Doc): Doc = Doc.Concat(this, right)
  def <+>(right: Doc): Doc = this <> Doc.space <> right
  def <|>(right: Doc): Doc = this <> Doc.line <> right
}

object Doc {

  case object Empty extends Doc
  case class Text(str: String) extends Doc
  case class Concat(left: Doc, right: Doc) extends Doc
  case class Line() extends Doc
  case class Nest(level: Int, doc: Doc) extends Doc
  case class Group(doc: Doc) extends Doc

  def text(str: String): Doc = Text(str)
  def line: Doc = Line()
  def space: Doc = Text(" ")
  def empty: Doc = Empty

  def pretty(doc: Doc): String = {
    val sb = new StringBuilder
    render(sb, 0, List((0, doc)))
    sb.toString()
  }

  @tailrec
  private def render(sb: StringBuilder, k: Int, docs: List[(Int, Doc)]): Unit = docs match {
    case Nil                      =>
    case (_, Empty) :: z          => render(sb, k, z)
    case (i, Concat(x, y)) :: z  => render(sb, k, (i, x) :: (i, y) :: z)
    case (i, Nest(j, x)) :: z    => render(sb, k, (i + j, x) :: z)
    case (i, Text(s)) :: z       => sb.append(s); render(sb, k + s.length, z)
    case (i, Line()) :: z        => sb.append('\n'); sb.append(" " * i); render(sb, i, z)
    case (i, Group(x)) :: z      => render(sb, k, (i, x) :: z)
  }
}
