package ca.uwaterloo.flix.tools.fmt

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

  private sealed trait SimpleDoc
  private case object SNil extends SimpleDoc
  private case class SText(str: String, rest: SimpleDoc) extends SimpleDoc
  private case class SLine(indent: Int, rest: SimpleDoc) extends SimpleDoc

  def pretty(doc: Doc): String = {
    val resolved = best(0, List((0, doc)))
    layout(resolved)
  }

  private def layout(sdoc: SimpleDoc): String = {
    val sb = new StringBuilder
    var current = sdoc
    while (current != SNil) {
      current match {
        case SText(s, rest) =>
          sb.append(s)
          current = rest
        case SLine(indent, rest) =>
          sb.append('\n')
          sb.append(" " * indent)
          current = rest
        case SNil =>
      }
    }
    sb.toString()
  }

  private def best(k: Int, docs: List[(Int, Doc)]): SimpleDoc = docs match {
    case Nil                      => SNil
    case (_, Empty) :: z          => best(k, z)
    case (i, Concat(x, y)) :: z  => best(k, (i, x) :: (i, y) :: z)
    case (i, Nest(j, x)) :: z    => best(k, (i + j, x) :: z)
    case (i, Text(s)) :: z       => SText(s, best(k + s.length, z))
    case (i, Line()) :: z        => SLine(i, best(i, z))
    case (i, Group(x)) :: z      => best(k, (i, x) :: z)
  }
}
