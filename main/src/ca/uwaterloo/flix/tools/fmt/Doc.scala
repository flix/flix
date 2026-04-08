package ca.uwaterloo.flix.tools.fmt

import scala.annotation.tailrec

sealed trait Layout
object Layout {
  case object SingleLine extends Layout
  case object MultiLine extends Layout
}

sealed trait Doc {
  def <>(right: Doc): Doc = Doc.Concat(this, right)
  def <+>(right: Doc): Doc = this <> Doc.space <> right
  def <|>(right: Doc): Doc = this <> Doc.line <> right
  def <||>(right: Doc): Doc = Doc.LayoutChoice(this, right)
}

object Doc {

  case object Empty extends Doc
  case class Text(str: String) extends Doc
  case class Concat(left: Doc, right: Doc) extends Doc
  case class Line() extends Doc
  case class HardLine() extends Doc
  case class Nest(level: Int, doc: Doc) extends Doc
  case class LayoutChoice(singleLine: Doc, multiLine: Doc) extends Doc
  case class SetLayout(layout: Layout, doc: Doc) extends Doc

  def text(str: String): Doc = Text(str)
  def line: Doc = Line()
  def hardline: Doc = HardLine()
  def space: Doc = Text(" ")
  def empty: Doc = Empty
  def nest(level: Int, doc: Doc): Doc = Nest(level, doc)
  def setLayout(layout: Layout, doc: Doc): Doc = SetLayout(layout, doc)
  def layoutChoice(singleLine: Doc, multiLine: Doc): Doc = LayoutChoice(singleLine, multiLine)

  def pretty(doc: Doc): String = {
    val sb = new StringBuilder
    render(sb, 0, List((0, Layout.MultiLine, doc)))
    sb.toString()
  }

  def pretty(layout: Layout, doc: Doc): String = {
    val sb = new StringBuilder
    render(sb, 0, List((0, layout, doc)))
    sb.toString()
  }

  @tailrec
  private def render(sb: StringBuilder, k: Int, docs: List[(Int, Layout, Doc)]): Unit = docs match {
    case Nil => ()

    case (_, _, Empty) :: z =>
      render(sb, k, z)

    case (i, l, Concat(x, y)) :: z =>
      render(sb, k, (i, l, x) :: (i, l, y) :: z)

    case (i, l, Nest(j, x)) :: z =>
      l match {
        case Layout.SingleLine => render(sb, k, (i, l, x) :: z)
        case Layout.MultiLine  => render(sb, k, (i + j, l, x) :: z)
      }

    case (_, _, Text(s)) :: z =>
      sb.append(s)
      render(sb, k + s.length, z)

    case (i, l, Line()) :: z =>
      l match {
        case Layout.SingleLine =>
          sb.append(' ')
          render(sb, k + 1, z)
        case Layout.MultiLine =>
          sb.append('\n')
          sb.append(" " * i)
          render(sb, i, z)
      }

    case (i, _, HardLine()) :: z =>
      sb.append('\n')
      sb.append(" " * i)
      render(sb, i, z)

    case (i, l, LayoutChoice(s, m)) :: z =>
      l match {
        case Layout.SingleLine => render(sb, k, (i, l, s) :: z)
        case Layout.MultiLine  => render(sb, k, (i, l, m) :: z)
      }

    case (i, _, SetLayout(newLayout, x)) :: z =>
      render(sb, k, (i, newLayout, x) :: z)
  }

  def folddoc(f: (Doc, Doc) => Doc, docs: List[Doc]): Doc = docs match {
    case Nil      => Empty
    case x :: Nil => x
    case x :: xs  => f(x, folddoc(f, xs))
  }
  def spread(docs: List[Doc]): Doc = folddoc(_ <+> _, docs)
  def stack(docs: List[Doc]): Doc = folddoc(_ <|> _, docs)
  def hardStack(docs: List[Doc]): Doc = folddoc((a, b) => a <> hardline <> b, docs)
  def sep(separator: Doc, docs: List[Doc]): Doc = folddoc((a, b) => a <> separator <> b, docs)
  def bracket(l: String, x: Doc, r: String): Doc =
    text(l) <> nest(2, line <> x) <> line <> text(r)
}
