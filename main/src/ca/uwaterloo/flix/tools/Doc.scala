package ca.uwaterloo.flix.tools

/**
 * Represents a structured document for pretty-printing.
 */
sealed trait Doc {
  def <>(right: Doc): Doc = Doc.Concat(this, right)
}

object Doc {
  case object Empty extends Doc
  case class Text(s: String) extends Doc
  case class Concat(left: Doc, right: Doc) extends Doc
  case class Nest(indent: Int, doc: Doc) extends Doc
  case class SoftLine(s: String) extends Doc
  case class Group(doc: Doc) extends Doc

  def empty: Doc = Empty
  def text(s: String): Doc = Text(s)
  def space: Doc = SoftLine(" ")
  def lineBreak(s: String): Doc = SoftLine(s)
  def nest(indent: Int, doc: Doc): Doc = Nest(indent, doc)
  def group(doc: Doc): Doc = Group(doc)

  def pretty(doc: Doc): String = doc match {
    case Empty => ""
    case Text(s) => s
    case Concat(left, right) => pretty(left) + pretty(right)
    case Nest(_, inner) => pretty(inner)
    case SoftLine(s) => s
    case Group(inner) => pretty(inner)
  }
}
