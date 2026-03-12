package ca.uwaterloo.flix.tools

class PrettyPrinter {
  private sealed  trait Doc

  private case object Nil extends Doc
  private case class Cons(left: Doc, right: Doc) extends Doc
  private case class Text(s: String) extends Doc
  private case class Nest(indent: Int, doc: Doc) extends Doc
  private case class SoftLine(s: String) extends Doc
  private case class Group(doc: Doc) extends Doc

  private def empty: Doc = Nil
  private def <>(left: Doc, right: Doc): Doc = Cons(left, right)
  private def text(s: String): Doc = Text(s)
  private def nest(indent: Int, doc: Doc): Doc = Nest(indent, doc)
  private def softLine(s: String): Doc = SoftLine(s)
  private def group(doc: Doc): Doc = Group(doc)
}
