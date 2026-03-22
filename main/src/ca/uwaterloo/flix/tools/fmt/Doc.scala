package ca.uwaterloo.flix.tools


/**
  * A simple pretty-printing Document type based on the Wadler-Lindig algorithm.
  */
sealed trait Doc {
  def <>(right: Doc): Doc = Doc.Concat(this, right)
  def <+>(right: Doc): Doc = Doc.Concat(this, Doc.Concat(Doc.space, right))
}

object Doc {

  /**
    * The Doc type represents a document that can be pretty-printed. It can be one of the following:
    * - Empty: An empty document.
    * - Text: A document containing a string of text.
    * - Concat: A document that is the concatenation of two other documents.
    * - Nest: A document that is nested with a given indentation level.
    * - SoftLine: A document that represents a soft line, which can be rendered as a space or a line break
    * - Group: A document that is grouped together
    */
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

  /**
    * Pretty prints the given document by traversing the Doc structure and concatenating the text.
    * This is a simple implementation that does not handle line breaking or grouping for now.
    * TODO: Implement line breaking and grouping according to the Wadler-Lindig combinatoric.
    * @param doc
    * @return
    */
  def pretty(doc: Doc): String = {
    val stack = scala.collection.mutable.Stack[Doc](doc)
    val result = new StringBuilder

    while (stack.nonEmpty) {
      stack.pop() match {
        case Empty => empty
        case Text(s) => result.append(s)
        case Concat(left, right) =>
          stack.push(right)
          stack.push(left)
        case Nest(_, inner) => stack.push(inner)
        case SoftLine(s) => result.append(s)
        case Group(inner) => stack.push(inner)
      }
    }

    result.toString()
  }
}
