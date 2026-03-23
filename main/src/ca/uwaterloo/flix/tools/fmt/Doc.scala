package ca.uwaterloo.flix.tools.fmt

sealed trait Doc {
  def <>(that: Doc): Doc = Doc.Concat(this, that)
  def <+>(that: Doc): Doc = this <> Doc.space <> that
}

object Doc {

  case object Empty extends Doc
  case class Text(s: String) extends Doc
  case class Concat(left: Doc, right: Doc) extends Doc
  case class Nest(indent: Int, doc: Doc) extends Doc
  case object Line extends Doc
  case object SoftLine extends Doc
  case class Group(doc: Doc) extends Doc

  def empty: Doc = Empty
  def text(s: String): Doc = Text(s)
  def space: Doc = Text(" ")
  def line: Doc = Line
  def softline: Doc = SoftLine
  def group(doc: Doc): Doc = Group(doc)
  def nest(indent: Int, doc: Doc): Doc = Nest(indent, doc)


  sealed trait Mode
  object Mode {
    case object Flat extends Mode
    case object Break extends Mode
  }

  def pretty(doc: Doc, width: Int = 80): String = {
    val out = new StringBuilder
    val stack = scala.collection.mutable.Stack[(Int, Mode, Doc)]()
    stack.push((0, Mode.Break, doc))

    var col = 0

    while (stack.nonEmpty) {
      stack.pop() match {

        case (_, _, Empty) =>

        case (_, _, Text(s)) =>
          out.append(s)
          col += s.length

        case (indent, mode, SoftLine) =>
          if (mode == Mode.Flat) {
            out.append(" ")
            col += 1
          } else {
            out.append("\n")
            out.append(" " * indent)
            col = indent
          }

        case (indent, _, Line) =>
          out.append("\n")
          out.append(" " * indent)
          col = indent

        case (indent, mode, Concat(l, r)) =>
          stack.push((indent, mode, r))
          stack.push((indent, mode, l))

        case (indent, mode, Nest(i, d)) =>
          stack.push((indent + i, mode, d))

        case (indent, mode, Group(d)) =>
          val trial = (indent, Mode.Flat, d) :: stack.toList
          if (fits(width - col, trial)) {
            stack.push((indent, Mode.Flat, d))
          } else {
            stack.push((indent, Mode.Break, d))
          }
      }
    }

    out.toString()
  }


  private def fits(width: Int, stack: List[(Int, Mode, Doc)]): Boolean = {
    var w = width
    val localStack = scala.collection.mutable.Stack.from(stack)

    while (w >= 0 && localStack.nonEmpty) {
      localStack.pop() match {

        case (_, _, Empty) =>

        case (_, _, Text(s)) =>
          w -= s.length

        case (_, mode, SoftLine) =>
          if (mode == Mode.Flat) w -= 1
          else return true

        case (_, _, Line) =>
          return true

        case (i, m, Concat(l, r)) =>
          localStack.push((i, m, r))
          localStack.push((i, m, l))

        case (i, m, Nest(j, d)) =>
          localStack.push((i + j, m, d))

        case (i, _, Group(d)) =>
          localStack.push((i, Mode.Flat, d))
      }
    }

    w >= 0
  }
}
