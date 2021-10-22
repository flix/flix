package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.language.ast.SourceLocation

object Format {

  // Maybe move this function somewhere else and simply wrap left and right in LineTag
  def line(left: String, right: String): String =
    this.blue(s"-- $left -------------------------------------------------- $right${System.lineSeparator()}")

  // Maybe move this function somewhere else and simply wrap in CodeTag
  def code(loc: SourceLocation, msg: String): String = {
    val beginLine = loc.beginLine
    val beginCol = loc.beginCol
    val endLine = loc.endLine
    val endCol = loc.endCol
    val lineAt = loc.lineAt

    def underline: String = {
      val sb = new StringBuilder()
      val lineNo = beginLine.toString + " | "
      sb.append(lineNo)
        .append(lineAt(beginLine))
        .append(System.lineSeparator())
        .append(" " * (beginCol + lineNo.length - 1))
        .append(red("^" * (endCol - beginCol)))
        .append(System.lineSeparator())
        .append(" " * (beginCol + lineNo.length - 1))
        .append(msg)
        .append(System.lineSeparator())
        .toString()
    }

    def leftline: String = {
      val sb = new StringBuilder()
      for (lineNo <- beginLine to endLine) {
        val currentLine = lineAt(lineNo)
        sb.append(lineNo)
          .append(" |")
          .append(red(">"))
          .append(" ")
          .append(currentLine)
          .append(System.lineSeparator())
      }
      sb.append(System.lineSeparator())
        .append(msg)
        .append(System.lineSeparator())
        .toString()
    }

    if (beginLine == endLine)
      underline
    else
      leftline
  }

  def text(s: String): String = wrap(s, TextTag)

  def black(s: String): String = wrap(s, BlackTag)

  def blue(s: String): String = wrap(s, BlueTag)

  def cyan(s: String): String = wrap(s, CyanTag)

  def green(s: String): String = wrap(s, GreenTag)

  def magenta(s: String): String = wrap(s, MagentaTag)

  def red(s: String): String = wrap(s, RedTag)

  def yellow(s: String): String = wrap(s, YellowTag)

  def white(s: String): String = wrap(s, WhiteTag)

  def bold(s: String): String = wrap(s, BoldTag)

  def underline(s: String): String = wrap(s, UnderlineTag)

  private def wrap(s: String, t: Tag): String = t.open + s + t.close

  sealed trait Tag {
    def open: String

    def close: String = open.replace("<", "</")
  }

  case object CodeTag extends Tag {
    override def open: String = "<Code>"
  }

  case object LocTag extends Tag {
    override def open: String = "<Loc>"
  }

  case object TextTag extends Tag {
    override def open: String = "<Text>"
  }

  case object BlackTag extends Tag {
    override def open: String = "<Black>"
  }

  case object BlueTag extends Tag {
    override def open: String = "<Blue>"
  }

  case object CyanTag extends Tag {
    override def open: String = "<Cyan>"
  }

  case object GreenTag extends Tag {
    override def open: String = "<Green>"
  }

  case object MagentaTag extends Tag {
    override def open: String = "<Magenta>"
  }

  case object RedTag extends Tag {
    override def open: String = "<Red>"
  }

  case object YellowTag extends Tag {
    override def open: String = "<Yellow>"
  }

  case object WhiteTag extends Tag {
    override def open: String = "<White>"
  }

  case object BoldTag extends Tag {
    override def open: String = "<Bold>"
  }

  case object UnderlineTag extends Tag {
    override def open: String = "<Underline>"
  }
}
