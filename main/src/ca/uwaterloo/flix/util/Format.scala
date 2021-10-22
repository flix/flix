package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.language.ast.SourceLocation

object Format {

  def line(left: String, right: String): String = {
    wrap(
      wrap(left, LeftTag) +
        wrap(right, RightTag),
      LineTag)
  }

  def code(loc: SourceLocation, text: String): String =
    wrap(
      wrap(loc.format, LocTag) +
        wrap(text, TextTag),
      CodeTag)


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

  case object LineTag extends Tag {
    override def open: String = "<Line>"
  }

  case object RightTag extends Tag {
    override def open: String = "<Right>"
  }

  case object LeftTag extends Tag {
    override def open: String = "<Left>"
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
