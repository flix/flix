package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.language.ast.SourceLocation

trait Formatter {

  def line(left: String, right: String): String =
    this.blue(s"-- $left -------------------------------------------------- $right${System.lineSeparator()}")

  def code(loc: SourceLocation, msg: String): String = {
    val beginLine = loc.beginLine
    val beginCol = loc.beginCol
    val endLine = loc.endLine
    val endCol = loc.endCol

    def arrowUnderline: String = {
      val sb = new StringBuilder()
      val lineAt = loc.lineAt(beginLine)
      val lineNo = beginLine.toString + " | "
      sb.append(lineNo)
        .append(lineAt)
        .append(System.lineSeparator())
        .append(" " * (beginCol + lineNo.length - 1))
        .append(red("^" * (endCol - beginCol)))
        .append(System.lineSeparator())
        .append(" " * (beginCol + lineNo.length - 1))
        .append(msg)
        .toString()
    }

    def leftline: String = {
      val numWidth = endLine.toString.length
      val sb = new StringBuilder()
      for (lineNo <- beginLine to endLine) {
        val currentLine = loc.lineAt(lineNo)
        sb.append(padLeft(numWidth, lineNo.toString))
          .append(" |")
          .append(red(">"))
          .append(" ")
          .append(currentLine)
          .append(System.lineSeparator())
      }
      sb.append(System.lineSeparator())
        .append(msg)
        .toString()
    }

    if (beginLine == endLine)
      arrowUnderline
    else
      leftline
  }

  def black(s: String): String

  def blue(s: String): String

  def cyan(s: String): String

  def green(s: String): String

  def magenta(s: String): String

  def red(s: String): String

  def yellow(s: String): String

  def white(s: String): String

  def bold(s: String): String

  def underline(s: String): String

  private def padLeft(width: Int, s: String): String = String.format("%" + width + "s", s)
}

object Formatter {

  /**
    * A formatter that does not apply any color styling.
    */
  object NoFormatter extends Formatter {

    override def black(s: String): String = s

    override def blue(s: String): String = s

    override def cyan(s: String): String = s

    override def green(s: String): String = s

    override def magenta(s: String): String = s

    override def red(s: String): String = s

    override def yellow(s: String): String = s

    override def white(s: String): String = s

    override def bold(s: String): String = s

    override def underline(s: String): String = s

  }

  /**
    * A terminal context compatible with an ANSI terminal.
    */
  object AnsiTerminalFormatter extends Formatter {

    override def black(s: String): String = Console.BLACK + s + Console.RESET

    override def blue(s: String): String = Console.BLUE + s + Console.RESET

    override def cyan(s: String): String = Console.CYAN + s + Console.RESET

    override def green(s: String): String = Console.GREEN + s + Console.RESET

    override def magenta(s: String): String = Console.MAGENTA + s + Console.RESET

    override def red(s: String): String = Console.RED + s + Console.RESET

    override def yellow(s: String): String = Console.YELLOW + s + Console.RESET

    override def white(s: String): String = Console.WHITE + s + Console.RESET

    override def bold(s: String): String = Console.BOLD + s + Console.RESET

    override def underline(s: String): String = Console.UNDERLINED + s + Console.RESET

  }

  /**
    * Returns `true` if the terminal appears to support at least 256 colors.
    */
  def hasColorSupport: Boolean = isAnsiTerminal || isTrueColorTerminal || isWindowsTerminal

  /**
    * Returns `true` if the terminal appears to be an ANSI terminal.
    */
  private def isAnsiTerminal: Boolean = {
    val term = System.getenv("TERM")
    term != null && (
      term.contains("256") ||
        term.contains("ansi") ||
        term.contains("xterm") ||
        term.contains("screen"))
  }

  /**
    * Returns `true` if the terminal appears to support 24bit colors.
    */
  private def isTrueColorTerminal: Boolean = {
    val colorTerm = System.getenv("COLORTERM")
    colorTerm != null && colorTerm.contains("truecolor")
  }

  /**
    * Returns `true` if the terminal appears to be a Windows Terminal.
    */
  private def isWindowsTerminal: Boolean = {
    val wtSession = System.getenv("WT_SESSION")
    wtSession != null
  }
}
