package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.language.ast.SourceLocation

import scala.collection.mutable

trait Formatter {

  def line(left: String, right: String): String =
    this.blue(s"-- $left -------------------------------------------------- $right${System.lineSeparator()}")

  def code(loc: SourceLocation, msg: String): String = {
    val beginLine = loc.beginLine
    val beginCol = loc.beginCol
    val endLine = loc.endLine
    val endCol = loc.endCol

    def arrowUnderline: String = {
      val sb = new mutable.StringBuilder
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
      val sb = new mutable.StringBuilder
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

  def brightBlack(s: String): String

  def bgBlack(s: String): String

  def bgBrightBlack(s: String): String

  def blue(s: String): String

  def brightBlue(s: String): String

  def bgBlue(s: String): String

  def bgBrightBlue(s: String): String

  def cyan(s: String): String

  def brightCyan(s: String): String

  def bgCyan(s: String): String

  def bgBrightCyan(s: String): String

  def green(s: String): String

  def brightGreen(s: String): String

  def bgGreen(s: String): String

  def bgBrightGreen(s: String): String

  def magenta(s: String): String

  def brightMagenta(s: String): String

  def bgMagenta(s: String): String

  def bgBrightMagenta(s: String): String

  def red(s: String): String

  def brightRed(s: String): String

  def bgRed(s: String): String

  def bgBrightRed(s: String): String

  def yellow(s: String): String

  def brightYellow(s: String): String

  def bgYellow(s: String): String

  def bgBrightYellow(s: String): String

  def white(s: String): String

  def brightWhite(s: String): String

  def bgWhite(s: String): String

  def bgBrightWhite(s: String): String

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

    override def brightBlack(s: String): String = s

    override def bgBlack(s: String): String = s

    override def bgBrightBlack(s: String): String = s

    override def blue(s: String): String = s

    override def brightBlue(s: String): String = s

    override def bgBlue(s: String): String = s

    override def bgBrightBlue(s: String): String = s

    override def cyan(s: String): String = s

    override def brightCyan(s: String): String = s

    override def bgCyan(s: String): String = s

    override def bgBrightCyan(s: String): String = s

    override def green(s: String): String = s

    override def brightGreen(s: String): String = s

    override def bgGreen(s: String): String = s

    override def bgBrightGreen(s: String): String = s

    override def magenta(s: String): String = s

    override def brightMagenta(s: String): String = s

    override def bgMagenta(s: String): String = s

    override def bgBrightMagenta(s: String): String = s

    override def red(s: String): String = s

    override def brightRed(s: String): String = s

    override def bgRed(s: String): String = s

    override def bgBrightRed(s: String): String = s

    override def yellow(s: String): String = s

    override def brightYellow(s: String): String = s

    override def bgYellow(s: String): String = s

    override def bgBrightYellow(s: String): String = s

    override def white(s: String): String = s

    override def brightWhite(s: String): String = s

    override def bgWhite(s: String): String = s

    override def bgBrightWhite(s: String): String = s

    override def bold(s: String): String = s

    override def underline(s: String): String = s

  }

  /**
    * A terminal context compatible with an ANSI terminal.
    */
  object AnsiTerminalFormatter extends Formatter {

    override def black(s: String): String = fgColor(1, 1, 1, s)

    override def brightBlack(s: String): String = fgColor(128, 128, 128, s)

    override def bgBlack(s: String): String = bgColor(1, 1, 1, brightWhite(s))

    override def bgBrightBlack(s: String): String = bgColor(128, 128, 128, brightWhite(s))

    override def blue(s: String): String = fgColor(0, 111, 184, s)

    override def brightBlue(s: String): String = fgColor(0, 0, 255, s)

    override def bgBlue(s: String): String = bgColor(0, 111, 184, brightWhite(s))

    override def bgBrightBlue(s: String): String = bgColor(0, 0, 255, brightWhite(s))

    override def cyan(s: String): String = fgColor(44, 181, 233, s)

    override def brightCyan(s: String): String = fgColor(0, 255, 255, s)

    override def bgCyan(s: String): String = bgColor(44, 181, 233, brightWhite(s))

    override def bgBrightCyan(s: String): String = bgColor(0, 255, 255, black(s))

    override def green(s: String): String = fgColor(57, 181, 74, s)

    override def brightGreen(s: String): String = fgColor(0, 255, 0, s)

    override def bgGreen(s: String): String = bgColor(57, 181, 74, brightWhite(s))

    override def bgBrightGreen(s: String): String = bgColor(0, 255, 0, black(s))

    override def magenta(s: String): String = fgColor(118, 38, 113, s)

    override def brightMagenta(s: String): String = fgColor(255, 0, 255, s)

    override def bgMagenta(s: String): String = bgColor(118, 38, 113, brightWhite(s))

    override def bgBrightMagenta(s: String): String = bgColor(255, 0, 255, brightWhite(s))

    override def red(s: String): String = fgColor(222, 56, 43, s)

    override def brightRed(s: String): String = fgColor(255, 0, 0, s)

    override def bgRed(s: String): String = bgColor(222, 56, 43, brightWhite(s))

    override def bgBrightRed(s: String): String = bgColor(255, 0, 0, brightWhite(s))

    override def yellow(s: String): String = fgColor(255, 199, 6, s)

    override def brightYellow(s: String): String = fgColor(255, 255, 0, s)

    override def bgYellow(s: String): String = bgColor(255, 199, 6, brightWhite(s))

    override def bgBrightYellow(s: String): String = bgColor(255, 255, 0, black(s))

    override def white(s: String): String = fgColor(204, 204, 204, s)

    override def brightWhite(s: String): String = fgColor(255, 255, 255, s)

    override def bgWhite(s: String): String = bgColor(204, 204, 204, black(s))

    override def bgBrightWhite(s: String): String = bgColor(255, 255, 255, brightWhite(s))

    override def bold(s: String): String = Console.BOLD + s + Console.RESET

    override def underline(s: String): String = Console.UNDERLINED + s + Console.RESET

    private def fgColor(r: Int, g: Int, b: Int, s: String): String = escape() + s"[38;2;$r;$g;${b}m" + s + escape() + "[0m"

    private def bgColor(r: Int, g: Int, b: Int, s: String): String = escape() + s"[48;2;$r;$g;${b}m" + s + escape() + "[0m"

    private def escape(): String = "\u001b"

  }

  /**
    * Returns the default formatter based on the detected color support.
    */
  def getDefault: Formatter = if (hasColorSupport) AnsiTerminalFormatter else NoFormatter

  /**
    * Returns `true` if the terminal appears to support at least 256 colors.
    */
  def hasColorSupport: Boolean = isAnsiTerminal || isTrueColorTerminal || isWindowsTerminal || isIdeaTerminal

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

  /**
    * Returns `true` if the terminal appears to be an IDEA emulator.
    */
  private def isIdeaTerminal: Boolean = {
    val emulator = System.getenv("TERMINAL_EMULATOR")
    emulator != null && emulator.contains("JetBrains")
  }

}
