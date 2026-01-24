package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.language.CompilationMessageKind
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.ast.shared.Source
import ca.uwaterloo.flix.language.errors.ErrorCode
import ca.uwaterloo.flix.util.collection.ListOps

import scala.collection.mutable

trait Formatter {

  def line(kind: CompilationMessageKind, code: ErrorCode, source: Source): String = {
    val minWidth = 80
    val fixedChars = 8
    val k = kind.toString
    val c = code.toString
    val s = source.name
    val numberOfDashes = Math.max(3, minWidth - fixedChars - k.length - c.length - s.length)
    val dashes = "-" * numberOfDashes
    s"-- ${blue(k)} ${blue(s"[$c]")} $dashes ${blue(s)}${System.lineSeparator()}"
  }

  def src(loc: SourceLocation, msg: String): String = {
    val beginLine = loc.startLine
    val beginCol = loc.startCol
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

  /**
    * Create a table.
    *
    * @param colHeaders    The headers for each column.
    * @param colFormatters The functions to format (e.g. color) the contents of the respective columns
    *                      in the same order as the `colHeaders`. This is applied after the contents are padded.
    *                      Must have the same length as `colHeaders`.
    * @param rows          The list of the table's rows. Each row must have the same length as `colHeaders`.
    */
  def table(colHeaders: List[String], colFormatters: List[String => String], rows: List[List[String]]): String = {
    val cols = rows.transpose
    val (headersPadded, colsPadded) = ListOps.zip(colHeaders, cols).map { case (header, col) =>
      val headerPadded :: colPadded = padToLongest(header :: col)
      (headerPadded, colPadded)
    }.unzip

    // Formatting must happen after padding
    val colsFormatted = ListOps.zip(colsPadded, colFormatters).map { case (c, f) => c.map(f) }
    val rowsFormatted = colsFormatted.transpose

    val sb = new mutable.StringBuilder
    val colSeparator = "    "
    sb.append(headersPadded.mkString(colSeparator))
    for (row <- rowsFormatted) {
      sb.append(System.lineSeparator())
      sb.append(row.mkString(colSeparator))
    }
    sb.toString()
  }

  def black(s: String): String

  def bgBlack(s: String): String

  def blue(s: String): String

  def bgBlue(s: String): String

  def cyan(s: String): String

  def bgCyan(s: String): String

  def green(s: String): String

  def bgGreen(s: String): String

  def magenta(s: String): String

  def bgMagenta(s: String): String

  def red(s: String): String

  def bgRed(s: String): String

  def yellow(s: String): String

  def bgYellow(s: String): String

  def white(s: String): String

  def bgWhite(s: String): String

  def bold(s: String): String

  def underline(s: String): String

  def fgColor(r: Int, g: Int, b: Int, s: String): String

  private def padLeft(width: Int, s: String): String = String.format("%" + width + "s", s)

  /**
    * Takes a list of strings and right-pads them with spaces to all take up the same space.
    */
  private def padToLongest(l: List[String]): List[String] = {
    val longestLength = l.map(s => s.length).max
    l.map(s => s.padTo(longestLength, ' '))
  }
}

object Formatter {

  /**
    * A formatter that does not apply any color styling.
    */
  object NoFormatter extends Formatter {

    override def black(s: String): String = s

    override def bgBlack(s: String): String = s

    override def blue(s: String): String = s

    override def bgBlue(s: String): String = s

    override def cyan(s: String): String = s

    override def bgCyan(s: String): String = s

    override def green(s: String): String = s

    override def bgGreen(s: String): String = s

    override def magenta(s: String): String = s

    override def bgMagenta(s: String): String = s

    override def red(s: String): String = s

    override def bgRed(s: String): String = s

    override def yellow(s: String): String = s

    override def bgYellow(s: String): String = s

    override def white(s: String): String = s

    override def bgWhite(s: String): String = s

    override def bold(s: String): String = s

    override def underline(s: String): String = s

    override def fgColor(r: Int, g: Int, b: Int, s: String): String = s

  }

  /**
    * A terminal context compatible with an ANSI terminal.
    */
  object AnsiTerminalFormatter extends Formatter {

    override def black(s: String): String = fgColor(1, 1, 1, s)

    override def bgBlack(s: String): String = bgColor(1, 1, 1, white(s))

    override def blue(s: String): String = fgColor(0, 111, 184, s)

    override def bgBlue(s: String): String = bgColor(0, 111, 184, white(s))

    override def cyan(s: String): String = fgColor(44, 181, 233, s)

    override def bgCyan(s: String): String = bgColor(44, 181, 233, white(s))

    override def green(s: String): String = fgColor(57, 181, 74, s)

    override def bgGreen(s: String): String = bgColor(57, 181, 74, white(s))

    override def magenta(s: String): String = fgColor(118, 38, 113, s)

    override def bgMagenta(s: String): String = bgColor(118, 38, 113, white(s))

    override def red(s: String): String = fgColor(222, 56, 43, s)

    override def bgRed(s: String): String = bgColor(222, 56, 43, white(s))

    override def yellow(s: String): String = fgColor(255, 199, 6, s)

    override def bgYellow(s: String): String = bgColor(255, 199, 6, white(s))

    override def white(s: String): String = fgColor(204, 204, 204, s)

    override def bgWhite(s: String): String = bgColor(204, 204, 204, black(s))

    override def bold(s: String): String = Console.BOLD + s + Console.RESET

    override def underline(s: String): String = Console.UNDERLINED + s + Console.RESET

    override def fgColor(r: Int, g: Int, b: Int, s: String): String = escape() + s"[38;2;$r;$g;${b}m" + s + escape() + "[0m"

    private def bgColor(r: Int, g: Int, b: Int, s: String): String = escape() + s"[48;2;$r;$g;${b}m" + s + escape() + "[0m"

    private def escape(): String = "\u001b"

  }

  /**
    * Returns the default formatter based on the detected color support.
    */
  def getDefault: Formatter = if (hasColorSupport) AnsiTerminalFormatter else NoFormatter

  /**
    * Returns `true` if the terminal appears to support colors.
    *
    * Assumes color support by default, only disabling for explicitly unsupported terminals.
    */
  def hasColorSupport: Boolean = !isDumbTerminal && !hasNoColorEnv

  /**
    * Returns `true` if the terminal is explicitly a dumb terminal with no capabilities.
    */
  private def isDumbTerminal: Boolean = {
    val term = System.getenv("TERM")
    term != null && term.equalsIgnoreCase("dumb")
  }

  /**
    * Returns `true` if the NO_COLOR environment variable is set.
    * See https://no-color.org/
    */
  private def hasNoColorEnv: Boolean = {
    System.getenv("NO_COLOR") != null
  }

}
