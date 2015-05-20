package util.ascii

import Align._

import scala.collection.mutable
import java.io.{StringWriter, FileWriter, BufferedWriter, PrintWriter}

/**
 * A class for printing ASCII tables.
 */
class AsciiTable {
  private val cols = mutable.ListBuffer.empty[String];
  private val rows = mutable.ListBuffer.empty[List[String]];
  private val aligns = mutable.ListBuffer.empty[Align];

  /**
   * Adds a column of the given name with the given alignment.
   */
  def mkCol(name: String, align: Align = Left): AsciiTable = {
    cols.append(name);
    aligns.append(align);
    this;
  }

  /**
   * Adds the given row.
   */
  def mkRow(xs: List[Any]): AsciiTable = {
    rows.append(xs.map(String.valueOf(_)));
    this;
  }

  /**
   * Adds the given rows.
   */
  def mkRows(xss: Seq[List[Any]]): AsciiTable = {
    for (xs <- xss) {
      rows.append(xs.map(String.valueOf(_)));
    }
    this;
  }

  /**
   * Draws the ASCII table to the given print writer `w`.
   */
  def draw(w: PrintWriter): Unit = {
    val xs = rows.toList;
    val ys = cols.toList;
    val ws = columnWidths(ys :: xs);
    val as = aligns.toList;

    w.println(formatLine(ws));
    w.println(formatRow(ys, ws, as));
    w.println(formatLine(ws));
    for (row <- rows) {
      w.println(formatRow(row, ws, as));
    }
    w.println(formatLine(ws));
  }

  /**
   * Returns the string representation.
   */
  def output: String = {
    val w = new StringWriter()
    draw(new PrintWriter(w))
    w.toString
  }

  /**
   * Draws the ASCII table to the given filename `f`.
   */
  def draw(f: String): Unit = {
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(f)));
    try {
      draw(writer);
    } finally {
      writer.close();
    }
  }

  /**
   * Returns the column widths of a sequence of rows.
   */
  private def columnWidths(xs: List[List[String]]): List[Int] = (List.empty[Int] /: xs) {
    case (acc, ys) => mergeWidths(acc, columnWidth(ys));
  }

  /**
   * Merge two column widths.
   */
  private def mergeWidths(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Nil, Nil) => Nil;
    case (as, Nil) => as;
    case (Nil, bs) => bs;
    case (a :: as, b :: bs) => math.max(a, b) :: mergeWidths(as, bs);
  }

  /**
   * Returns the column width of a given row.
   */
  private def columnWidth(xs: List[String]): List[Int] = xs.map(x => x.length);

  /**
   * Returns a string corresponding to the horizontal ruler in the table.
   */
  private def formatLine(ws: List[Int]): String = {
    val cells = ws.map(w => "-" * w);
    "+-" + cells.mkString("-+-") + "-+";
  }

  /**
   * Returns a string which is a formatting of the given row `xs` with widths `ws` and alignments `as`.
   */
  private def formatRow(xs: List[String], ws: List[Int], as: List[Align]): String = {
    val cells = xs.zip(ws).zip(as).map {
      case ((s, w), a) => align(s, w, a);
    }
    "| " + cells.mkString(" | ") + " |";
  }

  /**
   * Returns a string of length `w` where the given string `s` is aligned according to `a`.
   */
  private def align(s: String, w: Int, a: Align): String = a match {
    case Left => alignLeft(s, w);
    case Middle => alignCenter(s, w);
    case Right => alignRight(s, w);
  }

  /**
   * Returns a string of length `w` where the given string `s` is left aligned.
   */
  private def alignLeft(s: String, w: Int): String = s + " " * (w - s.length);

  /**
   * Returns a string of length `w` where the given string `s` is aligned in the center.
   */
  private def alignCenter(s: String, w: Int): String = {
    val v = w - s.length;
    " " * (v / 2) + s + " " * (v - v / 2);
  }

  /**
   * Returns a string of length `w` where the given string `s` is right aligned.
   */
  private def alignRight(s: String, w: Int): String = " " * (w - s.length) + s;
}
