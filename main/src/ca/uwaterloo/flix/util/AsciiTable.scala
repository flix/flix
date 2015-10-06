/*
 * Copyright (c) 2015. Magnus Madsen.
 *
 * Source code available under the MIT license. See LICENSE.md for details.
 */

package ca.uwaterloo.flix.util

import java.io.{PrintStream, PrintWriter}

/**
 * A class for printing ASCII tables.
 */
class AsciiTable {

  /**
   * The names of the columns in the table.
   */
  private var columns = List.empty[String]
  /**
   * The rows in the table.
   */
  private var rows = List.empty[List[String]]
  /**
   * The alignments used for each column.
   */
  private var alignment = List.empty[Align]
  /**
   * A filtering function used to determine what rows to include.
   */
  private var filter = (x: String) => true

  /**
   * Sets the columns of the table.
   */
  def withCols(columns: String*): AsciiTable = {
    this.columns = columns.toList
    this.alignment = this.columns.map(x => Align.Left)
    this
  }

  /**
   * Sets the filter of the table. None is interpreted as no filter.
   */
  def withFilter(pattern: Option[String]): AsciiTable = pattern match {
    case None =>
      this.filter = (x: String) => true
      this
    case Some(pat) =>
      val r = pat.r
      this.filter = (x: String) => r.findFirstIn(x).nonEmpty
      this
  }

  /**
   * Adds the given row.
   */
  def mkRow(row: List[Any]): AsciiTable = {
    rows = row.map(_.toString) :: rows
    alignment = row map {
      case _: Double => Align.Right
      case _: Float => Align.Right
      case _: Int => Align.Right
      case _ => Align.Left
    }
    this
  }

  /**
   * Writes the table to the given `stream`.
   */
  def write(stream: PrintStream): Unit = {
    val xs = rows
    val ys = columns
    val ws = columnWidths(ys :: xs)
    val as = alignment

    var numberOfRows = 0
    var numberOfMatchedRows = 0

    val w = new PrintWriter(stream)
    w.println(formatLine(ws))
    w.println(formatRow(ys, ws, as))
    w.println(formatLine(ws))
    for (row <- rows.reverse) {
      if (row.exists(filter)) {
        w.println(formatRow(row, ws, as))
        numberOfMatchedRows += 1
      }
      numberOfRows += 1
    }
    w.println(formatLine(ws))
    w.println(s"Query matched $numberOfMatchedRows row(s) out of $numberOfRows total row(s).")

    w.flush()
  }

  /**
   * Returns the column widths of a sequence of rows.
   */
  private def columnWidths(xs: List[List[String]]): List[Int] = (List.empty[Int] /: xs) {
    case (acc, ys) => mergeWidths(acc, columnWidth(ys))
  }

  /**
   * Merge two column widths.
   */
  private def mergeWidths(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Nil, Nil) => Nil
    case (as, Nil) => as
    case (Nil, bs) => bs
    case (a :: as, b :: bs) => math.max(a, b) :: mergeWidths(as, bs)
  }

  /**
   * Returns the column width of a given row.
   */
  private def columnWidth(xs: List[String]): List[Int] = xs.map(x => x.length)

  /**
   * Returns a string corresponding to the horizontal ruler in the table.
   */
  private def formatLine(ws: List[Int]): String = {
    val cells = ws.map(w => "-" * w)
    "+-" + cells.mkString("-+-") + "-+"
  }

  /**
   * Returns a string which is a formatting of the given row `xs` with widths `ws` and alignments `as`.
   */
  private def formatRow(xs: List[String], ws: List[Int], as: List[Align]): String = {
    val cells = xs.zip(ws).zip(as).map {
      case ((s, w), a) => align(s, w, a)
    }
    "| " + cells.mkString(" | ") + " |"
  }

  /**
   * Returns a string of length `w` where the given string `s` is aligned according to `a`.
   */
  private def align(s: String, w: Int, a: Align): String = a match {
    case Align.Left => alignLeft(s, w)
    case Align.Middle => alignCenter(s, w)
    case Align.Right => alignRight(s, w)
  }

  /**
   * Returns a string of length `w` where the given string `s` is left aligned.
   */
  private def alignLeft(s: String, w: Int): String = s + " " * (w - s.length)

  /**
   * Returns a string of length `w` where the given string `s` is aligned in the center.
   */
  private def alignCenter(s: String, w: Int): String = {
    val v = w - s.length
    " " * (v / 2) + s + " " * (v - v / 2)
  }

  /**
   * Returns a string of length `w` where the given string `s` is right aligned.
   */
  private def alignRight(s: String, w: Int): String = " " * (w - s.length) + s

  /**
   * A common super-type for alignment of columns.
   */
  sealed trait Align

  object Align {

    /**
     * Align to the left.
     */
    case object Left extends Align

    /**
     * Align in the middle.
     */
    case object Middle extends Align

    /**
     * Align to the right.
     */
    case object Right extends Align

  }

}
