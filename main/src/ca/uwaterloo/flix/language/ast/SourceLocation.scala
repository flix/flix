package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.Ast.Source

/**
  * Companion object for the [[SourceLocation]] class.
  */
object SourceLocation {

  /**
    * Represents an unknown source location.
    *
    * Must only be used if *absolutely necessary*.
    */
  val Unknown: SourceLocation = mk(SourcePosition.Unknown, SourcePosition.Unknown)

  /**
    * Returns the source location constructed from the source positions `b` and `e.`
    */
  def mk(b: SourcePosition, e: SourcePosition, isReal: Boolean = true): SourceLocation =
    SourceLocation(b.source, isReal, b.line, b.col, e.line, e.col)

  implicit object Order extends Ordering[SourceLocation] {

    import scala.math.Ordered.orderingToOrdered

    def compare(x: SourceLocation, y: SourceLocation): Int =
      (x.source.name, x.beginLine, x.beginCol, x.endLine, x.endCol).compare(y.source.name, y.beginLine, y.beginCol, y.endLine, y.endCol)
  }

}

/**
  * A class that represents the physical source location of some parsed syntactic entity.
  *
  * We take extra efforts to ensure that source locations are compact, i.e. have small memory footprint.
  *
  * We do so because [[SourceLocation]]s are very common objects.
  *
  * Specifically, we:
  *
  * - Use a `Boolean` to represent whether a source location is real (true) or synthetic (false).
  * - Use `Short`s instead of `Int`s to represent column offsets (i.e. `beginCol` and `endCol`).
  *
  * @param source       the source input.
  * @param isReal       true if real location, false if synthetic location.
  * @param beginLine    the line number where the entity begins.
  * @param beginCol     the column number where the entity begins.
  * @param endLine      the line number where the entity ends.
  * @param endCol       the column number where the entity ends.
  */
case class SourceLocation(source: Source, isReal: Boolean, beginLine: Int, beginCol: Short, endLine: Int, endCol: Short) {

  /**
    * Returns `true` if this source location spans a single line.
    */
  def isSingleLine: Boolean = beginLine == endLine

  /**
    * Returns `true` if this source location spans more than one line.
    */
  def isMultiLine: Boolean = !isSingleLine

  /**
    * Returns `true` if this source location is synthetic.
    */
  def isSynthetic: Boolean = !isReal

  /**
    * Returns `this` source location but as a synthetic kind.
    */
  def asSynthetic: SourceLocation = copy(isReal = false)

  /**
    * Returns `this` source location but as a real kind.
    */
  def asReal: SourceLocation = copy(isReal = true)

  /**
    * Returns the left-most [[SourcePosition]] of `this` [[SourceLocation]].
    */
  def sp1: SourcePosition = SourcePosition(source, beginLine, beginCol)

  /**
    * Returns the left-most [[SourcePosition]] of `this` [[SourceLocation]].
    */
  def sp2: SourcePosition = SourcePosition(source, endLine, endCol)

  /**
    * Returns the smallest (i.e. the first that appears in the source code) of `this` and `that`.
    */
  def min(that: SourceLocation): SourceLocation = SourceLocation.Order.min(this, that)

  /**
    * Returns the text at the given `line`.
    *
    * The line does not have to refer to `this` source location.
    */
  def lineAt(line: Int): String = source.getLine(line)
        .replaceAll("\n", "")
        .replaceAll("\r", "")

  /**
    * Returns a string representation of `this` source location with the line number.
    */
  def formatWithLine: String = s"${source.name}:$beginLine"

  /**
    * Returns a string representation of `this` source location with the line and column numbers.
    */
  def format: String = s"${source.name}:$beginLine:$beginCol"

  /**
    * Returns the source text of the source location.
    */
  def text: Option[String] = {
    if (isMultiLine) {
      None
    } else {
      val line = lineAt(beginLine)
      val b = Math.min(beginCol - 1, line.length)
      val e = Math.min(endCol - 1, line.length)
      Some(line.substring(b, e))
    }
  }

  /**
    * Returns the hashCode of `this` source location.
    */
  override def hashCode(): Int = source.hashCode() + beginLine + beginCol + endLine + endCol

  /**
    * Returns `true` if `this` and `o` represent the same source location.
    */
  override def equals(o: Any): Boolean = o match {
    case that: SourceLocation =>
      this.source == that.source &&
        this.beginLine == that.beginLine &&
        this.beginCol == that.beginCol &&
        this.endLine == that.endLine &&
        this.endCol == that.endCol
    case _ => false
  }

  /**
    * Returns a human-readable representation of `this` source location.
    *
    * Note: The `toString` method should only be used for debugging.
    */
  override def toString: String = format

}
