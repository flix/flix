package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.{SecurityContext, Source}

/**
  * Companion object for the [[SourceLocation]] class.
  */
object SourceLocation {

  /**
    * Represents an unknown source location.
    *
    * Must only be used if *absolutely necessary*.
    */
  val Unknown: SourceLocation = SourceLocation(isReal = true, Source.Unknown, SourcePosition.Unknown, SourcePosition.Unknown)

  /** Returns the [[SourceLocation]] that refers the the zero-width location `sp`. */
  def zeroPoint(isReal: Boolean, src: Source, sp: SourcePosition): SourceLocation =
    SourceLocation(isReal, src, sp, sp)

  /** Returns the [[SourceLocation]] that refers the the single-width location `sp`. */
  def point(isReal: Boolean, src: Source, sp: SourcePosition): SourceLocation =
    SourceLocation(isReal, src, sp, SourcePosition.moveRight(sp))

  implicit object Order extends Ordering[SourceLocation] {

    import scala.math.Ordered.orderingToOrdered

    def compare(x: SourceLocation, y: SourceLocation): Int =
      (x.source.name, x.start, x.end).compare((y.source.name, y.start, y.end))
  }

}

/**
  * A class that represents the physical source location of some parsed syntactic entity.
  *
  * @param isReal true if real location, false if synthetic location.
  */
case class SourceLocation(isReal: Boolean, source: Source, start: SourcePosition, end: SourcePosition) {

  /**
   * Returns the security context associated with the source location.
   */
  def security: SecurityContext = source.input.security

  /**
    * Returns the one-indexed line where the entity begins.
    */
  def startLine: Int = start.lineOneIndexed

  /**
    * Returns the one-indexed column where the entity begins.
    */
  def startCol: Int = start.colOneIndexed

  /**
    * Returns `true` if `this` [[SourceLocation]] completely contains `that`, otherwise `false`.
    *
    * What is meant by "contained" is that `this` begins before or at the same position as `that`
    * and `this` ends after `that` or at the same position. Returns `false` otherwise.
    */
  def contains(that: SourceLocation): Boolean = {
    if (this.source != that.source) false
    else {
      val thatBeginsLater = SourcePosition.Order.lteq(this.start, that.start)
      val thatEndsBefore = SourcePosition.Order.lteq(that.end, this.end)
      thatBeginsLater && thatEndsBefore
    }
  }

  /**
    * Returns the one-indexed line where the entity ends.
    */
  def endLine: Int = end.lineOneIndexed

  /**
    * Returns the one-indexed column where the entity ends.
    */
  def endCol: Int = end.colOneIndexed

  /**
    * Returns `true` if this source location spans a single line.
    */
  def isSingleLine: Boolean = startLine == endLine

  /**
    * Returns `true` if this source location is synthetic.
    */
  def isSynthetic: Boolean = !isReal

  /**
    * Returns `this` source location but as a synthetic kind.
    */
  def asSynthetic: SourceLocation = copy(isReal = false)

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
    * Returns a string representation of `this` source location with the line and column numbers.
    */
  def format: String = s"${source.name}:$startLine:$startCol"

  /**
    * Returns the source text of the source location.
    */
  def text: Option[String] = {
    if (isSingleLine) {
      val line = lineAt(startLine)
      val b = Math.min(startCol - 1, line.length)
      val e = Math.min(endCol - 1, line.length)
      Some(line.substring(b, e))
    } else {
      None
    }
  }

  /**
    * Returns the hashCode of `this` source location.
    */
  override def hashCode(): Int = source.hashCode() + startLine + startCol + endLine + endCol

  /**
    * Returns `true` if `this` and `o` represent the same source location.
    */
  override def equals(o: Any): Boolean = o match {
    case that: SourceLocation =>
      this.source == that.source &&
        this.startLine == that.startLine &&
        this.startCol == that.startCol &&
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
