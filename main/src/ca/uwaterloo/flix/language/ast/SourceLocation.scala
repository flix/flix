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
  val Unknown: SourceLocation = SourceLocation(isReal = true, Source.Unknown, 0, 0)

  /** Returns the [[SourceLocation]] that refers the zero-width location at `index`. */
  def zeroPoint(isReal: Boolean, src: Source, index: Int): SourceLocation =
    SourceLocation(isReal, src, index, index)

  /** Returns the [[SourceLocation]] that refers the single-width location at `index`. */
  def point(isReal: Boolean, src: Source, index: Int): SourceLocation =
    SourceLocation(isReal, src, index, index + 1)

  implicit object Order extends Ordering[SourceLocation] {

    import scala.math.Ordered.orderingToOrdered

    def compare(x: SourceLocation, y: SourceLocation): Int =
      (x.source.name, x.startIndex, x.endIndex).compare((y.source.name, y.startIndex, y.endIndex))
  }

}

/**
  * A class that represents the physical source location of some parsed syntactic entity.
  *
  * @param isReal true if real location, false if synthetic location.
  */
case class SourceLocation(isReal: Boolean, source: Source, startIndex: Int, endIndex: Int) {

  /**
   * Returns the security context associated with the source location.
   */
  def security: SecurityContext = source.input.security

  /**
    * Returns the start position.
    *
    * Time Complexity: O(log lineCount)
    */
  def start: SourcePosition =
    source.lines.getPosition(startIndex)

  /**
    * Returns `true` if `this` [[SourceLocation]] completely contains `that`, otherwise `false`.
    *
    * What is meant by "contained" is that `this` begins before or at the same position as `that`
    * and `this` ends after `that` or at the same position. Returns `false` otherwise.
    */
  def contains(that: SourceLocation): Boolean = {
    if (this.source != that.source) false
    else {
      val thatBeginsLater = this.startIndex <= that.startIndex
      val thatEndsBefore = that.endIndex <= this.endIndex
      thatBeginsLater && thatEndsBefore
    }
  }

  /**
    * Returns the end position (exclusive)
    *
    * Time Complexity: O(log lineCount)
    */
  def end: SourcePosition =
    source.lines.getPositionExclusive(endIndex)

  /**
    * Returns `true` if this source location spans a single line.
    */
  def isSingleLine: Boolean = start.lineOneIndexed == end.lineOneIndexed

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
    * Returns a string representation of `this` source location with the line and column numbers.
    */
  def format: String = {
    val SourcePosition(startLine, startCol) = start
    s"${source.name}:$startLine:$startCol"
  }

  /**
    * Returns the source text of the source location.
    */
  def text: Option[String] = {
    if (isSingleLine) {
      Some(source.getData(startIndex, endIndex))
    } else {
      None
    }
  }

  /**
    * Returns the hashCode of `this` source location.
    */
  override def hashCode(): Int = source.hashCode() + startIndex + endIndex

  /**
    * Returns `true` if `this` and `o` represent the same source location.
    */
  override def equals(o: Any): Boolean = o match {
    case that: SourceLocation =>
      this.source == that.source &&
        this.startIndex == that.startIndex &&
        this.endIndex == that.endIndex
    case _ => false
  }

  /**
    * Returns a human-readable representation of `this` source location.
    *
    * Note: The `toString` method should only be used for debugging.
    */
  override def toString: String = format

}
