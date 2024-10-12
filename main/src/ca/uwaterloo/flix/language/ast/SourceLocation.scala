package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.{Input, SecurityContext, Source}

/**
  * Companion object for the [[SourceLocation]] class.
  */
object SourceLocation {

  /**
    * Represents an unknown source location.
    *
    * Must only be used if *absolutely necessary*.
    */
  val Unknown: SourceLocation = SourceLocation(isReal = true, Source(Input.Unknown, Array.emptyCharArray), Range(Position(0,0), Position(0,0)))

  implicit object Order extends Ordering[SourceLocation] {

    import scala.math.Ordered.orderingToOrdered

    def compare(x: SourceLocation, y: SourceLocation): Int =
      (x.source.name, x.range).compare(y.source.name, y.range)
  }
}

/**
  * A class that represents the physical source location of some parsed syntactic entity.
  *
  * @param isReal true if real location, false if synthetic location.
  */
case class SourceLocation(isReal: Boolean, source: Source, range: Range) {

  /**
   * Returns the security context associated with the source location.
   */
  def security: SecurityContext = source.input.security

  def start: Position = range.start
  def end: Position = range.end

  def beginLine: Int = range.start.line
  def beginCol: Int = range.start.character
  def endLine: Int = range.end.line
  def endCol: Int = range.end.character

  def contains(pos: Position): Boolean = range.contains(pos)

  /**
    * Returns `true` if this source location spans a single line.
    */
  def isSingleLine: Boolean = start.line == end.line

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
  def formatWithLine: String = s"${source.name}:${range.start.line}"

  /**
    * Returns a string representation of `this` source location with the line and column numbers.
    */
  def format: String = s"${source.name}:${range.start.line}:${range.start.character}"

  /**
    * Returns the source text of the source location.
    */
  def text: Option[String] = {
    if (isSingleLine) {
      val line = lineAt(range.start.line)
      val b = Math.min(range.start.character - 1, line.length)
      val e = Math.min(range.end.character - 1, line.length)
      Some(line.substring(b, e))
    } else {
      None
    }
  }

  /**
    * Returns the hashCode of `this` source location.
    */
  override def hashCode(): Int = source.hashCode() + start.hashCode() + end.hashCode()

  /**
    * Returns `true` if `this` and `o` represent the same source location.
    */
  override def equals(o: Any): Boolean = o match {
    case that: SourceLocation =>
      this.source == that.source &&
        this.range == that.range
    case _ => false
  }

  /**
    * Returns a human-readable representation of `this` source location.
    *
    * Note: The `toString` method should only be used for debugging.
    */
  override def toString: String = format

}
