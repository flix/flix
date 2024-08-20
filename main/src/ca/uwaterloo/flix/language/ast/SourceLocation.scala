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
  val Unknown: SourceLocation = SourceLocation(isReal = true, SourcePosition.Unknown, SourcePosition.Unknown)

  implicit object Order extends Ordering[SourceLocation] {

    import scala.math.Ordered.orderingToOrdered

    def compare(x: SourceLocation, y: SourceLocation): Int =
      (x.source.name, x.beginLine, x.beginCol, x.endLine, x.endCol).compare(y.source.name, y.beginLine, y.beginCol, y.endLine, y.endCol)
  }

}

/**
  * A class that represents the physical source location of some parsed syntactic entity.
  *
  * @param isReal true if real location, false if synthetic location.
  */
case class SourceLocation(isReal: Boolean, sp1: SourcePosition, sp2: SourcePosition) {

  // Invariant: Ensure that sp1 and sp2 come from the same source.
  assert(sp1.source eq sp2.source)

  /**
    * Returns the source associated with the source location.
    */
  def source: Source = sp1.source

  /**
   * Returns the security context associated with the source location.
   */
  def security: SecurityContext = sp1.source.input.security

  /**
    * Returns the line where the entity begins.
    */
  def beginLine: Int = sp1.line

  /**
    * Returns the column where the entity begins.
    */
  def beginCol: Int = sp1.col

  /**
    * Returns the line where the entity ends.
    */
  def endLine: Int = sp2.line

  /**
    * Returns the column where the entity ends.
    */
  def endCol: Int = sp2.col

  /**
    * Returns `true` if this source location spans a single line.
    */
  def isSingleLine: Boolean = beginLine == endLine

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
  def formatWithLine: String = s"${source.name}:$beginLine"

  /**
    * Returns a string representation of `this` source location with the line and column numbers.
    */
  def format: String = s"${source.name}:$beginLine:$beginCol"

  /**
    * Returns the source text of the source location.
    */
  def text: Option[String] = {
    if (isSingleLine) {
      val line = lineAt(beginLine)
      val b = Math.min(beginCol - 1, line.length)
      val e = Math.min(endCol - 1, line.length)
      Some(line.substring(b, e))
    } else {
      None
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
