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

  /** Returns the [[SourceLocation]] that refers the zero-width location `sp`. */
  def zeroPoint(isReal: Boolean, src: Source, index: Int): SourceLocation =
    SourceLocation(isReal, src, index, index)

  /** Returns the [[SourceLocation]] that refers the single-width location `sp`. */
  def point(isReal: Boolean, src: Source, index: Int): SourceLocation =
    SourceLocation(isReal, src, index, index + 1)

  implicit object Order extends Ordering[SourceLocation] {

    import scala.math.Ordered.orderingToOrdered

    def compare(x: SourceLocation, y: SourceLocation): Int =
      (x.source.name, x.sp1, x.sp2).compare((y.source.name, y.sp1, y.sp2))
  }

}

/**
  * A class that represents the physical source location of some parsed syntactic entity.
  *
  * @param isReal true if real location, false if synthetic location.
  */
case class SourceLocation(isReal: Boolean, source: Source, sp1: Int, sp2: Int) {

  /**
   * Returns the security context associated with the source location.
   */
  def security: SecurityContext = source.input.security

  /**
    * Returns the one-indexed line where the entity begins.
    */
  def beginLine: Int = source.lines.getLine(sp1)

  /**
    * Returns the one-indexed column where the entity begins.
    */
  def beginCol: Int = source.lines.getColumn(sp1)

  /**
    * Returns `true` if `this` [[SourceLocation]] completely contains `that`, otherwise `false`.
    *
    * What is meant by "contained" is that `this` begins before or at the same position as `that`
    * and `this` ends after `that` or at the same position. Returns `false` otherwise.
    */
  def contains(that: SourceLocation): Boolean = {
    if (this.source != that.source) false
    else {
      val thatBeginsLater = this.sp1 <= that.sp1
      val thatEndsBefore = that.sp2 <= this.sp2
      thatBeginsLater && thatEndsBefore
    }
  }

  /**
    * Returns the one-indexed line where the entity ends.
    */
  def endLine: Int = source.lines.getLine(sp2)

  /**
    * Returns the one-indexed column where the entity ends.
    */
  def endCol: Int = source.lines.getColumn(sp2)

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

//  /**
//    * Returns a string representation of `this` source location with the line number.
//    */
//  def formatWithLine: String = s"${source.name}:$beginLine"

//  /**
//    * Returns a string representation of `this` source location with the line and column numbers.
//    */
//  def format: String = s"${source.name}:$beginLine:$beginCol"

//  /**
//    * Returns the source text of the source location.
//    */
//  def text: Option[String] = {
//    if (isSingleLine) {
//      val line = lineAt(beginLine)
//      val b = Math.min(beginCol - 1, line.length)
//      val e = Math.min(endCol - 1, line.length)
//      Some(line.substring(b, e))
//    } else {
//      None
//    }
//  }

  /**
    * Returns the hashCode of `this` source location.
    */
  override def hashCode(): Int = source.hashCode() + sp1 + sp2

  /**
    * Returns `true` if `this` and `o` represent the same source location.
    */
  override def equals(o: Any): Boolean = o match {
    case that: SourceLocation =>
      this.source == that.source &&
        this.sp1 == that.sp1 &&
        this.sp2 == that.sp2
    case _ => false
  }

  /**
    * Returns a human-readable representation of `this` source location.
    *
    * Note: The `toString` method should only be used for debugging.
    */
  override def toString: String = ???

}
