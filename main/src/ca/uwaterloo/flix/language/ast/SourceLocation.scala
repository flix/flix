package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.Ast.Source
import ca.uwaterloo.flix.language.debug.FormatSourceLocation
import flix.runtime.ReifiedSourceLocation


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
  def mk(b: SourcePosition, e: SourcePosition): SourceLocation = {
    val lineAt = b.input match {
      case None => (i: Int) => ""
      case Some(input) => (i: Int) => input.getLine(i)
    }
    SourceLocation(b.source, b.line, b.col, e.line, e.col, lineAt)
  }

  implicit object Order extends Ordering[SourceLocation] {

    import scala.math.Ordered.orderingToOrdered

    def compare(x: SourceLocation, y: SourceLocation): Int =
      (x.source.format, x.beginLine, x.beginCol) compare(y.source.format, y.beginLine, y.beginCol)
  }

}

/**
  * A class that represents the physical source location of some parsed syntactic entity.
  *
  * @param source    the source input.
  * @param beginLine the line number where the entity begins.
  * @param beginCol  the column number where the entity begins.
  * @param endLine   the line number where the entity ends.
  * @param endCol    the column number where the entity ends.
  * @param lineAt    a closure which returns the text at the given line offset.
  */
case class SourceLocation(source: Source, beginLine: Int, beginCol: Int, endLine: Int, endCol: Int, lineAt: Int => String) {

  /**
    * Returns a formatted string representation of `this` source location.
    */
  def format: String = s"${source.format}:$beginLine:$beginCol"

  /**
    * Returns a reified source location.
    */
  def reified: ReifiedSourceLocation = new ReifiedSourceLocation(source.format, beginLine, beginCol, endLine, endCol)

  /**
    * Returns the smallest (i.e. the first that appears in the source code) of `this` and `that`.
    */
  def min(that: SourceLocation): SourceLocation = SourceLocation.Order.min(this, that)

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
    * Returns a human-readable string representation for debugging.
    */
  override def toString: String = FormatSourceLocation.format(this)

}
