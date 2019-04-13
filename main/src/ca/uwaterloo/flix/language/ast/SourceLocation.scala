package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.Ast.Source
import ca.uwaterloo.flix.language.debug.FormatSourceLocation
import flix.runtime.ReifiedSourceLocation


/**
  * Companion object for the [[SourceLocation]] class.
  */
object SourceLocation {
  /**
    * Represents a generated source location.
    */
  val Generated: SourceLocation = mk(SourcePosition.Unknown, SourcePosition.Unknown)

  /**
    * Represents an unknown source location.
    */
  val Unknown: SourceLocation = mk(SourcePosition.Unknown, SourcePosition.Unknown)

  def mk(b: SourcePosition, e: SourcePosition): SourceLocation = {
    assert(b.source == e.source)
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
    * Returns a human-readable string representation for debugging.
    */
  override def toString: String = FormatSourceLocation.format(this)

}
