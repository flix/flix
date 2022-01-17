package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.Ast.Source
import ca.uwaterloo.flix.language.debug.FormatSourceLocation
import org.parboiled2.ParserInput

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
  def mk(b: SourcePosition, e: SourcePosition, k: SourceKind = SourceKind.Real): SourceLocation =
    SourceLocation(b.input, b.source, k, b.line, b.col, e.line, e.col)

  implicit object Order extends Ordering[SourceLocation] {

    import scala.math.Ordered.orderingToOrdered

    def compare(x: SourceLocation, y: SourceLocation): Int =
      (x.source.name, x.beginLine, x.beginCol) compare(y.source.name, y.beginLine, y.beginCol)
  }

}

/**
  * A class that represents the physical source location of some parsed syntactic entity.
  *
  * @param input        the parser input.
  * @param source       the source input.
  * @param locationKind the source location kind.
  * @param beginLine    the line number where the entity begins.
  * @param beginCol     the column number where the entity begins.
  * @param endLine      the line number where the entity ends.
  * @param endCol       the column number where the entity ends.
  */
case class SourceLocation(input: Option[ParserInput], source: Source, locationKind: SourceKind, beginLine: Int, beginCol: Int, endLine: Int, endCol: Int) {

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
  def isSynthetic: Boolean = locationKind == SourceKind.Synthetic

  /**
    * Returns `this` source location but as a synthetic kind.
    */
  def asSynthetic: SourceLocation = copy(locationKind = SourceKind.Synthetic)

  /**
    * Returns `this` source location but as a real kind.
    */
  def asReal: SourceLocation = copy(locationKind = SourceKind.Real)

  /**
    * Returns the smallest (i.e. the first that appears in the source code) of `this` and `that`.
    */
  def min(that: SourceLocation): SourceLocation = SourceLocation.Order.min(this, that)

  /**
    * Returns the text at the given `line`.
    *
    * The line does not have to refer to `this` source location.
    */
  def lineAt(line: Int): String = input match {
    case None => ""
    case Some(input) => input.getLine(line)
  }

  /**
    * Returns a formatted string representation of `this` source location.
    */
  def format: String = s"${source.name}:$beginLine:$beginCol"

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
