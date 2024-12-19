package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.{Input, Source}

object SourcePosition {
  /**
    * Represents an unknown source position.
    */
  val Unknown: SourcePosition = SourcePosition(Source(Input.Unknown, Array.emptyCharArray), 0, 0)

  implicit object PartialOrder extends PartialOrdering[SourcePosition] {
    override def tryCompare(x: SourcePosition, y: SourcePosition): Option[Int] = {
      if (x.source != y.source) { return None }
      if (x.line != y.line) { return Some(x.line - y.line) }
      if (x.col != y.col) { return Some(x.col - y.col) }
      Some(0)
    }

    override def lteq(x: SourcePosition, y: SourcePosition): Boolean = {
      tryCompare(x, y) match {
        case None => false
        case Some(value) => value <= 0
      }
    }
  }
}

/**
  * A class that represent a physical source position inside a source input.
  *
  * A [[SourcePosition]] must always be one-indexed.
  *
  * @param line the line number. Must be one-indexed.
  * @param col  the column number. Must be one-indexed.
  */
case class SourcePosition(source: Source, line: Int, col: Short) {

  /**
    * Returns `true` if `this` and `o` represent the same source position.
    */
  override def equals(o: Any): Boolean = o match {
    case that: SourcePosition =>
      this.source == that.source &&
        this.line == that.line &&
        this.col == that.col
    case _ => false
  }

  /**
    * Returns the hashCode of `this` source position.
    */
  override def hashCode(): Int = source.hashCode() + line + col

}
