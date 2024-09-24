package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.{Input, Source}

object SourcePosition {
  /** Represents an unknown source position. */
  val Unknown: SourcePosition = SourcePosition(Source(Input.Unknown, Array.emptyCharArray), 0, 0)
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

  /** Returns `true` if `this` and `o` represent the same source position. */
  override def equals(o: Any): Boolean = o match {
    case that: SourcePosition =>
      this.source == that.source &&
        this.line == that.line &&
        this.col == that.col
    case _ => false
  }

  /** Returns the hashCode of `this` source position. */
  override def hashCode(): Int = source.hashCode() + line + col

}
