package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.Ast.Source

object SourcePosition {
  /**
    * Represents an unknown source position.
    */
  val Unknown: SourcePosition = SourcePosition(Source(Ast.Input.Text("<unknown>", "", stable = true), Array.emptyCharArray, stable = true), 0, 0)
}

/**
  * A class that represent a physical source position inside a source input.
  *
  * @param line  the line number.
  * @param col   the column number.
  */
case class SourcePosition(source: Source, line: Int, col: Short) {

  /**
    * Returns the hashCode of `this` source position.
    */
  override def hashCode(): Int = source.hashCode() + line + col

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
}
