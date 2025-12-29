package ca.uwaterloo.flix.language.ast

object SourcePosition {

  /**
    * Returns the first [[SourcePosition]].
    *
    * OBS: This might not be a real position if the relevant source is empty.
    */
  val FirstPosition: SourcePosition = SourcePosition(lineOneIndexed = 1, colOneIndexed = 1)

  /** An unknown source position. */
  val Unknown: SourcePosition = FirstPosition

  /**
    * Returns a [[SourcePosition]] in `source` with the zero-indexed `line` and `col`.
    *
    * `line` and `col` must be 1 or greater.
    */
  def mkFromOneIndexed(line: Int, col: Int): SourcePosition =
    SourcePosition(lineOneIndexed = line, colOneIndexed = col.toShort)
}

/** Represents a source position. */
case class SourcePosition(lineOneIndexed: Int, colOneIndexed: Int)
