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
    * `line` and `col` must be 0 or greater.
    */
  def mkFromZeroIndexed(line: Int, col: Int): SourcePosition =
    SourcePosition(lineOneIndexed = line + 1, colOneIndexed = (col + 1).toShort)

  /**
    * Returns a [[SourcePosition]] in `source` with the zero-indexed `line` and `col`.
    *
    * `line` and `col` must be 1 or greater.
    */
  def mkFromOneIndexed(line: Int, col: Int): SourcePosition =
    SourcePosition(lineOneIndexed = line, colOneIndexed = col.toShort)

  /** [[PartialOrdering]] for [[SourcePosition]]s. */
  implicit object Order extends Ordering[SourcePosition] {

    /**
      * Returns a comparison (`Some(cmp)`) between `x` and `y` if they point to the same file.
      * Returns `None` otherwise.
      *
      *   - If `x` is strictly before `y` then `cmp < 0`
      *   - If `x` is strictly after `y` then `cmp > 0`
      *   - If they are the same then `cmp == 0`.
      */
    override def compare(x: SourcePosition, y: SourcePosition): Int = {
      if (x.lineOneIndexed != y.lineOneIndexed) x.lineOneIndexed - y.lineOneIndexed
      else if (x.colOneIndexed != y.colOneIndexed) x.colOneIndexed - y.colOneIndexed
      else 0
    }
  }
}

/** Represents a source position. */
case class SourcePosition(lineOneIndexed: Int, colOneIndexed: Short)
