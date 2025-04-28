package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.{Input, Source}
import ca.uwaterloo.flix.util.InternalCompilerException

object SourcePosition {

  /**
    * Returns the first [[SourcePosition]] of `source`.
    *
    * OBS: This might not be a real position if `source` is empty.
    */
  def firstPosition(source: Source): SourcePosition =
    new SourcePosition(source, lineOneIndexed = 1, colOneIndexed = 1)

  /**
    * Returns a [[SourcePosition]] in `source` with the zero-indexed `line` and `col`.
    *
    * `line` and `col` must be 0 or greater.
    */
  def mkFromZeroIndexed(source: Source, line: Int, col: Int): SourcePosition =
    new SourcePosition(source, lineOneIndexed = line + 1, colOneIndexed = (col + 1).toShort)

  /**
    * Returns a [[SourcePosition]] in `source` with the zero-indexed `line` and `col`.
    *
    * `line` and `col` must be 1 or greater.
    */
  def mkFromOneIndexed(source: Source, line: Int, col: Int): SourcePosition =
    new SourcePosition(source, lineOneIndexed = line, colOneIndexed = col.toShort)

  /** An unknown source position. */
  val Unknown: SourcePosition = firstPosition(Source(Input.Unknown, Array.emptyCharArray))

  /**
    * Returns a new [[SourcePosition]] where the column of `pos` is reduced by 1.
    *
    * OBS: Crashes if the column cannot be reduced.
    */
  def moveLeft(pos: SourcePosition): SourcePosition = {
    if (pos.colOneIndexed <= 1) throw InternalCompilerException(s"Trying to reduce the column of ${SourceLocation.zeroPoint(isReal = false, pos).toString}", SourceLocation.Unknown)
    else new SourcePosition(pos.source, pos.lineOneIndexed, (pos.colOneIndexed - 1).toShort)
  }

  /**
    * Returns a new [[SourcePosition]] where the column of `pos` is increased by 1.
    *
    * OBS: This new position might not be a real position in the source.
    */
  def moveRight(pos: SourcePosition): SourcePosition = {
    new SourcePosition(pos.source, pos.lineOneIndexed, (pos.colOneIndexed + 1).toShort)
  }

  /** [[PartialOrdering]] for [[SourcePosition]]s. */
  implicit object PartialOrder extends PartialOrdering[SourcePosition] {

    /**
      * Returns a comparison (`Some(cmp)`) between `x` and `y` if they point to the same file.
      * Returns `None` otherwise.
      *
      *   - If `x` is strictly before `y` then `cmp < 0`
      *   - If `x` is strictly after `y` then `cmp > 0`
      *   - If they are the same then `cmp == 0`.
      */
    override def tryCompare(x: SourcePosition, y: SourcePosition): Option[Int] = {
      if (x.source != y.source) None
      else if (x.lineOneIndexed != y.lineOneIndexed) Some(x.lineOneIndexed - y.lineOneIndexed)
      else if (x.colOneIndexed != y.colOneIndexed) Some(x.colOneIndexed - y.colOneIndexed)
      else Some(0)
    }

    /**
      * Returns `true` if `x` is a position before or equal to `y` and both point to the same source.
      *
      * Returns `false` if `x` and `y` aren't from the same source or `x` is after `y`.
      */
    override def lteq(x: SourcePosition, y: SourcePosition): Boolean = {
      tryCompare(x, y) match {
        case None => false
        case Some(value) => value <= 0
      }
    }
  }
}

/** Represents a physical position inside a source. */
final class SourcePosition private(val source: Source, val lineOneIndexed: Int, val colOneIndexed: Short) {

  /**
    * The absolute character offset into `source`, zero-indexed.
    */
  def offset: Int = {
    var offset = 0
    for (i <- 1 until lineOneIndexed) {
      offset += source.getLine(i).length + 1 // +1 for the newline
    }
    offset + colOneIndexed - 1
  }

  /** Returns `true` if `this` and `o` represent the same source position. */
  override def equals(o: Any): Boolean = o match {
    case that: SourcePosition =>
      this.source == that.source &&
        this.lineOneIndexed == that.lineOneIndexed &&
        this.colOneIndexed == that.colOneIndexed
    case _ => false
  }

  /** Returns the hashCode of `this` source position. */
  override def hashCode(): Int = source.hashCode() + lineOneIndexed + colOneIndexed

}
