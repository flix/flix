/*
 * Copyright 2025 Google LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.language.ast.{SourceLocation, SourcePosition}
import ca.uwaterloo.flix.util.FileLines.LineInfo

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/**
  * A utility class for conversion between absolute indices and lines and columns in a text file.
  *
  * @param lineStarts        A sorted, non-empty array containing the start indices of each line in the file (zero-indexed). MUST have the same length as lineLengths.
  * @param lineLengths       A non-empty array providing the length of each line. MUST have the same length as lineStarts.
  * @param endIndexExclusive The exclusive end index of the file content (zero-indexed). MUST be `>= lineStarts.last + lineLengths.last`.
  */
class FileLines private(private var lineStarts: Array[Int], private var lineLengths: Array[Int], private val endIndexExclusive: Int) {
  assert(lineStarts.nonEmpty)
  assert(lineLengths.nonEmpty)
  assert(lineStarts.length == lineLengths.length)
  assert(endIndexExclusive >= lineStarts.last + lineLengths.last)

  /** Returns the index of the first character of the given line (one-indexed). */
  def indexOfLine(line: Int): Int =
    lineStarts(line - 1)

  /** Returns the non-newline length of the given line (one-indexed). */
  def lengthOfLine(line: Int): Int =
    lineLengths(line - 1)

  /**
    * Returns the line number of the given index (one-indexed).
    *
    * Returns `1` for negative indices and the last line for indices positively out of bounds.
    *
    * Time Complexity: O(log lineCount)
    */
  def getLine(index: Int): Int = {
    assert(0 <= index, index)
    assert(index <= endIndexExclusive, index.toString + ", " + endIndexExclusive)
    if (index < 0) {
      return 1
    }

    /** low (inclusive), high (exclusive). */
    @tailrec
    def search(low: Int, high: Int): Int = {
      if (low >= high) throw InternalCompilerException("Unreachable: lineStarts is non-empty.", SourceLocation.Unknown)
      if (low + 1 == high) return low
      val mid = low + ((high - low) / 2)
      val cmp = index.compareTo(lineStarts(mid))
      if (cmp == 0) {
        mid
      } else if (cmp < 0) {
        search(low, mid)
      } else {
        // Ensure that mid is still considered in the search.
        search(mid, high)
      }
    }

    search(0, lineStarts.length) + 1
  }

  /**
    * Returns `(line, col)` (one-indexed).
    *
    * Returns `(1, 1)` for out-of-bounds indices.
    *
    * Time Complexity: O(log lineCount)
    */
  def getPosition(index: Int): SourcePosition = {
    val line = getLine(index)
    val lineStart = indexOfLine(line)
    val col = index - lineStart + 1
    SourcePosition.mkFromOneIndexed(line, col)
  }

  /**
    * Returns `(line, col)` (one-indexed).
    *
    * Instead of returning `(line, 1)`, `(line - 1, lastColOfPreviousLine + 1)` is returned (unless `line == 0`).
    *
    * Time Complexity: O(log lineCount)
    */
  def getPositionExclusive(index: Int): SourcePosition = {
    val SourcePosition(line0, col0) = getPosition(index)
    if (col0 == 1 && line0 > 1) {
      SourcePosition.mkFromOneIndexed(line0 - 1, lengthOfLine(line0 - 1) + 1)
    } else {
      SourcePosition.mkFromOneIndexed(line0, col0)
    }

  }

  /** Returns [[LineInfo]] for `line` if it exists. */
  def nthLineInfo(line: Int): Option[LineInfo] = {
    if (1 <= line && line <= lineStarts.length) {
      Some(LineInfo(indexOfLine(line), lengthOfLine(line)))
    } else {
      None
    }
  }

  /** Returns the number of lines. */
  def lineCount: Int = lineStarts.length

}

object FileLines {

  /** A singleton instance representing an empty file. */
  val empty: FileLines = new FileLines(Array(0), Array(0), 0)

  /**
    * Returns a new [[FileLines]] for the given `chars`.
    *
    * N.B.: `\r` is assumed only to exist before `\n`.
    */
  def fromChars(chars: Array[Char]): FileLines = {
    val lineStarts = ArrayBuffer[Int](0)
    val lineLengths = ArrayBuffer[Int]()
    var index = 0
    while (index < chars.length) {
      if (chars(index) == '\n') {
        val extra = if (index > 0 && chars(index - 1) == '\r') 1 else 0;
        lineLengths += (index - extra - lineStarts.last)
        lineStarts += index + 1
      }
      index += 1
    }
    lineLengths += (index - lineStarts.last)
    new FileLines(lineStarts.toArray, lineLengths.toArray, chars.length)
  }

  /**
    * Represents information about a single line in a file.
    *
    * @param startIndex    The start index of the line within the file (zero-based).
    * @param visibleLength The length of the line, excluding line terminators.
    */
  case class LineInfo(startIndex: Int, visibleLength: Int)
}
