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

import ca.uwaterloo.flix.util.FileLines.LineInfo

import scala.collection.Searching
import scala.collection.mutable.ArrayBuffer

class FileLines(private var lineStarts: Array[Int], private var lineLengths: Array[Int]) {

  def getLine(index: Int): Int = {
    // Written by AI
    @annotation.tailrec
    def loop(start: Int, end: Int): Int = {
      if (start > end) start
      else {
        val mid = (start + end) / 2
        val cmp = lineStarts(mid).compareTo(index)
        if (cmp < 0) loop(mid + 1, end)
        else if (cmp > 0) loop(start, mid - 1)
        else mid
      }
    }
    loop(0, lineStarts.length - 1)
  }

  def getColumn(index: Int): Int = {
    val line = getLine(index)
    val lineStart = lineStarts(line)
    index - lineStart + 1
  }

  def nthLineInfo(line: Int): Option[LineInfo] = {
    if (0 <= line && line < lineStarts.length) {
      Some(LineInfo(lineStarts(line), lineLengths(line)))
    } else {
      None
    }
  }

  def lineCount: Int = lineStarts.length

}

object FileLines {
  val empty: FileLines = new FileLines(Array.empty, Array.empty)

  /**
    * N.B.: `\r` is assumed only to exist before `\n`.
    */
  def fromChars(chars: Array[Char]): FileLines = {
    val lineStarts = ArrayBuffer[Int]()
    val lineLengths = ArrayBuffer[Int]()
    var index = 0
    while (index <= chars.length) {
      if (chars(index) == '\n') {
        val extra = if (index > 0 && chars(index - 1) == '\r') 1 else 0;
        if (lineStarts.nonEmpty) lineLengths += (index - lineStarts.last - extra - 1)
        else lineLengths += index - extra - 1
        lineStarts += index + 1
      }
      index += 1
    }
    new FileLines(lineStarts.toArray, lineLengths.toArray)
  }

  case class LineInfo(index: Int, realLength: Int)
}
