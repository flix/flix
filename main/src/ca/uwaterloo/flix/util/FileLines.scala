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

import scala.collection.mutable.ArrayBuffer

class FileLines {

  private var lineStarts: ArrayBuffer[Int] = ArrayBuffer()

  private var lineLengths: ArrayBuffer[Int] = ArrayBuffer()

  /**
    * Add a line at zero-indexed `startOffset` where the last line was ended with `newLineChars` number of characters (1 or 2).
    *
    * {{{
    *   12345\r\n
    * }}}
    *
    * This file would induce the call `addLine(7, 2)` to add the line at offset `7` with `2` characters of new line characters (`\r\n`).
    */
  def addLine(startOffset: Int, newLineChars: Int): Unit = {
    assert(1 <= newLineChars && newLineChars <= 2)
    val prev = lineStarts.lastOption.getOrElse(0)
    assert(startOffset > prev)
    lineStarts.append(startOffset)
    lineLengths.append(startOffset - prev - newLineChars)
  }

}
