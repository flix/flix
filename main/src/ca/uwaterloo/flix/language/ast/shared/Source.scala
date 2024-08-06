/*
 * Copyright 2024 Magnus Madsen
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
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.Sourceable

import scala.annotation.tailrec

/**
 * A source is a name and an array of character data.
 *
 * A source is stable if it cannot change after being loaded (e.g. the standard library, etc).
 */
case class Source(input: Input, data: Array[Char], stable: Boolean) extends Sourceable {

  def name: String = input match {
    case Input.Text(name, _, _) => name
    case Input.TxtFile(path) => path.toString
    case Input.PkgFile(path) => path.toString
    case Input.Unknown => "unknown"
  }

  def src: Source = this

  override def equals(o: scala.Any): Boolean = o match {
    case that: Source => this.input == that.input
  }

  override def hashCode(): Int = input.hashCode()

  override def toString: String = name


  /**
   * Gets a line of text from the source as a string.
   * If line is out of bounds the empty string is returned.
   *
   * This function has been adapted from parboiled2 when moving away from the library.
   * We now produce its accompanying license in full:
   *
   * Copyright 2009-2019 Mathias Doenitz
   *
   * Licensed under the Apache License, Version 2.0 (the "License");
   * you may not use this file except in compliance with the License.
   * You may obtain a copy of the License at
   *
   * http://www.apache.org/licenses/LICENSE-2.0
   *
   * Unless required by applicable law or agreed to in writing, software
   * distributed under the License is distributed on an "AS IS" BASIS,
   * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   * See the License for the specific language governing permissions and
   * limitations under the License.
   */
  def getLine(line: Int): String = {
    @tailrec
    def rec(ix: Int, lineStartIx: Int, lineNr: Int): String =
      if (ix < data.length)
        if (data(ix) == '\n')
          if (lineNr < line) rec(ix + 1, ix + 1, lineNr + 1)
          else new String(data, lineStartIx, math.max(ix - lineStartIx, 0))
        else rec(ix + 1, lineStartIx, lineNr)
      else if (lineNr == line) new String(data, lineStartIx, math.max(ix - lineStartIx, 0))
      else ""

    rec(ix = 0, lineStartIx = 0, lineNr = 1)
  }
}
