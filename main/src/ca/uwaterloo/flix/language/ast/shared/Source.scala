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

import java.nio.file.Path
import scala.annotation.tailrec

object Source {
  /** An unknown source. */
  val Unknown: Source = Source.empty(SourceName.PathName(Path.of("unknown")), Origin.Unknown, SecurityContext.Unrestricted)

  /** Returns an empty source with the given name, origin, and security context. */
  def empty(sourceName: SourceName, origin: Origin, sctx: SecurityContext): Source =
    Source(sourceName, origin, sctx, Array.emptyCharArray)

  /** Returns a source with the given name, origin, and security context, and the text `str`. */
  def fromString(sourceName: SourceName, origin: Origin, sctx: SecurityContext, str: String): Source =
    Source(sourceName, origin, sctx, str.toCharArray)
}

/**
  * A source: its name, where it came from, the security context it is compiled under, and its text.
  *
  * Two sources are equal if they have the same name. The text is deliberately not part of equality:
  * a cache keyed by source must still find the source after its text has changed.
  */
case class Source(sourceName: SourceName, origin: Origin, sctx: SecurityContext, data: Array[Char]) extends Sourceable {

  /**
    * The name as a string, for display and for the language server.
    */
  def name: String = sourceName.toString

  def src: Source = this

  override def equals(o: scala.Any): Boolean = o match {
    case that: Source => this.sourceName == that.sourceName
    case _ => false
  }

  override def hashCode(): Int = sourceName.hashCode()

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
