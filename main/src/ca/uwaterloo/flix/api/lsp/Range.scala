/*
 * Copyright 2020 Magnus Madsen
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
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}

import org.json4s.JsonDSL.*
import org.json4s.*

/**
  * Companion object of [[Range]].
  */
object Range {

  /**
    * Returns a range from the given source location `loc`.
    */
  def from(loc: SourceLocation): Range = {
    // NB: LSP line and column numbers are zero-indexed.
    Range(Position.fromBegin(loc), Position.fromEnd(loc))
  }

  /**
    * Tries to parse the given `json` value as a [[Range]].
    */
  def parse(json: JValue): Result[Range, String] = {
    val startResult = Position.parse(json \\ "start")
    val endResult = Position.parse(json \\ "end")
    for {
      start <- startResult
      end <- endResult
    } yield Range(start, end)
  }

}

/**
  * Represent a `Range` in LSP.
  *
  * @param start The range's start position.
  * @param end   The range's end position.
  */
case class Range(start: Position, end: Position) {
  def toJSON: JValue = ("start" -> start.toJSON) ~ ("end" -> end.toJSON)

  /**
    * Returns the range that starts earlier.
    */
  private def earlierStart(that: Range): Range =
     if (start < that.start) this else that

  /**
    * Returns the range that ends later.
    */
  private def laterEnd(that: Range): Range =
    if (end > that.end) this else that

  /**
    * Checks if this range overlaps with the other range.
    */
  def overlapsWith(that: Range): Boolean = {
    val fst = earlierStart(that)
    val lst = laterEnd(that)
    fst == lst || fst.end > lst.start
  }
}
