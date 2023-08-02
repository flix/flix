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
import org.json4s.JsonDSL._
import org.json4s._
import scala.math.Ordered.orderingToOrdered

/**
  * Companion object for [[Position]].
  */
object Position {

  /**
    * Returns a position from the given source location `loc` using its beginning line and col.
    */
  def fromBegin(loc: SourceLocation): Position =
    Position(loc.beginLine - 1, loc.beginCol - 1)

  /**
    * Returns a position from the given source location `loc` using its ending line and col.
    */
  def fromEnd(loc: SourceLocation): Position =
    Position(loc.endLine - 1, loc.endCol - 1)

  /**
    * Tries to parse the given `json` value as a [[Position]].
    */
  def parse(json: JValue): Result[Position, String] = {
    // NB: LSP line and column numbers are zero-indexed, but Flix uses 1-indexed numbers internally.
    val lineResult: Result[Int, String] = json \\ "line" match {
      case JInt(i) => Ok(i.toInt + 1) // Flix uses 1-indexed line numbers.
      case v => Err(s"Unexpected non-integer line number: '$v'.")
    }
    val characterResult: Result[Int, String] = json \\ "character" match {
      case JInt(i) => Ok(i.toInt + 1) // Flix uses 1-indexed column numbers.
      case v => Err(s"Unexpected non-integer character: '$v'.")
    }
    for {
      line <- lineResult
      character <- characterResult
    } yield Position(line, character)
  }
}

/**
  * Represent a `Position` in LSP.
  *
  * @param line      Line position in a document (zero-based).
  * @param character Character offset on a line in a document (zero-based).
  */
case class Position(line: Int, character: Int) extends Ordered[Position] {
  def toJSON: JValue = ("line" -> line) ~ ("character" -> character)

  override def compare(that: Position): Int =
    (this.line, this.character) compare (that.line, that.character)
}
