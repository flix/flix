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
import org.json4s.JsonAST.{JField, JObject}

/**
  * Companion object of [[Range]].
  */
object Range {

  /**
    * Returns a range from the given source location `loc`.
    */
  def from(loc: SourceLocation): Range = {
    Range(Position(loc.beginLine, loc.beginCol), Position(loc.endLine, loc.endCol))
  }

}

/**
  * Represent a `Range` in LSP.
  */
case class Range(start: Position, end: Position) {
  def toJSON: JObject = JObject(
    JField("start", start.toJSON),
    JField("end", end.toJSON)
  )
}
