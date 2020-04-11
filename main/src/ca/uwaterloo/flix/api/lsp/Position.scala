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

import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.json4s.JValue
import org.json4s.JsonAST.{JField, JInt, JObject, JString}

/**
  * Companion object for [[Position]].
  */
object Position {

  /**
    * Tries to parse the given `json` value as a [[Position]].
    */
  def parse(json: JValue): Result[Position, String] = {
    val lineResult: Result[Int, String] = json \\ "line" match {
      case JInt(i) => Ok(i.toInt)
      case v => Err(s"Unexpected line number: '$v'.")
    }
    val columnResult: Result[Int, String] = json \\ "col" match {
      case JInt(i) => Ok(i.toInt)
      case v => Err(s"Unexpected column number: '$v'.")
    }
    for {
      line <- lineResult
      column <- columnResult
    } yield Position(line, column)
  }
}

/**
  * Represent a `Position` in LSP.
  */
case class Position(line: Int, col: Int) {
  def toJSON: JObject = JObject(
    JField("line", JInt(line)),
    JField("character", JInt(col))
  )
}
