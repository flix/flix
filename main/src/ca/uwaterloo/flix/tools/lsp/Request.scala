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
package ca.uwaterloo.flix.tools.lsp

import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.json4s
import org.json4s.DefaultFormats
import org.json4s.JsonAST.{JArray, JObject, JString, JValue}

sealed trait Request

object Request {

  /**
    * A request to compile the `path` source files.
    */
  case class Compile(paths: List[String]) extends Request

  /**
    * A requests to get the type and effect in the document `doc` at position `pos`.
    */
  case class TypeOf(doc: Document, pos: Position) extends Request


  case class JumpToDef(doc: Document, pos: Position) extends Request

  /**
    * A request to shutdown the language server.
    */
  case object Shutdown extends Request

  // TODO: Shutdown message.
  // TODO: Get type/effect/typeandeffect
  // TODO: completion?
  // TODO: signature help?
  // TODO: GoTODef
  // TODO: Goto type def.
  // TODO: FindUsages.

  /**
    * Tries to parse the given `json` value as a [[Compile]] request.
    */
  def parseCompile(json: JValue): Result[Request, String] = {
    json \\ "paths" match {
      case JArray(arr) =>
        val xs = arr.collect {
          case JString(s) => s
        }
        Ok(Request.Compile(xs))
      case _ => Err("Cannot find property 'paths'. Missing or incorrect type?")
    }
  }

  def parseTypeOf(json: json4s.JValue): Result[Request, String] = {

    // TODO: Errors
    val doc = Document.parse(json \\ "document")
    val pos = Position.parse(json \\ "position")
    Ok(Request.TypeOf(doc, pos))
  }

  def parseJumpToDef(json: json4s.JValue): Result[Request, String] = {

    // TODO: Errors
    val doc = Document.parse(json \\ "document")
    val pos = Position.parse(json \\ "position")
    Ok(Request.JumpToDef(doc, pos))
  }

}

