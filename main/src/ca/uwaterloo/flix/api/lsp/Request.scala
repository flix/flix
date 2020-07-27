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

import java.nio.file.{Path, Paths}

import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.json4s
import org.json4s.JsonAST.{JArray, JString, JValue}

/**
  * A common super-type for language server requests.
  */
sealed trait Request

object Request {

  /**
    * A 'textDocument/codeLens' request.
    */
  case class CodeLens(uri: Path) extends Request

  /**
    * A request for code completion.
    */
  case class Complete(uri: Path, pos: Position) extends Request

  /**
    * A request to get all defs.
    */
  case class GetDefs(uri: Path) extends Request

  /**
    * A request to get all enums.
    */
  case class GetEnums(uri: Path) extends Request

  /**
    * A request to go to a declaration.
    */
  case class Goto(uri: Path, pos: Position) extends Request

  /**
    * A 'textDocument/foldingRange' request.
    */
  case class FoldingRange(uri: Path) extends Request

  /**
    * A request to shutdown the language server.
    */
  case object Shutdown extends Request

  /**
    * A request to get the type and effect of an expression.
    */
  case class TypeAndEffectOf(uri: Path, pos: Position) extends Request

  /**
    * A request to find all uses of an entity.
    */
  case class Uses(uri: Path, pos: Position) extends Request

  /**
    * A request to validate the source files in `paths`.
    */
  case class Validate(paths: List[Path]) extends Request

  /**
    * A request to return the compiler version.
    */
  case object Version extends Request


  /**
    * Tries to parse the given `json` value as a [[Complete]] request.
    */
  def parseComplete(json: json4s.JValue): Result[Request, String] = {
    val docRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      doc <- docRes
      pos <- Position.parse(json \\ "position")
    } yield Request.Complete(Paths.get(doc).normalize(), pos)
  }

  /**
    * Tries to parse the given `json` value as a [[CodeLens]] request.
    */
  def parseCodeLens(json: json4s.JValue): Result[Request, String] = {
    val docRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      doc <- docRes
    } yield Request.CodeLens(Paths.get(doc).normalize())
  }

  /**
    * Tries to parse the given `json` value as a [[FoldingRange]] request.
    */
  def parseFoldingRange(json: json4s.JValue): Result[Request, String] = {
    val docRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      doc <- docRes
    } yield Request.FoldingRange(Paths.get(doc).normalize())
  }

  /**
    * Tries to parse the given `json` value as a [[Goto]] request.
    */
  def parseGoto(json: json4s.JValue): Result[Request, String] = {
    val docRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      doc <- docRes
      pos <- Position.parse(json \\ "position")
    } yield Request.Goto(Paths.get(doc).normalize(), pos)
  }

  /**
    * Tries to parse the given `json` value as a [[TypeAndEffectOf]] request.
    */
  def parseTypeAndEffectOf(json: JValue): Result[Request, String] = {
    val docRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      doc <- docRes
      pos <- Position.parse(json \\ "position")
    } yield Request.TypeAndEffectOf(Paths.get(doc).normalize(), pos)
  }

  /**
    * Tries to parse the given `json` value as a [[Uses]] request.
    */
  def parseUses(json: json4s.JValue): Result[Request, String] = {
    val docRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      doc <- docRes
      pos <- Position.parse(json \\ "position")
    } yield Request.Uses(Paths.get(doc).normalize(), pos)
  }

  /**
    * Tries to parse the given `json` value as a [[Validate]] request.
    */
  def parseValidate(json: JValue): Result[Request, String] = {
    json \\ "paths" match {
      case JArray(arr) =>
        val xs = arr.collect {
          case JString(s) => s
        }
        Ok(Request.Validate(xs.map(x => Paths.get(x))))
      case _ => Err("Cannot find property 'paths'. Missing or incorrect type?")
    }
  }

}
