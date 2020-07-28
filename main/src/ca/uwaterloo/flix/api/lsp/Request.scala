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
import org.json4s
import org.json4s.JsonAST.{JArray, JString, JValue}

/**
  * A common super-type for language server requests.
  */
sealed trait Request

object Request {

  /**
    * A request to add (or update) the given uri with the given source code.
    */
  case class AddUri(uri: String, src: String) extends Request

  /**
    * A request to remove the given uri.
    */
  case class RemUri(uri: String) extends Request

  /**
    * A code lens request.
    */
  case class CodeLens(uri: String) extends Request

  /**
    * A request for code completion.
    */
  case class Complete(uri: String, pos: Position) extends Request

  /**
    * A request to get all defs.
    */
  case class GetDefs(uri: String) extends Request

  /**
    * A request to get all enums.
    */
  case class GetEnums(uri: String) extends Request

  /**
    * A request to go to a declaration.
    */
  case class Goto(uri: String, pos: Position) extends Request

  /**
    * A folding range request.
    */
  case class FoldingRange(uri: String) extends Request

  /**
    * A request to shutdown the language server.
    */
  case object Shutdown extends Request

  /**
    * A request to get the type and effect of an expression.
    */
  case class Context(uri: String, pos: Position) extends Request

  /**
    * A request to find all uses of an entity.
    */
  case class Uses(uri: String, pos: Position) extends Request

  /**
    * A request to validate the source files in `paths`.
    */
  case class Validate(paths: List[String]) extends Request

  /**
    * A request to return the compiler version.
    */
  case object Version extends Request

  /**
    * Tries to parse the given `json` value as a [[AddUri]] request.
    */
  def parseAddUri(json: json4s.JValue): Result[Request, String] = {
    val uriRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    val srcRes: Result[String, String] = json \\ "src" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected src: '$s'.")
    }
    for {
      uri <- uriRes
      src <- srcRes
    } yield Request.AddUri(uri, src)
  }

  /**
    * Tries to parse the given `json` value as a [[RemUri]] request.
    */
  def parseRemUri(json: json4s.JValue): Result[Request, String] = {
    val uriRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      uri <- uriRes
    } yield Request.RemUri(uri)
  }

  /**
    * Tries to parse the given `json` value as a [[Complete]] request.
    */
  def parseComplete(json: json4s.JValue): Result[Request, String] = {
    val uriRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      uri <- uriRes
      pos <- Position.parse(json \\ "position")
    } yield Request.Complete(uri, pos)
  }

  /**
    * Tries to parse the given `json` value as a [[Context]] request.
    */
  def parseContext(json: JValue): Result[Request, String] = {
    val uriRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      uri <- uriRes
      pos <- Position.parse(json \\ "position")
    } yield Request.Context(uri, pos)
  }

  /**
    * Tries to parse the given `json` value as a [[CodeLens]] request.
    */
  def parseCodeLens(json: json4s.JValue): Result[Request, String] = {
    val uriRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      uri <- uriRes
    } yield Request.CodeLens(uri)
  }

  /**
    * Tries to parse the given `json` value as a [[FoldingRange]] request.
    */
  def parseFoldingRange(json: json4s.JValue): Result[Request, String] = {
    val uriRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      uri <- uriRes
    } yield Request.FoldingRange(uri)
  }

  /**
    * Tries to parse the given `json` value as a [[Goto]] request.
    */
  def parseGoto(json: json4s.JValue): Result[Request, String] = {
    val uriRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      uri <- uriRes
      pos <- Position.parse(json \\ "position")
    } yield Request.Goto(uri, pos)
  }

  /**
    * Tries to parse the given `json` value as a [[Uses]] request.
    */
  def parseUses(json: json4s.JValue): Result[Request, String] = {
    val uriRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      uri <- uriRes
      pos <- Position.parse(json \\ "position")
    } yield Request.Uses(uri, pos)
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
        Ok(Request.Validate(xs))
      case _ => Err("Cannot find property 'paths'. Missing or incorrect type?")
    }
  }

}
