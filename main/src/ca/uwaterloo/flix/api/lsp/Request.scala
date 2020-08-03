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
import org.json4s.JsonAST.{JString, JValue}

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
    * A request to compile and check all source files.
    */
  case class Check() extends Request

  /**
    * A request to get the type and effect of an expression.
    */
  case class Context(uri: String, pos: Position) extends Request

  /**
    * A code lens request.
    */
  case class CodeLens(uri: String) extends Request

  /**
    * A request for code completion.
    */
  case class Complete(uri: String, pos: Position) extends Request

  /**
    * A request to go to a declaration.
    */
  case class Goto(uri: String, pos: Position) extends Request

  /**
    * A folding range request.
    */
  case class FoldingRange(uri: String) extends Request

  /**
    * A request to run all benchmarks.
    */
  case object RunBenchmarks extends Request

  /**
    * A request to run main.
    */
  case object RunMain extends Request

  /**
    * A request to run all tests.
    */
  case object RunTests extends Request

  /**
    * A request to shutdown the language server.
    */
  case object Shutdown extends Request

  /**
    * A request for all symbols.
    */
  case class Symbols(uri: String) extends Request

  /**
    * A request to find all uses of an entity.
    */
  case class Uses(uri: String, pos: Position) extends Request

  /**
    * A request to build the project.
    */
  case object PackagerBuild extends Request

  /**
    * A request to build the project documentation.
    */
  case object PackagerBuildDoc extends Request

  /**
    * A request to build the JAR from the project.
    */
  case object PackagerBuildJar extends Request

  /**
    * A request to build a Flix package from the project.
    */
  case object PackagerBuildPkg extends Request

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
    * Tries to parse the given `json` value as a [[Symbols]] request.
    */
  def parseSymbols(json: json4s.JValue): Result[Request, String] = {
    val uriRes: Result[String, String] = json \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
    for {
      uri <- uriRes
    } yield Request.Symbols(uri)
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
    * Tries to parse the given `json` value as a [[Check]] request.
    */
  def parseCheck(json: JValue): Result[Request, String] = Ok(Request.Check())

}
