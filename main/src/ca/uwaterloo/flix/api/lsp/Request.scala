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
import org.json4s.jvalue2monadic
import org.json4s.JsonAST.{JString, JValue}

import java.util.Base64

/** A common super-type for language server requests. */
sealed trait Request {
  /** A unique number that identifies this specific request. */
  def requestId: String
}

object Request {

  /** A request to add (or update) the given uri with the given source code. */
  case class AddUri(requestId: String, uri: String, src: String) extends Request

  /** A request to remove the given uri. */
  case class RemUri(requestId: String, uri: String) extends Request

  /** A request to add (or update) the package at the given uri with the given binary data. */
  case class AddPkg(requestId: String, uri: String, data: Array[Byte]) extends Request

  /** A request to remove the package at the given uri. */
  case class RemPkg(requestId: String, uri: String) extends Request

  /** A request to add (or update) the JAR at the given uri. */
  case class AddJar(requestId: String, uri: String) extends Request

  /** A request to remove the package at the given uri. */
  case class RemJar(requestId: String, uri: String) extends Request

  /** A request for the compiler version. */
  case class Version(requestId: String) extends Request

  /** A request to shutdown the language server. */
  case class Shutdown(requestId: String) extends Request

  /**
    * A request to temporarily disconnect from the socket.
    * Used for testing purposes.
    */
  case class Disconnect(requestId: String) extends Request

  /** A request to compile and check all source files. */
  case class Check(requestId: String) extends Request

  /** A code lens request. */
  case class Codelens(requestId: String, uri: String) extends Request

  /** A complete request. */
  case class Complete(requestId: String, uri: String, pos: Position) extends Request

  /** A request to go to a declaration. */
  case class Goto(requestId: String, uri: String, pos: Position) extends Request

  /** A request to find implementations. */
  case class Implementation(requestId: String, uri: String, pos: Position) extends Request

  /** A request to get highlight information. */
  case class Highlight(requestId: String, uri: String, pos: Position) extends Request

  /** A request to get hover information. */
  case class Hover(requestId: String, uri: String, pos: Position) extends Request

  /** A request to rename a definition, local variable, or other named entity. */
  case class Rename(requestId: String, newName: String, uri: String, pos: Position) extends Request

  /** A request to find all uses of an entity. */
  case class Uses(requestId: String, uri: String, pos: Position) extends Request

  /** A request to get document symbols information. */
  case class DocumentSymbols(requestId: String, uri: String) extends Request

  /** A request to get semantic tokens for a file. */
  case class SemanticTokens(requestId: String, uri: String) extends Request

  /** A request to get workspace symbols information. */
  case class WorkspaceSymbols(requestId: String, query: String) extends Request

  /** A request to get the inlay hints for the given [[range]] in a file denoted by [[uri]] */
  case class InlayHint(requestId: String, uri: String, range: Range) extends Request

  /**
    * A request to print the ASTs following each phase.
    * Returns the folder path that holds the ASTs.
    */
  case class ShowAst(requestId: String) extends Request

  /** A request to view available code actions. */
  case class CodeAction(requestId: String, uri: String, range: Range, context: CodeActionContext) extends Request

  /** Tries to parse the given `json` value as a [[AddUri]] request. */
  def parseAddUri(json: json4s.JValue): Result[Request, String] = {
    val srcRes: Result[String, String] = json \\ "src" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected src: '$s'.")
    }
    for {
      id <- parseId(json)
      uri <- parseUri(json)
      src <- srcRes
    } yield Request.AddUri(id, uri, src)
  }

  /** Tries to parse the given `json` value as a [[RemUri]] request. */
  def parseRemUri(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      uri <- parseUri(json)
    } yield Request.RemUri(id, uri)
  }

  /** Tries to parse the given `json` value as a [[AddPkg]] request. */
  def parseAddPkg(json: json4s.JValue): Result[Request, String] = {
    val base64Res: Result[String, String] = json \\ "base64" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected base64: '$s'.")
    }

    try {
      for {
        id <- parseId(json)
        uri <- parseUri(json)
        base64 <- base64Res
      } yield {
        val decoder = Base64.getDecoder
        val data = decoder.decode(base64)
        Request.AddPkg(id, uri, data)
      }
    } catch {
      case ex: IllegalArgumentException => Result.Err(ex.getMessage)
    }
  }

  /** Tries to parse the given `json` value as a [[RemPkg]] request. */
  def parseRemPkg(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      uri <- parseUri(json)
    } yield Request.RemPkg(id, uri)
  }


  /** Tries to parse the given `json` value as a [[AddJar]] request. */
  def parseAddJar(json: json4s.JValue): Result[Request, String] = {
    try {
      for {
        id <- parseId(json)
        uri <- parseUri(json)
      } yield {
        Request.AddJar(id, uri)
      }
    } catch {
      case ex: IllegalArgumentException => Result.Err(ex.getMessage)
    }
  }

  /** Tries to parse the given `json` value as a [[RemJar]] request. */
  def parseRemJar(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      uri <- parseUri(json)
    } yield Request.RemJar(id, uri)
  }

  /** Tries to parse the given `json` value as a [[Version]] request. */
  def parseVersion(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
    } yield Request.Version(id)
  }

  /** Tries to parse the given `json` value as a [[Shutdown]] request. */
  def parseShutdown(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
    } yield Request.Shutdown(id)
  }

  /** Tries to parse the given `json` value as a [[Disconnect]] request. */
  def parseDisconnect(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
    } yield Request.Disconnect(id)
  }

  /** Tries to parse the given `json` value as a [[Check]] request. */
  def parseCheck(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
    } yield Request.Check(id)
  }

  /** Tries to parse the given `json` value as a [[Codelens]] request. */
  def parseCodelens(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      uri <- parseUri(json)
    } yield Request.Codelens(id, uri)
  }

  /** Tries to parse the given `json` value as a [[Complete]] request. */
  def parseComplete(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      uri <- parseUri(json)
      pos <- Position.parse(json \\ "position")
    } yield Request.Complete(id, uri, pos)
  }

  /** Tries to parse the given `json` value as a [[Goto]] request. */
  def parseGoto(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      uri <- parseUri(json)
      pos <- Position.parse(json \\ "position")
    } yield Request.Goto(id, uri, pos)
  }

  /** Tries to parse the given `json` value as a [[Implementation]] request. */
  def parseImplementation(json: JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      uri <- parseUri(json)
      pos <- Position.parse(json \\ "position")
    } yield Request.Implementation(id, uri, pos)
  }

  /** Tries to parse the given `json` value as a [[Highlight]] request. */
  def parseHighlight(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      uri <- parseUri(json)
      pos <- Position.parse(json \\ "position")
    } yield Request.Highlight(id, uri, pos)
  }

  /** Tries to parse the given `json` value as a [[Hover]] request. */
  def parseHover(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      uri <- parseUri(json)
      pos <- Position.parse(json \\ "position")
    } yield Request.Hover(id, uri, pos)
  }

  /** Tries to parse the given `json` value as a [[Rename]] request. */
  def parseRename(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      newName <- parseString("newName", json)
      uri <- parseUri(json)
      pos <- Position.parse(json \\ "position")
    } yield Request.Rename(id, newName, uri, pos)
  }

  /** Tries to parse the given `json` value as a [[Uses]] request. */
  def parseUses(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      uri <- parseUri(json)
      pos <- Position.parse(json \\ "position")
    } yield Request.Uses(id, uri, pos)
  }

  /** Tries to parse the given `json` value as a [[DocumentSymbols]] request. */
  def parseDocumentSymbols(v: JValue): Result[Request, String] = {
    for {
      id <- parseId(v)
      uri <- parseUri(v)
    } yield Request.DocumentSymbols(id, uri)
  }

  /** Tries to parse the given `json` value as a [[WorkspaceSymbols]] request. */
  def parseWorkspaceSymbols(v: JValue): Result[Request, String] = {
    for {
      id <- parseId(v)
      query <- parseString("query", v)
    } yield Request.WorkspaceSymbols(id, query)
  }

  /** Tries to parse the given `json` value as a [[SemanticTokens]] request. */
  def parseSemanticTokens(json: JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      uri <- parseUri(json)
    } yield Request.SemanticTokens(id, uri)
  }

  /** Tries to parse the given `json` value as a [[InlayHint]] request. */
  def parseInlayHint(json: JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      uri <- parseUri(json)
      range <- Range.parse(json \\ "range")
    } yield Request.InlayHint(id, uri, range)
  }

  /** Tries to parse the given `json` value as a [[ShowAst]] request. */
  def parseShowAst(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
    } yield Request.ShowAst(id)
  }

  /** Attempts to parse the `id` from the given JSON value `v`. */
  private def parseId(v: JValue): Result[String, String] = {
    v \\ "id" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected id: '$s'.")
    }
  }

  /** Attempts to parse the `uri` from the given JSON value `v`. */
  private def parseUri(v: JValue): Result[String, String] = {
    v \\ "uri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected uri: '$s'.")
    }
  }

  /** Attempts to parse the given `key` as a String from the given JSON value `v`. */
  private def parseString(k: String, v: JValue): Result[String, String] = {
    v \\ k match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected $k: '$s'.")
    }
  }

  /** Attempts to parse the `projectRootUri` from the given JSON value `v`. */
  private def parseProjectRootUri(v: JValue): Result[String, String] = {
    v \\ "projectRootUri" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected projectRootUri: '$s'.")
    }
  }

  /** Tries to parse the given `json` value as a [[CodeAction]] request. */
  def parseCodeAction(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      uri <- parseUri(json)
      range <- Range.parse(json \ "range")
      context <- CodeActionContext.parse(json \ "context")
    } yield Request.CodeAction(id, uri, range, context)
  }

}
