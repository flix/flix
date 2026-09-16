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

import ca.uwaterloo.flix.language.ast.shared.SourceName
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.json4s
import org.json4s.JsonAST.{JString, JValue}
import org.json4s.jvalue2monadic
import java.net.{URI, URISyntaxException}

/**
  * A common super-type for language server requests.
  */
sealed trait Request {
  /**
    * A unique number that identifies this specific request.
    */
  def requestId: String
}

object Request {

  /**
    * A request to add the workspace root at the given uri.
    */
  case class AddWorkspace(requestId: String, uri: URI) extends Request

  /**
    * A request to add (or update) the given uri with the given source code.
    */
  case class AddUri(requestId: String, name: SourceName, src: String) extends Request

  /**
    * A request to remove the given uri.
    */
  case class RemUri(requestId: String, name: SourceName) extends Request

  /**
    * A request to add (or update) the package at the given uri.
    */
  case class AddPkg(requestId: String, uri: URI) extends Request

  /**
    * A request to remove the package at the given uri.
    */
  case class RemPkg(requestId: String, uri: URI) extends Request

  /**
    * A request to add (or update) the JAR at the given uri.
    */
  case class AddJar(requestId: String, uri: URI) extends Request

  /**
    * A request to remove the package at the given uri.
    */
  case class RemJar(requestId: String, uri: URI) extends Request

  /**
    * A request for the compiler version.
    */
  case class Version(requestId: String) extends Request

  /**
    * A request to load the project again and start over with a fresh compiler.
    */
  case class Restart(requestId: String) extends Request

  /**
    * A request to shutdown the language server.
    */
  case class Shutdown(requestId: String) extends Request

  /**
    * A request to temporarily disconnect from the socket.
    * Used for testing purposes.
    */
  case class Disconnect(requestId: String) extends Request

  /**
    * A request to compile and check all source files.
    */
  case class Check(requestId: String) extends Request

  /**
    * A code lens request.
    */
  case class Codelens(requestId: String, name: SourceName) extends Request

  /**
    * A complete request.
    */
  case class Complete(requestId: String, name: SourceName, pos: Position) extends Request

  /**
    * A request to go to a declaration.
    */
  case class Goto(requestId: String, name: SourceName, pos: Position) extends Request

  /**
    * A request to find implementations.
    */
  case class Implementation(requestId: String, name: SourceName, pos: Position) extends Request

  /**
    * A request to get highlight information.
    */
  case class Highlight(requestId: String, name: SourceName, pos: Position) extends Request

  /**
    * A request to get hover information.
    */
  case class Hover(requestId: String, name: SourceName, pos: Position) extends Request

  /**
    * A request to rename a definition, local variable, or other named entity.
    */
  case class Rename(requestId: String, newName: String, name: SourceName, pos: Position) extends Request

  /**
    * A request to find all uses of an entity.
    */
  case class Uses(requestId: String, name: SourceName, pos: Position) extends Request

  /**
    * A request to get document symbols information.
    */
  case class DocumentSymbols(requestId: String, name: SourceName) extends Request

  /**
   * A request to get semantic tokens for a file.
   */
  case class SemanticTokens(requestId: String, name: SourceName) extends Request

  /**
    * A request to get the signature information.
    */
  case class Signature(requestId: String, name: SourceName, pos: Position) extends Request

  /**
    * A request to get workspace symbols information.
    */
  case class WorkspaceSymbols(requestId: String, query: String) extends Request

  /**
    * A request to get the inlay hints for the given [[range]] in a file denoted by [[uri]]
    */
  case class InlayHint(requestId: String, name: SourceName, range: Range) extends Request

  /**
    * A request to print the ASTs following each phase.
    * Returns the folder path that holds the ASTs.
    */
  case class ShowAst(requestId: String) extends Request

  /**
    * A request to view available code actions.
    */
  case class CodeAction(requestId: String, name: SourceName, range: Range, context: CodeActionContext) extends Request

  /**
    * A request to format a file.
    */
  case class Formatting(requestId: String, name: SourceName, options: FormattingOptions) extends Request

  /**
    * A request to get the folding ranges for a file.
    */
  case class FoldingRange(requestId: String, name: SourceName) extends Request

  /**
    * Tries to parse the given `json` value as a [[AddWorkspace]] request.
    */
  def parseAddWorkspace(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      uri <- parseUri(json)
    } yield Request.AddWorkspace(id, uri)
  }

  /**
    * Tries to parse the given `json` value as a [[AddUri]] request.
    */
  def parseAddUri(json: json4s.JValue): Result[Request, String] = {
    val srcRes: Result[String, String] = json \ "src" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected src: '$s'.")
    }
    for {
      id <- parseId(json)
      name <- parseSourceName(json)
      src <- srcRes
    } yield Request.AddUri(id, name, src)
  }

  /**
    * Tries to parse the given `json` value as a [[RemUri]] request.
    */
  def parseRemUri(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      name <- parseSourceName(json)
    } yield Request.RemUri(id, name)
  }

  /**
    * Tries to parse the given `json` value as a [[AddPkg]] request.
    *
    * The package is read from the file at the uri. Older clients also send the contents of the
    * package in a `base64` field, which is ignored.
    */
  def parseAddPkg(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      uri <- parseUri(json)
    } yield Request.AddPkg(id, uri)
  }

  /**
    * Tries to parse the given `json` value as a [[RemPkg]] request.
    */
  def parseRemPkg(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      uri <- parseUri(json)
    } yield Request.RemPkg(id, uri)
  }


  /**
    * Tries to parse the given `json` value as a [[AddJar]] request.
    */
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

  /**
    * Tries to parse the given `json` value as a [[RemJar]] request.
    */
  def parseRemJar(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      uri <- parseUri(json)
    } yield Request.RemJar(id, uri)
  }

  /**
    * Tries to parse the given `json` value as a [[Version]] request.
    */
  def parseVersion(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
    } yield Request.Version(id)
  }

  /**
    * Tries to parse the given `json` value as a [[Restart]] request.
    */
  def parseRestart(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
    } yield Request.Restart(id)
  }

  /**
    * Tries to parse the given `json` value as a [[Shutdown]] request.
    */
  def parseShutdown(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
    } yield Request.Shutdown(id)
  }

  /**
    * Tries to parse the given `json` value as a [[Disconnect]] request.
    */
  def parseDisconnect(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
    } yield Request.Disconnect(id)
  }

  /**
    * Tries to parse the given `json` value as a [[Check]] request.
    */
  def parseCheck(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
    } yield Request.Check(id)
  }

  /**
    * Tries to parse the given `json` value as a [[Codelens]] request.
    */
  def parseCodelens(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      name <- parseSourceName(json)
    } yield Request.Codelens(id, name)
  }

  /**
    * Tries to parse the given `json` value as a [[Complete]] request.
    */
  def parseComplete(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      name <- parseSourceName(json)
      pos <- Position.parse(json \ "position")
    } yield Request.Complete(id, name, pos)
  }

  /**
    * Tries to parse the given `json` value as a [[Goto]] request.
    */
  def parseGoto(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      name <- parseSourceName(json)
      pos <- Position.parse(json \ "position")
    } yield Request.Goto(id, name, pos)
  }

  /**
    * Tries to parse the given `json` value as a [[Implementation]] request.
    */
  def parseImplementation(json: JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      name <- parseSourceName(json)
      pos <- Position.parse(json \ "position")
    } yield Request.Implementation(id, name, pos)
  }

  /**
    * Tries to parse the given `json` value as a [[Highlight]] request.
    */
  def parseHighlight(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      name <- parseSourceName(json)
      pos <- Position.parse(json \ "position")
    } yield Request.Highlight(id, name, pos)
  }

  /**
    * Tries to parse the given `json` value as a [[Hover]] request.
    */
  def parseHover(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      name <- parseSourceName(json)
      pos <- Position.parse(json \ "position")
    } yield Request.Hover(id, name, pos)
  }

  /**
    * Tries to parse the given `json` value as a [[Rename]] request.
    */
  def parseRename(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      newName <- parseString("newName", json)
      name <- parseSourceName(json)
      pos <- Position.parse(json \ "position")
    } yield Request.Rename(id, newName, name, pos)
  }

  /**
    * Tries to parse the given `json` value as a [[Uses]] request.
    */
  def parseUses(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      name <- parseSourceName(json)
      pos <- Position.parse(json \ "position")
    } yield Request.Uses(id, name, pos)
  }

  /**
    * Tries to parse the given `json` value as a [[DocumentSymbols]] request.
    */
  def parseDocumentSymbols(v: JValue): Result[Request, String] = {
    for {
      id <- parseId(v)
      name <- parseSourceName(v)
    } yield Request.DocumentSymbols(id, name)
  }

  /**
    * Tries to parse the given `json` value as a [[WorkspaceSymbols]] request.
    */
  def parseWorkspaceSymbols(v: JValue): Result[Request, String] = {
    for {
      id <- parseId(v)
      query <- parseString("query", v)
    } yield Request.WorkspaceSymbols(id, query)
  }

  /**
   * Tries to parse the given `json` value as a [[SemanticTokens]] request.
   */
  def parseSemanticTokens(json: JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      name <- parseSourceName(json)
    } yield Request.SemanticTokens(id, name)
  }

  /**
    * Tries to parse the given `json` value as a [[Signature]] request.
    */
  def parseSignature(json: JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      name <- parseSourceName(json)
      pos <- Position.parse(json \ "position")
    } yield Request.Signature(id, name, pos)
  }

  /**
    * Tries to parse the given `json` value as a [[InlayHint]] request.
    */
  def parseInlayHint(json: JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      name <- parseSourceName(json)
      range <- Range.parse(json \ "range")
    } yield Request.InlayHint(id, name, range)
  }

  /**
    * Tries to parse the given `json` value as a [[ShowAst]] request.
    */
  def parseShowAst(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
    } yield Request.ShowAst(id)
  }

  /**
    * Attempts to parse the `id` from the given JSON value `v`.
    */
  private def parseId(v: JValue): Result[String, String] = {
    v \ "id" match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected id: '$s'.")
    }
  }

  /**
    * Attempts to parse the `uri` from the given JSON value `v`.
    */
  private def parseUri(v: JValue): Result[URI, String] = {
    v \ "uri" match {
      case JString(s) =>
        try {
          Ok(new URI(s))
        } catch {
          case ex: URISyntaxException => Err(s"Malformed uri: '$s': ${ex.getMessage}")
        }
      case s => Err(s"Unexpected uri: '$s'.")
    }
  }

  /**
    * Attempts to parse the `uri` from the given JSON value `v` as the name of a source.
    */
  private def parseSourceName(v: JValue): Result[SourceName, String] = parseUri(v).map(ClientUri.toSourceName)

  /**
    * Attempts to parse the given `key` as a String from the given JSON value `v`.
    */
  private def parseString(k: String, v: JValue): Result[String, String] = {
    v \ k match {
      case JString(s) => Ok(s)
      case s => Err(s"Unexpected $k: '$s'.")
    }
  }

  /**
    * Tries to parse the given `json` value as a [[CodeAction]] request.
    */
  def parseCodeAction(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      name <- parseSourceName(json)
      range <- Range.parse(json \ "range")
      context <- CodeActionContext.parse(json \ "context")
    } yield Request.CodeAction(id, name, range, context)
  }

  /**
    * Tries to parse the given `json` value as a [[Formatting]] request.
    *
    * @param json the json value
    * @return the formatting request
    */
  def parseFormatting(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      name <- parseSourceName(json)
      options = FormattingOptions.parse(json \ "options")
    } yield Request.Formatting(id, name, options)
  }

  /**
    * Tries to parse the given `json` value as a [[FoldingRange]] request.
    */
  def parseFoldingRange(json: json4s.JValue): Result[Request, String] = {
    for {
      id <- parseId(json)
      name <- parseSourceName(json)
    } yield Request.FoldingRange(id, name)
  }

}
