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

import ca.uwaterloo.flix.api.lsp.provider.*
import ca.uwaterloo.flix.api.{CompilerLog, CrashHandler, Version}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.phase.extra.CodeHinter
import ca.uwaterloo.flix.util.*
import ca.uwaterloo.flix.util.Formatter.NoFormatter
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.java_websocket.WebSocket
import org.java_websocket.handshake.ClientHandshake
import org.java_websocket.server.WebSocketServer
import org.json4s.*
import org.json4s.JsonAST.{JArray, JString, JValue}
import org.json4s.JsonDSL.*
import org.json4s.ParserUtil.ParseException
import org.json4s.native.JsonMethods
import org.json4s.native.JsonMethods.parse

import java.net.InetSocketAddress
import java.text.SimpleDateFormat
import java.util.Date

/**
  * A Compiler Interface for the Language Server Protocol.
  *
  * Does not implement the LSP protocol directly, but relies on an intermediate TypeScript server.
  *
  *
  * Example:
  *
  * $ wscat -c ws://localhost:8000
  *
  * > {"id": "0", "request": "api/addWorkspace", "uri": "file:///path/to/project"}
  * > {"id": "1", "request": "api/addUri", "uri": "foo.flix", "src": "def main(): Unit \ IO = println(\"Hello World\")"}
  * > {"id": "2", "request": "lsp/check"}
  * > {"id": "3", "request": "lsp/hover", "uri": "foo.flix", "position": {"line": 1, "character": 25}}
  *
  * The NPM package "wscat" is useful for experimenting with these commands from the shell.
  *
  * NB: All errors must be printed to std err.
  */
class VSCodeLspServer(port: Int, o: Options) extends WebSocketServer(new InetSocketAddress("localhost", port)) {

  /**
    * The maximum acceptable latency -- in nanoseconds -- before a request is considered slow.
    */
  private val MaxLatencyNS: Long = 100_000_000 // 100ms

  /**
    * The custom date format to use for logging.
    */
  private val DateFormat: String = "yyyy-MM-dd HH:mm:ss"

  /**
    * The project served by this server.
    */
  private val project: LspProject = new LspProject(o)

  /**
    * The current AST root. The root is null until the source code is compiled.
    */
  private var root: Root = TypedAst.empty

  /**
    * The current compilation errors.
    */
  private var currentErrors: List[CompilationMessage] = Nil

  /**
    * Invoked when the server is started.
    */
  override def onStart(): Unit = {
    Console.println(s"Listen on '$getAddress'.")
  }

  /**
    * Invoked when a client connects.
    */
  override def onOpen(ws: WebSocket, ch: ClientHandshake): Unit = {
    /* nop */
    Console.println(s"Client at '${ws.getRemoteSocketAddress}' connected.")
  }

  /**
    * Invoked when a client disconnects.
    */
  override def onClose(ws: WebSocket, i: Int, s: String, b: Boolean): Unit = {
    Console.println(s"Client at '${ws.getRemoteSocketAddress}' disconnected.")
  }

  /**
    * Invoked when a client sends a message.
    */
  override def onMessage(ws: WebSocket, data: String): Unit = try {
    parseRequest(data) match {
      case Ok(request) =>
        val t = System.nanoTime()
        val result = processRequest(request)(ws, root)
        if (ws.isOpen) {
          val jsonCompact = JsonMethods.compact(JsonMethods.render(result))
          // val jsonPretty = JsonMethods.pretty(JsonMethods.render(result))

          val e = System.nanoTime() - t
          if (e > MaxLatencyNS) {
            CompilerLog.log(s"Slow request: '${request.getClass.getSimpleName}' took ${e / 1_000_000} ms.")
          }
          ws.send(jsonCompact)
        }
      case Err(msg) =>
        log(msg)(ws)
        answerUnparsable(data, msg)(ws)
    }
  } catch {
    case ex: Throwable =>
      // We try to scream everywhere to ensure the message is shown.
      CrashHandler.handleCrash(ex)(project.compiler)
      ex.printStackTrace(System.out)
      ex.printStackTrace(System.err)
  }

  /**
    * Answers the message `data`, which could not be parsed as a request, with the error `msg`.
    *
    * A client may wait for a response to every message it sends, so a message it can match a
    * response against is answered even when it is not a request the server knows. A message without
    * an id is only logged: a response to it could not be matched against anything.
    */
  private def answerUnparsable(data: String, msg: String)(implicit ws: WebSocket): Unit = requestId(data) match {
    case Some(id) if ws.isOpen =>
      val result: JValue = ("id" -> id) ~ ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> msg)
      ws.send(JsonMethods.compact(JsonMethods.render(result)))
    case _ => // nop
  }

  /**
    * Returns the id of the message `data`, if it is JSON with an id.
    */
  private def requestId(data: String): Option[String] = try {
    parse(data) \ "id" match {
      case JString(id) => Some(id)
      case _ => None
    }
  } catch {
    case _: ParseException => None
  }

  /**
    * Invoked when an error occurs.
    */
  override def onError(ws: WebSocket, e: Exception): Unit = {
    // Nop - Keep LanguageServer alive.
  }

  /**
    * Parse the request.
    */
  private def parseRequest(s: String): Result[Request, String] = try {
    // Parse the string `s` into a json value.
    val json = parse(s)

    // Determine the type of request.
    json \\ "request" match {
      case JString("api/addWorkspace") => Request.parseAddWorkspace(json)
      case JString("api/addUri") => Request.parseAddUri(json)
      case JString("api/remUri") => Request.parseRemUri(json)
      case JString("api/addPkg") => Request.parseAddPkg(json)
      case JString("api/remPkg") => Request.parseRemPkg(json)
      case JString("api/addJar") => Request.parseAddJar(json)
      case JString("api/remJar") => Request.parseRemJar(json)
      case JString("api/version") => Request.parseVersion(json)
      case JString("api/restart") => Request.parseRestart(json)
      case JString("api/shutdown") => Request.parseShutdown(json)
      case JString("api/disconnect") => Request.parseDisconnect(json)

      case JString("lsp/check") => Request.parseCheck(json)
      case JString("lsp/codelens") => Request.parseCodelens(json)
      case JString("lsp/complete") => Request.parseComplete(json)
      case JString("lsp/highlight") => Request.parseHighlight(json)
      case JString("lsp/hover") => Request.parseHover(json)
      case JString("lsp/goto") => Request.parseGoto(json)
      case JString("lsp/implementation") => Request.parseImplementation(json)
      case JString("lsp/rename") => Request.parseRename(json)
      case JString("lsp/documentSymbols") => Request.parseDocumentSymbols(json)
      case JString("lsp/workspaceSymbols") => Request.parseWorkspaceSymbols(json)
      case JString("lsp/uses") => Request.parseUses(json)
      case JString("lsp/semanticTokens") => Request.parseSemanticTokens(json)
      case JString("lsp/signature") => Request.parseSignature(json)
      case JString("lsp/inlayHints") => Request.parseInlayHint(json)
      case JString("lsp/showAst") => Request.parseShowAst(json)
      case JString("lsp/codeAction") => Request.parseCodeAction(json)
      case JString("lsp/formatting") => Request.parseFormatting(json)
      case JString("lsp/foldingRange") => Request.parseFoldingRange(json)

      // The name of the request is reported on its own: the message it came in is answered with
      // this text, and a message may carry a whole source file or package.
      case JString(request) => Err(s"Unsupported request: '$request'.")
      case _ => Err("Malformed request. The 'request' field is missing or is not a string.")
    }
  } catch {
    case ex: ParseException => Err(s"Malformed request. Unable to parse JSON: '${ex.getMessage}'.")
  }

  /**
    * Process the request.
    */
  private def processRequest(request: Request)(implicit ws: WebSocket, root: Root): JValue = request match {

    case Request.AddWorkspace(id, uri) =>
      ClientUri.toPath(uri) match {
        case Some(path) =>
          project.addWorkspace(path)
          ("id" -> id) ~ ("status" -> ResponseStatus.Success)
        case None =>
          ("id" -> id) ~ ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> s"The uri '$uri' does not denote a directory.")
      }

    case Request.AddUri(id, name, src) =>
      project.addSource(name, src)
      ("id" -> id) ~ ("status" -> ResponseStatus.Success)

    case Request.RemUri(id, name) =>
      project.remSource(name)
      ("id" -> id) ~ ("status" -> ResponseStatus.Success)

    case Request.AddPkg(id, _) => processDependencyChange(id)

    case Request.RemPkg(id, _) => processDependencyChange(id)

    case Request.AddJar(id, _) => processDependencyChange(id)

    case Request.RemJar(id, _) => processDependencyChange(id)

    case Request.Version(id) => processVersion(id)

    case Request.Restart(id) => processRestart(id)

    case Request.Shutdown(_) => processShutdown()

    case Request.Disconnect(_) => processDisconnect()

    case Request.Check(id) => processCheck(id)

    case Request.Codelens(id, name) =>
      ("id" -> id) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> JArray(CodeLensProvider.processCodeLens(name)(root).map(_.toJSON)))

    case Request.Complete(id, name, pos) =>
      // Find the source of the given URI (which should always exist).
      val completions = CompletionProvider
        .getCompletions(name, pos, currentErrors)(root, project.compiler)
        .map(_.toCompletionItem(project.compiler))
      val completionList = CompletionList(isIncomplete = true, completions)
      ("id" -> id) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> completionList.toJSON)

    case Request.Highlight(id, name, pos) =>
      val highlights = HighlightProvider.processHighlight(name, pos)(root)
      if (highlights.isEmpty)
        ("id" -> id) ~ ("status" -> ResponseStatus.InvalidRequest) ~ ("result" -> "Nothing found for this highlight.")
      else
        ("id" -> id) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> JArray(highlights.map(_.toJSON).toList))

    case Request.Hover(id, name, pos) =>
      HoverProvider.processHover(name, pos)(root, project.compiler) match {
        case Some(hover) => ("id" -> id) ~ hover.toJSON
        case None => ("id" -> id) ~ ("status" -> ResponseStatus.InvalidRequest) ~ ("result" -> "Nothing found for this hover.")
      }

    case Request.Goto(id, name, pos) =>
      GotoProvider.processGoto(name, pos)(root) match {
        case Some(location) => ("id" -> id) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> location.toJSON)
        case None => ("id" -> id) ~ ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> s"Nothing found in '$name' at '$pos'.")
      }

    case Request.Implementation(id, name, pos) =>
      ("id" -> id) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> ImplementationProvider.processImplementation(name, pos)(root).map(_.toJSON))

    case Request.Rename(id, newName, name, pos) =>
      RenameProvider.processRename(newName, name, pos)(root) match {
        case Some(rename) => ("id" -> id) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> rename.toJSON)
        case None => ("id" -> id) ~ ("status" -> ResponseStatus.InvalidRequest) ~ ("result" -> "Nothing found for this rename.")
      }

    case Request.DocumentSymbols(id, name) =>
      ("id" -> id) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> SymbolProvider.processDocumentSymbols(name)(root).map(_.toJSON))

    case Request.WorkspaceSymbols(id, query) =>
      ("id" -> id) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> SymbolProvider.processWorkspaceSymbols(query)(root).map(_.toJSON))

    case Request.Uses(id, name, pos) =>
      ("id" -> id) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> FindReferencesProvider.findRefs(name, pos)(root).map(_.toJSON))

    case Request.SemanticTokens(id, name) =>
      ("id" -> id) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> ("data" -> SemanticTokensProvider.provideSemanticTokens(name)(root)))

    case Request.Signature(id, name, pos) =>
      SignatureHelpProvider.provideSignatureHelp(name, pos)(root, project.compiler) match {
        case Some(signature) => ("id" -> id) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> signature.toJSON)
        case None => ("id" -> id) ~ ("status" -> ResponseStatus.InvalidRequest) ~ ("result" -> "Nothing found for this signature.")
      }

    case Request.InlayHint(id, name, range) =>
      ("id" -> id) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> InlayHintProvider.getInlayHints(name, range, currentErrors).map(_.toJSON))

    case Request.ShowAst(id) =>
      ("id" -> id) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> ("path" -> ShowAstProvider.showAst()(project.compiler).toAbsolutePath.toString))

    case Request.CodeAction(id, name, range, _) =>
      ("id" -> id) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> CodeActionProvider.getCodeActions(name, range, currentErrors)(root, project.compiler).map(_.toJSON))

    case Request.Formatting(id, name, options) =>
      val edits = FormattingProvider.formatDocument(name, options)(project.compiler).map(_.toJSON)
      ("id" -> id) ~ ("uri" -> ClientUri.fromSourceName(name)) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> JArray(edits))

    case Request.FoldingRange(id, name) =>
      ("id" -> id) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> JArray(FoldingRangeProvider.getFoldingRanges(name)(root).map(_.toJSON)))

  }

  /**
    * Processes a request that reports a change to the packages or JARs of the project.
    *
    * The dependencies of the project are those its manifest declares, so the change itself is
    * ignored: it only means that the project must be loaded again at the next check.
    */
  private def processDependencyChange(requestId: String): JValue = {
    project.markDependenciesChanged()
    ("id" -> requestId) ~ ("status" -> ResponseStatus.Success)
  }

  /**
    * Processes a restart request: loads the project again and starts over with a fresh compiler.
    */
  private def processRestart(requestId: String): JValue = project.restart() match {
    case None =>
      ("id" -> requestId) ~ ("status" -> ResponseStatus.Success)
    case Some(err) =>
      ("id" -> requestId) ~ ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> err.message(NoFormatter))
  }

  /**
    * Processes a validate request.
    */
  private def processCheck(requestId: String): JValue = {
    // Measure elapsed time.
    val t = System.nanoTime()
    try {
      // Run the compiler up to the type checking phase. The project is loaded again first if its
      // packages or JARs changed.
      project.check() match {
        case (Some(r), Nil) =>
          // Case 1: Compilation was successful. Build the reverse index.
          processSuccessfulCheck(requestId, r, List.empty, t)

        case (Some(r), errors) =>
          // Case 2: Compilation had non-critical errors. Build the reverse index.
          processSuccessfulCheck(requestId, r, errors, t)

        case (None, errors) =>
          // Case 3: Compilation failed. Send back the error messages.

          // Update the current errors.
          this.currentErrors = errors

          // Publish diagnostics.
          val results = PublishDiagnosticsParams.fromMessages(currentErrors, None)
          ("id" -> requestId) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> results.map(_.toJSON))
      }
    } catch {
      case ex: Throwable =>
        val reportPath = CrashHandler.handleCrash(ex)(project.compiler)
        ("id" -> requestId) ~
          ("status" -> ResponseStatus.CompilerError) ~
          ("result" -> ("reportPath" -> reportPath.map(_.toString)))
    }
  }

  /**
    * Helper function for [[processCheck]] which handles successful and soft failure compilations.
    */
  private def processSuccessfulCheck(requestId: String, root: Root, errors: List[CompilationMessage], t0: Long): JValue = {
    // Update the root and the errors.
    this.root = root
    this.currentErrors = errors

    // Compute elapsed time.
    val e = System.nanoTime() - t0

    // Print query time.
    // println(s"lsp/check: ${e / 1_000_000}ms")

    // Compute Code Quality hints.
    val codeHints = CodeHinter.run(project.sourceNames)(root)

    // Determine the status based on whether there are errors.
    // Merge by URI so that errors and code hints for the same file are combined into one entry rather than
    // published separately (each entry replaces previous diagnostics for that URI in the LSP protocol).
    val results = PublishDiagnosticsParams.merge(
      PublishDiagnosticsParams.fromMessages(currentErrors, Some(root)) :::
      PublishDiagnosticsParams.fromCodeHints(codeHints)
    )
    ("id" -> requestId) ~ ("status" -> ResponseStatus.Success) ~ ("time" -> e) ~ ("result" -> results.map(_.toJSON))
  }

  /**
    * Processes a shutdown request.
    */
  private def processShutdown(): Nothing = {
    project.close()
    System.exit(0)
    throw null // unreachable
  }

  /**
    * Processes a disconnection request.
    */
  private def processDisconnect()(implicit ws: WebSocket): JValue = {
    val code = 1013 // 'Try again later'
    ws.closeConnection(code, "Simulating disconnection...")
    JNothing
  }

  /**
    * Processes the version request.
    */
  private def processVersion(requestId: String): JValue = {
    val major = Version.CurrentVersion.major
    val minor = Version.CurrentVersion.minor
    val revision = Version.CurrentVersion.revision
    val version = ("major" -> major) ~ ("minor" -> minor) ~ ("revision" -> revision)
    ("id" -> requestId) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> version)
  }

  /**
    * Logs the given message `msg` along with information about the connection `ws`.
    */
  private def log(msg: String)(implicit ws: WebSocket): Unit = {
    val dateFormat = new SimpleDateFormat(DateFormat)
    val datePart = dateFormat.format(new Date())
    val clientPart = if (ws == null) "n/a" else ws.getRemoteSocketAddress
    Console.err.println(s"[$datePart] [$clientPart]: $msg")
  }

}
