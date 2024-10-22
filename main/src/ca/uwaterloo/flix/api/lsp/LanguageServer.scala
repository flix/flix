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
import ca.uwaterloo.flix.api.{CrashHandler, Flix, Version}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.phase.extra.CodeHinter
import ca.uwaterloo.flix.util.Formatter.NoFormatter
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.*
import ca.uwaterloo.flix.util.collection.Chain
import org.java_websocket.WebSocket
import org.java_websocket.handshake.ClientHandshake
import org.java_websocket.server.WebSocketServer
import org.json4s.JsonAST.{JString, JValue}
import org.json4s.JsonDSL.*
import org.json4s.ParserUtil.ParseException
import org.json4s.*
import org.json4s.native.JsonMethods
import org.json4s.native.JsonMethods.parse

import java.io.ByteArrayInputStream
import java.net.{InetSocketAddress, URI}
import java.nio.charset.Charset
import java.nio.file.Path
import java.text.SimpleDateFormat
import java.util.Date
import java.util.concurrent.{ExecutorService, Executors, Future}
import java.util.zip.ZipInputStream
import scala.collection.mutable

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
  * > {"id": "1", "request": "api/addUri", "uri": "foo.flix", "src": "def main(): Unit \ IO = println(\"Hello World\")"}
  * > {"id": "2", "request": "lsp/check"}
  * > {"id": "3", "request": "lsp/hover", "uri": "foo.flix", "position": {"line": 1, "character": 25}}
  *
  * The NPM package "wscat" is useful for experimenting with these commands from the shell.
  *
  * NB: All errors must be printed to std err.
  */
class LanguageServer(port: Int, o: Options) extends WebSocketServer(new InetSocketAddress("localhost", port)) {

  /**
    * The custom date format to use for logging.
    */
  private val DateFormat: String = "yyyy-MM-dd HH:mm:ss"

  /**
    * The Flix instance (the same instance is used for incremental compilation).
    */
  private val flix: Flix = new Flix().setFormatter(NoFormatter).setOptions(o)

  /**
    * A map from source URIs to source code.
    */
  private val sources: mutable.Map[String, String] = mutable.Map.empty

  /**
    * The current AST root. The root is null until the source code is compiled.
    */
  private var root: Root = TypedAst.empty

  /**
    * The current reverse index. The index is empty until the source code is compiled.
    *
    * Note: The index is updated *asynchronously* by a different thread, hence:
    *
    * - The field must volatile because it is modified by a different thread.
    * - The index may not always reflect the very latest version of the program.
    */
  @volatile
  private var index: Index = Index.empty

  /**
    * A thread pool, with a single thread, which we use to execute indexing operations.
    */
  private val indexingPool: ExecutorService = Executors.newFixedThreadPool(1)

  /**
    * A (possibly-null) volatile reference to a future that represents the latest indexing operation.
    *
    * Note: Multiple indexing operations may be pending in the thread pool. This field points to the latest submitted.
    */
  private var indexingFuture: Future[?] = _

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
        val result = processRequest(request)(ws)
        if (ws.isOpen) {
          val jsonCompact = JsonMethods.compact(JsonMethods.render(result))
          // val jsonPretty = JsonMethods.pretty(JsonMethods.render(result))
          ws.send(jsonCompact)
        }
      case Err(msg) => log(msg)(ws)
    }
  } catch {
    case ex: Throwable =>
      // We try to scream everywhere to ensure the message is shown.
      CrashHandler.handleCrash(ex)(flix)
      ex.printStackTrace(System.out)
      ex.printStackTrace(System.err)
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
      case JString("api/addUri") => Request.parseAddUri(json)
      case JString("api/remUri") => Request.parseRemUri(json)
      case JString("api/addPkg") => Request.parseAddPkg(json)
      case JString("api/remPkg") => Request.parseRemPkg(json)
      case JString("api/addJar") => Request.parseAddJar(json)
      case JString("api/remJar") => Request.parseRemJar(json)
      case JString("api/version") => Request.parseVersion(json)
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
      case JString("lsp/inlayHints") => Request.parseInlayHint(json)
      case JString("lsp/showAst") => Request.parseShowAst(json)
      case JString("lsp/codeAction") => Request.parseCodeAction(json)

      case s => Err(s"Unsupported request: '$s'.")
    }
  } catch {
    case ex: ParseException => Err(s"Malformed request. Unable to parse JSON: '${ex.getMessage}'.")
  }

  /**
    * Add the given source code to the compiler
    */
  private def addSourceCode(uri: String, src: String): Unit = {
    flix.addSourceCode(uri, src)(SecurityContext.AllPermissions) // TODO
    sources += (uri -> src)
  }

  /**
    * Remove the source code associated with the given uri from the compiler
    */
  private def remSourceCode(uri: String): Unit = {
    flix.remSourceCode(uri)
    sources -= uri
  }

  /**
    * Process the request.
    */
  private def processRequest(request: Request)(implicit ws: WebSocket): JValue = request match {

    case Request.AddUri(id, uri, src) =>
      addSourceCode(uri, src)
      ("id" -> id) ~ ("status" -> ResponseStatus.Success)

    case Request.RemUri(id, uri) =>
      remSourceCode(uri)
      ("id" -> id) ~ ("status" -> ResponseStatus.Success)

    case Request.AddPkg(id, uri, data) =>
      // TODO: Possibly move into Input class?
      val inputStream = new ZipInputStream(new ByteArrayInputStream(data))
      var entry = inputStream.getNextEntry
      while (entry != null) {
        val name = entry.getName
        if (name.endsWith(".flix")) {
          val bytes = StreamOps.readAllBytes(inputStream)
          val src = new String(bytes, Charset.forName("UTF-8"))
          addSourceCode(s"$uri/$name", src)
        }
        entry = inputStream.getNextEntry
      }
      inputStream.close()

      ("id" -> id) ~ ("status" -> ResponseStatus.Success)

    case Request.RemPkg(id, uri) =>
      // clone is necessary because `remSourceCode` modifies `sources`
      for ((file, _) <- sources.clone()
           if file.startsWith(uri)) {
        remSourceCode(file)
      }
      ("id" -> id) ~ ("status" -> ResponseStatus.Success)

    case Request.AddJar(id, uri) =>
      val path = Path.of(new URI(uri))
      flix.addJar(path)
      ("id" -> id) ~ ("status" -> ResponseStatus.Success)

    case Request.RemJar(id, _) =>
      // No-op (there is no easy way to remove a Jar from the JVM)
      ("id" -> id) ~ ("status" -> ResponseStatus.Success)

    case Request.Version(id) => processVersion(id)

    case Request.Shutdown(_) => processShutdown()

    case Request.Disconnect(_) => processDisconnect()

    case Request.Check(id) => processCheck(id)

    case Request.Codelens(id, uri) =>
      ("id" -> id) ~ CodeLensProvider.processCodeLens(uri)(root)

    case Request.Complete(id, uri, pos) =>
      // Find the source of the given URI (which should always exist).
      val sourceCode = sources(uri)
      ("id" -> id) ~ CompletionProvider.autoComplete(uri, pos, sourceCode, currentErrors)(flix, index, root)

    case Request.Highlight(id, uri, pos) =>
      ("id" -> id) ~ HighlightProvider.processHighlight(uri, pos)(index, root)

    case Request.Hover(id, uri, pos) =>
      ("id" -> id) ~ HoverProvider.processHover(uri, pos)(root, flix)

    case Request.Goto(id, uri, pos) =>
      ("id" -> id) ~ GotoProvider.processGoto(uri, pos)(index, root)

    case Request.Implementation(id, uri, pos) =>
      ("id" -> id) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> ImplementationProvider.processImplementation(uri, pos)(root).map(_.toJSON))

    case Request.Rename(id, newName, uri, pos) =>
      synchronouslyAwaitIndex()
      ("id" -> id) ~ RenameProvider.processRename(newName, uri, pos)(index, root)

    case Request.DocumentSymbols(id, uri) =>
      ("id" -> id) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> SymbolProvider.processDocumentSymbols(uri)(root).map(_.toJSON))

    case Request.WorkspaceSymbols(id, query) =>
      ("id" -> id) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> SymbolProvider.processWorkspaceSymbols(query)(root).map(_.toJSON))

    case Request.Uses(id, uri, pos) =>
      ("id" -> id) ~ FindReferencesProvider.findRefs(uri, pos)(index, root)

    case Request.SemanticTokens(id, uri) =>
      ("id" -> id) ~ ("status" -> ResponseStatus.Success) ~ SemanticTokensProvider.provideSemanticTokens(uri)(index, root)

    case Request.InlayHint(id, _, _) =>
      // InlayHints disabled due to poor ergonomics.
      // ("id" -> id) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> InlayHintProvider.processInlayHints(uri, range)(index, flix).map(_.toJSON))
      ("id" -> id) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> Nil)

    case Request.ShowAst(id) =>
      ("id" -> id) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> ShowAstProvider.showAst()(flix))

    case Request.CodeAction(id, uri, range, context) =>
      ("id" -> id) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> CodeActionProvider.getCodeActions(uri, range, context, currentErrors)(index, root, flix).map(_.toJSON))

  }

  /**
    * Processes a validate request.
    */
  private def processCheck(requestId: String): JValue = {

    // Measure elapsed time.
    val t = System.nanoTime()
    try {
      // Run the compiler up to the type checking phase.
      flix.check().toSoftResult match {
        case Result.Ok((root, Chain.empty)) =>
          // Case 1: Compilation was successful. Build the reverse index.
          processSuccessfulCheck(requestId, root, Chain.empty, flix.options.explain, t)

        case Result.Ok((root, errors)) =>
          // Case 2: Compilation had non-critical errors. Build the reverse index.
          processSuccessfulCheck(requestId, root, errors, flix.options.explain, t)

        case Result.Err(errors) =>
          // Case 3: Compilation failed. Send back the error messages.

          // Update the current errors.
          this.currentErrors = errors.toList

          // Publish diagnostics.
          val results = PublishDiagnosticsParams.fromMessages(currentErrors, flix.options.explain)
          ("id" -> requestId) ~ ("status" -> ResponseStatus.Success) ~ ("result" -> results.map(_.toJSON))
      }
    } catch {
      case ex: Throwable =>
        val reportPath = CrashHandler.handleCrash(ex)(flix)
        ("id" -> requestId) ~
          ("status" -> ResponseStatus.CompilerError) ~
          ("result" -> ("reportPath" -> reportPath.map(_.toString)))
    }
  }

  /**
    * Helper function for [[processCheck]] which handles successful and soft failure compilations.
    */
  private def processSuccessfulCheck(requestId: String, root: Root, errors: Chain[CompilationMessage], explain: Boolean, t0: Long): JValue = {
    // Update the root and the errors.
    this.root = root
    this.currentErrors = errors.toList

    // Asynchronously compute the reverse index.
    asynchronouslyUpdateIndex(root)

    // Compute elapsed time.
    val e = System.nanoTime() - t0

    // Print query time.
    // println(s"lsp/check: ${e / 1_000_000}ms")

    // Compute Code Quality hints.
    val codeHints = CodeHinter.run(root, sources.keySet.toSet)(flix, index)

    // Determine the status based on whether there are errors.
    val results = PublishDiagnosticsParams.fromMessages(currentErrors, explain) ::: PublishDiagnosticsParams.fromCodeHints(codeHints)
    ("id" -> requestId) ~ ("status" -> ResponseStatus.Success) ~ ("time" -> e) ~ ("result" -> results.map(_.toJSON))
  }

  /**
    * Asynchronously compute the reverse index using the thread pool.
    */
  private def asynchronouslyUpdateIndex(root: Root): Unit = {
    this.indexingFuture = indexingPool.submit(new Runnable {
      override def run(): Unit = {
        LanguageServer.this.index = Indexer.visitRoot(root)
      }
    })
  }

  /**
    * Synchronously wait for the most recent indexing operation to complete.
    *
    * This function is used to ensure the index is up-to-date before certain operations, e.g. rename.
    */
  private def synchronouslyAwaitIndex(): Unit = {
    if (indexingFuture != null) indexingFuture.get()
  }

  /**
    * Processes a shutdown request.
    */
  private def processShutdown(): Nothing = {
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
