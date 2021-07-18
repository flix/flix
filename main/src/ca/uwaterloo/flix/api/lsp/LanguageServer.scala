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

import ca.uwaterloo.flix.api.lsp.provider._
import ca.uwaterloo.flix.api.{Flix, Version}
import ca.uwaterloo.flix.language.ast.Ast.Source
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.debug._
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation.{Failure, Success}
import ca.uwaterloo.flix.util.vt.TerminalContext
import ca.uwaterloo.flix.util.{InternalCompilerException, InternalRuntimeException, Options, Result, StreamOps}
import org.java_websocket.WebSocket
import org.java_websocket.handshake.ClientHandshake
import org.java_websocket.server.WebSocketServer
import org.json4s.JsonAST.{JArray, JString, JValue}
import org.json4s.JsonDSL._
import org.json4s.ParserUtil.ParseException
import org.json4s._
import org.json4s.native.JsonMethods
import org.json4s.native.JsonMethods.parse

import java.io.ByteArrayInputStream
import java.net.InetSocketAddress
import java.nio.charset.Charset
import java.text.SimpleDateFormat
import java.util.Date
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
  * > {"id": "1", "request": "api/addUri", "uri": "foo.flix", "src": "def main(_: Array[String]): Int32 & Impure = println(\"Hello World\"); 0"}
  * > {"id": "2", "request": "lsp/check"}
  * > {"id": "3", "request": "lsp/hover", "uri": "foo.flix", "position": {"line": 1, "character": 25}}
  *
  * The NPM package "wscat" is useful for experimenting with these commands from the shell.
  *
  * NB: All errors must be printed to std err.
  */
class LanguageServer(port: Int) extends WebSocketServer(new InetSocketAddress("localhost", port)) {

  /**
    * The custom date format to use for logging.
    */
  val DateFormat: String = "yyyy-MM-dd HH:mm:ss"

  /**
    * The audience used for formatting.
    */
  implicit val DefaultAudience: Audience = Audience.External

  /**
    * The terminal context used for formatting.
    */
  implicit val DefaultTerminalContext: TerminalContext = TerminalContext.NoTerminal

  /**
    * The default compiler options.
    */
  val DefaultOptions: Options = Options.Default

  /**
    * A map from source URIs to source code.
    */
  val sources: mutable.Map[String, String] = mutable.Map.empty

  /**
    * A map from package URIs to source code.
    */
  val packages: mutable.Map[String, List[String]] = mutable.Map.empty

  /**
    * The current AST root. The root is null until the source code is compiled.
    */
  var root: Root = _

  /**
    * The current reverse index. The index is empty until the source code is compiled.
    */
  var index: Index = Index.empty

  /**
    * Invoked when the server is started.
    */
  override def onStart(): Unit = {
    Console.println(s"LSP listening on: '$getAddress'.")
  }

  /**
    * Invoked when a client connects.
    */
  override def onOpen(ws: WebSocket, ch: ClientHandshake): Unit = {
    /* nop */
  }

  /**
    * Invoked when a client disconnects.
    */
  override def onClose(ws: WebSocket, i: Int, s: String, b: Boolean): Unit = {
    /* nop */
  }

  /**
    * Invoked when a client sends a message.
    */
  override def onMessage(ws: WebSocket, data: String): Unit = try {
    parseRequest(data)(ws) match {
      case Ok(request) =>
        val result = processRequest(request)(ws)
        val jsonCompact = JsonMethods.compact(JsonMethods.render(result))
        val jsonPretty = JsonMethods.pretty(JsonMethods.render(result))
        ws.send(jsonPretty)
      case Err(msg) => log(msg)(ws)
    }
  } catch {
    case t: InternalCompilerException =>
      t.printStackTrace(System.err)
      System.exit(1)
    case t: InternalRuntimeException =>
      t.printStackTrace(System.err)
      System.exit(2)
    case t: Throwable =>
      t.printStackTrace(System.err)
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
  private def parseRequest(s: String)(implicit ws: WebSocket): Result[Request, String] = try {
    // Parse the string `s` into a json value.
    val json = parse(s)

    // Determine the type of request.
    json \\ "request" match {
      case JString("api/addUri") => Request.parseAddUri(json)
      case JString("api/remUri") => Request.parseRemUri(json)
      case JString("api/addPkg") => Request.parseAddPkg(json)
      case JString("api/remPkg") => Request.parseRemPkg(json)
      case JString("api/version") => Request.parseVersion(json)
      case JString("api/shutdown") => Request.parseShutdown(json)

      case JString("lsp/check") => Request.parseCheck(json)
      case JString("lsp/codelens") => Request.parseCodelens(json)
      case JString("lsp/complete") => Request.parseComplete(json)
      case JString("lsp/highlight") => Request.parseHighlight(json)
      case JString("lsp/hover") => Request.parseHover(json)
      case JString("lsp/goto") => Request.parseGoto(json)
      case JString("lsp/rename") => Request.parseRename(json)
      case JString("lsp/uses") => Request.parseUses(json)
      case JString("lsp/symbols") => Request.parseSymbol(json)

      case s => Err(s"Unsupported request: '$s'.")
    }
  } catch {
    case ex: ParseException => Err(s"Malformed request. Unable to parse JSON: '${ex.getMessage}'.")
  }

  /**
    * Process the request.
    */
  private def processRequest(request: Request)(implicit ws: WebSocket): JValue = request match {
    case Request.AddUri(id, uri, src) =>
      sources += (uri -> src)
      ("id" -> id) ~ ("status" -> "success")

    case Request.RemUri(id, uri) =>
      sources -= uri
      ("id" -> id) ~ ("status" -> "success")

    case Request.AddPkg(id, uri, data) =>
      // TODO: Possibly move into Input class?
      val inputStream = new ZipInputStream(new ByteArrayInputStream(data))
      val items = mutable.ListBuffer.empty[String]
      var entry = inputStream.getNextEntry
      while (entry != null) {
        val name = entry.getName
        if (name.endsWith(".flix")) {
          val bytes = StreamOps.readAllBytes(inputStream)
          val src = new String(bytes, Charset.forName("UTF-8"))
          items += src
        }
        entry = inputStream.getNextEntry
      }
      inputStream.close()

      packages += (uri -> items.toList)
      ("id" -> id) ~ ("status" -> "success")

    case Request.RemPkg(id, uri) =>
      packages -= uri
      ("id" -> id) ~ ("status" -> "success")

    case Request.Version(id) => processVersion(id)

    case Request.Shutdown(id) => processShutdown()

    case Request.Check(id) => processCheck(id)

    case Request.Codelens(id, uri) => processCodelens(id, uri)

    case Request.Complete(id, uri, pos) => processComplete(id, uri, pos)

    case Request.Symbol(id, uri, pos) => processSymbol(id, uri, pos)

    case Request.Highlight(id, uri, pos) =>
      ("id" -> id) ~ HighlightProvider.processHighlight(uri, pos)(index, root)

    case Request.Hover(id, uri, pos) =>
      ("id" -> id) ~ HoverProvider.processHover(uri, pos)(index, root)

    case Request.Goto(id, uri, pos) =>
      ("id" -> id) ~ GotoProvider.processGoto(uri, pos)(index, root)

    case Request.Rename(id, newName, uri, pos) =>
      ("id" -> id) ~ RenameProvider.processRename(newName, uri, pos)(index, root)

    case Request.Uses(id, uri, pos) =>
      ("id" -> id) ~ FindReferencesProvider.findRefs(uri, pos)(index, root)

  }

  /**
    * Processes a validate request.
    */
  private def processCheck(requestId: String)(implicit ws: WebSocket): JValue = {
    // Configure the Flix compiler.
    val flix = new Flix()

    // Add sources.
    for ((uri, source) <- sources) {
      flix.addInput(uri, source)
    }

    // Add sources from packages.
    for ((uri, items) <- packages) {
      for (src <- items) {
        flix.addInput(uri, src)
      }
    }

    // Measure elapsed time.
    val t = System.nanoTime()
    try {
      // Run the compiler up to the type checking phase.
      flix.check() match {
        case Success(root) =>
          // Case 1: Compilation was successful. Build the reverse index.
          this.root = root
          this.index = Indexer.visitRoot(root)

          // Compute elapsed time.
          val e = System.nanoTime() - t

          // Send back a status message.
          ("id" -> requestId) ~ ("status" -> "success") ~ ("time" -> e)

        case Failure(errors) =>
          // Case 2: Compilation failed. Send back the error messages.
          val results = PublishDiagnosticsParams.from(errors)
          ("id" -> requestId) ~ ("status" -> "failure") ~ ("result" -> results.map(_.toJSON))
      }
    } catch {
      case t: Throwable =>
        t.printStackTrace(System.err)
        ("id" -> requestId) ~ ("status" -> "failure")
    }
  }

  /**
    * Processes a codelens request.
    */
  private def processCodelens(requestId: String, uri: String)(implicit ws: WebSocket): JValue = {
    /**
      * Returns a code lens for main (if present).
      */
    def mkCodeLensForMain(): List[CodeLens] = {
      if (root == null) {
        return Nil
      }

      val main = Symbol.Main
      root.defs.get(main) match {
        case Some(defn) if matchesUri(uri, defn.spec.loc) =>
          val runMain = Command("Run", "flix.runMain", Nil)
          val runMainWithArgs = Command("Run with args...", "flix.runMainWithArgs", Nil)
          val runMainNewTerminal = Command("Run (in new terminal)", "flix.runMainNewTerminal", Nil)
          val runMainNewTerminalWithArgs = Command("Run with args... (in new terminal)", "flix.runMainNewTerminalWithArgs", Nil)
          val loc = defn.sym.loc
          List(
            CodeLens(Range.from(loc), Some(runMain)),
            CodeLens(Range.from(loc), Some(runMainWithArgs)),
            CodeLens(Range.from(loc), Some(runMainNewTerminal)),
            CodeLens(Range.from(loc), Some(runMainNewTerminalWithArgs))
          )
        case _ => Nil
      }
    }

    /**
      * Returns a list of code lenses for the unit tests in the program.
      */
    def mkCodeLensesForUnitTests(): List[CodeLens] = {
      // Case 1: No root. Return immediately.
      if (root == null) {
        return Nil
      }

      val result = mutable.ListBuffer.empty[CodeLens]
      for ((sym, defn) <- root.defs) {
        if (matchesUri(uri, defn.spec.loc) && defn.spec.ann.exists(_.name.isInstanceOf[Ast.Annotation.Test])) {
          val loc = defn.sym.loc
          val cmd = Command("Run All Tests", "flix.cmdRunAllTests", Nil)
          result.addOne(CodeLens(Range.from(loc), Some(cmd)))
        }
      }
      result.toList
    }

    //
    // Compute all code lenses.
    //
    val allCodeLenses = mkCodeLensForMain()
    ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> JArray(allCodeLenses.map(_.toJSON)))
  }


  /**
   * Processes a symbol request
   */
  private def processSymbol(requestId: String, uri: String, pos: Position): JValue = {

    // TODO implement the function
    ("id" -> requestId) ~ ("status" -> "success")
  }
  /**
    * Processes a complete request.
    */
  private def processComplete(requestId: String, uri: String, pos: Position)(implicit ws: WebSocket): JValue = {
    val word = for {
      source <- sources.get(uri)
      line <- lineAt(source, pos.line - 1)
      word <- wordAt(line, pos.character - 1)
    } yield word

    val t = System.nanoTime()
    val suggestions = CompleteProvider.autoComplete(uri, pos, word)(index, root)
    // println(s"Found ${suggestions.size} suggestions for '$word' (elapsed: " + ((System.nanoTime() - t) / 1_000_000) + "ms)")

    val result = CompletionList(isIncomplete = true, suggestions)
    ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> result.toJSON)
  }

  /**
    * Optionally returns line number `n` in the string `s`.
    */
  private def lineAt(s: String, n: Int): Option[String] = {
    import java.io.{BufferedReader, StringReader}

    val br = new BufferedReader(new StringReader(s))

    var i = 0
    var line = br.readLine()
    while (line != null && i < n) {
      line = br.readLine()
      i = i + 1
    }
    Option(line)
  }

  /**
    * Optionally returns the word at the given index `n` in the string `s`.
    */
  private def wordAt(s: String, n: Int): Option[String] = {
    def isValidChar(c: Char): Boolean = Character.isLetterOrDigit(c) || c == '.' || c == '/'

    // Bounds Check
    if (!(0 <= n && n <= s.length)) {
      return None
    }

    // Determine if the word is to the left of us, to the right of us, or out of bounds.
    val leftOf = 0 < n && isValidChar(s.charAt(n - 1))
    val rightOf = n < s.length && isValidChar(s.charAt(n))

    val i = (leftOf, rightOf) match {
      case (true, _) => n - 1
      case (_, true) => n
      case _ => return None
    }

    // Compute the beginning of the word.
    var begin = i
    while (0 < begin && isValidChar(s.charAt(begin - 1))) {
      begin = begin - 1
    }

    // Compute the ending of the word.
    var end = i
    while (end < s.length && isValidChar(s.charAt(end))) {
      end = end + 1
    }

    // Return the word.
    Some(s.substring(begin, end))
  }

  /**
    * Processes a shutdown request.
    */
  private def processShutdown()(implicit ws: WebSocket): Nothing = {
    System.exit(0)
    throw null // unreachable
  }

  /**
    * Processes the version request.
    */
  private def processVersion(requestId: String)(implicit ws: WebSocket): JValue = {
    val major = Version.CurrentVersion.major
    val minor = Version.CurrentVersion.minor
    val revision = Version.CurrentVersion.revision
    val version = ("major" -> major) ~ ("minor" -> minor) ~ ("revision" -> revision)
    ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> version)
  }

  /**
    * Returns `true` if the given source location `loc` matches the given `uri`.
    */
  private def matchesUri(uri: String, loc: SourceLocation): Boolean = uri == loc.source.name

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(requestId: String, uri: String, pos: Position): JValue =
    ("id" -> requestId) ~ ("status" -> "failure") ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

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
