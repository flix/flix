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

import java.net.InetSocketAddress
import java.nio.file.Path
import java.text.SimpleDateFormat
import java.util.Date

import ca.uwaterloo.flix.api.{Flix, Version}
import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, Pattern, Root}
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.debug.{Audience, FormatType}
import ca.uwaterloo.flix.tools.{Packager, Tester}
import ca.uwaterloo.flix.tools.Tester.TestResult
import ca.uwaterloo.flix.util.Options
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation.{Failure, Success}
import ca.uwaterloo.flix.util.vt.TerminalContext
import ca.uwaterloo.flix.util.{InternalCompilerException, InternalRuntimeException, Result}
import org.java_websocket.WebSocket
import org.java_websocket.handshake.ClientHandshake
import org.java_websocket.server.WebSocketServer
import org.json4s.ParserUtil.ParseException
import org.json4s.native.JsonMethods
import org.json4s.native.JsonMethods.parse
import org.json4s.JsonAST.{JArray, JString, JValue}
import org.json4s.JsonDSL._
import org.json4s._

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
  * > {"request": "api/addUri", "uri": "foo.flix", "src": "def main(): Int = 123"}
  * < {"status":"success"}
  *
  * > {"request": "lsp/check"}
  * < {"status":"success","time":1749621900}
  *
  * > {"request": "lsp/context", "uri": "foo.flix", "position": {"line": 1, "character": 20}}
  * < {"status":"success","result":{"tpe":"Int32","eff":"true"}}
  *
  * The NPM package "wscat" is useful for experimenting with these commands from the shell.
  */
class LanguageServer(port: Int) extends WebSocketServer(new InetSocketAddress(port)) {

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
    Console.println(s"LSP listening on: ws://localhost:$port")
  }

  /**
    * Invoked when a client connects.
    */
  override def onOpen(ws: WebSocket, ch: ClientHandshake): Unit = {
    log("Client Connected.")(ws)
  }

  /**
    * Invoked when a client disconnects.
    */
  override def onClose(ws: WebSocket, i: Int, s: String, b: Boolean): Unit = {
    log("Client Disconnected.")(ws)
  }

  /**
    * Invoked when a client sends a message.
    */
  override def onMessage(ws: WebSocket, data: String): Unit = {
    // Parse and process request.
    parseRequest(data)(ws) match {
      case Ok(request) =>
        val result = processRequest(request)(ws)
        val jsonCompact = JsonMethods.compact(JsonMethods.render(result))
        val jsonPretty = JsonMethods.pretty(JsonMethods.render(result))
        log("Sending reply: " + jsonCompact)(ws)
        ws.send(jsonPretty)
      case Err(msg) =>
        log(msg)(ws)
        ws.closeConnection(5000, msg)
    }
  }

  /**
    * Invoked when an error occurs.
    */
  override def onError(ws: WebSocket, e: Exception): Unit = e match {
    case ex: InternalCompilerException =>
      log(s"Unexpected error: ${e.getMessage}")(ws)
      e.printStackTrace()
    case ex: InternalRuntimeException =>
      log(s"Unexpected error: ${e.getMessage}")(ws)
      e.printStackTrace()
    case ex => throw ex
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
      case JString("api/shutdown") => Ok(Request.Shutdown)

      case JString("cmd/runBenchmarks") => Request.parseRunBenchmarks(json)
      case JString("cmd/runMain") => Request.parseRunMain(json)
      case JString("cmd/runTests") => Request.parseRunTests(json)

      case JString("lsp/check") => Request.parseCheck(json)
      case JString("lsp/codelens") => Request.parseCodelens(json)
      case JString("lsp/complete") => Request.parseComplete(json)
      case JString("lsp/context") => Request.parseContext(json)
      case JString("lsp/foldingRange") => Request.parseFoldingRange(json)
      case JString("lsp/goto") => Request.parseGoto(json)
      case JString("lsp/symbols") => Request.parseSymbols(json)
      case JString("lsp/uses") => Request.parseUses(json)

      case JString("pkg/benchmark") => Request.parsePackageBenchmark(json)
      case JString("pkg/build") => Request.parsePackageBuild(json)
      case JString("pkg/buildDoc") => Request.parsePackageBuildDoc(json)
      case JString("pkg/buildJar") => Request.parsePackageBuildJar(json)
      case JString("pkg/buildPkg") => Request.parsePackageBuildPkg(json)
      case JString("pkg/init") => Request.parsePackageInit(json)
      case JString("pkg/test") => Request.parsePackageTest(json)

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
      log("Added URI: " + uri)
      sources += (uri -> src)
      ("id" -> id) ~ ("status" -> "success")

    case Request.RemUri(id, uri) =>
      log("Removed URI: " + uri)
      sources -= uri
      ("id" -> id) ~ ("status" -> "success")

    case Request.Shutdown => processShutdown()

    case Request.RunBenchmarks(id) => runBenchmarks(id)
    case Request.RunMain(id) => runMain(id)
    case Request.RunTests(id) => runTests(id)

    case Request.Check(id) => processCheck(id)
    case Request.Codelens(id, uri) => processCodelens(id, uri)
    case Request.Context(id, uri, pos) => processContext(id, uri, pos)
    case Request.Complete(id, uri, pos) => processComplete(id, uri, pos)
    case Request.FoldingRange(id, uri) => processFoldingRange(id, uri)
    case Request.Goto(id, uri, pos) => processGoto(id, uri, pos)
    case Request.Symbols(id, uri) => processSymbols(id, uri)
    case Request.Uses(id, uri, pos) => processUses(id, uri, pos)

    case Request.PackageBenchmark(id, projectRoot) => benchmarkPackage(id, projectRoot)
    case Request.PackageBuild(id, projectRoot) => buildPackage(id, projectRoot)
    case Request.PackageBuildDoc(id, projectRoot) => buildDoc(id, projectRoot)
    case Request.PackageBuildJar(id, projectRoot) => buildJar(id, projectRoot)
    case Request.PackageBuildPkg(id, projectRoot) => buildPkg(id, projectRoot)
    case Request.PackageInit(id, projectRoot) => initPackage(id, projectRoot)
    case Request.PackageTest(id, projectRoot) => testPackage(id, projectRoot)

  }

  /**
    * Processes a validate request.
    */
  private def processCheck(requestId: String)(implicit ws: WebSocket): JValue = {
    // Configure the Flix compiler.
    val flix = new Flix()
    for ((uri, source) <- sources) {
      flix.addInput(uri, source)
    }

    // Measure elapsed time.
    val t = System.nanoTime()

    // Run the compiler up to the type checking phase.
    flix.check() match {
      case Success(root) =>
        // Case 1: Compilation was successful. Build the reverse the reverse index.
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

      val main = Symbol.mkDefnSym("main")
      root.defs.get(main) match {
        case Some(defn) if matchesUri(uri, defn.loc) =>
          val loc = defn.sym.loc
          val cmd = Command("Run Main", "runMain", Nil)
          CodeLens(Range.from(loc), Some(cmd)) :: Nil
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
        if (matchesUri(uri, defn.loc) && defn.ann.exists(_.name.isInstanceOf[Ast.Annotation.Test])) {
          val loc = defn.sym.loc
          val cmd = Command("Run All Tests", "runAllTests", Nil)
          result.addOne(CodeLens(Range.from(loc), Some(cmd)))
        }
      }
      result.toList
    }

    //
    // Compute all code lenses.
    //
    val allCodeLenses = mkCodeLensForMain() ::: mkCodeLensesForUnitTests()
    ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> JArray(allCodeLenses.map(_.toJSON)))
  }

  /**
    * Processes a complete request.
    */
  private def processComplete(requestId: String, uri: String, pos: Position)(implicit ws: WebSocket): JValue = {
    def mkDefaultCompletions(): JValue = {
      val items = List(
        CompletionItem("Hello!", None, None, None, Some(TextEdit(Range(pos, pos), "Hi there!"))),
        CompletionItem("Goodbye!", None, None, None, Some(TextEdit(Range(pos, pos), "Farewell!")))
      )
      ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> items.map(_.toJSON))
    }

    index.query(uri, pos) match {
      case Some(Entity.Exp(exp)) => exp match {
        case Expression.Hole(sym, _, _, _) =>
          val holeCtx = TypedAstOps.holesOf(root)(sym)
          val items = holeCtx.env.map {
            case (sym, tpe) => CompletionItem(sym.text, Some(CompletionItemKind.Variable), Some(FormatType.formatType(tpe)), None, Some(TextEdit(Range(pos, pos), sym.text)))
          }
          ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> items.map(_.toJSON))
        case _ => mkDefaultCompletions
      }
      case _ => mkDefaultCompletions
    }
  }

  /**
    * Processes a type and effect request.
    */
  private def processContext(requestId: String, uri: String, pos: Position)(implicit ws: WebSocket): JValue = {
    index.query(uri, pos) match {
      case Some(Entity.Exp(exp)) =>
        implicit val _ = Audience.External
        val tpe = FormatType.formatType(exp.tpe)
        val eff = FormatType.formatType(exp.eff)
        ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> (("tpe" -> tpe) ~ ("eff" -> eff)))
      case _ =>
        mkNotFound(requestId, uri, pos)
    }
  }

  /**
    * Processes a folding range request.
    */
  private def processFoldingRange(requestId: String, uri: String)(implicit ws: WebSocket): JValue = {
    if (root == null) {
      return ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> JArray(Nil))
    }

    val defsFoldingRanges = root.defs.foldRight(List.empty[FoldingRange]) {
      case ((sym, defn), acc) if matchesUri(uri, defn.loc) => FoldingRange(defn.loc.beginLine, Some(defn.loc.beginCol), defn.loc.endLine, Some(defn.loc.endCol), Some(FoldingRangeKind.Region)) :: acc
      case (_, acc) => acc
    }
    ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> JArray(defsFoldingRanges.map(_.toJSON)))
  }

  /**
    * Processes a goto request.
    */
  private def processGoto(requestId: String, uri: String, pos: Position)(implicit ws: WebSocket): JValue = {
    index.query(uri, pos) match {
      case Some(Entity.Exp(exp)) => exp match {
        case Expression.Def(sym, _, loc) =>
          ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> LocationLink.fromDefSym(sym, root, loc).toJSON)

        case Expression.Var(sym, _, loc) =>
          ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> LocationLink.fromVarSym(sym, loc).toJSON)

        case Expression.Tag(sym, tag, _, _, _, loc) =>
          ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> LocationLink.fromEnumSym(sym, tag, root, loc).toJSON)

        case _ => mkNotFound(requestId, uri, pos)
      }
      case Some(Entity.Pat(pat)) => pat match {
        case Pattern.Var(sym, _, loc) =>
          ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> LocationLink.fromVarSym(sym, loc).toJSON)

        case Pattern.Tag(sym, tag, _, _, loc) =>
          ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> LocationLink.fromEnumSym(sym, tag, root, loc).toJSON)

        case _ => mkNotFound(requestId, uri, pos)
      }
      case _ => mkNotFound(requestId, uri, pos)
    }
  }

  /**
    * Processes a request to run all benchmarks. Re-compiles and runs the program.
    */
  private def runBenchmarks(requestId: String): JValue = {
    // TODO: runBenchmarks
    ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> "failure")
  }

  /**
    * Processes a request to run main. Re-compiles and runs the program.
    */
  private def runMain(requestId: String): JValue = {
    // Configure the Flix compiler.
    val flix = new Flix()
    for ((uri, source) <- sources) {
      flix.addInput(uri, source)
    }

    flix.compile() match {
      case Success(t) => t.getMain match {
        case None =>
          ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> "Compilation successful. No main to run.")
        case Some(main) =>
          val result = main()
          ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> result.toString)
      }
      case Failure(errors) =>
        // Case 2: Compilation failed. Send back the error messages.
        val results = PublishDiagnosticsParams.from(errors)
        ("id" -> requestId) ~ ("status" -> "failure") ~ ("result" -> results.map(_.toJSON))
    }
  }

  /**
    * Processes a request to run all tests. Re-compiles and runs all unit tests.
    */
  private def runTests(requestId: String): JValue = {
    // Configure the Flix compiler.
    val flix = new Flix()
    for ((uri, source) <- sources) {
      flix.addInput(uri, source)
    }

    flix.compile() match {
      case Success(t) =>
        val results: List[JValue] = Tester.test(t).results.map {
          case TestResult.Success(sym, _) =>
            ("name" -> sym.toString) ~ ("location" -> Location.from(sym.loc).toJSON) ~ ("outcome" -> "success")
          case TestResult.Failure(sym, m) =>
            ("name" -> sym.toString) ~ ("location" -> Location.from(sym.loc).toJSON) ~ ("outcome" -> "failure") ~ ("message" -> m)
        }

        ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> JArray(results))
      case Failure(errors) =>
        // Case 2: Compilation failed. Send back the error messages.
        val results = PublishDiagnosticsParams.from(errors)
        ("id" -> requestId) ~ ("status" -> "failure") ~ ("result" -> results.map(_.toJSON))
    }
  }

  /**
    * Processes a request to run all benchmarks in the project.
    */
  private def benchmarkPackage(requestId: String, projectRoot: Path): JValue = {
    Packager.benchmark(projectRoot, DefaultOptions)
    ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> "NotYetImplemented")
  }

  /**
    * Processes a request to build the project.
    */
  private def buildPackage(requestId: String, projectRoot: Path): JValue = {
    Packager.build(projectRoot, DefaultOptions) match {
      case None =>
        ("id" -> requestId) ~ ("status" -> "failure")
      case Some(_) =>
        ("id" -> requestId) ~ ("status" -> "success")
    }
  }

  /**
    * Processes a request to build the documentation.
    */
  private def buildDoc(requestId: String, projectRoot: Path): JValue = {
    // TODO: runBuildDoc
    ("id" -> requestId) ~ ("status" -> "failure")
  }

  /**
    * Processes a request to build a jar from the project.
    */
  private def buildJar(requestId: String, projectRoot: Path): JValue = {
    Packager.buildJar(projectRoot, DefaultOptions)
    ("id" -> requestId) ~ ("status" -> "success")
  }

  /**
    * Processes a request to build a flix package from the project.
    */
  private def buildPkg(requestId: String, projectRoot: Path): JValue = {
    Packager.buildPkg(projectRoot, DefaultOptions)
    ("id" -> requestId) ~ ("status" -> "success")
  }

  /**
    * Processes a request to init a new flix package.
    */
  private def initPackage(requestId: String, projectRoot: Path): JValue = {
    Packager.init(projectRoot, DefaultOptions)
    ("id" -> requestId) ~ ("status" -> "success")
  }

  /**
    * Processes a request to run all tests in the package.
    */
  private def testPackage(requestId: String, projectRoot: Path): JValue = {
    Packager.test(projectRoot, DefaultOptions)
    ("id" -> requestId) ~ ("status" -> "success")
  }

  /**
    * Processes a shutdown request.
    */
  private def processShutdown()(implicit ws: WebSocket): Nothing = {
    System.exit(0)
    throw null
  }

  /**
    * Processes a symbols request.
    */
  private def processSymbols(requestId: String, uri: String): JValue = {
    if (root == null) {
      return ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> JArray(Nil))
    }

    // Find all definition symbols.
    val defSymbols = root.defs.values.collect {
      case decl0 if matchesUri(uri, decl0.loc) => DocumentSymbol.from(decl0)
    }

    // FInd all enum symbols.
    val enumSymbols = root.enums.values.collect {
      case decl0 if matchesUri(uri, decl0.loc) => DocumentSymbol.from(decl0)
    }

    // Compute all available symbols.
    val allSymbols = defSymbols ++ enumSymbols

    ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> allSymbols.map(_.toJSON))
  }

  /**
    * Processes a uses request.
    */
  private def processUses(requestId: String, uri: String, pos: Position)(implicit ws: WebSocket): JValue = {
    index.query(uri, pos) match {
      case Some(Entity.Exp(exp)) => exp match {
        case Expression.Def(sym, _, _) =>
          val uses = index.usesOf(sym)
          val locs = uses.toList.map(Location.from)
          ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))

        case Expression.Var(sym, _, _) =>
          val uses = index.usesOf(sym)
          val locs = uses.toList.map(Location.from)
          ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))

        case Expression.Tag(sym, _, _, _, _, _) =>
          val uses = index.usesOf(sym)
          val locs = uses.toList.map(Location.from)
          ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))

        case _ => mkNotFound(requestId, uri, pos)
      }

      case _ => mkNotFound(requestId, uri, pos)
    }
  }

  /**
    * Processes the version request.
    */
  private def processVersion(requestId: String)(implicit ws: WebSocket): JValue = {
    val major = Version.CurrentVersion.major
    val minor = Version.CurrentVersion.minor
    val revision = Version.CurrentVersion.revision
    val version = ("major" -> major) ~ ("minor" -> minor) ~ ("revision" -> revision)
    ("id" -> requestId) ~ ("status" -> "success") ~ ("version" -> version)
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
    Console.println(s"[$datePart] [$clientPart]: $msg")
  }

}
