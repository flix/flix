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
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.debug.{Audience, FormatCase, FormatDoc, FormatSignature, FormatType}
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
  * > {"id": "1", "request": "api/addUri", "uri": "foo.flix", "src": "def main(): List[Int] = List.range(1, 10)"}
  * > {"id": "2", "request": "lsp/check"}
  * > {"id": "3", "request": "lsp/hover", "uri": "foo.flix", "position": {"line": 1, "character": 25}}
  *
  * The NPM package "wscat" is useful for experimenting with these commands from the shell.
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
  override def onMessage(ws: WebSocket, data: String): Unit = try {
    // Parse and process request.
    parseRequest(data)(ws) match {
      case Ok(request) =>
        val result = processRequest(request)(ws)
        val jsonCompact = JsonMethods.compact(JsonMethods.render(result))
        val jsonPretty = JsonMethods.pretty(JsonMethods.render(result))
        log("Sending reply: " + jsonCompact)(ws)
        ws.send(jsonPretty)
      case Err(msg) => log(msg)(ws)
    }
  } catch {
    case t: InternalCompilerException =>
      t.printStackTrace()
      System.exit(1)
    case t: InternalRuntimeException =>
      t.printStackTrace()
      System.exit(2)
    case t: Throwable =>
      t.printStackTrace()
      System.exit(3)
  }

  /**
    * Invoked when an error occurs.
    */
  override def onError(ws: WebSocket, e: Exception): Unit = {
    e.printStackTrace()
    System.exit(4)
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
      case JString("api/version") => Request.parseVersion(json)
      case JString("api/shutdown") => Request.parseShutdown(json)

      case JString("cmd/runBenchmarks") => Request.parseRunBenchmarks(json)
      case JString("cmd/runMain") => Request.parseRunMain(json)
      case JString("cmd/runTests") => Request.parseRunTests(json)

      case JString("lsp/check") => Request.parseCheck(json)
      case JString("lsp/codelens") => Request.parseCodelens(json)
      case JString("lsp/hover") => Request.parseHover(json)
      case JString("lsp/goto") => Request.parseGoto(json)
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

    case Request.Version(id) => processVersion(id)

    case Request.Shutdown(id) => processShutdown()

    case Request.RunBenchmarks(id) => runBenchmarks(id)
    case Request.RunMain(id) => runMain(id)
    case Request.RunTests(id) => runTests(id)

    case Request.Check(id) => processCheck(id)
    case Request.Codelens(id, uri) => processCodelens(id, uri)
    case Request.Hover(id, uri, pos) => processHover(id, uri, pos)
    case Request.Goto(id, uri, pos) => processGoto(id, uri, pos)
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
    * Processes a hover request.
    */
  private def processHover(requestId: String, uri: String, pos: Position)(implicit ws: WebSocket): JValue = {
    implicit val _ = Audience.External

    def formatTypAndEff(tpe0: Type, eff0: Type): String = {
      val t = FormatType.formatType(tpe0)
      eff0 match {
        case Type.Cst(TypeConstructor.True) => t
        case Type.Cst(TypeConstructor.False) => s"$t & Impure"
        case eff => s"$t & ${FormatType.formatType(eff)}"
      }
    }

    def formatStratification(stf: Ast.Stratification): String = {
      val sb = new StringBuilder()
      val max = stf.m.values.max
      for (i <- 0 to max) {
        sb.append("Stratum ").append(i).append(":").append("\r\n")
        for ((sym, stratum) <- stf.m) {
          if (i == stratum) {
            sb.append("  ").append(sym).append("\r\n")
          }
        }
      }
      sb.toString()
    }

    index.query(uri, pos) match {
      case Some(Entity.Exp(exp)) =>
        exp match {
          case Expression.Def(sym, _, _) =>
            val decl = root.defs(sym)
            val markup =
              s"""${FormatSignature.asMarkDown(decl)}
                 |
                 |${FormatDoc.asMarkDown(decl.doc)}
                 |""".stripMargin
            val contents = MarkupContent(MarkupKind.Markdown, markup)
            val range = Range.from(exp.loc)
            val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
            ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> result)

          case Expression.Hole(sym, _, _, _) =>
            val holes = TypedAstOps.holesOf(root)
            val holeContext = holes(sym)
            val env = holeContext.env.map {
              case (localSym, localTpe) => s"${localSym.text}: ${FormatType.formatType(localTpe)}"
            }
            val markup =
              s"""Hole: ${sym.name}
                 |
                 |Context:
                 |  ${holeContext.env.mkString("\r\n  ")}
                 |""".stripMargin
            val contents = MarkupContent(MarkupKind.Markdown, markup)
            val range = Range.from(exp.loc)
            val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
            ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> result)

          case Expression.Tag(sym, tag, _, _, _, _) =>
            val decl = root.enums(sym)
            val caze = decl.cases(tag)
            val markup =
              s"""${FormatCase.asMarkDown(caze)}
                 |
                 |${FormatDoc.asMarkDown(decl.doc)}
                 |""".stripMargin
            val contents = MarkupContent(MarkupKind.Markdown, markup)
            val range = Range.from(exp.loc)
            val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
            ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> result)

          case Expression.FixpointConstraintSet(_, stf, _, _) =>
            val markup =
              s"""${formatTypAndEff(exp.tpe, exp.eff)}
                 |
                 |${formatStratification(stf)}
                 |""".stripMargin
            val contents = MarkupContent(MarkupKind.Markdown, markup)
            val range = Range.from(exp.loc)
            val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
            ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> result)

          case Expression.FixpointCompose(_, _, stf, _, _, _) =>
            val markup =
              s"""${formatTypAndEff(exp.tpe, exp.eff)}
                 |
                 |${formatStratification(stf)}
                 |""".stripMargin
            val contents = MarkupContent(MarkupKind.Markdown, markup)
            val range = Range.from(exp.loc)
            val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
            ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> result)

          case Expression.FixpointSolve(_, stf, _, _, _) =>
            val markup =
              s"""${formatTypAndEff(exp.tpe, exp.eff)}
                 |
                 |${formatStratification(stf)}
                 |""".stripMargin
            val contents = MarkupContent(MarkupKind.Markdown, markup)
            val range = Range.from(exp.loc)
            val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
            ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> result)

          case _ =>
            //
            // Other Expression.
            //
            val markup = formatTypAndEff(exp.tpe, exp.eff)
            val contents = MarkupContent(MarkupKind.Markdown, markup)
            val range = Range.from(exp.loc)
            val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
            ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> result)
        }

      case Some(Entity.Pat(pat)) => pat match {
        case Pattern.Tag(sym, tag, _, _, _) =>
          val decl = root.enums(sym)
          val caze = decl.cases(tag)
          val markup =
            s"""${FormatCase.asMarkDown(caze)}
               |
               |${FormatDoc.asMarkDown(decl.doc)}
               |""".stripMargin
          val contents = MarkupContent(MarkupKind.Markdown, markup)
          val range = Range.from(pat.loc)
          val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
          ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> result)

        case _ =>
          //
          // Other pattern.
          //
          val markup = FormatType.formatType(pat.tpe)
          val contents = MarkupContent(MarkupKind.Markdown, markup)
          val range = Range.from(pat.loc)
          val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
          ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> result)
      }


      case _ =>
        mkNotFound(requestId, uri, pos)
    }
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
    throw null // unreachable
  }

  /**
    * Processes a uses request.
    */
  private def processUses(requestId: String, uri: String, pos: Position)(implicit ws: WebSocket): JValue = {
    index.query(uri, pos) match {

      case Some(Entity.Def(defn)) =>
        val uses = index.usesOf(defn.sym)
        val locs = uses.toList.map(Location.from)
        ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))

      case Some(Entity.FormalParam(param)) =>
        val uses = index.usesOf(param.sym)
        val locs = uses.toList.map(Location.from)
        ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))

      case Some(Entity.Exp(exp)) => exp match {
        case Expression.Def(sym, _, _) =>
          val uses = index.usesOf(sym)
          val locs = uses.toList.map(Location.from)
          ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))

        case Expression.Var(sym, _, _) =>
          val uses = index.usesOf(sym)
          val locs = uses.toList.map(Location.from)
          ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))

        case Expression.Tag(sym, tag, _, _, _, _) =>
          val uses = index.usesOf(sym, tag)
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
    Console.println(s"[$datePart] [$clientPart]: $msg")
  }

}
