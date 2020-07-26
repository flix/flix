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
import java.text.SimpleDateFormat
import java.util.Date

import ca.uwaterloo.flix.api.{Flix, Version}
import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, Pattern, Root}
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.debug.{Audience, FormatType}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation.{Failure, Success}
import ca.uwaterloo.flix.util.vt.TerminalContext
import ca.uwaterloo.flix.util.vt.TerminalContext.NoTerminal
import ca.uwaterloo.flix.util.{InternalCompilerException, InternalRuntimeException, Result}
import org.java_websocket.WebSocket
import org.java_websocket.handshake.ClientHandshake
import org.java_websocket.server.WebSocketServer
import org.json4s.ParserUtil.ParseException
import org.json4s.native.JsonMethods
import org.json4s.native.JsonMethods.parse
import org.json4s.JsonAST.{JArray, JString, JValue}

import scala.collection.mutable

/**
  * A Compiler Interface for the Language Server Protocol.
  *
  * Does not implement the LSP protocol directly, but relies on an intermediate TypeScript server.
  *
  *
  * Examples:
  *
  * When connecting or whenever a source file is changed, the client must issue the request:
  *
  * -   {"request":"validate", "paths":[]}
  *
  * If this is successful then the following requests can be made:
  *
  * Get the type and effect of an expression:
  *
  * -   {"request": "typeAndEffOf", "uri": "Option.flix", "position": {"line": 35, "character": 22}}
  *
  * Get the location of a definition or variable:
  *
  * -   {"request": "gotoDef", "uri": "Option.flix", "position": {"line": 214, "character": 40}}
  *
  * Shutdown the server:
  *
  * -   {"request": "shutdown"}
  *
  *
  * The NPM package "wscat" is useful for experimenting with these commands from the shell.
  */
class LanguageServer(port: Int) extends WebSocketServer(new InetSocketAddress(port)) {

  /**
    * The custom date format to use for logging.
    */
  val DateFormat: String = "yyyy-MM-dd HH:mm:ss"

  /**
    * The current AST root. The root is null until the source code is compiled.
    */
  var root: Root = _ // TODO: We should be more careful if root can be null.

  /**
    * The current reverse index. The index is empty until the source code is compiled.
    */
  var index: Index = Index.empty

  /**
    * The audience used for formatting.
    */
  implicit val audience: Audience = Audience.External

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
        val json = JsonMethods.pretty(JsonMethods.render(result.toJSON))
        ws.send(json)
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
      case JString("version") => Ok(Request.Version)
      case JString("validate") => Request.parseValidate(json)
      case JString("typeAndEffOf") => Request.parseTypeAndEffectOf(json)
      case JString("goto") => Request.parseGoto(json)
      case JString("uses") => Request.parseUses(json)
      case JString("prepareRename") => Request.parsePrepareRename(json)
      case JString("complete") => Request.parseComplete(json)
      case JString("textDocument/codeLens") => Request.parseCodeLens(json)
      case JString("textDocument/foldingRange") => Request.parseFoldingRange(json)
      case JString("shutdown") => Ok(Request.Shutdown)
      case s => Err(s"Unsupported request: '$s'.")
    }
  } catch {
    case ex: ParseException => Err(s"Malformed request. Unable to parse JSON: '${ex.getMessage}'.")
  }

  /**
    * Process the request.
    */
  private def processRequest(request: Request)(implicit ws: WebSocket): Reply = request match {

    case Request.Version =>
      val major = Version.CurrentVersion.major
      val minor = Version.CurrentVersion.minor
      val revision = Version.CurrentVersion.revision
      Reply.Version(major, minor, revision)

    case Request.Validate(paths) =>
      // Configure the Flix compiler.
      val flix = new Flix()
      for (path <- paths) {
        log(s"Adding path: '$path'.")
        flix.addPath(path)
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
          Reply.CompilationSuccess(e, Version.CurrentVersion.toString)
        case Failure(errors) =>
          // Case 2: Compilation failed. Send back the error messages.
          implicit val ctx: TerminalContext = NoTerminal

          // Group the error messages by source.
          val errorsBySource = errors.toList.groupBy(_.loc.source)

          // Translate each compilation error to a diagnostic.
          val results = errorsBySource.foldLeft(Nil: List[PublishDiagnosticsParams]) {
            case (acc, (source, compilationErrors)) =>
              val diagnostics = compilationErrors.map {
                case compilationError =>
                  val range = Range.from(compilationError.loc)
                  val code = compilationError.kind
                  val message = compilationError.summary
                  Diagnostic(range, code, message)
              }
              PublishDiagnosticsParams(source.name, diagnostics) :: acc
          }

          Reply.CompilationFailure(results)
      }

    case Request.TypeAndEffectOf(doc, pos) =>
      index.query(doc, pos) match {
        case Some(Entity.Exp(exp)) => Reply.EffAndTypeOf(exp)
        case _ =>
          log(s"No entry for: '$doc' at '$pos'.")
          Reply.NotFound()
      }

    case Request.GetDefs(uri) => ??? // TODO

    case Request.GetEnums(uri) => ??? // TODO

    case Request.Goto(uri, pos) =>
      index.query(uri, pos) match {
        case Some(Entity.Exp(exp)) => exp match {
          case Expression.Def(sym, _, loc) => Reply.Goto(mkGotoDef(sym, loc))
          case Expression.Var(sym, _, loc) => Reply.Goto(mkGotoVar(sym, loc))
          case Expression.Tag(sym, tag, _, _, _, loc) => Reply.Goto(mkGotoEnum(sym, tag, loc))
          case _ => Reply.NotFound()
        }
        case Some(Entity.Pat(pat)) => pat match {
          case Pattern.Var(sym, _, loc) => Reply.Goto(mkGotoVar(sym, loc))
          case Pattern.Tag(sym, tag, _, _, loc) => Reply.Goto(mkGotoEnum(sym, tag, loc))
          case _ => Reply.NotFound()
        }
        case _ =>
          log(s"No entry for: '$uri,' at '$pos'.")
          Reply.NotFound()
      }

    case Request.Uses(uri, pos) =>
      index.query(uri, pos) match {
        case Some(Entity.Exp(exp)) => exp match {
          case Expression.Def(sym, _, _) =>
            val uses = index.usesOf(sym)
            val locs = uses.toList.map(Location.from)
            Reply.Uses(locs)

          case Expression.Var(sym, _, _) =>
            val uses = index.usesOf(sym)
            val locs = uses.toList.map(Location.from)
            Reply.Uses(locs)

          case Expression.Tag(sym, _, _, _, _, _) =>
            val uses = index.usesOf(sym)
            val locs = uses.toList.map(Location.from)
            Reply.Uses(locs)

          case _ => Reply.NotFound()
        }

        case _ =>
          log(s"No entry for: '$uri,' at '$pos'.")
          Reply.NotFound()
      }

    case Request.PrepareRename(uri, pos) =>
      index.query(uri, pos) match {
        case Some(Entity.Exp(exp)) => exp match {
          case Expression.Def(_, _, loc) =>
            Reply.PreparedRename(Range.from(loc))

          case Expression.Var(_, _, loc) =>
            Reply.PreparedRename(Range.from(loc))

          case Expression.Tag(_, _, _, _, _, loc) =>
            Reply.PreparedRename(Range.from(loc))

          case _ => Reply.NotFound()
        }

        case _ => Reply.NotFound()
      }

    case Request.Complete(uri, pos) =>
      // TODO: Fake it till you make it:
      val items = List(
        CompletionItem("Hello!", None, None, None, Some(TextEdit(Range(pos, pos), "Hi there!"))),
        CompletionItem("Goodbye!", None, None, None, Some(TextEdit(Range(pos, pos), "Farewell!")))
      )
      val default = Reply.Completions(items)

      index.query(uri, pos) match {
        case Some(Entity.Exp(exp)) => exp match {
          case Expression.Hole(sym, _, _, _) =>
            // TODO: This is just a first approximation. Have to check the types etc.
            val holeCtx = TypedAstOps.holesOf(root)(sym)
            val items = holeCtx.env.map {
              case (sym, tpe) => CompletionItem(sym.text, Some(CompletionItemKind.Variable), Some(FormatType.formatType(tpe)), None, Some(TextEdit(Range(pos, pos), sym.text)))
            }
            Reply.Completions(items.toList)
          case _ => default
        }
        case _ => default
      }

    case Request.CodeLens(uri) =>
      //
      // A mutable collection of code lenses.
      //
      val codeLenses = mutable.ListBuffer.empty[CodeLens]

      // TODO: Refactor into separate methods.
      // TODO: Check that we are in the right file (i.e. uri).

      //
      // Add a CodeLens for main (if present).
      //
      val main = Symbol.mkDefnSym("main")
      root.defs.get(main) match {
        case None =>
          // Case 1: No main. Nothing to be done.
          ()
        case Some(defn) =>
          // Case 2: Main found. Add a CodeLens.
          val loc = defn.sym.loc
          val cmd = Command("Run Main", "runMain")
          codeLenses.addOne(CodeLens(Range.from(loc), Some(cmd)))
      }

      //
      // Add CodeLenses for unit tests.
      //
      for ((sym, defn) <- root.defs) {
        if (defn.ann.exists(_.name.isInstanceOf[Ast.Annotation.Test])) {
          val loc = defn.sym.loc
          val cmd1 = Command("Run Test", "runTest") // TODO: Need extra information about the test.
          val cmd2 = Command("Run All Tests", "runAllTests")
          codeLenses.addOne(CodeLens(Range.from(loc), Some(cmd1)))
          codeLenses.addOne(CodeLens(Range.from(loc), Some(cmd2)))
        }
      }

      Reply.JSON(JArray(codeLenses.map(_.toJSON).toList))

    case Request.FoldingRange(uri) =>
      val defsFoldingRanges = root.defs.foldRight(List.empty[FoldingRange]) {
        case ((sym, defn), acc) =>
          val loc = defn.exp.loc
          if (uri.toString != loc.source.name) {
            acc
          } else {
            val foldingRange = FoldingRange(loc.beginLine, Some(loc.beginCol), loc.endLine, Some(loc.endCol), Some(FoldingRangeKind.Region))
            foldingRange :: acc
          }
      }
      // TODO: Add enums etc.
      val result = JArray(defsFoldingRanges.map(_.toJSON))
      Reply.JSON(result)

    case Request.Shutdown =>
      ws.close(1000, "Shutting down...")
      System.exit(0)
      null
  }


  /**
    * Returns a location link to the given symbol `sym`.
    */
  private def mkGotoDef(sym: Symbol.DefnSym, loc: SourceLocation): LocationLink = {
    val defDecl = root.defs(sym)
    val originSelectionRange = Range.from(loc)
    val targetUri = sym.loc.source.name
    val targetRange = Range.from(sym.loc)
    val targetSelectionRange = Range.from(defDecl.loc)
    LocationLink(originSelectionRange, targetUri, targetRange, targetSelectionRange)
  }

  /**
    * Returns a location link to the given symbol `sym`.
    */
  private def mkGotoEnum(sym: Symbol.EnumSym, tag: String, loc: SourceLocation): LocationLink = {
    val enumDecl = root.enums(sym)
    val caseDecl = enumDecl.cases(tag)
    val originSelectionRange = Range.from(loc)
    val targetUri = sym.loc.source.name
    val targetRange = Range.from(caseDecl.loc)
    val targetSelectionRange = Range.from(caseDecl.loc)
    LocationLink(originSelectionRange, targetUri, targetRange, targetSelectionRange)
  }

  /**
    * Returns a reference to the variable symbol `sym`.
    */
  private def mkGotoVar(sym: Symbol.VarSym, originLoc: SourceLocation): LocationLink = {
    val originSelectionRange = Range.from(originLoc)
    val targetUri = sym.loc.source.name
    val targetRange = Range.from(sym.loc)
    val targetSelectionRange = Range.from(sym.loc)
    LocationLink(originSelectionRange, targetUri, targetRange, targetSelectionRange)
  }

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
