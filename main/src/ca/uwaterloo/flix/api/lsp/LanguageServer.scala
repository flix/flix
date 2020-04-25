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
import java.nio.file.Paths
import java.text.SimpleDateFormat
import java.util.Date

import ca.uwaterloo.flix.api.{Flix, Version}
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst.{CatchRule, Constraint, Def, Expression, MatchRule, Predicate, Root, SelectChannelRule}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation.{Failure, Success}
import ca.uwaterloo.flix.util.vt.TerminalContext
import ca.uwaterloo.flix.util.vt.TerminalContext.NoTerminal
import ca.uwaterloo.flix.util.{InternalCompilerException, InternalRuntimeException, Result}
import org.java_websocket.WebSocket
import org.java_websocket.handshake.ClientHandshake
import org.java_websocket.server.WebSocketServer
import org.json4s.JsonAST.JString
import org.json4s.ParserUtil.ParseException
import org.json4s.native.JsonMethods
import org.json4s.native.JsonMethods.parse

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
  * -   {"request": "typeAndEffOf", "uri": "Option.flix", "position": {"line": 35, "col": 22}}
  *
  * Get the location of a definition or variable:
  *
  * -   {"request": "gotoDef", "uri": "Option.flix", "position": {"line": 214, "col": 40}}
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
    // Log the length and size of the received data.
    log(s"Received ${data.length} characters of input (${data.getBytes.length} bytes).")(ws)

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
      case JString("validate") => Request.parseValidate(json)
      case JString("typeAndEffOf") => Request.parseTypeAndEffectOf(json)
      case JString("gotoDef") => Request.parseGotoDef(json)
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
          index = visitRoot(root)

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
                  val message = compilationError.message.fmt
                  Diagnostic(range, code, message)
              }
              PublishDiagnosticsParams(source.name, diagnostics) :: acc
          }

          Reply.CompilationFailure(results)
      }

    case Request.TypeAndEffectOf(doc, pos) =>
      index.query(doc, pos) match {
        case None =>
          log(s"No entry for: '$doc' at '$pos'.")
          Reply.NotFound()
        case Some(exp) => Reply.EffAndTypeOf(exp)
      }

    case Request.GotoDef(doc, pos) =>
      index.query(doc, pos) match {
        case None =>
          log(s"No entry for: '$doc' at '$pos'.")
          Reply.NotFound()
        case Some(exp) => exp match {
          case Expression.Def(sym, _, originLoc) =>
            val originSelectionRange = Range.from(originLoc)
            val targetUri = sym.loc.source.name
            val targetRange = Range.from(sym.loc)
            val targetSelectionRange = Range.from(sym.loc)
            val locationLink = LocationLink(originSelectionRange, targetUri, targetRange, targetSelectionRange)
            Reply.GotoDef(locationLink)

          case Expression.Var(sym, _, originLoc) =>
            val originSelectionRange = Range.from(originLoc)
            val targetUri = sym.loc.source.name
            val targetRange = Range.from(sym.loc)
            val targetSelectionRange = Range.from(sym.loc)
            val locationLink = LocationLink(originSelectionRange, targetUri, targetRange, targetSelectionRange)
            Reply.GotoVar(locationLink)

          case _ => Reply.NotFound()
        }
      }

    case Request.Shutdown =>
      ws.close(1000, "Shutting down...")
      System.exit(0)
      null
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

  /**
    * Returns a reverse index for the given AST `root`.
    */
  private def visitRoot(root: Root): Index = root.defs.foldLeft(Index.empty) {
    case (index, (_, def0)) => index ++ visitDef(def0)
  }

  /**
    * Returns a reverse index for the given definition `def0`.
    */
  private def visitDef(def0: Def): Index = visitExp(def0.exp)

  /**
    * Returns a reverse index for the given expression `exp0`.
    */
  private def visitExp(exp0: Expression): Index = exp0 match {
    case Expression.Unit(_) =>
      Index.of(exp0)

    case Expression.True(_) =>
      Index.of(exp0)

    case Expression.False(_) =>
      Index.of(exp0)

    case Expression.Char(_, _) =>
      Index.of(exp0)

    case Expression.Float32(_, _) =>
      Index.of(exp0)

    case Expression.Float64(_, _) =>
      Index.of(exp0)

    case Expression.Int8(_, _) =>
      Index.of(exp0)

    case Expression.Int16(_, _) =>
      Index.of(exp0)

    case Expression.Int32(_, _) =>
      Index.of(exp0)

    case Expression.Int64(_, _) =>
      Index.of(exp0)

    case Expression.BigInt(_, _) =>
      Index.of(exp0)

    case Expression.Str(_, _) =>
      Index.of(exp0)

    case Expression.Wild(_, _) =>
      Index.of(exp0)

    case Expression.Var(_, _, _) =>
      Index.of(exp0)

    case Expression.Def(_, _, _) =>
      Index.of(exp0)

    case Expression.Hole(_, _, _, _) =>
      Index.of(exp0)

    case Expression.Lambda(_, exp, _, _) =>
      visitExp(exp) + exp0

    case Expression.Apply(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.Unary(_, exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.Let(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.LetRec(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) + exp0

    case Expression.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Match(exp, rules, _, _, _) =>
      val i0 = visitExp(exp) + exp0
      rules.foldLeft(i0) {
        case (index, MatchRule(_, guard, exp)) => index ++ visitExp(guard) ++ visitExp(exp)
      }

    case Expression.Tag(_, _, exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.Tuple(exps, tpe, eff, loc) =>
      visitExps(exps) + exp0

    case Expression.RecordEmpty(tpe, loc) =>
      Index.of(exp0)

    case Expression.RecordSelect(exp, _, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.RecordExtend(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.RecordRestrict(_, exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.ArrayLit(exps, _, _, _) =>
      visitExps(exps) + exp0

    case Expression.ArrayNew(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.ArrayLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.ArrayLength(exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.ArrayStore(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) + exp0

    case Expression.ArraySlice(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) + exp0

    case Expression.VectorLit(exps, _, _, _) =>
      visitExps(exps) + exp0

    case Expression.VectorNew(exp, _, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.VectorLoad(exp, _, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.VectorStore(exp1, _, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.VectorLength(exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.VectorSlice(exp, _, _, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.Ref(exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.Deref(exp1, _, _, _) =>
      visitExp(exp1) + exp0

    case Expression.Assign(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.Existential(_, exp, _) =>
      visitExp(exp) + exp0

    case Expression.Universal(_, exp, _) =>
      visitExp(exp) + exp0

    case Expression.Ascribe(exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.Cast(exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.TryCatch(exp, rules, _, _, _) =>
      val i0 = visitExp(exp) + exp0
      rules.foldLeft(i0) {
        case (index, CatchRule(_, _, exp)) => index ++ visitExp(exp)
      }

    case Expression.InvokeConstructor(_, args, _, _, _) =>
      visitExps(args) + exp0

    case Expression.InvokeMethod(_, exp, args, _, _, _) =>
      visitExp(exp) ++ visitExps(args) + exp0

    case Expression.InvokeStaticMethod(_, args, _, _, _) =>
      visitExps(args) + exp0

    case Expression.GetField(_, exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.PutField(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.GetStaticField(_, _, _, _) =>
      Index.of(exp0)

    case Expression.PutStaticField(_, exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.NewChannel(exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.GetChannel(exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.PutChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.SelectChannel(rules, default, _, _, _) =>
      val i0 = default.map(visitExp).getOrElse(Index.empty) + exp0
      rules.foldLeft(i0) {
        case (index, SelectChannelRule(_, _, exp)) => index ++ visitExp(exp)
      }

    case Expression.ProcessSpawn(exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.ProcessPanic(_, _, _, _) =>
      Index.empty

    case Expression.FixpointConstraintSet(cs, _, _) =>
      cs.foldLeft(Index.empty) {
        case (index, c) => index ++ visitConstraint(c)
      }

    case Expression.FixpointCompose(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.FixpointSolve(exp, _, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.FixpointProject(_, exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.FixpointEntails(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.FixpointFold(_, exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) + exp0
  }

  /**
    * Returns a reverse index for the given expressions `exps0`.
    */
  private def visitExps(exps0: List[Expression]): Index =
    exps0.foldLeft(Index.empty) {
      case (index, exp0) => index ++ visitExp(exp0)
    }

  /**
    * Returns a reverse index for the given constraint `c0`.
    */
  private def visitConstraint(c0: Constraint): Index = {
    val i = visitHead(c0.head)
    c0.body.foldLeft(i) {
      case (index, b0) => index ++ visitBody(b0)
    }
  }

  /**
    * Returns a reverse index for the given head predicate `h0`.
    */
  private def visitHead(h0: Predicate.Head): Index = h0 match {
    case Head.Atom(_, _, terms, _, _) => visitExps(terms)
    case Head.Union(exp, _, _) => visitExp(exp)
  }

  /**
    * Returns a reverse index for the given body predicate `b0`.
    */
  private def visitBody(b0: Predicate.Body): Index = b0 match {
    case Body.Atom(_, _, _, _, _, _) => Index.empty
    case Body.Guard(exp, _) => visitExp(exp)
  }

}
