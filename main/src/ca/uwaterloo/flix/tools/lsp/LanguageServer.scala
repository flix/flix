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
package ca.uwaterloo.flix.tools.lsp

import java.net.InetSocketAddress
import java.text.SimpleDateFormat
import java.util.Date

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst.{CatchRule, Def, Expression, MatchRule, Root, SelectChannelRule}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.Validation.{Failure, Success}
import ca.uwaterloo.flix.util.vt.TerminalContext
import ca.uwaterloo.flix.util.vt.TerminalContext.{AnsiTerminal, NoTerminal}
import ca.uwaterloo.flix.util.{InternalCompilerException, InternalRuntimeException, Options}
import org.java_websocket.WebSocket
import org.java_websocket.handshake.ClientHandshake
import org.java_websocket.server.WebSocketServer
import org.json4s.JsonAST.{JArray, JBool, JField, JLong, JObject, JString}
import org.json4s.ParserUtil.ParseException
import org.json4s.native.JsonMethods
import org.json4s.native.JsonMethods.parse

import scala.collection.mutable

/**
  * A Compiler Interface for the Language Server Protocol.
  *
  * Does not implement the LSP protocol itself, but relies on a JavaScript/TypeScript intermediary.
  */
class LanguageServer(port: Int) extends WebSocketServer(new InetSocketAddress(port)) {

  // TODO: Start socket server on port.
  // TODO: Accept request telling what files to compile, return compilation status.
  // TODO: Accept queries.
  // TODO: Add Range?
  // TODO: Add LocationLink?
  // TODO: Add diagonostic?
  // TODO: Shutdown message.
  // TODO: Get type/effect/typeandeffect
  // TODO: completion?
  // TODO: signature help?
  // TODO: GoTODef
  // TODO: Goto type def.
  // TODO: FindUsages.

  /**
    * The custom date format to use for logging.
    */
  val DateFormat: String = "yyyy-MM-dd HH:mm:ss"

  var index: Index = null

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
    for {
      request <- parseRequest(data)(ws)
    } yield {
      val result = processRequest(request)(ws)
      val json = JsonMethods.pretty(JsonMethods.render(result))
      ws.send(json)
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
  private def parseRequest(s: String)(implicit ws: WebSocket): Option[Request] = try {
    // Parse the string into a json object.
    val json = parse(s)

    json \\ "request" match {
      case JString("compile") =>
        val paths = json \\ "paths" match {
          case JArray(arr) =>
            // TODO
            Nil
          case _ => ???
        }
        Some(Request.Compile(paths))

      case JString("typeOf") =>
        // TODO: Errors
        val doc = Document.parse(json \\ "document")
        val pos = Position.parse(json \\ "position")
        Some(Request.TypeOf(doc, pos))

      case JString("jumpToDef") =>
        // TODO: Errors
        val doc = Document.parse(json \\ "document")
        val pos = Position.parse(json \\ "position")
        Some(Request.JumpToDef(doc, pos))

      case s =>
        log(s"Unsupported request: '$s'.")
        None
    }
  } catch {
    case ex: ParseException =>
      val msg = s"Malformed request. Unable to parse JSON: '${ex.getMessage}'."
      log(msg)
      ws.closeConnection(5000, msg)
      None
  }

  /**
    * Process the request.
    */
  private def processRequest(request: Request)(implicit ws: WebSocket): JObject = request match {
    case Request.Compile(paths) =>
      // Configure the Flix compiler.
      val flix = new Flix()
      for (path <- paths) {
        flix.addPath(path)
      }

      // Run the compiler up to the type checking phase.
      flix.check() match {
        case Success(root) =>
          // Case 1: Compilation was successful. Build the reverse the reverse index.
          index = visitRoot(root)

          // Send back a status message.
          JObject(
            JField("status", JString("success"))
          )
        case Failure(errors) =>
          // Case 2: Compilation failed. Send back the error messages.
          implicit val ctx: TerminalContext = NoTerminal
          JObject(
            JField("status", JString("success")),
            JField("result", JString(errors.head.message.fmt))
          )
      }

    case Request.TypeOf(doc, pos) =>
      index.query(doc, pos) match {
        case None => JObject(
          JField("status", JString("success")),
          JField("result", JString("unknown"))
        )

        case Some(exp) =>
          val tpe = exp.tpe.toString
          val eff = exp.eff.toString
          JObject(
            JField("status", JString("success")),
            JField("result", JString(s"$tpe & $eff"))
          )
      }

    case Request.JumpToDef(doc, pos) =>
      index.query(doc, pos) match {
        case None => JObject(
          JField("status", JString("success")),
          JField("result", JString("unknown"))
        )

        case Some(exp) =>
          exp match {
            case Expression.Var(sym, _, _) =>
              JObject(
                JField("status", JString("success")),
                JField("result", JString(sym.loc.format))
              )

            case Expression.Def(sym, _, _) =>
              JObject(
                JField("status", JString("success")),
                JField("result", JString(sym.loc.format))
              )

            case _ =>
              JObject(
                JField("status", JString("success")),
                JField("result", JString("unknown"))
              )
          }
      }
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

  // TODO: Need to somehow track variables in scope?

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
      visitExps(exps)

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

    //        case class VectorLit(elms: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class VectorNew(elm: TypedAst.Expression, len: Int, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class VectorLoad(base: TypedAst.Expression, index: Int, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class VectorStore(base: TypedAst.Expression, index: Int, elm: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class VectorLength(base: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class VectorSlice(base: TypedAst.Expression, startIndex: Int, endIndex: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //

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
      // TODO
      Index.empty

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

}
