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
import ca.uwaterloo.flix.language.ast.TypedAst.{Def, Expression, Root}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.Validation.{Failure, Success}
import ca.uwaterloo.flix.util.{InternalCompilerException, InternalRuntimeException, Options}
import org.java_websocket.WebSocket
import org.java_websocket.handshake.ClientHandshake
import org.java_websocket.server.WebSocketServer
import org.json4s.JsonAST.{JBool, JField, JLong, JObject, JString}
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

    val request = json \\ "request" match {
      case JString("compile") =>
        // TODO
        ???
      case JString("typeOf") =>
        // TODO: Errors
        val doc = Document.parse(json \\ "document")
        val pos = Position.parse(json \\ "position")
        Request.TypeOf(doc, pos)
      case s => log(s"Unsupported request: '$s'.")
    }

    // TODO
    None
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
    case Request.Compile() =>
      val flix = new Flix()
      flix.addPath("foo")
      flix.compile() match {
        case Success(t) =>
          val root: Root = null
          index = visitRoot(root)
          ??? // TODO
        case Failure(errors) =>
          ??? // TODO
      }

    case Request.TypeOf(doc, pos) =>
      index.query(doc, pos) match {
        case None =>
          JObject(
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
  def visitRoot(root: Root): Index = root.defs.foldLeft(Index.Empty) {
    case (index, (_, def0)) => index ++ visitDef(def0)
  }

  /**
    * Returns a reverse index for the given definition `def0`.
    */
  def visitDef(def0: Def): Index = visitExp(def0.exp)

  /**
    * Returns a reverse index for the given expression `exp0`.
    */
  def visitExp(exp0: Expression): Index = exp0 match {
    case Expression.Unit(_) => Index.of(exp0)

    case Expression.True(_) => Index.of(exp0)

    case Expression.False(_) => Index.of(exp0)

    case Expression.Char(_, _) => Index.of(exp0)

    case Expression.Float32(_, _) => Index.of(exp0)

    case Expression.Float64(_, _) => Index.of(exp0)

    case Expression.Int8(_, _) => Index.of(exp0)

    case Expression.Int16(_, _) => Index.of(exp0)

    case Expression.Int32(_, _) => Index.of(exp0)

    case Expression.Int64(_, _) => Index.of(exp0)

    case Expression.BigInt(_, _) => Index.of(exp0)

    case Expression.Float32(_, _) => Index.of(exp0)

    case Expression.Float64(_, _) => Index.of(exp0)

    case Expression.Str(_, _) => Index.of(exp0)

    case Expression.Wild(_, _) => Index.of(exp0)

    //        case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
    //          def eff: Type = Type.Pure
    //        }
    //
    //        case class Def(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
    //          def eff: Type = Type.Pure
    //        }
    //
    //        case class Hole(sym: Symbol.HoleSym, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class Lambda(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
    //          def eff: Type = Type.Pure
    //        }
    //
    //        case class Apply(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class Unary(op: UnaryOperator, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    case Expression.Unary(_, exp, _, _, _) =>
      visitExp(exp) + exp0

    //        case class Binary(op: BinaryOperator, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class Let(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class LetRec(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class IfThenElse(exp1: TypedAst.Expression, exp2: TypedAst.Expression, exp3: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class Stm(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class Match(exp: TypedAst.Expression, rules: List[TypedAst.MatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class Tag(sym: Symbol.EnumSym, tag: String, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class Tuple(elms: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class RecordEmpty(tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
    //          def eff: Type = Type.Pure
    //        }
    //
    //        case class RecordSelect(exp: TypedAst.Expression, label: String, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class RecordExtend(label: String, value: TypedAst.Expression, rest: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class RecordRestrict(label: String, rest: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class ArrayLit(elms: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class ArrayNew(elm: TypedAst.Expression, len: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class ArrayLoad(base: TypedAst.Expression, index: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class ArrayLength(base: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class ArrayStore(base: TypedAst.Expression, index: TypedAst.Expression, elm: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class ArraySlice(base: TypedAst.Expression, beginIndex: TypedAst.Expression, endIndex: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
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
    //        case class Ref(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class Deref(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class Assign(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class Existential(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Expression {
    //          def tpe: Type = Type.Bool
    //
    //          def eff: Type = Type.Pure
    //        }
    //
    //        case class Universal(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Expression {
    //          def tpe: Type = Type.Bool
    //
    //          def eff: Type = Type.Pure
    //        }
    //
    //        case class Ascribe(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class Cast(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class TryCatch(exp: TypedAst.Expression, rules: List[TypedAst.CatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class InvokeConstructor(constructor: Constructor[_], args: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class InvokeMethod(method: Method, exp: TypedAst.Expression, args: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class InvokeStaticMethod(method: Method, args: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class GetField(field: Field, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class PutField(field: Field, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class GetStaticField(field: Field, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class PutStaticField(field: Field, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class NewChannel(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class GetChannel(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class PutChannel(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class SelectChannel(rules: List[TypedAst.SelectChannelRule], default: Option[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class ProcessSpawn(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class ProcessPanic(msg: String, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class FixpointConstraintSet(cs: List[TypedAst.Constraint], tpe: Type, loc: SourceLocation) extends TypedAst.Expression {
    //          def eff: Type = Type.Pure
    //        }
    //
    //        case class FixpointCompose(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class FixpointSolve(exp: TypedAst.Expression, stf: Ast.Stratification, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class FixpointProject(name: String, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class FixpointEntails(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression
    //
    //        case class FixpointFold(name: String, exp1: TypedAst.Expression, exp2: TypedAst.Expression, exp3: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression

  }

}
