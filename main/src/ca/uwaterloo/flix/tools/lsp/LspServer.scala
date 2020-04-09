package ca.uwaterloo.flix.tools.lsp

import java.net.InetSocketAddress
import java.text.SimpleDateFormat
import java.util.Date

import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, Root}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.{InternalCompilerException, InternalRuntimeException, Options}
import org.java_websocket.WebSocket
import org.java_websocket.handshake.ClientHandshake
import org.java_websocket.server.WebSocketServer
import org.json4s.JsonAST.{JBool, JString}
import org.json4s.ParserUtil.ParseException
import org.json4s.native.JsonMethods.parse

import scala.collection.mutable

// TODO: DOC
class LspServer(port: Int) extends WebSocketServer(new InetSocketAddress(port)) {

  // TODO: Start socket server on port.
  // TODO: Accept request telling what files to compile, return compilation status.
  // TODO: Accept queries.

  /**
    * The custom date format to use for logging.
    */
  val DateFormat: String = "yyyy-MM-dd HH:mm:ss"

  /**
    * Invoked when the server is started.
    */
  override def onStart(): Unit = {
    Console.println(s"WebSocket server listening on: ws://localhost:$port")
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
      processRequest(request)(ws)
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

    val requestType = json \\ "type" match {
      case JString(s) => s
      case _ => ""
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
  private def processRequest(request: Request)(implicit ws: WebSocket): Unit = {
    // TODO
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




  case class Location(line: Int, col: Int)

  def toLoc(loc: SourceLocation): Location = ???

  class Tree(root: Root) {

    private val m = mutable.Map.empty[Location, Expression]
    private val types = mutable.Map.empty[Location, Type]
    private val defn = mutable.Map.empty[Location, Symbol.DefnSym]


    def visitExp(exp0: Expression): Index = exp0 match {
      case Expression.Unit(_) => Index.of(exp0)

      case Expression.True(_) => Index.of(exp0)

      case Expression.False(_) => Index.of(exp0)

      case Expression.Char(_, _) => Index.of(exp0)

      case Expression.Float32(_, _) => Index.of(exp0)

      case Expression.Float64(_, _) => Index.of(exp0)

      //        case class Int8(lit: scala.Byte, loc: SourceLocation) extends TypedAst.Expression {
      //          def tpe: Type = Type.Int8
      //
      //          def eff: Type = Type.Pure
      //        }
      //
      //        case class Int16(lit: scala.Short, loc: SourceLocation) extends TypedAst.Expression {
      //          def tpe: Type = Type.Int16
      //
      //          def eff: Type = Type.Pure
      //        }
      //
      //        case class Int32(lit: scala.Int, loc: SourceLocation) extends TypedAst.Expression {
      //          def tpe: Type = Type.Int32
      //
      //          def eff: Type = Type.Pure
      //        }
      //
      //        case class Int64(lit: scala.Long, loc: SourceLocation) extends TypedAst.Expression {
      //          def tpe: Type = Type.Int64
      //
      //          def eff: Type = Type.Pure
      //        }
      //
      //        case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends TypedAst.Expression {
      //          def tpe: Type = Type.BigInt
      //
      //          def eff: Type = Type.Pure
      //        }
      //
      //        case class Str(lit: java.lang.String, loc: SourceLocation) extends TypedAst.Expression {
      //          def tpe: Type = Type.Str
      //
      //          def eff: Type = Type.Pure
      //        }
      //

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

    def getSym(loc: Location): Option[Symbol.DefnSym] = ???

    def getTyp(loc: Location): Option[Type] = ???

    def getExp(loc: Location): Option[Expression] = ???


  }

}
