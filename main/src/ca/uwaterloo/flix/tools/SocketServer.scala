/*
 * Copyright 2019 Magnus Madsen
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
package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.{Flix, Version}
import ca.uwaterloo.flix.util.Formatter.NoFormatter
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util._
import org.java_websocket.WebSocket
import org.java_websocket.handshake.ClientHandshake
import org.java_websocket.server.WebSocketServer
import org.json4s.JsonAST._
import org.json4s.ParserUtil.ParseException
import org.json4s.native.JsonMethods
import org.json4s.native.JsonMethods._

import java.net.InetSocketAddress
import java.text.SimpleDateFormat
import java.util.Date

/**
  * A WebSocket server implementation that receives and evaluates Flix programs.
  *
  * @param port the local port to listen on.
  */
class SocketServer(port: Int) extends WebSocketServer(new InetSocketAddress(port)) {

  /**
    * The custom date format to use for logging.
    */
  val DateFormat: String = "yyyy-MM-dd HH:mm:ss"

  /**
    * The Flix instance (the same instance is used for incremental compilation).
    */
  private val flix: Flix = new Flix().setFormatter(NoFormatter)

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
      (src, opts) <- parseRequest(data)(ws)
    } yield {
      processRequest(src, opts)(ws)
    }
  }

  /**
    * Invoked when an error occurs.
    */
  override def onError(ws: WebSocket, e: Exception): Unit = e match {
    case _: InternalCompilerException =>
      log(s"Unexpected error: ${e.getMessage}")(ws)
      e.printStackTrace()
    case _: RuntimeException =>
      log(s"Unexpected error: ${e.getMessage}")(ws)
      e.printStackTrace()
    case ex => throw ex
  }

  /**
    * Parse the request.
    */
  private def parseRequest(s: String)(implicit ws: WebSocket): Option[(String, Options)] = try {
    // Parse the string into a json object.
    val json = parse(s)

    // Retrieve the source code.
    val src = json \\ "src" match {
      case JString(s) => s
      case _ => ""
    }

    // --Xcore
    val xcore = json \\ "xcore" match {
      case JBool(b) => b
      case _ => false
    }

    // Construct the options object.
    val opts = Options.Default.copy(
      lib = if (xcore) LibLevel.Min else LibLevel.All
    )

    // Return the source and options.
    Some((src, opts))
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
  private def processRequest(src: String, opts: Options)(implicit ws: WebSocket): Unit = {
    // Log the string.
    for (line <- src.split("\n")) {
      log("  >  " + line)(ws)
    }

    // Evaluate the string.
    val result = eval(src, opts)(ws)

    // Log whether evaluation was successful.
    log("")(ws)
    result match {
      case Ok(_) => log("Evaluation was successful. Sending response:")(ws)
      case Err(_) => log("Evaluation failure. Sending response:")(ws)
    }
    log("")(ws)

    // Convert the result to JSON.
    val json = JsonMethods.pretty(JsonMethods.render(getJSON(result)))

    // Log the JSON data.
    for (line <- json.split("\n")) {
      log("  <  " + line)(ws)
    }

    // And finally send the JSON data.
    ws.send(json)
  }

  /**
    * Evaluates the given string `input` as a Flix program.
    */
  private def eval(input: String, opts: Options)(implicit ws: WebSocket): Result[(String, Long, Long), String] = {
    try {
      // Compile the program.
      flix.addSourceCode("<input>", input)
      flix.setOptions(opts)

      flix.compile() match {
        case Success(compilationResult) =>
          // Compilation was successful.

          // Determine if the main function is present.
          compilationResult.getMain match {
            case None =>
              // The main function was not present. Just report successful compilation.
              Ok("Compilation was successful. No main function to run.", compilationResult.totalTime, 0L)
            case Some(main) =>
              // Evaluate the main function and get the result as a string.
              val timer = new Timer({
                val (_, stdOut, stdErr) = SafeExec.execute(() => main(Array.empty))
                stdOut + stdErr
              })
              Ok(timer.getResult, compilationResult.totalTime, timer.getElapsed)
          }

        case failure =>
          // Compilation failed. Retrieve and format the first error message.
          Err(failure.errors.head.message(flix.getFormatter))
      }
    } catch {
      case ex: RuntimeException => Err(ex.getMessage)
    }
  }

  /**
    * Returns the given `result` as a JSON object.
    */
  private def getJSON(result: Result[(String, Long, Long), String]): JObject =
    result match {
      case Ok((msg, compilationTime, evaluationTime)) => JObject(
        JField("status", JString("success")),
        JField("result", JString(msg)),
        JField("version", JString(getVersionWithPort)),
        JField("compilationTime", JLong(compilationTime)),
        JField("evaluationTime", JLong(evaluationTime)),
      )
      case Err(msg) => JObject(
        JField("status", JString("failure")),
        JField("result", JString(msg)),
        JField("version", JString(getVersionWithPort)),
      )
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
    * Returns the current Flix version.
    */
  private def getVersionWithPort: String = "flix-" + Version.CurrentVersion.toString + " on port " + port

}
