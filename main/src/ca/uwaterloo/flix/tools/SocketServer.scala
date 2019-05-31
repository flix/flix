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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.{Options, Result}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.vt.TerminalContext
import java.net.InetSocketAddress
import java.text.SimpleDateFormat
import java.util.Date

import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.java_websocket.WebSocket
import org.java_websocket.handshake.ClientHandshake
import org.java_websocket.server.WebSocketServer
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods

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
  override def onMessage(ws: WebSocket, s: String): Unit = {
    // Log the length and size of the received data.
    log(s"Received ${s.length} characters of input (${s.getBytes.length} bytes).")(ws)

    // Log the string.
    for (line <- s.split("\n")) {
      log("  >  " + line)(ws)
    }

    // Evaluate the string.
    val result = eval(s)(ws)

    // Log whether evaluation was successful.
    log("")(ws)
    result match {
      case Ok(__) => log("Evaluation was successful. Sending response:")(ws)
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
    * Invoked when an error occurs.
    */
  override def onError(ws: WebSocket, e: Exception): Unit = {
    log(s"Unexpected error: ${e.getMessage}")(ws)
    e.printStackTrace()
  }

  /**
    * Evaluates the given string `input` as a Flix program.
    */
  private def eval(input: String)(implicit ws: WebSocket): Result[String, String] = {
    try {
      // Compile the program.
      mkFlix(input).compile() match {
        case Success(compilationResult) =>
          // Compilation was successful.

          // Determine if the main function is present.
          compilationResult.getMain match {
            case None =>
              // The main function was not present. Just report successful compilation.
              Ok("Compilation was successful. No main function to run.")
            case Some(_) =>
              // Evaluate the main function and get the result as a string.
              Ok(compilationResult.evalToString("main"))
          }

        case Failure(errors) =>
          // Compilation failed. Retrieve and format the first error message.
          Err(errors.head.message.fmt(TerminalContext.NoTerminal))
      }
    } catch {
      case ex: RuntimeException => Err(ex.getMessage)
    }
  }

  /**
    * Returns the given `result` as a JSON object.
    */
  private def getJSON(result: Result[String, String]): JObject =
    result match {
      case Ok(msg) => JObject(
        JField("status", JString("success")),
        JField("result", JString(msg)),
      )
      case Err(msg) => JObject(
        JField("status", JString("failure")),
        JField("result", JString(msg)),
      )
    }

  /**
    * Returns a fresh Flix object for the given `input` string.
    */
  private def mkFlix(input: String)(implicit ws: WebSocket): Flix = {
    val flix = new Flix()
    val opts = Options.Default.copy(writeClassFiles = false, xallowredundancies = true)
    flix.setOptions(opts)
    flix.addStr(input)
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
