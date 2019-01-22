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
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.util.{Options, Validation}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.vt.TerminalContext

import java.net.InetSocketAddress

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

  override def onStart(): Unit = {
    Console.println(s"WebSocket server listening on: ws://localhost:$port")
  }

  override def onOpen(ws: WebSocket, ch: ClientHandshake): Unit = {
    log("Connected.", ws)
  }

  override def onClose(ws: WebSocket, i: Int, s: String, b: Boolean): Unit = {
    log("Disconnected.", ws)
  }

  override def onMessage(ws: WebSocket, s: String): Unit = {
    // Print the length and size of the received data.
    log(s"Received ${s.length} characters (${s.getBytes.length} bytes of data).", ws)

    // Print the source code.
    for (line <- s.lines) {
      log("  >  " + line, ws)
    }

    // Evaluate the source code.
    val result = run(s, ws)

    log("Result available.", ws)

    // Convert the JSON result to a string.
    val data = JsonMethods.pretty(JsonMethods.render(result))

    // Print the result.
    for (line <- data.lines) {
      log("  <  " + line, ws)
    }

    // Send the result.
    ws.send(data)
  }

  override def onError(ws: WebSocket, e: Exception): Unit = {
    log("Error", ws)
  }

  private def log(s: String, ws: WebSocket): Unit = {
    val clientPart = s"[${ws.getRemoteSocketAddress}]"
    Console.println(s"$clientPart $s")
  }

  private def run(input: String, ws: WebSocket): JObject = {
    eval(input, ws) match {
      case Success(result) =>
        JObject(
          JField("status", JString("success")),
          JField("result", JString(result)),
        )
      case Failure(err) =>
        JObject(
          JField("status", JString("success")),
          JField("result", JString(err)),
        )
    }
  }

  private def eval(input: String, ws: WebSocket): Validation[String, String] = {
    try {
      // Instantiate fresh Flix instance.
      val flix = mkFlix(input)

      // Evaluate the Flix program.
      flix.compile() match {
        case Success(compilationResult) =>

          // Evaluate the main function.
          compilationResult.evalToString("main").toSuccess

        case Failure(errors) =>
          errors.head.message.fmt(TerminalContext.NoTerminal).toFailure
      }
    } catch {
      case ex: RuntimeException =>
        Console.err.println(ex)
        ex.getMessage.toFailure
    }
  }

  private def mkFlix(input: String): Flix = {
    val flix = new Flix()
    val opts = Options.Default.copy(writeClassFiles = false)
    flix.setOptions(opts)
    flix.addStr(input)
  }

}
