/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.util

import java.net.InetSocketAddress
import java.time.Duration
import java.util.concurrent.{Executors, TimeUnit}

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.Validation.{Failure, Success}
import ca.uwaterloo.flix.util.vt.TerminalContext
import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods

/**
  * A simple web server that listens on the given `port` and evaluates Flix programs in response to requests.
  */
class RpcServer(port: Int) {

  /**
    * The HTTP server associated with `this` instance.
    */
  private var server: HttpServer = _

  class SolverHandler extends HttpHandler {

    /**
      * The default solver timeout.
      */
    val SolverTimeout: Duration = Duration.ofSeconds(10)

    /**
      * Evaluates the given `input` program and returns a JSON object with the result.
      */
    def solve(input: String): JValue = {
      try {
        // Instantiate fresh Flix instance.
        val flix = new Flix()
        val opts = Options.Default.copy(timeout = Some(SolverTimeout))
        flix.setOptions(opts)
        flix.addStr(input)

        // Evaluate the Flix program.
        flix.compile() match {
          case Success(compilationResult) =>

            // Evaluate the main function.
            val result = compilationResult.evalToString("f")

            // Translate the computed relations to JSON data.
            val relations = Nil // TODO: RpcServer needs access to tables.

            // Translate the computed lattices to JSON data.
            val lattices = Nil // TODO: RpcServer needs access to tables.

            JObject(
              JField("status", JString("success")),
              JField("result", if (result == null) JNull else JString(result)),
              JField("relations", JArray(relations.toList)),
              JField("lattices", JArray(lattices.toList))
            )
          case Failure(errors) =>
            JObject(
              JField("status", JString("failure")),
              JField("message", JString(errors.head.message.fmt(TerminalContext.NoTerminal)))
            )
        }
      } catch {
        case ex: RuntimeException =>
          Console.err.println(ex)
          JObject(
            JField("status", JString("failure")),
            JField("message", JString(ex.getMessage))
          )
      }
    }

    /**
      * Handles every incoming http request.
      */
    def handle(t: HttpExchange): Unit = {
      // Read the input program from the request body.
      val input = StreamOps.readAll(t.getRequestBody)

      // Print debugging information about the request.
      Console.println(s"Received ${input.getBytes.length} bytes of input from ${t.getRemoteAddress.getHostString}.")

      // Evaluate the input program.
      val result = solve(input)

      // Send the JSON response.
      t.getResponseHeaders.add("Content-Type", "application/javascript")
      t.getResponseHeaders.add("Access-Control-Allow-Origin", "*")

      val data = JsonMethods.pretty(JsonMethods.render(result))
      t.sendResponseHeaders(200, 0)

      val outputStream = t.getResponseBody
      outputStream.write(data.getBytes)
      outputStream.close()
      t.close()
    }

    /**
      * Returns the relation with the name `fqn` and the rows `rs` as a JSON object.
      */
    private def relation2json(fqn: String, rs: Iterable[List[String]]): JValue = {
      val rows = rs.map {
        case row => JArray(row.map(v => JString(v)))
      }
      JObject(JField("name", JString(fqn)), JField("rows", JArray(rows.toList)))
    }

    /**
      * Returns the lattice with the name `fqn` and the rows `rs` as a JSON object.
      */
    private def lattice2json(fqn: String, rs: Iterable[List[String]]): JValue = {
      val rows = rs.map {
        case row => JArray(row.map(v => JString(v)))
      }
      JObject(JField("name", JString(fqn)), JField("rows", JArray(rows.toList)))
    }
  }

  /**
    * Bootstraps the internal http server.
    */
  def start(): Unit = {
    // Create the http server.
    server = HttpServer.create(new InetSocketAddress(port), 0)

    // Emit some debugging information.
    Console.println(s"Listening for connections on port $port.")

    // Mount the solver context.
    server.createContext("/", new SolverHandler())

    // Use a thread pool with multiple threads.
    server.setExecutor(Executors.newCachedThreadPool())

    // Start server!
    server.start()
  }

  /**
    * Waits for the server to shutdown.
    */
  def await(): Unit = {
    while (!Thread.currentThread().isInterrupted) {
      Thread.sleep(1000 * 1000)
    }

    if (server != null) {
      server.stop(0)
    }
  }

}
