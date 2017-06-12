/*
 * Copyright 2015-2016 Magnus Madsen
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

package ca.uwaterloo.flix.runtime.debugger

import java.io.{IOException, ByteArrayOutputStream}
import java.net.{BindException, InetSocketAddress}
import java.nio.file.{Paths, Files}
import java.util.concurrent.Executors

import ca.uwaterloo.flix.runtime.{Value, Monitor, Solver}
import ca.uwaterloo.flix.runtime.datastore.{IndexedLattice, IndexedRelation}

import com.sun.net.httpserver.{HttpServer, HttpExchange, HttpHandler}

import org.json4s.JsonAST._
import org.json4s.native.JsonMethods

/**
  * A built-in HTTP REST server that provides a JSON interface to debugging facilities of Flix.
  *
  * Usage of this class may incur additional solver overhead.
  */
class RestServer(solver: Solver) {

  /**
    * The minimum port number to bind to.
    */
  val MinPort = 8000

  /**
    * The maximum port number to bind to.
    */
  val MaxPort = 8100

  /**
    * A collection of static resources included in the Jar.
    */
  val StaticResources = Set[String](
    // HTML
    "/web/index.html",

    // Stylesheet
    "/web/css/bootstrap.min.css",
    "/web/css/bootstrap-theme.min.css",

    // JavaScript
    "/web/js/app.js",
    "/web/js/lib/jquery.min.js",
    "/web/js/lib/bootstrap.min.js",
    "/web/js/lib/react.min.js",
    "/web/js/lib/react-dom.min.js",
    "/web/js/lib/chart.min.js",
    "/web/js/lib/numeral.min.js",

    // Fonts
    "/web/fonts/glyphicons-halflings-regular.woff2",
    "/web/fonts/glyphicons-halflings-regular.woff",
    "/web/fonts/glyphicons-halflings-regular.ttf"
  )

  /**
    * A simple http handler which serves static resources.
    */
  class FileHandler extends HttpHandler {

    /**
      * A loaded resources is an array of bytes and its associated mimetype.
      */
    case class LoadedResource(bytes: Array[Byte], mimetype: String)

    /**
      * Immediately loads all the given `resources` into memory.
      */
    def loadResources(resources: Set[String]): Map[String, LoadedResource] = StaticResources.foldLeft(Map.empty[String, LoadedResource]) {
      case (m, path) =>

        // load the resource from the source directory (if it exists).
        // otherwise load the resource from the JAR.
        val localPath = Paths.get("main/src/ca/uwaterloo/flix/runtime/debugger" + path)
        val inputStream =
          if (Files.exists(localPath))
            Files.newInputStream(localPath)
          else
            getClass.getResourceAsStream("/ca/uwaterloo/flix/runtime/debugger" + path)

        if (inputStream == null) {
          throw new IOException(s"Unable to load static resource '$path'.")
        }

        // load the file.
        val buffer = new ByteArrayOutputStream()
        var byte = inputStream.read()
        while (byte != -1) {
          buffer.write(byte)
          byte = inputStream.read()
        }

        m + (path -> LoadedResource(buffer.toByteArray, mimetypeOf(path)))
    }

    /**
      * All resources are loaded upon startup.
      */
    val LoadedResources: Map[String, LoadedResource] = loadResources(StaticResources)

    /**
      * Returns the mime-type corresponding to the given `path`.
      */
    def mimetypeOf(path: String): String = path match {
      case p if p.endsWith(".css") => "text/css"
      case p if p.endsWith(".js") => "text/javascript; charset=utf-8"
      case p if p.endsWith(".jsx") => "text/javascript; charset=utf-8"
      case p if p.endsWith(".html") => "text/html; charset=utf-8"
      case p if p.endsWith(".png") => "image/png"
      case p if p.endsWith(".ttf") => "font/opentype"
      case p if p.endsWith(".woff") => "font/woff"
      case p if p.endsWith(".woff2") => "font/woff2"
      case _ =>
        throw new RuntimeException(s"Unknown mimetype for path $path")
    }

    /**
      * Handles every incoming http request.
      */
    def handle(t: HttpExchange): Unit = try {
      // construct the local path
      val requestPath = t.getRequestURI.getPath

      // rewrite / to /index.html
      val path = if (requestPath == "/")
        "/web/index.html"
      else
        "/web" + requestPath

      // lookup the requested resource
      LoadedResources.get(path) match {
        case None =>
          t.sendResponseHeaders(404, 0)
          t.close()

        case Some(LoadedResource(bytes, mimetype)) =>
          t.getResponseHeaders.add("Content-Type", mimetype)
          t.getResponseHeaders.add("Cache-Control", "max-age=" + (31 * 24 * 60 * 60))

          t.sendResponseHeaders(200, bytes.length)
          val outputStream = t.getResponseBody
          outputStream.write(bytes)
          outputStream.close()
          t.close()
      }
    } catch {
      case e: RuntimeException => throw new RuntimeException("Exception in RestServer.", e)
    }

  }

  /**
    * A simple http handler which serves JSON.
    */
  abstract class JsonHandler extends HttpHandler {
    /**
      * An abstract method which returns the JSON object to be sent.
      */
    def json: JValue

    /**
      * Handles every incoming http request.
      */
    def handle(t: HttpExchange): Unit = {
      t.getResponseHeaders.add("Content-Type", "application/javascript")

      val data = JsonMethods.pretty(JsonMethods.render(json))
      t.sendResponseHeaders(200, data.length())

      val outputStream = t.getResponseBody
      outputStream.write(data.getBytes)
      outputStream.close()
      t.close()
    }
  }

  /**
    * Returns the current status of the computation.
    */
  class GetStatus extends JsonHandler {
    def json: JValue = JObject(List(
      if (solver.worklist.nonEmpty)
        JField("status", JString("running"))
      else
        JField("status", JString("complete"))
    ))
  }

  /**
    * Returns the name and size of all relations.
    */
  class GetRelations extends JsonHandler {
    def json: JValue = JArray(solver.dataStore.relations.toList.map {
      case (name, relation) => JObject(List(
        JField("name", JString(name.toString)),
        JField("size", JInt(relation.getSize))
      ))
    })
  }

  /**
    * Returns the name and size of all lattices.
    */
  class GetLattices extends JsonHandler {
    def json: JValue = JArray(solver.dataStore.lattices.toList.map {
      case (name, lattice) => JObject(List(
        JField("name", JString(name.toString)),
        JField("size", JInt(lattice.getSize))
      ))
    })
  }

  /**
    * Returns all the rows in the relation.
    *
    * @param relation a reference to the datastore backing the relation.
    */
  class ListRelation(relation: IndexedRelation[AnyRef]) extends JsonHandler {
    def json: JValue = JObject(
      JField("cols", JArray(relation.relation.attributes.toList.map(a => JString(a.name)))),
      JField("rows", JArray(relation.scan.toList.map {
        case row => JArray(row.toList.map(e => JString(Value.pretty(e))))
      })))
  }

  /**
    * Returns all the rows in the lattice.
    *
    * @param lattice a reference to the datastore backing the lattice.
    */
  class ListLattice(lattice: IndexedLattice[AnyRef]) extends JsonHandler {
    def json: JValue = JObject(
      JField("cols", JArray(lattice.lattice.keys.toList.map(a => JString(a.name)) ::: JString(lattice.lattice.value.name) :: Nil)),
      JField("rows", JArray(lattice.scan.toList.map {
        case (key, elm) => JArray(key.toArray.map(k => JString(Value.pretty(k))).toList ::: JString(Value.pretty(elm)) :: Nil)
      })))
  }

  /**
    * Returns a list of telemetry samples.
    */
  class GetTelemetry extends JsonHandler {
    def json: JValue = JArray(solver.monitor.getTelemetry.reverse.map {
      case Monitor.Sample(time, readTasks, writeTasks, facts, memory) =>
        JObject(List(
          JField("time", JInt(time / 1000000)),
          JField("readTasks", JInt(readTasks)),
          JField("writeTasks", JInt(writeTasks)),
          JField("facts", JInt(facts)),
          JField("memory", JInt(memory))
        ))
    })
  }

  /**
    * Returns rule performance statistics.
    */
  class GetRulePerformance extends JsonHandler {
    def json: JValue = JArray(solver.getRuleStats.map {
      case (rule, hits, time) => JObject(List(
        JField("rule", JString(rule.head.loc.lineAt(rule.head.loc.beginLine))),
        JField("loc", JString(rule.head.loc.format)),
        JField("hits", JInt(hits)),
        JField("time", JInt(time / 1000000))
      ))
    })
  }

  /**
    * Returns predicate performance statistics.
    */
  class GetPredicatePerformance extends JsonHandler {
    def json: JValue = JArray(solver.dataStore.predicateStats.map {
      case (name, size, indexedLookups, indexedScans, fullScans) => JObject(List(
        JField("name", JString(name)),
        JField("size", JInt(size)),
        JField("indexedLookups", JInt(indexedLookups)),
        JField("indexedScans", JInt(indexedScans)),
        JField("fullScans", JInt(fullScans))
      ))
    })
  }

  /**
    * Returns index hits statistics.
    */
  class GetIndexHits extends JsonHandler {
    def json: JValue = JArray(solver.dataStore.indexHits.map {
      case (name, index, hits) => JObject(List(
        JField("name", JString(name)),
        JField("index", JString(index)),
        JField("hits", JInt(hits))
      ))
    })
  }

  /**
    * Returns index misses statistics.
    */
  class GetIndexMisses extends JsonHandler {
    def json: JValue = JArray(solver.dataStore.indexMisses.map {
      case (name, index, misses) => JObject(List(
        JField("name", JString(name)),
        JField("index", JString(index)),
        JField("misses", JInt(misses))
      ))
    })
  }

  /**
    * Returns compiler performance statistics.
    */
  class GetCompilerPhasePerformance extends JsonHandler {
    def json: JValue = JArray(List(
      JObject(List(JField("name", JString("Parser")), JField("time", JInt(solver.root.time.parser / 1000000)))),
      JObject(List(JField("name", JString("Weeder")), JField("time", JInt(solver.root.time.weeder / 1000000)))),
      JObject(List(JField("name", JString("Namer")), JField("time", JInt(solver.root.time.namer / 1000000)))),
      JObject(List(JField("name", JString("Resolver")), JField("time", JInt(solver.root.time.resolver / 1000000)))),
      JObject(List(JField("name", JString("Typer")), JField("time", JInt(solver.root.time.typer / 1000000)))),
      JObject(List(JField("name", JString("Pattern Exhaustiveness")), JField("time", JInt(solver.root.time.patsExhaustive / 1000000)))),
      JObject(List(JField("name", JString("Documentor")), JField("time", JInt(solver.root.time.documentor / 1000000)))),
      JObject(List(JField("name", JString("Stratifier")), JField("time", JInt(solver.root.time.stratifier / 1000000)))),
      JObject(List(JField("name", JString("Monomorph")), JField("time", JInt(solver.root.time.monomorph / 1000000)))),
      JObject(List(JField("name", JString("PropertyGen")), JField("time", JInt(solver.root.time.propertyGen / 1000000)))),
      JObject(List(JField("name", JString("Verifier")), JField("time", JInt(solver.root.time.verifier / 1000000)))),
      JObject(List(JField("name", JString("LambdaLift")), JField("time", JInt(solver.root.time.lambdaLift / 1000000)))),
      JObject(List(JField("name", JString("TailRec")), JField("time", JInt(solver.root.time.tailrec / 1000000)))),
      JObject(List(JField("name", JString("Simplifier")), JField("time", JInt(solver.root.time.simplifier / 1000000)))),
      JObject(List(JField("name", JString("TreeShaker")), JField("time", JInt(solver.root.time.treeshaker / 1000000)))),
      JObject(List(JField("name", JString("VarNumbering")), JField("time", JInt(solver.root.time.varNumbering / 1000000)))),
      JObject(List(JField("name", JString("CodeGen")), JField("time", JInt(solver.root.time.codeGen / 1000000))))
    ))
  }

  /**
    * Bootstraps the internal http server.
    */
  def start(): Unit = {
    // initialize server.
    val server = newServer(MinPort, MaxPort)
    val port = server.getAddress.getPort
    Console.println(s"Attached debugger to http://localhost:$port/.")

    // mount ajax handlers.
    server.createContext("/status", new GetStatus())
    server.createContext("/relations", new GetRelations())
    for ((name, relation) <- solver.dataStore.relations) {
      server.createContext("/relation/" + name, new ListRelation(relation))
    }
    server.createContext("/lattices", new GetLattices())
    for ((name, lattice) <- solver.dataStore.lattices) {
      server.createContext("/lattice/" + name, new ListLattice(lattice))
    }
    server.createContext("/telemetry", new GetTelemetry())
    server.createContext("/performance/rules", new GetRulePerformance())
    server.createContext("/performance/predicates", new GetPredicatePerformance())
    server.createContext("/performance/index/hits", new GetIndexHits())
    server.createContext("/performance/index/misses", new GetIndexMisses())
    server.createContext("/compiler/phases", new GetCompilerPhasePerformance())

    // mount file handler.
    server.createContext("/", new FileHandler())

    // ensure that multiple threads are used.
    server.setExecutor(Executors.newCachedThreadPool())

    // start server.
    server.start()
  }

  /**
    * Returns a new HttpServer bound to a port between the given `minPort` and `maxPort`.
    */
  private def newServer(minPort: Int, maxPort: Int): HttpServer = {
    assert(minPort <= maxPort)

    for (port <- minPort to maxPort) {
      try {
        return HttpServer.create(new InetSocketAddress(port), 0)
      } catch {
        case e: BindException => // nop - try next port.
      }
    }

    throw new IOException(s"Unable to find an available port between $minPort and $maxPort.")
  }

}
