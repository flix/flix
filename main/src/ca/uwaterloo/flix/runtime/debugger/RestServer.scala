package ca.uwaterloo.flix.runtime.debugger

import java.io.{IOException, ByteArrayOutputStream}
import java.net.InetSocketAddress
import java.nio.file.{Paths, Files}
import java.util.concurrent.Executors

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.runtime.Solver
import ca.uwaterloo.flix.runtime.datastore.IndexedRelation
import com.sun.net.httpserver.{HttpServer, HttpExchange, HttpHandler}
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods

class RestServer(solver: Solver) {

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

        val root = "main/src/ca/uwaterloo/flix/runtime/debugger"

        val inputStream = Files.newInputStream(Paths.get(root + path)) // TODO: Switch on whether we are in a packaged jar.

        if (inputStream == null) {
          throw new IOException(s"Unable to load static resource '$path'.")
        }

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
        // TODO logger.error(s"Unknown mimetype for path $path")
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
      case e: RuntimeException =>
        e.printStackTrace()
      // TODO
      // logger.error("Unknown error during http exchange.", e)
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
   * Returns the name and size of all relations.
   */
  class ViewRelation(name: IndexedRelation) extends JsonHandler {
    def json: JValue = JObject(List(
      JField("name", JString("Hello WORLD")) // TODO
    ))
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
   * Returns the time spent in each compiler phase.
   */
  class GetCompilerPhases extends JsonHandler {
    def json: JValue = JArray(List(
      JObject(List(JField("name", JString("Parser")), JField("time", JInt(322)))),
      JObject(List(JField("name", JString("Weeder")), JField("time", JInt(231)))),
      JObject(List(JField("name", JString("Namer")), JField("time", JInt(86)))),
      JObject(List(JField("name", JString("Linker")), JField("time", JInt(243)))),
      JObject(List(JField("name", JString("Typer")), JField("time", JInt(467)))),
      JObject(List(JField("name", JString("Normalizer")), JField("time", JInt(357)))),
      JObject(List(JField("name", JString("Emitter")), JField("time", JInt(322))))
    ))
  }

  /**
   * Bootstraps the internal http server.
   */
  def start(): Unit = try {
    //  TODO logger.trace("Starting WebServer.")

    val port = 9090

    // bind to the requested port.
    val server = HttpServer.create(new InetSocketAddress(port), 0) // TODO: port

    Console.println("Attached debugger to http://localhost:" + port + "/")

    // mount ajax handlers.
    server.createContext("/relations", new GetRelations())
    for ((name, relation) <- solver.dataStore.relations) {
      server.createContext("/relation/" + name, new ViewRelation(relation))
    }

    server.createContext("/lattices", new GetLattices())
    server.createContext("/compiler/phases", new GetCompilerPhases())

    // mount file handler.
    server.createContext("/", new FileHandler())

    // ensure that multiple threads are used.
    server.setExecutor(Executors.newCachedThreadPool())

    // start server.
    server.start()
  } catch {
    case e: IOException =>
      // TODO
      // logger.error(s"Unable to start web server. The REST API will not be available. Error: ${e.getMessage}", e)
      e.printStackTrace()
  }


}
