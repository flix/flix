/*
 * Copyright 2026 Magnus Madsen
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

package ca.uwaterloo.flix

import ca.uwaterloo.flix.util.{FlixSuite, Options, StdlibProfile}
import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import org.scalatest.BeforeAndAfterAll

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.concurrent.{ExecutorService, Executors}

/**
  * Runtime smoke tests for the portable HTTP primop on the JVM backend.
  *
  * These tests use an in-process HTTP server bound to an ephemeral port (no external network dependency).
  */
class PortableHttpSuite extends FlixSuite(incremental = false) with BeforeAndAfterAll {

  private implicit val TestOptions: Options =
    Options.TestWithLibAll.copy(stdlibProfile = StdlibProfile.Portable)

  private var server: HttpServer = _
  private var executor: ExecutorService = _
  private var baseUrl: String = _

  private val testFile: Path = Files.createTempFile("flix-portable-http-", ".flix")

  // Register a single Flix test file (written in beforeAll).
  mkTest(testFile.toString, None)

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    executor = Executors.newCachedThreadPool()

    server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    server.setExecutor(executor)

    server.createContext("/hello", new HttpHandler {
      override def handle(exchange: HttpExchange): Unit = {
        val bytes = "hello".getBytes(StandardCharsets.UTF_8)
        exchange.getResponseHeaders.add("X-Foo", "bar")
        exchange.sendResponseHeaders(200, bytes.length)
        val os = exchange.getResponseBody
        os.write(bytes)
        os.close()
      }
    })

    server.createContext("/multi", new HttpHandler {
      override def handle(exchange: HttpExchange): Unit = {
        val bytes = "multi".getBytes(StandardCharsets.UTF_8)
        exchange.getResponseHeaders.add("X-Multi", "v1")
        exchange.getResponseHeaders.add("X-Multi", "v2")
        exchange.sendResponseHeaders(200, bytes.length)
        val os = exchange.getResponseBody
        os.write(bytes)
        os.close()
      }
    })

    server.createContext("/head", new HttpHandler {
      override def handle(exchange: HttpExchange): Unit = {
        exchange.getResponseHeaders.add("X-Head", "yes")
        // A HEAD response has no message-body.
        exchange.sendResponseHeaders(200, -1)
        exchange.close()
      }
    })

    server.createContext("/method", new HttpHandler {
      override def handle(exchange: HttpExchange): Unit = {
        val in = exchange.getRequestBody
        in.readAllBytes()
        in.close()

        val bytes = exchange.getRequestMethod.getBytes(StandardCharsets.UTF_8)
        exchange.sendResponseHeaders(200, bytes.length)
        val os = exchange.getResponseBody
        os.write(bytes)
        os.close()
      }
    })

    server.createContext("/redir302", new HttpHandler {
      override def handle(exchange: HttpExchange): Unit = {
        val in = exchange.getRequestBody
        in.readAllBytes()
        in.close()

        exchange.getResponseHeaders.add("Location", "/method")
        exchange.sendResponseHeaders(302, -1)
        exchange.close()
      }
    })

    server.createContext("/redir302reqheader", new HttpHandler {
      override def handle(exchange: HttpExchange): Unit = {
        val in = exchange.getRequestBody
        in.readAllBytes()
        in.close()

        exchange.getResponseHeaders.add("Location", "/reqheader")
        exchange.sendResponseHeaders(302, -1)
        exchange.close()
      }
    })

    server.createContext("/redir-loop", new HttpHandler {
      override def handle(exchange: HttpExchange): Unit = {
        val in = exchange.getRequestBody
        in.readAllBytes()
        in.close()

        exchange.getResponseHeaders.add("Location", "/redir-loop")
        exchange.sendResponseHeaders(302, -1)
        exchange.close()
      }
    })

    server.createContext("/redir303", new HttpHandler {
      override def handle(exchange: HttpExchange): Unit = {
        val in = exchange.getRequestBody
        in.readAllBytes()
        in.close()

        exchange.getResponseHeaders.add("Location", "/method")
        exchange.sendResponseHeaders(303, -1)
        exchange.close()
      }
    })

    server.createContext("/redir307", new HttpHandler {
      override def handle(exchange: HttpExchange): Unit = {
        val in = exchange.getRequestBody
        in.readAllBytes()
        in.close()

        exchange.getResponseHeaders.add("Location", "/method")
        exchange.sendResponseHeaders(307, -1)
        exchange.close()
      }
    })

    server.createContext("/redir-unsupported-scheme", new HttpHandler {
      override def handle(exchange: HttpExchange): Unit = {
        val in = exchange.getRequestBody
        in.readAllBytes()
        in.close()

        exchange.getResponseHeaders.add("Location", "ftp://example.com/")
        exchange.sendResponseHeaders(302, -1)
        exchange.close()
      }
    })

    server.createContext("/echo", new HttpHandler {
      override def handle(exchange: HttpExchange): Unit = {
        val in = exchange.getRequestBody
        val body = in.readAllBytes()
        in.close()

        exchange.sendResponseHeaders(200, body.length)
        val os = exchange.getResponseBody
        os.write(body)
        os.close()
      }
    })

    server.createContext("/chunked", new HttpHandler {
      override def handle(exchange: HttpExchange): Unit = {
        val bytes = "chunked".getBytes(StandardCharsets.UTF_8)
        // Use chunked transfer-encoding (length = 0).
        exchange.sendResponseHeaders(200, 0)
        val os = exchange.getResponseBody
        os.write(bytes)
        os.close()
      }
    })

    server.createContext("/invalid-utf8", new HttpHandler {
      override def handle(exchange: HttpExchange): Unit = {
        val bytes = Array[Byte](0xC3.toByte, 0x28.toByte) // invalid UTF-8 sequence
        exchange.sendResponseHeaders(200, bytes.length)
        val os = exchange.getResponseBody
        os.write(bytes)
        os.close()
      }
    })

    server.createContext("/reqheader", new HttpHandler {
      override def handle(exchange: HttpExchange): Unit = {
        val v = Option(exchange.getRequestHeaders.getFirst("X-Req")).getOrElse("")
        val bytes = v.getBytes(StandardCharsets.UTF_8)
        exchange.sendResponseHeaders(200, bytes.length)
        val os = exchange.getResponseBody
        os.write(bytes)
        os.close()
      }
    })

    server.start()
    baseUrl = s"http://127.0.0.1:${server.getAddress.getPort}"

    val program =
      s"""
         |mod Test.Portable.HttpRuntime {
         |
         |    def expect(cond: Bool, msg: String): Unit =
         |        if (cond) () else bug!(msg)
         |
         |    @Test
         |    def hello01(): Unit \\ IO = {
         |        match Http.runWithIO(() -> Http.get("${baseUrl}/hello", Map.empty())) {
         |            case Ok(r) => {
         |                let hs = Http.Response.headers(r);
         |                expect(
         |                    Http.Response.status(r) == 200 and
         |                    Http.Response.body(r) == "hello" and
         |                    Map.get("x-foo", hs) == Some("bar" :: Nil),
         |                    "expected status 200, body 'hello', and header x-foo = bar"
         |                )
         |            }
         |            case Err(e) => bug!("unexpected Err: " + ToString.toString(e))
         |        }
         |    }
         |
         |    @Test
         |    def multiHeader01(): Unit \\ IO = {
         |        match Http.runWithIO(() -> Http.get("${baseUrl}/multi", Map.empty())) {
         |            case Ok(r) => {
         |                let hs = Http.Response.headers(r);
         |                let expected = "v1" :: "v2" :: Nil;
         |                expect(
         |                    Http.Response.body(r) == "multi" and Map.get("x-multi", hs) == Some(expected),
         |                    "expected body 'multi' and header x-multi values [v1, v2]"
         |                )
         |            }
         |            case Err(e) => bug!("unexpected Err: " + ToString.toString(e))
         |        }
         |    }
         |
         |    @Test
         |    def head01(): Unit \\ IO = {
         |        match Http.runWithIO(() -> Http.head("${baseUrl}/head", Map.empty())) {
         |            case Ok(r) => {
         |                let hs = Http.Response.headers(r);
         |                expect(
         |                    Http.Response.status(r) == 200 and
         |                    Http.Response.body(r) == "" and
         |                    Map.get("x-head", hs) == Some("yes" :: Nil),
         |                    "expected HEAD to return empty body and header x-head = yes"
         |                )
         |            }
         |            case Err(e) => bug!("unexpected Err: " + ToString.toString(e))
         |        }
         |    }
         |
         |    @Test
         |    def redirectGet30201(): Unit \\ IO =
         |        match Http.runWithIO(() -> Http.get("${baseUrl}/redir302", Map.empty())) {
         |            case Ok(r) => expect(
         |                Http.Response.status(r) == 200 and Http.Response.body(r) == "GET",
         |                "expected 302 redirect to be followed for GET"
         |            )
         |            case Err(e) => bug!("unexpected Err: " + ToString.toString(e))
         |        }
         |
         |    @Test
         |    def redirectPreservesHeaders30201(): Unit \\ IO = {
         |        let hs = Map.insert("X-Req", "abc" :: Nil, Map.empty());
         |        match Http.runWithIO(() -> Http.get("${baseUrl}/redir302reqheader", hs)) {
         |            case Ok(r) => expect(
         |                Http.Response.status(r) == 200 and Http.Response.body(r) == "abc",
         |                "expected request header X-Req to be preserved across 302 redirect"
         |            )
         |            case Err(e) => bug!("unexpected Err: " + ToString.toString(e))
         |        }
         |    }
         |
         |    @Test
         |    def redirectLoop01(): Unit \\ IO =
         |        match Http.runWithIO(() -> Http.get("${baseUrl}/redir-loop", Map.empty())) {
         |            case Err(_) => ()
         |            case Ok(_) => bug!("expected redirect loop to fail")
         |        }
         |
         |    @Test
         |    def redirectPost30201(): Unit \\ IO =
         |        match Http.runWithIO(() -> Http.post("${baseUrl}/redir302", Map.empty(), "ping")) {
         |            case Ok(r) => expect(
         |                Http.Response.status(r) == 200 and Http.Response.body(r) == "GET",
         |                "expected 302 redirect to be followed and POST to become GET"
         |            )
         |            case Err(e) => bug!("unexpected Err: " + ToString.toString(e))
         |        }
         |
         |    @Test
         |    def redirectPut30301(): Unit \\ IO =
         |        match Http.runWithIO(() -> Http.put("${baseUrl}/redir303", Map.empty(), "ping")) {
         |            case Ok(r) => expect(
         |                Http.Response.status(r) == 200 and Http.Response.body(r) == "GET",
         |                "expected 303 redirect to be followed and method to become GET"
         |            )
         |            case Err(e) => bug!("unexpected Err: " + ToString.toString(e))
         |        }
         |
         |    @Test
         |    def redirectPost30701(): Unit \\ IO =
         |        match Http.runWithIO(() -> Http.post("${baseUrl}/redir307", Map.empty(), "ping")) {
         |            case Ok(r) => expect(
         |                Http.Response.status(r) == 200 and Http.Response.body(r) == "POST",
         |                "expected 307 redirect to be followed and method preserved"
         |            )
         |            case Err(e) => bug!("unexpected Err: " + ToString.toString(e))
         |        }
         |
         |    @Test
         |    def redirectUnsupportedScheme01(): Unit \\ IO =
         |        match Http.runWithIO(() -> Http.get("${baseUrl}/redir-unsupported-scheme", Map.empty())) {
         |            case Err(IoError.IoError(IoError.ErrorKind.Unsupported, _)) => ()
         |            case Err(IoError.IoError(kind, msg)) => bug!("expected Unsupported, got " + ToString.toString(kind) + ": " + msg)
         |            case Ok(_) => bug!("expected Unsupported, got Ok(_)")
         |        }
         |
         |    @Test
         |    def echo01(): Unit \\ IO = {
         |        match Http.runWithIO(() -> Http.post("${baseUrl}/echo", Map.empty(), "ping")) {
         |            case Ok(r) => expect(
         |                Http.Response.status(r) == 200 and Http.Response.body(r) == "ping",
         |                "expected status 200 and body 'ping'"
         |            )
         |            case Err(e) => bug!("unexpected Err: " + ToString.toString(e))
         |        }
         |    }
         |
         |    @Test
         |    def chunked01(): Unit \\ IO = {
         |        match Http.runWithIO(() -> Http.get("${baseUrl}/chunked", Map.empty())) {
         |            case Ok(r) => expect(
         |                Http.Response.status(r) == 200 and Http.Response.body(r) == "chunked",
         |                "expected status 200 and body 'chunked' (chunked transfer decoded)"
         |            )
         |            case Err(e) => bug!("unexpected Err: " + ToString.toString(e))
         |        }
         |    }
         |
         |    @Test
         |    def invalidUtf801(): Unit \\ IO = {
         |        match Http.runWithIO(() -> Http.get("${baseUrl}/invalid-utf8", Map.empty())) {
         |            case Ok(r) => expect(
         |                Http.Response.body(r) == "�(",
         |                "expected invalid UTF-8 to be decoded lossily"
         |            )
         |            case Err(e) => bug!("unexpected Err: " + ToString.toString(e))
         |        }
         |    }
         |
         |    @Test
         |    def reqHeader01(): Unit \\ IO = {
         |        let hs = Map.insert("X-Req", "abc" :: Nil, Map.empty());
         |        match Http.runWithIO(() -> Http.get("${baseUrl}/reqheader", hs)) {
         |            case Ok(r) => expect(
         |                Http.Response.body(r) == "abc",
         |                "expected server to observe request header X-Req"
         |            )
         |            case Err(e) => bug!("unexpected Err: " + ToString.toString(e))
         |        }
         |    }
         |
         |    @Test
         |    def invalidUrl01(): Unit \\ IO =
         |        match Http.runWithIO(() -> Http.get("not a url", Map.empty())) {
         |            case Err(IoError.IoError(IoError.ErrorKind.InvalidInput, _)) => ()
         |            case Err(IoError.IoError(kind, msg)) => bug!("expected InvalidInput, got " + ToString.toString(kind) + ": " + msg)
         |            case Ok(_) => bug!("expected InvalidInput, got Ok(_)")
         |        }
         |
         |    @Test
         |    def unsupportedScheme01(): Unit \\ IO =
         |        match Http.runWithIO(() -> Http.get("ftp://example.com", Map.empty())) {
         |            case Err(IoError.IoError(IoError.ErrorKind.Unsupported, _)) => ()
         |            case Err(IoError.IoError(kind, msg)) => bug!("expected Unsupported, got " + ToString.toString(kind) + ": " + msg)
         |            case Ok(_) => bug!("expected Unsupported, got Ok(_)")
         |        }
         |}
         |""".stripMargin

    Files.writeString(testFile, program, StandardCharsets.UTF_8)
  }

  override protected def afterAll(): Unit = {
    try {
      if (server != null) server.stop(0)
      if (executor != null) executor.shutdownNow()
      Files.deleteIfExists(testFile)
    } finally {
      super.afterAll()
    }
  }
}
