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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.util.{CompilationTarget, Options, StdlibProfile}
import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

import java.io.IOException
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.{ExecutorService, Executors, TimeUnit}
import scala.jdk.CollectionConverters.*

/**
  * Portable HTTP runtime semantics suite for the LLVM-wasm backend.
  *
  * Mirrors `PortableHttpSuite` (JVM) and `PortableHttpLlvmNativeSuite` (native) to keep portable HTTP
  * behavior consistent across targets (redirect policy, timeouts, header handling, UTF-8 decoding, etc.).
  *
  * Runs the wasm component via the Node host bridge (`tools/wasm-runner-js/run-flix.mjs`).
  */
class PortableHttpLlvmWasmSuite extends AnyFunSuite with BeforeAndAfterAll {

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmWasm,
      incremental = false,
      outputJvm = false,
    )

  private val wasmRunnerScript: Path =
    Paths.get("tools/wasm-runner-js/run-flix.mjs").toAbsolutePath.normalize()

  private var server: HttpServer = _
  private var executor: ExecutorService = _
  private var baseUrl: String = _

  private val testFile: Path = Files.createTempFile("flix-portable-http-llvm-wasm-", ".flix")

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
         |def expect(cond: Bool, msg: String): Unit =
         |    if (cond) () else bug!(msg)
         |
         |def unexpectedErr(e: a): Unit with ToString[a] =
         |    expect(false, "unexpected Err: " + ToString.toString(e))
         |
         |def hello01(): Unit \\ IO = {
         |    match Http.runWithIO(() -> Http.get("${baseUrl}/hello", Map.empty())) {
         |        case Ok(r) => {
         |            let hs = Http.Response.headers(r);
         |            expect(
         |                Http.Response.status(r) == 200 and
         |                Http.Response.body(r) == "hello" and
         |                Map.get("x-foo", hs) == Some("bar" :: Nil),
         |                "expected status 200, body 'hello', and header x-foo = bar"
         |            )
         |        }
         |        case Err(e) => unexpectedErr(e)
         |    }
         |}
         |
         |def multiHeader01(): Unit \\ IO = {
         |    match Http.runWithIO(() -> Http.get("${baseUrl}/multi", Map.empty())) {
         |        case Ok(r) => {
         |            let hs = Http.Response.headers(r);
         |            let expected = "v1" :: "v2" :: Nil;
         |            expect(
         |                Http.Response.body(r) == "multi" and Map.get("x-multi", hs) == Some(expected),
         |                "expected body 'multi' and header x-multi values [v1, v2]"
         |            )
         |        }
         |        case Err(e) => unexpectedErr(e)
         |    }
         |}
         |
         |def head01(): Unit \\ IO = {
         |    match Http.runWithIO(() -> Http.head("${baseUrl}/head", Map.empty())) {
         |        case Ok(r) => {
         |            let hs = Http.Response.headers(r);
         |            expect(
         |                Http.Response.status(r) == 200 and
         |                Http.Response.body(r) == "" and
         |                Map.get("x-head", hs) == Some("yes" :: Nil),
         |                "expected HEAD to return empty body and header x-head = yes"
         |            )
         |        }
         |        case Err(e) => unexpectedErr(e)
         |    }
         |}
         |
         |def redirectGet30201(): Unit \\ IO =
         |    match Http.runWithIO(() -> Http.get("${baseUrl}/redir302", Map.empty())) {
         |        case Ok(r) => expect(
         |            Http.Response.status(r) == 200 and Http.Response.body(r) == "GET",
         |            "expected 302 redirect to be followed for GET"
         |        )
         |        case Err(e) => unexpectedErr(e)
         |    }
         |
         |def redirectPreservesHeaders30201(): Unit \\ IO = {
         |    let hs = Map.insert("X-Req", "abc" :: Nil, Map.empty());
         |    match Http.runWithIO(() -> Http.get("${baseUrl}/redir302reqheader", hs)) {
         |        case Ok(r) => expect(
         |            Http.Response.status(r) == 200 and Http.Response.body(r) == "abc",
         |            "expected request header X-Req to be preserved across 302 redirect"
         |        )
         |        case Err(e) => unexpectedErr(e)
         |    }
         |}
         |
         |def redirectLoop01(): Unit \\ IO =
         |    match Http.runWithIO(() -> Http.get("${baseUrl}/redir-loop", Map.empty())) {
         |        case Err(_) => ()
         |        case Ok(_) => expect(false, "expected redirect loop to fail")
         |    }
         |
         |def redirectPost30201(): Unit \\ IO =
         |    match Http.runWithIO(() -> Http.post("${baseUrl}/redir302", Map.empty(), "ping")) {
         |        case Ok(r) => expect(
         |            Http.Response.status(r) == 200 and Http.Response.body(r) == "GET",
         |            "expected 302 redirect to be followed and POST to become GET"
         |        )
         |        case Err(e) => unexpectedErr(e)
         |    }
         |
         |def redirectPut30301(): Unit \\ IO =
         |    match Http.runWithIO(() -> Http.put("${baseUrl}/redir303", Map.empty(), "ping")) {
         |        case Ok(r) => expect(
         |            Http.Response.status(r) == 200 and Http.Response.body(r) == "GET",
         |            "expected 303 redirect to be followed and method to become GET"
         |        )
         |        case Err(e) => unexpectedErr(e)
         |    }
         |
         |def redirectPost30701(): Unit \\ IO =
         |    match Http.runWithIO(() -> Http.post("${baseUrl}/redir307", Map.empty(), "ping")) {
         |        case Ok(r) => expect(
         |            Http.Response.status(r) == 200 and Http.Response.body(r) == "POST",
         |            "expected 307 redirect to be followed and method preserved"
         |        )
         |        case Err(e) => unexpectedErr(e)
         |    }
         |
         |def redirectUnsupportedScheme01(): Unit \\ IO =
         |    match Http.runWithIO(() -> Http.get("${baseUrl}/redir-unsupported-scheme", Map.empty())) {
         |        case Err(IoError.IoError(IoError.ErrorKind.Unsupported, _)) => ()
         |        case Err(IoError.IoError(kind, msg)) => expect(false, "expected Unsupported, got " + ToString.toString(kind) + ": " + msg)
         |        case Ok(_) => expect(false, "expected Unsupported, got Ok(_)")
         |    }
         |
         |def echo01(): Unit \\ IO = {
         |    match Http.runWithIO(() -> Http.post("${baseUrl}/echo", Map.empty(), "ping")) {
         |        case Ok(r) => expect(
         |            Http.Response.status(r) == 200 and Http.Response.body(r) == "ping",
         |            "expected status 200 and body 'ping'"
         |        )
         |        case Err(e) => unexpectedErr(e)
         |    }
         |}
         |
         |def chunked01(): Unit \\ IO = {
         |    match Http.runWithIO(() -> Http.get("${baseUrl}/chunked", Map.empty())) {
         |        case Ok(r) => expect(
         |            Http.Response.status(r) == 200 and Http.Response.body(r) == "chunked",
         |            "expected status 200 and body 'chunked' (chunked transfer decoded)"
         |        )
         |        case Err(e) => unexpectedErr(e)
         |    }
         |}
         |
         |def invalidUtf801(): Unit \\ IO = {
         |    match Http.runWithIO(() -> Http.get("${baseUrl}/invalid-utf8", Map.empty())) {
         |        case Ok(r) => expect(
         |            Http.Response.body(r) == "�(",
         |            "expected invalid UTF-8 to be decoded lossily"
         |        )
         |        case Err(e) => unexpectedErr(e)
         |    }
         |}
         |
         |def reqHeader01(): Unit \\ IO = {
         |    let hs = Map.insert("X-Req", "abc" :: Nil, Map.empty());
         |    match Http.runWithIO(() -> Http.get("${baseUrl}/reqheader", hs)) {
         |        case Ok(r) => expect(
         |            Http.Response.body(r) == "abc",
         |            "expected server to observe request header X-Req"
         |        )
         |        case Err(e) => unexpectedErr(e)
         |    }
         |}
         |
         |def invalidUrl01(): Unit \\ IO =
         |    match Http.runWithIO(() -> Http.get("not a url", Map.empty())) {
         |        case Err(IoError.IoError(IoError.ErrorKind.InvalidInput, _)) => ()
         |        case Err(IoError.IoError(kind, msg)) => expect(false, "expected InvalidInput, got " + ToString.toString(kind) + ": " + msg)
         |        case Ok(_) => expect(false, "expected InvalidInput, got Ok(_)")
         |    }
         |
         |def unsupportedScheme01(): Unit \\ IO =
         |    match Http.runWithIO(() -> Http.get("ftp://example.com", Map.empty())) {
         |        case Err(IoError.IoError(IoError.ErrorKind.Unsupported, _)) => ()
         |        case Err(IoError.IoError(kind, msg)) => expect(false, "expected Unsupported, got " + ToString.toString(kind) + ": " + msg)
         |        case Ok(_) => expect(false, "expected Unsupported, got Ok(_)")
         |    }
         |
         |def main(): Unit \\ IO = {
         |    hello01();
         |    multiHeader01();
         |    head01();
         |    redirectGet30201();
         |    redirectPreservesHeaders30201();
         |    redirectLoop01();
         |    redirectPost30201();
         |    redirectPut30301();
         |    redirectPost30701();
         |    redirectUnsupportedScheme01();
         |    echo01();
         |    chunked01();
         |    invalidUtf801();
         |    reqHeader01();
         |    invalidUrl01();
         |    unsupportedScheme01();
         |    ()
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

  test("portable-http-llvm-wasm") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm portable HTTP runtime test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm portable HTTP runtime test)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm portable HTTP runtime test)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm portable HTTP runtime test)")

    val outDir = Files.createTempDirectory("flix-llvm-wasm-http-")
    val sandboxDir = Files.createTempDirectory("flix-llvm-wasm-http-sandbox-")
    try {
      val (componentJs, exportsManifest) = compileLlvmWasm(testFile, outDir)
      val (exit, output) = runNode(componentJs, exportsManifest, sandboxDir, timeoutSeconds = 45)
      if (exit != 0) {
        fail(s"LLVM-wasm portable HTTP test program failed with exit $exit:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
      deleteRecursive(sandboxDir)
    }
  }

  private def compileLlvmWasm(file: Path, outDir: Path): (Path, Path) = {
    val flix = new Flix()
    flix.setOptions(TestOptions.copy(outputPath = outDir))
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted
    flix.addFile(file)

    val (optRoot, errors) = flix.check()
    if (errors.nonEmpty) {
      fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
    }

    flix.codeGen(optRoot.get)

    val componentJs = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmDriver.componentJsPath(outDir)
    val exportsManifest = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmExportWriter.manifestPath(outDir)

    if (!Files.exists(componentJs)) {
      fail(s"Missing wasm JS component artifact: $componentJs")
    }
    if (!Files.exists(exportsManifest)) {
      fail(s"Missing wasm exports manifest: $exportsManifest")
    }

    (componentJs, exportsManifest)
  }

  private def runNode(componentJs: Path,
                      exportsManifest: Path,
                      rootDir: Path,
                      timeoutSeconds: Long): (Int, String) = {
    val cmd = List(
      "node",
      wasmRunnerScript.toString,
      "--js",
      componentJs.toAbsolutePath.normalize().toString,
      "--exports",
      exportsManifest.toAbsolutePath.normalize().toString,
      "--rootDir",
      rootDir.toAbsolutePath.normalize().toString,
      "--budget",
      "500",
      "--maxRedirects",
      "20",
      "--httpTimeoutMs",
      "5000",
    )

    val pb = new ProcessBuilder(cmd.asJava)
    pb.redirectErrorStream(true)

    val p = pb.start()
    p.getOutputStream.close()

    val baos = new java.io.ByteArrayOutputStream()
    val is = p.getInputStream

    val readerThread = new Thread(() => {
      val buf = new Array[Byte](4096)
      try {
        var n = is.read(buf)
        while (n != -1) {
          baos.write(buf, 0, n)
          n = is.read(buf)
        }
      } catch {
        case _: IOException => ()
      }
    })
    readerThread.setDaemon(true)
    readerThread.start()

    val finished = p.waitFor(timeoutSeconds, TimeUnit.SECONDS)
    if (!finished) {
      p.destroyForcibly()
      p.waitFor(2, TimeUnit.SECONDS)
    }

    readerThread.join(1_000)

    val output = new String(baos.toByteArray, StandardCharsets.UTF_8)
    if (!finished) {
      fail(s"Node runner timed out after ${timeoutSeconds}s. Output so far:\n$output")
    }
    (p.exitValue(), output)
  }

  private def hasWasmTools: Boolean =
    hasCmd(List("wasm-tools", "--version"))

  private def hasJco: Boolean =
    hasCmd(List("jco", "--version"))

  private def hasNode: Boolean =
    hasCmd(List("node", "--version"))

  private def hasCmd(cmd: List[String]): Boolean = {
    try {
      val p = new ProcessBuilder(cmd.asJava).redirectErrorStream(true).start()
      p.waitFor(2, TimeUnit.SECONDS) && p.exitValue() == 0
    } catch {
      case _: IOException => false
      case _: InterruptedException => false
    }
  }

  private def deleteRecursive(root: Path): Unit = {
    if (!Files.exists(root)) return
    val stream = Files.walk(root)
    try {
      stream.iterator().asScala.toList.sortBy(_.getNameCount).reverse.foreach(p => Files.deleteIfExists(p))
    } finally {
      stream.close()
    }
  }
}
