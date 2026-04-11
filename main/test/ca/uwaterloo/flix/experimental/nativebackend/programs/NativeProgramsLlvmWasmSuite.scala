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
import ca.uwaterloo.flix.util.{CompilationTarget, FileOps, Options, StdlibProfile}
import com.sun.net.httpserver.{HttpHandler, HttpServer}
import org.scalatest.funsuite.AnyFunSuite

import java.io.{BufferedReader, IOException, InputStreamReader}
import java.net.{InetAddress, InetSocketAddress, ServerSocket, Socket}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.{Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicReference
import scala.jdk.CollectionConverters.*

/**
  * End-to-end "real program" fixtures for the LLVM-wasm backend.
  *
  * These intentionally exercise the browser-first wasm component toolchain:
  *   - compile Flix → LLVM IR → wasm core module (zig) → component (wasm-tools) → JS package (jco)
  *   - run with the Node host bridge (`tools/wasm-runner-js/run-flix.mjs`)
  */
class NativeProgramsLlvmWasmSuite extends AnyFunSuite {

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmWasm,
      incremental = false,
      outputJvm = false,
    )

  private val fixturesDir = Paths.get("main/test/flix/native/apps")

  private val wasmRunnerScript: Path =
    Paths.get("tools/wasm-runner-js/run-flix.mjs").toAbsolutePath.normalize()

  test("llvm-wasm-fixture-process-spawn") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm real program fixture)")

    val outDir = Files.createTempDirectory("flix-llvm-wasm-fixture-process-spawn-")
    val sandboxDir = Files.createTempDirectory("flix-llvm-wasm-fixture-process-spawn-sandbox-")
    try {
      val (componentJs, exportsManifest) = compileLlvmWasm(fixturesDir.resolve("process_spawn"), outDir)
      val (exit, output) = runNode(
        componentJs,
        exportsManifest,
        rootDir = sandboxDir,
        argv = Nil,
        stdinLines = Nil,
        timeoutSeconds = 25,
      )
      if (exit != 0) {
        fail(s"Process spawn wasm fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("process-spawn: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
      deleteRecursive(sandboxDir)
    }
  }

  test("llvm-wasm-fixture-cli-console") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm real program fixture)")

    val outDir = Files.createTempDirectory("flix-llvm-wasm-fixture-cli-console-")
    val sandboxDir = Files.createTempDirectory("flix-llvm-wasm-fixture-cli-console-sandbox-")
    try {
      val (componentJs, exportsManifest) = compileLlvmWasm(fixturesDir.resolve("cli_console"), outDir)
      val (exit, output) = runNode(
        componentJs,
        exportsManifest,
        rootDir = sandboxDir,
        argv = List("adder"),
        stdinLines = List("40", "2"),
        timeoutSeconds = 25,
      )
      if (exit != 0) {
        fail(s"CLI console wasm fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("cli-console: ready")) {
        fail(s"Expected ready banner, but output was:\n$output")
      }
      if (!output.contains("adder:42")) {
        fail(s"Expected computed output, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
      deleteRecursive(sandboxDir)
    }
  }

  test("llvm-wasm-fixture-channel-rendezvous") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm real program fixture)")

    val outDir = Files.createTempDirectory("flix-llvm-wasm-fixture-channel-rendezvous-")
    val sandboxDir = Files.createTempDirectory("flix-llvm-wasm-fixture-channel-rendezvous-sandbox-")
    try {
      val (componentJs, exportsManifest) = compileLlvmWasm(fixturesDir.resolve("channel_rendezvous"), outDir)
      val (exit, output) = runNode(
        componentJs,
        exportsManifest,
        rootDir = sandboxDir,
        argv = Nil,
        stdinLines = Nil,
        timeoutSeconds = 25,
      )
      if (exit != 0) {
        fail(s"Channel rendezvous wasm fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("channel-rendezvous: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
      deleteRecursive(sandboxDir)
    }
  }

  test("llvm-wasm-fixture-http-file") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm real program fixture)")

    val executor = Executors.newCachedThreadPool()
    val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    server.setExecutor(executor)

    val token = "secret123"
    server.createContext("/kv", new HttpHandler {
      override def handle(exchange: com.sun.net.httpserver.HttpExchange): Unit = {
        val bytes = s"token=$token\n".getBytes(StandardCharsets.UTF_8)
        exchange.sendResponseHeaders(200, bytes.length)
        val os = exchange.getResponseBody
        os.write(bytes)
        os.close()
      }
    })

    server.start()
    val baseUrl = s"http://127.0.0.1:${server.getAddress.getPort}/kv"

    val outDir = Files.createTempDirectory("flix-llvm-wasm-fixture-http-file-")
    val sandboxDir = Files.createTempDirectory("flix-llvm-wasm-fixture-http-file-sandbox-")
    try {
      val inputRel = "input.txt"
      Files.writeString(sandboxDir.resolve(inputRel), s"$token\n", StandardCharsets.UTF_8)

      val (componentJs, exportsManifest) = compileLlvmWasm(fixturesDir.resolve("http_file_app"), outDir)
      val (exit, output) = runNode(
        componentJs,
        exportsManifest,
        rootDir = sandboxDir,
        argv = List(baseUrl, inputRel),
        stdinLines = Nil,
        timeoutSeconds = 30,
      )
      if (exit != 0) {
        fail(s"HTTP+File wasm fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("native-http-file: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
      deleteRecursive(sandboxDir)
      server.stop(0)
      executor.shutdown()
      executor.awaitTermination(2, TimeUnit.SECONDS)
    }
  }

  test("llvm-wasm-fixture-word-count") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm real program fixture)")

    val outDir = Files.createTempDirectory("flix-llvm-wasm-fixture-word-count-")
    val sandboxDir = Files.createTempDirectory("flix-llvm-wasm-fixture-word-count-sandbox-")
    try {
      val inputRel = "input.txt"
      Files.writeString(sandboxDir.resolve(inputRel), "a a b  c\nc\tc\n", StandardCharsets.UTF_8)

      val (componentJs, exportsManifest) = compileLlvmWasm(fixturesDir.resolve("word_count"), outDir)
      val (exit, output) = runNode(
        componentJs,
        exportsManifest,
        rootDir = sandboxDir,
        argv = List(inputRel),
        stdinLines = Nil,
        timeoutSeconds = 25,
      )
      if (exit != 0) {
        fail(s"Word count wasm fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("word-count: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
      deleteRecursive(sandboxDir)
    }
  }

  test("llvm-wasm-fixture-tce-mutual") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm real program fixture)")

    val outDir = Files.createTempDirectory("flix-llvm-wasm-fixture-tce-mutual-")
    val sandboxDir = Files.createTempDirectory("flix-llvm-wasm-fixture-tce-mutual-sandbox-")
    try {
      val (componentJs, exportsManifest) = compileLlvmWasm(fixturesDir.resolve("tce_mutual"), outDir)
      val (exit, output) = runNode(
        componentJs,
        exportsManifest,
        rootDir = sandboxDir,
        argv = Nil,
        stdinLines = Nil,
        timeoutSeconds = 25,
      )
      if (exit != 0) {
        fail(s"TCE mutual recursion wasm fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("tce-mutual: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
      deleteRecursive(sandboxDir)
    }
  }

  test("llvm-wasm-fixture-record-areas") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm real program fixture)")

    val outDir = Files.createTempDirectory("flix-llvm-wasm-fixture-record-areas-")
    val sandboxDir = Files.createTempDirectory("flix-llvm-wasm-fixture-record-areas-sandbox-")
    try {
      val (componentJs, exportsManifest) = compileLlvmWasm(fixturesDir.resolve("record_areas"), outDir)
      val (exit, output) = runNode(
        componentJs,
        exportsManifest,
        rootDir = sandboxDir,
        argv = Nil,
        stdinLines = Nil,
        timeoutSeconds = 25,
      )
      if (exit != 0) {
        fail(s"Record areas wasm fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("record-areas: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
      deleteRecursive(sandboxDir)
    }
  }

  test("llvm-wasm-fixture-struct-person") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm real program fixture)")

    val outDir = Files.createTempDirectory("flix-llvm-wasm-fixture-struct-person-")
    val sandboxDir = Files.createTempDirectory("flix-llvm-wasm-fixture-struct-person-sandbox-")
    try {
      val (componentJs, exportsManifest) = compileLlvmWasm(fixturesDir.resolve("struct_person"), outDir)
      val (exit, output) = runNode(
        componentJs,
        exportsManifest,
        rootDir = sandboxDir,
        argv = Nil,
        stdinLines = Nil,
        timeoutSeconds = 25,
      )
      if (exit != 0) {
        fail(s"Struct person wasm fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("struct-person: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
      deleteRecursive(sandboxDir)
    }
  }

  test("llvm-wasm-fixture-array-foreach") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm real program fixture)")

    val outDir = Files.createTempDirectory("flix-llvm-wasm-fixture-array-foreach-")
    val sandboxDir = Files.createTempDirectory("flix-llvm-wasm-fixture-array-foreach-sandbox-")
    try {
      val (componentJs, exportsManifest) = compileLlvmWasm(fixturesDir.resolve("array_foreach"), outDir)
      val (exit, output) = runNode(
        componentJs,
        exportsManifest,
        rootDir = sandboxDir,
        argv = Nil,
        stdinLines = Nil,
        timeoutSeconds = 25,
      )
      if (exit != 0) {
        fail(s"Array foreach wasm fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("array-foreach: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
      deleteRecursive(sandboxDir)
    }
  }

  test("llvm-wasm-fixture-float-semantics") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm real program fixture)")

    val outDir = Files.createTempDirectory("flix-llvm-wasm-fixture-float-semantics-")
    val sandboxDir = Files.createTempDirectory("flix-llvm-wasm-fixture-float-semantics-sandbox-")
    try {
      val (componentJs, exportsManifest) = compileLlvmWasm(fixturesDir.resolve("float_semantics"), outDir)
      val (exit, output) = runNode(
        componentJs,
        exportsManifest,
        rootDir = sandboxDir,
        argv = Nil,
        stdinLines = Nil,
        timeoutSeconds = 25,
      )
      if (exit != 0) {
        fail(s"Float semantics wasm fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("float-semantics: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
      deleteRecursive(sandboxDir)
    }
  }

  test("llvm-wasm-fixture-counter-effect") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm real program fixture)")

    val outDir = Files.createTempDirectory("flix-llvm-wasm-fixture-counter-effect-")
    val sandboxDir = Files.createTempDirectory("flix-llvm-wasm-fixture-counter-effect-sandbox-")
    try {
      val (componentJs, exportsManifest) = compileLlvmWasm(fixturesDir.resolve("counter_effect"), outDir)
      val (exit, output) = runNode(
        componentJs,
        exportsManifest,
        rootDir = sandboxDir,
        argv = Nil,
        stdinLines = Nil,
        timeoutSeconds = 25,
      )
      if (exit != 0) {
        fail(s"Counter effect wasm fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("counter-effect: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
      deleteRecursive(sandboxDir)
    }
  }

  test("llvm-wasm-fixture-tcp-client") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm real program fixture)")

    val server = new ServerSocket(0, 1, InetAddress.getByName("127.0.0.1"))
    server.setSoTimeout(10_000)
    val port = server.getLocalPort

    val serverErr = new AtomicReference[Throwable](null)
    val serverThread = new Thread(() => {
      try {
        val socket = server.accept()
        socket.setSoTimeout(10_000)
        try {
          val b = socket.getInputStream.read()
          if (b != 65) throw new AssertionError(s"expected client byte 65, got $b")
          socket.getOutputStream.write(Array(66.toByte))
          socket.getOutputStream.flush()
        } finally {
          socket.close()
        }
      } catch {
        case t: Throwable => serverErr.set(t)
      } finally {
        server.close()
      }
    })
    serverThread.setDaemon(true)
    serverThread.start()

    val outDir = Files.createTempDirectory("flix-llvm-wasm-fixture-tcp-client-")
    val sandboxDir = Files.createTempDirectory("flix-llvm-wasm-fixture-tcp-client-sandbox-")
    try {
      val (componentJs, exportsManifest) = compileLlvmWasm(fixturesDir.resolve("tcp_client"), outDir)
      val (exit, output) = runNode(
        componentJs,
        exportsManifest,
        rootDir = sandboxDir,
        argv = List("127.0.0.1", port.toString),
        stdinLines = Nil,
        timeoutSeconds = 30,
      )

      serverThread.join(2_000)
      val t = serverErr.get()
      if (t != null) throw t

      if (exit != 0) {
        fail(s"TCP client wasm fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("tcp-client: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
      deleteRecursive(sandboxDir)
    }
  }

  test("llvm-wasm-fixture-tcp-server") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm real program fixture)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm real program fixture)")

    val outDir = Files.createTempDirectory("flix-llvm-wasm-fixture-tcp-server-")
    val sandboxDir = Files.createTempDirectory("flix-llvm-wasm-fixture-tcp-server-sandbox-")
    try {
      val (componentJs, exportsManifest) = compileLlvmWasm(fixturesDir.resolve("tcp_server"), outDir)

      val (exit, output) =
        runTcpServerFixture(componentJs, exportsManifest, rootDir = sandboxDir, timeoutSeconds = 30)

      if (exit != 0) {
        fail(s"TCP server wasm fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("tcp-server: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
      deleteRecursive(sandboxDir)
    }
  }

  private def compileLlvmWasm(dir: Path, outDir: Path): (Path, Path) = {
    val flix = new Flix()
    flix.setOptions(TestOptions.copy(outputPath = outDir))
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted

    for (p <- FileOps.getFlixFilesIn(dir, Int.MaxValue)) flix.addFile(p)

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
                      argv: List[String],
                      stdinLines: List[String],
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
      "--httpTimeoutMs",
      "5000"
    ) ::: argv.flatMap(a => List("--argv", a)) ::: stdinLines.flatMap(l => List("--stdinLine", l))

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

  private def runTcpServerFixture(componentJs: Path,
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
      "500"
    )

    val pb = new ProcessBuilder(cmd.asJava)
    pb.redirectErrorStream(true)

    val p = pb.start()
    p.getOutputStream.close()

    val output = new StringBuilder
    val portRef = new AtomicReference[Option[Int]](None)
    val portReady = new java.util.concurrent.CountDownLatch(1)

    val readerThread = new Thread(() => {
      val br = new BufferedReader(new InputStreamReader(p.getInputStream, StandardCharsets.UTF_8))
      try {
        var line: String = br.readLine()
        while (line != null) {
          output.append(line).append('\n')
          val idx = line.indexOf("PORT=")
          if (idx >= 0 && portRef.get().isEmpty) {
            val rest = line.substring(idx + "PORT=".length).dropWhile(c => !c.isDigit)
            val digits = rest.takeWhile(_.isDigit)
            try {
              portRef.set(Some(digits.toInt))
              portReady.countDown()
            } catch {
              case _: NumberFormatException =>
                portRef.set(None)
                portReady.countDown()
            }
          }
          line = br.readLine()
        }
      } finally {
        br.close()
      }
    })
    readerThread.setDaemon(true)
    readerThread.start()

    val gotPort = portReady.await(10, TimeUnit.SECONDS)
    val portOpt = portRef.get()
    if (!gotPort || portOpt.isEmpty) {
      p.destroyForcibly()
      p.waitFor(2, TimeUnit.SECONDS)
      readerThread.join(1_000)
      fail(s"Did not observe PORT=... line from tcp_server wasm fixture. Output so far:\n$output")
    }

    val port = portOpt.get
    val client = new Socket()
    try {
      client.connect(new InetSocketAddress("127.0.0.1", port), 10_000)
      client.setSoTimeout(10_000)
      client.getOutputStream.write(Array(65.toByte))
      client.getOutputStream.flush()
      val b = client.getInputStream.read()
      if (b != 66) {
        fail(s"Expected server byte 66, got $b. Output so far:\n$output")
      }
    } finally {
      client.close()
    }

    val finished = p.waitFor(timeoutSeconds, TimeUnit.SECONDS)
    if (!finished) {
      p.destroyForcibly()
      p.waitFor(2, TimeUnit.SECONDS)
    }

    readerThread.join(1_000)
    val outStr = output.toString
    if (!finished) {
      fail(s"tcp_server wasm fixture timed out after ${timeoutSeconds}s. Output so far:\n$outStr")
    }

    (p.exitValue(), outStr)
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
