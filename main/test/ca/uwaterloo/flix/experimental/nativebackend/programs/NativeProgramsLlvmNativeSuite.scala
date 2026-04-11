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
import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import org.scalatest.funsuite.AnyFunSuite

import java.io.IOException
import java.io.{BufferedReader, InputStreamReader}
import java.net.{InetAddress, ServerSocket, Socket}
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{ExecutorService, Executors, TimeUnit}
import scala.jdk.CollectionConverters.*

/**
  * End-to-end "real program" fixtures for the LLVM-native backend.
  *
  * These tests are intentionally small but exercise whole-program compilation and
  * OS integration points (argv, stdio, filesystem, HTTP).
  */
class NativeProgramsLlvmNativeSuite extends AnyFunSuite {

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmNative,
      incremental = false,
      outputJvm = false,
    )

  private val fixturesDir = Paths.get("main/test/flix/native/apps")

  test("llvm-native-fixture-cli-console") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native real program fixture)")

    val outDir = Files.createTempDirectory("flix-llvm-native-fixture-cli-console-")
    try {
      val exe = compileLlvmNative(fixturesDir.resolve("cli_console"), outDir)
      val (exit, output) = runExecutable(
        exe,
        args = List("result"),
        stdin = "40\n2\n",
        timeoutSeconds = 10,
      )
      if (exit != 0) {
        fail(s"CLI/Console fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("cli-console: ready")) {
        fail(s"Expected readiness banner, but output was:\n$output")
      }
      if (!output.contains("result:42")) {
        fail(s"Expected computed result, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
    }
  }

  test("llvm-native-fixture-channel-rendezvous") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native real program fixture)")

    val outDir = Files.createTempDirectory("flix-llvm-native-fixture-channel-rendezvous-")
    try {
      val exe = compileLlvmNative(fixturesDir.resolve("channel_rendezvous"), outDir)
      val (exit, output) = runExecutable(
        exe,
        env = Map(
          "FLIX_GC_STRESS" -> "1",
          // Keep the heap tiny so we are likely to collect while messages are queued.
          "FLIX_GC_HEAP_LIMIT_BYTES" -> "65536",
        ),
        timeoutSeconds = 20,
      )
      if (exit != 0) {
        fail(s"Channel rendezvous fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("channel-rendezvous: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
    }
  }

  test("llvm-native-fixture-http-file") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native real program fixture)")

    val executor = Executors.newCachedThreadPool()
    val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    server.setExecutor(executor)

    val token = "secret123"
    server.createContext("/kv", new HttpHandler {
      override def handle(exchange: HttpExchange): Unit = {
        val bytes = s"token=$token\n".getBytes(StandardCharsets.UTF_8)
        exchange.sendResponseHeaders(200, bytes.length)
        val os = exchange.getResponseBody
        os.write(bytes)
        os.close()
      }
    })

    server.start()
    val baseUrl = s"http://127.0.0.1:${server.getAddress.getPort}/kv"

    val inputFile = Files.createTempFile("flix-llvm-native-fixture-http-file-", ".txt")
    Files.writeString(inputFile, s"$token\n", StandardCharsets.UTF_8)

    val outDir = Files.createTempDirectory("flix-llvm-native-fixture-http-file-")

    try {
      val exe = compileLlvmNative(fixturesDir.resolve("http_file_app"), outDir)
      val (exit, output) = runExecutable(
        exe,
        args = List(baseUrl, inputFile.toString),
        timeoutSeconds = 15,
      )
      if (exit != 0) {
        fail(s"HTTP+File fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("native-http-file: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      Files.deleteIfExists(inputFile)
      deleteRecursive(outDir)
      server.stop(0)
      shutdownExecutor(executor)
    }
  }

  test("llvm-native-fixture-word-count") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native real program fixture)")

    val inputFile = Files.createTempFile("flix-llvm-native-fixture-word-count-", ".txt")
    Files.writeString(inputFile, "a a b  c\nc\tc\n", StandardCharsets.UTF_8)

    val outDir = Files.createTempDirectory("flix-llvm-native-fixture-word-count-")
    try {
      val exe = compileLlvmNative(fixturesDir.resolve("word_count"), outDir)
      val (exit, output) = runExecutable(
        exe,
        args = List(inputFile.toString),
        timeoutSeconds = 15,
      )
      if (exit != 0) {
        fail(s"Word count fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("word-count: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      Files.deleteIfExists(inputFile)
      deleteRecursive(outDir)
    }
  }

  test("llvm-native-fixture-tce-mutual") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native real program fixture)")

    val outDir = Files.createTempDirectory("flix-llvm-native-fixture-tce-mutual-")
    try {
      val exe = compileLlvmNative(fixturesDir.resolve("tce_mutual"), outDir)
      val (exit, output) = runExecutable(exe, timeoutSeconds = 15)
      if (exit != 0) {
        fail(s"TCE mutual recursion fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("tce-mutual: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
    }
  }

  test("llvm-native-fixture-record-areas") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native real program fixture)")

    val outDir = Files.createTempDirectory("flix-llvm-native-fixture-record-areas-")
    try {
      val exe = compileLlvmNative(fixturesDir.resolve("record_areas"), outDir)
      val (exit, output) = runExecutable(exe, timeoutSeconds = 15)
      if (exit != 0) {
        fail(s"Record areas fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("record-areas: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
    }
  }

  test("llvm-native-fixture-struct-person") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native real program fixture)")

    val outDir = Files.createTempDirectory("flix-llvm-native-fixture-struct-person-")
    try {
      val exe = compileLlvmNative(fixturesDir.resolve("struct_person"), outDir)
      val (exit, output) = runExecutable(exe, timeoutSeconds = 15)
      if (exit != 0) {
        fail(s"Struct person fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("struct-person: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
    }
  }

  test("llvm-native-fixture-extern-native-abs") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native real program fixture)")

    val outDir = Files.createTempDirectory("flix-llvm-native-fixture-extern-native-abs-")
    try {
      val exe = compileLlvmNative(fixturesDir.resolve("extern_native_abs"), outDir)
      val (exit, output) = runExecutable(exe, timeoutSeconds = 15)
      if (exit != 0) {
        fail(s"Extern native fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("extern-native: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
    }
  }

  test("llvm-native-fixture-array-foreach") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native real program fixture)")

    val outDir = Files.createTempDirectory("flix-llvm-native-fixture-array-foreach-")
    try {
      val exe = compileLlvmNative(fixturesDir.resolve("array_foreach"), outDir)
      val (exit, output) = runExecutable(exe, timeoutSeconds = 15)
      if (exit != 0) {
        fail(s"Array foreach fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("array-foreach: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
    }
  }

  test("llvm-native-fixture-float-semantics") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native real program fixture)")

    val outDir = Files.createTempDirectory("flix-llvm-native-fixture-float-semantics-")
    try {
      val exe = compileLlvmNative(fixturesDir.resolve("float_semantics"), outDir)
      val (exit, output) = runExecutable(exe, timeoutSeconds = 15)
      if (exit != 0) {
        fail(s"Float semantics fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("float-semantics: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
    }
  }

  test("llvm-native-fixture-counter-effect") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native real program fixture)")

    val outDir = Files.createTempDirectory("flix-llvm-native-fixture-counter-effect-")
    try {
      val exe = compileLlvmNative(fixturesDir.resolve("counter_effect"), outDir)
      val (exit, output) = runExecutable(exe, timeoutSeconds = 15)
      if (exit != 0) {
        fail(s"Counter effect fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("counter-effect: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
    }
  }

  test("llvm-native-fixture-tcp-client") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native real program fixture)")

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

    val outDir = Files.createTempDirectory("flix-llvm-native-fixture-tcp-client-")
    try {
      val exe = compileLlvmNative(fixturesDir.resolve("tcp_client"), outDir)
      val (exit, output) = runExecutable(
        exe,
        args = List("127.0.0.1", port.toString),
        timeoutSeconds = 15,
      )
      serverThread.join(2_000)
      val t = serverErr.get()
      if (t != null) throw t

      if (exit != 0) {
        fail(s"TCP client fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("tcp-client: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
    }
  }

  test("llvm-native-fixture-tcp-server") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native real program fixture)")

    val outDir = Files.createTempDirectory("flix-llvm-native-fixture-tcp-server-")
    try {
      val exe = compileLlvmNative(fixturesDir.resolve("tcp_server"), outDir)

      val (exit, output) = runTcpServerFixture(exe, timeoutSeconds = 20)
      if (exit != 0) {
        fail(s"TCP server fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("tcp-server: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
    }
  }

  test("llvm-native-fixture-process-spawn") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native real program fixture)")

    val outDir = Files.createTempDirectory("flix-llvm-native-fixture-process-spawn-")
    try {
      val exe = compileLlvmNative(fixturesDir.resolve("process_spawn"), outDir)
      val (exit, output) = runExecutable(exe, timeoutSeconds = 20)
      if (exit != 0) {
        fail(s"Process spawn fixture failed with exit $exit:\n$output")
      }
      if (!output.contains("process-spawn: ok")) {
        fail(s"Expected success banner, but output was:\n$output")
      }
    } finally {
      deleteRecursive(outDir)
    }
  }

  private def compileLlvmNative(dir: Path, outDir: Path): Path = {
    val flix = new Flix()
    flix.setOptions(TestOptions.copy(outputPath = outDir))
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted

    for (p <- FileOps.getFlixFilesIn(dir, Int.MaxValue)) flix.addFile(p)

    val (optRoot, errors) = flix.check()
    if (errors.nonEmpty) {
      fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
    }

    flix.codeGen(optRoot.get)
    executablePath(outDir)
  }

  private def runExecutable(
    executable: Path,
    args: List[String] = Nil,
    env: Map[String, String] = Map.empty,
    stdin: String = "",
    timeoutSeconds: Long = 30,
  ): (Int, String) = {
    val cmd = (executable.toString :: args).asJava
    val pb = new ProcessBuilder(cmd)
    val pbEnv = pb.environment()
    env.foreach { case (k, v) => pbEnv.put(k, v) }
    pb.redirectErrorStream(true)

    val p = pb.start()

    // Provide stdin (always close to avoid hanging waiting for input).
    val os = p.getOutputStream
    try {
      if (stdin.nonEmpty) os.write(stdin.getBytes(StandardCharsets.UTF_8))
    } finally {
      os.close()
    }

    val is = p.getInputStream
    val baos = new java.io.ByteArrayOutputStream()
    val readerThread = new Thread(() => {
      val buf = new Array[Byte](8192)
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

    if (!finished) {
      try is.close() catch { case _: IOException => () }
      readerThread.join(1_000)
    } else {
      readerThread.join(1_000)
      if (readerThread.isAlive) {
        try is.close() catch { case _: IOException => () }
        readerThread.join(1_000)
      }
    }

    val output = new String(baos.toByteArray, StandardCharsets.UTF_8)
    if (!finished) {
      fail(s"Process timed out after ${timeoutSeconds}s. Output so far:\n$output")
    }
    (p.exitValue(), output)
  }

  private def runTcpServerFixture(executable: Path, timeoutSeconds: Long): (Int, String) = {
    val pb = new ProcessBuilder(executable.toString)
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
          if (line.startsWith("PORT=") && portRef.get().isEmpty) {
            val s = line.stripPrefix("PORT=")
            try {
              portRef.set(Some(s.toInt))
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
      fail(s"Did not observe PORT=... line from tcp_server fixture. Output so far:\n$output")
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
      fail(s"tcp_server fixture timed out after ${timeoutSeconds}s. Output so far:\n$outStr")
    }
    (p.exitValue(), outStr)
  }

  private def executablePath(outDir: Path): Path = {
    ca.uwaterloo.flix.language.phase.llvm.LlvmNativeDriver.executablePath(outDir)
  }

  private def isWindows: Boolean =
    System.getProperty("os.name", "").toLowerCase.contains("win")

  private def shutdownExecutor(executor: ExecutorService): Unit = {
    executor.shutdown()
    if (!executor.awaitTermination(2, TimeUnit.SECONDS)) executor.shutdownNow()
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
