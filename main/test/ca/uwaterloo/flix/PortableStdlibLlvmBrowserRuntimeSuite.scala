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
import org.scalatest.funsuite.AnyFunSuite

import java.io.IOException
import java.net.{InetSocketAddress, Socket, ServerSocket}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*

/**
  * End-to-end runtime smoke test for the portable conformance suite on the LLVM-wasm backend,
  * executed in a real browser (headless Chrome).
  *
  * This is the closest approximation of "real browser semantics" we can get without pulling in
  * a heavyweight browser automation dependency. We run the component output via the browser
  * runner harness at:
  *   `tools/wasm-runner-js/browser/run_component.html`
  *
  * Notes:
  * - Browsers do not support TCP/process, so we exclude tests that require sockets.
  * - OPFS (Origin Private File System) is used for filesystem operations.
  */
class PortableStdlibLlvmBrowserRuntimeSuite extends AnyFunSuite {

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmWasm,
      incremental = false,
      outputJvm = false,
    )

  private val preludeFile = Paths.get("main/test/flix/Prelude.flix")

  private val portableTestsDir = Paths.get("main/test/flix/portable")

  private val browserServerScript: Path =
    Paths.get("tools/wasm-runner-js/browser/serve.mjs").toAbsolutePath.normalize()

  private val browserHtmlPath: String =
    "/tools/wasm-runner-js/browser/run_component.html"

  private val headlessRunnerScript: Path =
    Paths.get("tools/wasm-runner-js/browser/run_headless_chrome.mjs").toAbsolutePath.normalize()

  /**
    * Tests that cannot run in browsers today (no TCP sockets).
    */
  private val unsupportedNamespaces: Set[List[String]] = Set(
    List("Test", "Portable", "TcpLoopback"),
  )

  test("portable-stdlib-llvm-wasm-browser-runtime") {
    assume(hasZig, "zig not found on PATH (skipping browser portable stdlib runtime test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping browser portable stdlib runtime test)")
    assume(hasJco, "jco not found on PATH (skipping browser portable stdlib runtime test)")
    assume(hasNode, "node not found on PATH (skipping browser portable stdlib runtime test)")

    val chromeOpt = findChromeBinary()
    assume(chromeOpt.nonEmpty, "chrome/chromium not found (skipping browser portable stdlib runtime test)")
    val chrome = chromeOpt.get

    val driverFile = Files.createTempFile("flix-portable-llvm-wasm-browser-driver-", ".flix")
    val outDir = Files.createTempDirectory(Paths.get("build"), "flix-llvm-wasm-portable-browser-")

    val port = findAvailablePort()
    val server = startServer(port)

    try {
      val driverSource = mkPortableDriverSource()
      Files.writeString(driverFile, driverSource, StandardCharsets.UTF_8)

      val flix = new Flix()
      flix.setOptions(TestOptions.copy(outputPath = outDir))
      implicit val sctx: SecurityContext = SecurityContext.Unrestricted

      flix.addFile(preludeFile)
      for (p <- FileOps.getFlixFilesIn(portableTestsDir, 1)) flix.addFile(p)
      flix.addFile(driverFile)

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

      waitForPort(port, timeoutMs = 5_000)

      val componentUrlPath = toUrlPath(componentJs)
      val exportsUrlPath = toUrlPath(exportsManifest)

      val url =
        s"http://127.0.0.1:$port$browserHtmlPath?component=$componentUrlPath&exports=$exportsUrlPath&budget=500"

      val (exit, output) = runHeadlessChrome(chrome, url)
      if (exit != 0) {
        fail(s"Browser portable conformance run failed with exit $exit.\nURL: $url\n$output")
      }
    } finally {
      Files.deleteIfExists(driverFile)
      deleteRecursive(outDir)
      stopServer(server)
    }
  }

  for (tc <- PortableExceptionParityCases.All) {
    test(s"portable-exception-parity-llvm-wasm-browser-${tc.id}") {
      assume(hasZig, s"zig not found on PATH (skipping browser exception parity test: ${tc.id})")
      assume(hasWasmTools, s"wasm-tools not found on PATH (skipping browser exception parity test: ${tc.id})")
      assume(hasJco, s"jco not found on PATH (skipping browser exception parity test: ${tc.id})")
      assume(hasNode, s"node not found on PATH (skipping browser exception parity test: ${tc.id})")

      val chromeOpt = findChromeBinary()
      assume(chromeOpt.nonEmpty, s"chrome/chromium not found (skipping browser exception parity test: ${tc.id})")
      val chrome = chromeOpt.get

      val driverFile = Files.createTempFile(s"flix-portable-llvm-wasm-browser-${tc.id}-", ".flix")
      val outDir = Files.createTempDirectory(Paths.get("build"), s"flix-llvm-wasm-browser-${tc.id}-")
      val port = findAvailablePort()
      val server = startServer(port)

      try {
        Files.writeString(driverFile, tc.source, StandardCharsets.UTF_8)

        val flix = new Flix()
        flix.setOptions(TestOptions.copy(outputPath = outDir))
        implicit val sctx: SecurityContext = SecurityContext.Unrestricted

        flix.addFile(driverFile)

        val (optRoot, errors) = flix.check()
        if (errors.nonEmpty) {
          fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
        }

        flix.codeGen(optRoot.get)

        val componentJs = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmDriver.componentJsPath(outDir)
        val exportsManifest = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmExportWriter.manifestPath(outDir)

        waitForPort(port, timeoutMs = 5_000)

        val componentUrlPath = toUrlPath(componentJs)
        val exportsUrlPath = toUrlPath(exportsManifest)
        val url =
          s"http://127.0.0.1:$port$browserHtmlPath?component=$componentUrlPath&exports=$exportsUrlPath&budget=500"

        val (exit, output) = runHeadlessChrome(chrome, url)
        if (exit != 0) {
          fail(s"Browser exception parity driver '${tc.id}' failed with exit $exit.\nURL: $url\n$output")
        }
        if (output.trim != tc.expectedOutput) {
          fail(s"Expected exception parity result ${tc.expectedOutput} for '${tc.id}', but got:\n$output")
        }
      } finally {
        Files.deleteIfExists(driverFile)
        deleteRecursive(outDir)
        stopServer(server)
      }
    }
  }

  for (tc <- PortableControlParityCases.All) {
    test(s"portable-control-parity-llvm-wasm-browser-${tc.id}") {
      assume(hasZig, s"zig not found on PATH (skipping LLVM-wasm browser control parity test: ${tc.id})")
      assume(hasWasmTools, s"wasm-tools not found on PATH (skipping LLVM-wasm browser control parity test: ${tc.id})")
      assume(hasJco, s"jco not found on PATH (skipping LLVM-wasm browser control parity test: ${tc.id})")
      assume(hasNode, s"node not found on PATH (skipping LLVM-wasm browser control parity test: ${tc.id})")
      val chromeOpt = findChromeBinary()
      assume(chromeOpt.nonEmpty, s"Chrome/Chromium not found on PATH (skipping LLVM-wasm browser control parity test: ${tc.id})")
      val chrome = chromeOpt.get

      val driverFile = Files.createTempFile(s"flix-portable-llvm-browser-${tc.id}-", ".flix")
      val outDir = Files.createTempDirectory(Paths.get("build"), s"flix-llvm-browser-${tc.id}-")
      val port = findAvailablePort()
      val server = startServer(port)
      try {
        Files.writeString(driverFile, tc.source, StandardCharsets.UTF_8)

        val flix = new Flix()
        flix.setOptions(TestOptions.copy(outputPath = outDir))
        implicit val sctx: SecurityContext = SecurityContext.Unrestricted

        flix.addFile(driverFile)

        val (optRoot, errors) = flix.check()
        if (errors.nonEmpty) {
          fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
        }

        flix.codeGen(optRoot.get)

        val componentJs = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmDriver.componentJsPath(outDir)
        val exportsManifest = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmExportWriter.manifestPath(outDir)

        waitForPort(port, timeoutMs = 5_000)

        val componentUrlPath = toUrlPath(componentJs)
        val exportsUrlPath = toUrlPath(exportsManifest)
        val url =
          s"http://127.0.0.1:$port$browserHtmlPath?component=$componentUrlPath&exports=$exportsUrlPath&budget=500"

        val (exit, output) = runHeadlessChrome(chrome, url)
        if (exit != 0) {
          fail(s"Browser control parity driver '${tc.id}' failed with exit $exit.\nURL: $url\n$output")
        }
        if (output.trim != tc.expectedOutput) {
          fail(s"Expected control parity result ${tc.expectedOutput} for '${tc.id}', but got:\n$output")
        }
      } finally {
        Files.deleteIfExists(driverFile)
        deleteRecursive(outDir)
        stopServer(server)
      }
    }
  }

  for (tc <- PortableCancellationParityCases.All) {
    test(s"portable-cancellation-parity-llvm-wasm-browser-${tc.id}") {
      assume(hasZig, s"zig not found on PATH (skipping LLVM-wasm browser cancellation parity test: ${tc.id})")
      assume(hasWasmTools, s"wasm-tools not found on PATH (skipping LLVM-wasm browser cancellation parity test: ${tc.id})")
      assume(hasJco, s"jco not found on PATH (skipping LLVM-wasm browser cancellation parity test: ${tc.id})")
      assume(hasNode, s"node not found on PATH (skipping LLVM-wasm browser cancellation parity test: ${tc.id})")
      val chromeOpt = findChromeBinary()
      assume(chromeOpt.nonEmpty, s"Chrome/Chromium not found on PATH (skipping LLVM-wasm browser cancellation parity test: ${tc.id})")
      val chrome = chromeOpt.get

      val driverFile = Files.createTempFile(s"flix-portable-llvm-browser-${tc.id}-", ".flix")
      val outDir = Files.createTempDirectory(Paths.get("build"), s"flix-llvm-browser-${tc.id}-")
      val port = findAvailablePort()
      val server = startServer(port)
      try {
        Files.writeString(driverFile, tc.source, StandardCharsets.UTF_8)

        val flix = new Flix()
        flix.setOptions(TestOptions.copy(outputPath = outDir))
        implicit val sctx: SecurityContext = SecurityContext.Unrestricted

        flix.addFile(driverFile)

        val (optRoot, errors) = flix.check()
        if (errors.nonEmpty) {
          fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
        }

        flix.codeGen(optRoot.get)

        val componentJs = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmDriver.componentJsPath(outDir)
        val exportsManifest = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmExportWriter.manifestPath(outDir)

        waitForPort(port, timeoutMs = 5_000)

        val componentUrlPath = toUrlPath(componentJs)
        val exportsUrlPath = toUrlPath(exportsManifest)
        val url =
          s"http://127.0.0.1:$port$browserHtmlPath?component=$componentUrlPath&exports=$exportsUrlPath&budget=500"

        val (exit, output) = runHeadlessChrome(chrome, url)
        if (exit != 0) {
          fail(s"Browser cancellation parity driver '${tc.id}' failed with exit $exit.\nURL: $url\n$output")
        }
        if (output.trim != tc.expectedOutput) {
          fail(s"Expected cancellation parity result ${tc.expectedOutput} for '${tc.id}', but got:\n$output")
        }
      } finally {
        Files.deleteIfExists(driverFile)
        deleteRecursive(outDir)
        stopServer(server)
      }
    }
  }

  private def mkPortableDriverSource(): String = {
    val flix = new Flix()
    flix.setOptions(TestOptions)
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted

    flix.addFile(preludeFile)
    for (p <- FileOps.getFlixFilesIn(portableTestsDir, 1)) flix.addFile(p)

    val (optRoot, errors) = flix.check()
    if (errors.nonEmpty) {
      fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
    }

    val root = optRoot.getOrElse {
      throw new IllegalStateException("Expected a TypedAst root for portable conformance suite.")
    }

    implicit val iflix: Flix = flix
    val tests0 = PortableConformance.collectPortableTests(root)
    val tests = tests0.filterNot(t => unsupportedNamespaces.contains(t.sym.namespace))
    PortableConformance.mkDriverSource(tests, banner = "portable-stdlib-llvm-wasm-browser-runtime")
  }

  private def startServer(port: Int): Process = {
    val pb = new ProcessBuilder(List("node", browserServerScript.toString).asJava)
    pb.redirectErrorStream(true)
    pb.environment().put("PORT", port.toString)
    pb.start()
  }

  private def stopServer(p: Process): Unit = {
    try {
      p.destroy()
      p.waitFor(2, TimeUnit.SECONDS)
    } catch {
      case _: InterruptedException =>
    } finally {
      p.destroyForcibly()
    }
  }

  private def findAvailablePort(): Int = {
    val ss = new ServerSocket()
    try {
      ss.bind(new InetSocketAddress("127.0.0.1", 0))
      ss.getLocalPort
    } finally {
      ss.close()
    }
  }

  private def waitForPort(port: Int, timeoutMs: Long): Unit = {
    val deadline = System.currentTimeMillis() + timeoutMs
    while (System.currentTimeMillis() < deadline) {
      try {
        val s = new Socket()
        s.connect(new InetSocketAddress("127.0.0.1", port), 200)
        s.close()
        return
      } catch {
        case _: IOException =>
          Thread.sleep(50)
      }
    }
    fail(s"Timed out waiting for browser harness server on 127.0.0.1:$port")
  }

  private def runHeadlessChrome(chrome: String, url: String): (Int, String) = {
    val cmd = List(
      "node",
      headlessRunnerScript.toString,
      "--chrome",
      chrome,
      "--url",
      url,
      "--timeoutMs",
      "60000",
    )
    val pb = new ProcessBuilder(cmd.asJava)
    pb.redirectErrorStream(true)
    val p = pb.start()
    val out = new String(p.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val ok = p.waitFor(70, TimeUnit.SECONDS)
    if (!ok) {
      p.destroyForcibly()
      fail(s"Timed out running headless Chrome for: $url")
    }
    (p.exitValue(), out)
  }

  private def toUrlPath(p: Path): String = {
    val root = Paths.get("").toAbsolutePath.normalize()
    val rel = root.relativize(p.toAbsolutePath.normalize())
    "/" + rel.iterator().asScala.mkString("/").replace('\\', '/')
  }

  private def hasWasmTools: Boolean =
    hasCmd(List("wasm-tools", "--version"))

  private def hasJco: Boolean =
    hasCmd(List("jco", "--version"))

  private def hasNode: Boolean =
    hasCmd(List("node", "--version"))

  private def findChromeBinary(): Option[String] = {
    val fromEnv = sys.env.get("FLIX_CHROME").map(_.trim).filter(_.nonEmpty)
    fromEnv match {
      case Some(bin) =>
        if (Files.isExecutable(Paths.get(bin))) return Some(bin)
      case None =>
    }

    // Prefer absolute app bundle paths when present (avoids PATH + cold-start quirks).
    val absCandidates = List(
      "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
      "/Applications/Chromium.app/Contents/MacOS/Chromium",
    )
    absCandidates.find(p => Files.isExecutable(Paths.get(p))) match {
      case some@Some(_) => return some
      case None =>
    }

    val cmdCandidates = List(
      "google-chrome",
      "chromium",
      "chromium-browser",
    )
    cmdCandidates.find(c => hasCmd(List(c, "--version")))
  }

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
