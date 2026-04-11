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
import org.scalatest.funsuite.AnyFunSuite

import java.io.IOException
import java.net.{InetSocketAddress, Socket, ServerSocket}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*

/**
  * Focused wasm-host process semantics.
  *
  * Honest scope:
  * - Node and Wasmtime hosts must support real process stdio.
  * - Browser must fail explicitly with `Unsupported` instead of pretending subprocess support exists.
  */
class PortableProcessLlvmWasmSuite extends AnyFunSuite {

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmWasm,
      incremental = false,
      outputJvm = false,
    )

  private val wasmRunnerScript: Path =
    Paths.get("tools/wasm-runner-js/run-flix.mjs").toAbsolutePath.normalize()

  private val wasmRunnerCargoToml: Path =
    Paths.get("tools/wasm-runner-rs/Cargo.toml").toAbsolutePath.normalize()

  private val browserServerScript: Path =
    Paths.get("tools/wasm-runner-js/browser/serve.mjs").toAbsolutePath.normalize()

  private val browserHtmlPath: String =
    "/tools/wasm-runner-js/browser/run_component.html"

  private val headlessRunnerScript: Path =
    Paths.get("tools/wasm-runner-js/browser/run_headless_chrome.mjs").toAbsolutePath.normalize()

  test("portable-process-stdio-llvm-wasm-node") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm portable process stdio test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm portable process stdio test)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm portable process stdio test)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm portable process stdio test)")

    val testFile = Files.createTempFile("flix-portable-process-stdio-llvm-wasm-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-wasm-process-stdio-")
    val sandboxDir = Files.createTempDirectory("flix-llvm-wasm-process-stdio-sandbox-")
    try {
      Files.writeString(testFile, processStdioProgram, StandardCharsets.UTF_8)
      val (componentJs, _, exportsManifest) = compileLlvmWasm(testFile, outDir)
      val (exit, output) = runNode(componentJs, exportsManifest, sandboxDir, timeoutSeconds = 45)
      if (exit != 0) {
        fail(s"LLVM-wasm Node process stdio test program failed with exit $exit:\n$output")
      }
      if (!output.contains("process-stdio: ok")) {
        fail(s"Expected process stdio success banner, but output was:\n$output")
      }
    } finally {
      Files.deleteIfExists(testFile)
      deleteRecursive(outDir)
      deleteRecursive(sandboxDir)
    }
  }

  test("portable-process-stdio-llvm-wasm-wasmtime") {
    assume(hasZig, "zig not found on PATH (skipping Wasmtime portable process stdio test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping Wasmtime portable process stdio test)")
    assume(hasJco, "jco not found on PATH (skipping Wasmtime portable process stdio test)")
    assume(hasCargoStable, "cargo +stable not available (skipping Wasmtime portable process stdio test)")
    assume(hasNode, "node not found on PATH (skipping Wasmtime portable process stdio test)")

    val testFile = Files.createTempFile("flix-portable-process-stdio-llvm-wasmtime-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-wasm-process-stdio-wasmtime-")
    val sandboxDir = Files.createTempDirectory("flix-llvm-wasm-process-stdio-wasmtime-sandbox-")
    try {
      Files.writeString(testFile, processStdioProgram, StandardCharsets.UTF_8)
      val (_, componentWasm, exportsManifest) = compileLlvmWasm(testFile, outDir)
      val (exit, output) = runWasmtime(componentWasm, exportsManifest, sandboxDir)
      if (exit != 0) {
        fail(s"LLVM-wasm Wasmtime process stdio test program failed with exit $exit:\n$output")
      }
      if (!output.contains("process-stdio: ok")) {
        fail(s"Expected process stdio success banner, but output was:\n$output")
      }
    } finally {
      Files.deleteIfExists(testFile)
      deleteRecursive(outDir)
      deleteRecursive(sandboxDir)
    }
  }

  test("portable-process-browser-unsupported-llvm-wasm") {
    assume(hasZig, "zig not found on PATH (skipping browser portable process unsupported test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping browser portable process unsupported test)")
    assume(hasJco, "jco not found on PATH (skipping browser portable process unsupported test)")
    assume(hasNode, "node not found on PATH (skipping browser portable process unsupported test)")

    val chromeOpt = findChromeBinary()
    assume(chromeOpt.nonEmpty, "chrome/chromium not found (skipping browser portable process unsupported test)")
    val chrome = chromeOpt.get

    val testFile = Files.createTempFile("flix-portable-process-browser-unsupported-", ".flix")
    val outDir = Files.createTempDirectory(Paths.get("build"), "flix-llvm-wasm-process-browser-")
    val port = findAvailablePort()
    val server = startServer(port)
    try {
      Files.writeString(testFile, browserUnsupportedProgram, StandardCharsets.UTF_8)
      val (componentJs, _, exportsManifest) = compileLlvmWasm(testFile, outDir)

      waitForPort(port, timeoutMs = 5_000)

      val componentUrlPath = toUrlPath(componentJs)
      val exportsUrlPath = toUrlPath(exportsManifest)
      val url =
        s"http://127.0.0.1:$port$browserHtmlPath?component=$componentUrlPath&exports=$exportsUrlPath&budget=500"

      val (exit, output) = runHeadlessChrome(chrome, url)
      if (exit != 0) {
        fail(s"Browser process unsupported test failed with exit $exit.\nURL: $url\n$output")
      }
      if (!output.contains("process-browser-unsupported: ok")) {
        fail(s"Expected browser unsupported success banner, but output was:\n$output")
      }
    } finally {
      Files.deleteIfExists(testFile)
      deleteRecursive(outDir)
      stopServer(server)
    }
  }

  private def compileLlvmWasm(file: Path, outDir: Path): (Path, Path, Path) = {
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
    val componentWasm = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmDriver.componentWasmPath(outDir)
    val exportsManifest = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmExportWriter.manifestPath(outDir)

    if (!Files.exists(componentJs)) {
      fail(s"Missing wasm JS component artifact: $componentJs")
    }
    if (!Files.exists(componentWasm)) {
      fail(s"Missing wasm component artifact: $componentWasm")
    }
    if (!Files.exists(exportsManifest)) {
      fail(s"Missing wasm exports manifest: $exportsManifest")
    }

    (componentJs, componentWasm, exportsManifest)
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
    )

    val pb = new ProcessBuilder(cmd.asJava)
    pb.redirectErrorStream(true)
    val p = pb.start()
    val output = new String(p.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val ok = p.waitFor(timeoutSeconds, TimeUnit.SECONDS)
    if (!ok) {
      p.destroyForcibly()
      fail(s"Node wasm process test timed out after ${timeoutSeconds}s. Output so far:\n$output")
    }
    (p.exitValue(), output)
  }

  private def runWasmtime(componentWasm: Path, exportsManifest: Path, rootDir: Path): (Int, String) = {
    val cmd = List(
      "cargo",
      "+stable",
      "run",
      "--quiet",
      "--manifest-path",
      wasmRunnerCargoToml.toString,
      "--bin",
      "run_flix",
      "--",
      componentWasm.toAbsolutePath.normalize().toString,
      "--exports",
      exportsManifest.toAbsolutePath.normalize().toString,
      "--rootDir",
      rootDir.toAbsolutePath.normalize().toString,
      "--budget",
      "500",
    )

    val pb = new ProcessBuilder(cmd.asJava)
    pb.redirectErrorStream(true)
    val p = pb.start()
    val output = new String(p.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val exit = p.waitFor()
    (exit, output)
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
      case _: InterruptedException => ()
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
        case _: IOException => Thread.sleep(50)
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

  private def hasCargoStable: Boolean =
    hasCmd(List("cargo", "+stable", "--version"))

  private def findChromeBinary(): Option[String] = {
    val fromEnv = sys.env.get("FLIX_CHROME").map(_.trim).filter(_.nonEmpty)
    fromEnv match {
      case Some(bin) if Files.isExecutable(Paths.get(bin)) => return Some(bin)
      case _ => ()
    }

    val absCandidates = List(
      "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
      "/Applications/Chromium.app/Contents/MacOS/Chromium",
    )
    absCandidates.find(p => Files.isExecutable(Paths.get(p))) match {
      case s @ Some(_) => s
      case None =>
        List("google-chrome", "chromium", "chromium-browser").find(bin => hasCmd(List(bin, "--version")))
    }
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

  private def escapeFlixString(s: String): String =
    "\"" + s.flatMap {
      case '\\' => "\\\\"
      case '"' => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c => c.toString
    } + "\""

  private val processChildScript =
    "process.stdin.once('data',()=>{process.stdout.write('O');process.stderr.write('E');process.exit(0);});process.stdin.resume();"

  private val processStdioProgram: String =
    s"""
       |def expect(cond: Bool, msg: String): Unit =
       |    if (cond) () else bug!(msg)
       |
       |def expectOk(res: Result[IoError, a]): a = match res {
       |    case Ok(v) => v
       |    case Err(e) => bug!("expected Ok but got Err: " + ToString.toString(e))
       |}
       |
       |def expectOkUnit(res: Result[IoError, Unit]): Unit = match res {
       |    case Ok(_) => ()
       |    case Err(e) => bug!("expected Ok but got Err: " + ToString.toString(e))
       |}
       |
       |def expectByte(buf: Array[Int8, r], n: Int32, expected: Int8, label: String): Unit \\ r =
       |    if (n != 1i32)
       |        bug!(label + ": expected exactly 1 byte, got " + Int32.toString(n))
       |    else if (Array.get(0, buf) != expected)
       |        bug!(label + ": unexpected byte " + Int8.toString(Array.get(0, buf)))
       |    else
       |        ()
       |
       |def main(): Unit \\ IO =
       |    run {
       |        let ph = ProcessWithResult.exec("node", "-e" :: ${escapeFlixString(processChildScript)} :: Nil) |> expectOk;
       |
       |        region rc {
       |            let stdin = Process.StdIn.StdIn(ph);
       |            let stdout = Process.StdOut.StdOut(ph);
       |            let stderr = Process.StdErr.StdErr(ph);
       |
       |            let input: Array[Int8, Static] = Array#{120i8} @ Static;
       |            let wrote = Writable.write(input, stdin) |> expectOk;
       |            let _ = expect(wrote == 1i32, "expected stdin write to report 1 byte");
       |
       |            let outBuf: Array[Int8, rc] = Array.empty(rc, 8);
       |            let errBuf: Array[Int8, rc] = Array.empty(rc, 8);
       |
       |            let outN = Readable.read(outBuf, stdout) |> expectOk;
       |            let errN = Readable.read(errBuf, stderr) |> expectOk;
       |
       |            expectByte(outBuf, outN, 79i8, "stdout");
       |            expectByte(errBuf, errN, 69i8, "stderr");
       |            ()
       |        };
       |
       |        let exitCode = ProcessWithResult.waitFor(ph) |> expectOk;
       |        let _ = expect(exitCode == 0i32, "expected child exit code 0");
       |        ProcessHandle.release(ph) |> expectOkUnit;
       |        println("process-stdio: ok");
       |        ()
       |    } with ProcessWithResult.runWithIO
       |""".stripMargin

  private val browserUnsupportedProgram: String =
    """
      |def main(): Unit \ IO =
      |    match ProcessWithResult.runWithIO(() -> ProcessWithResult.exec("node", "-e" :: "process.exit(0)" :: Nil)) {
      |        case Err(IoError.IoError(IoError.ErrorKind.Unsupported, _)) =>
      |            println("process-browser-unsupported: ok")
      |        case Err(e) =>
      |            bug!("expected Unsupported in browser, got: " + ToString.toString(e))
      |        case Ok(_) =>
      |            bug!("expected browser process exec to be unsupported")
      |    }
      |""".stripMargin
}
