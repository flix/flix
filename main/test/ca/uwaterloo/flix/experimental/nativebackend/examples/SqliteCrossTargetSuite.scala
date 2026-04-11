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

import ca.uwaterloo.flix.api.{Bootstrap, Flix}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.phase.llvm.LlvmNativeDriver
import ca.uwaterloo.flix.util.{CompilationTarget, Formatter, Options, StdlibProfile}
import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayOutputStream, IOException, OutputStream, PrintStream}
import java.net.{InetSocketAddress, ServerSocket, Socket}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*

class SqliteCrossTargetSuite extends AnyFunSuite {

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmNative,
      incremental = false,
      outputJvm = false,
      githubToken = sys.env.get("GITHUB_TOKEN"),
    )

  private val ExampleRoot: Path =
    Paths.get("examples/native-backend/sqlite-cross-target").toAbsolutePath.normalize()

  private val BrowserHeadlessRunnerScript: Path =
    Paths.get("tools/wasm-runner-js/browser/run_headless_chrome.mjs").toAbsolutePath.normalize()

  test("sqlite-cross-target-smoke") {
    assume(hasZig, "usable zig command not found (skipping sqlite cross-target smoke test)")
    assume(hasWasmTools, "wasm-tools not found (skipping sqlite cross-target smoke test)")
    assume(hasJco, "jco not found (skipping sqlite cross-target smoke test)")
    assume(hasNode, "node not found (skipping sqlite cross-target smoke test)")
    assume(hasNpm, "npm not found (skipping sqlite cross-target smoke test)")
    assume(hasCargoStable, "cargo +stable not found (skipping sqlite cross-target smoke test)")
    assume(hasPkgConfigSqlite, "pkg-config sqlite3 not available (skipping sqlite cross-target smoke test)")

    val chromeOpt = findChromeBinary()
    assume(chromeOpt.nonEmpty, "chrome/chromium not found (skipping sqlite cross-target smoke test)")
    val chrome = chromeOpt.get

    val workDir = Files.createTempDirectory("flix-sqlite-cross-target-")
    val copiedRoot = workDir.resolve("sqlite-cross-target").normalize()

    try {
      copyExampleFixture(ExampleRoot, copiedRoot)

      val nativeProject = copiedRoot.resolve("native").normalize()
      val wasmProject = copiedRoot.resolve("wasm").normalize()
      val jsHostDir = wasmProject.resolve("host/wasm-js").normalize()
      val rustHostDir = wasmProject.resolve("host/wasm-rust").normalize()

      val nativeOutDir = nativeProject.resolve("build/native").normalize()
      val wasmOutDir = wasmProject.resolve("build/wasm").normalize()

      val nativeExe = compileNativeProject(nativeProject, nativeOutDir)
      compileWasmProject(wasmProject, wasmOutDir)

      val (nativeExit, nativeOutput) = exec(List(nativeExe.toString), nativeProject, timeoutSeconds = 60)
      assert(nativeExit == 0, s"sqlite native example failed with exit $nativeExit:\n$nativeOutput")
      assert(nativeOutput.trim == "1:hello:4", s"unexpected sqlite native output:\n$nativeOutput")

      ensureNodeModules(jsHostDir)

      val (nodeExit, nodeOutput) = exec(List("node", "node-main.mjs"), jsHostDir, timeoutSeconds = 120)
      assert(nodeExit == 0, s"sqlite wasm Node host failed with exit $nodeExit:\n$nodeOutput")
      assertLastNonEmptyLine(nodeOutput, expected = "OK", label = "sqlite wasm Node host")

      val (rustExit, rustOutput) = exec(List("cargo", "+stable", "run", "--quiet"), rustHostDir, timeoutSeconds = 180)
      assert(rustExit == 0, s"sqlite wasm Wasmtime host failed with exit $rustExit:\n$rustOutput")
      assertLastNonEmptyLine(rustOutput, expected = "OK", label = "sqlite wasm Wasmtime host")

      val port = findAvailablePort()
      val server = startServer(jsHostDir, port)
      try {
        waitForPort(port, timeoutMs = 5_000)
        val (browserExit, browserOutput) = runHeadlessChrome(chrome, s"http://127.0.0.1:$port/")
        assert(browserExit == 0, s"sqlite wasm browser host failed with exit $browserExit:\n$browserOutput")
        assert(browserOutput.trim == "1:hello:4", s"unexpected sqlite wasm browser output:\n$browserOutput")
      } finally {
        stopServer(server)
      }
    } finally {
      deleteRecursive(workDir)
    }
  }

  private def compileNativeProject(projectRoot: Path, outDir: Path): Path = {
    implicit val formatter: Formatter = Formatter.getDefault
    implicit val out: PrintStream = new PrintStream(OutputStream.nullOutputStream())

    val flix = new Flix()
    flix.setOptions(TestOptions.copy(target = CompilationTarget.LlvmNative, outputPath = outDir))
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted

    val bootstrap = Bootstrap.bootstrap(projectRoot, TestOptions.githubToken) match {
      case ca.uwaterloo.flix.util.Result.Ok(b) => b
      case ca.uwaterloo.flix.util.Result.Err(e) => fail(s"Bootstrap failed for sqlite native example: $e")
    }

    bootstrap.reconfigureFlix(flix) match {
      case ca.uwaterloo.flix.util.Result.Ok(()) => ()
      case ca.uwaterloo.flix.util.Result.Err(e) => fail(s"Reconfigure failed for sqlite native example: ${e.message(formatter)}")
    }

    val (optRoot, errors) = flix.check()
    if (errors.nonEmpty) {
      fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
    }

    flix.codeGen(optRoot.get)
    LlvmNativeDriver.executablePath(outDir, "sqlite-native")
  }

  private def compileWasmProject(projectRoot: Path, outDir: Path): Unit = {
    implicit val formatter: Formatter = Formatter.getDefault
    implicit val out: PrintStream = new PrintStream(OutputStream.nullOutputStream())

    val flix = new Flix()
    flix.setOptions(TestOptions.copy(target = CompilationTarget.LlvmWasm, outputPath = outDir))
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted

    val bootstrap = Bootstrap.bootstrap(projectRoot, TestOptions.githubToken) match {
      case ca.uwaterloo.flix.util.Result.Ok(b) => b
      case ca.uwaterloo.flix.util.Result.Err(e) => fail(s"Bootstrap failed for sqlite wasm example: $e")
    }

    bootstrap.reconfigureFlix(flix) match {
      case ca.uwaterloo.flix.util.Result.Ok(()) => ()
      case ca.uwaterloo.flix.util.Result.Err(e) => fail(s"Reconfigure failed for sqlite wasm example: ${e.message(formatter)}")
    }

    val (optRoot, errors) = flix.check()
    if (errors.nonEmpty) {
      fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
    }

    flix.codeGen(optRoot.get)

    val componentJs = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmDriver.componentJsPath(outDir, "sqlite-wasm")
    val runnerModule = outDir.resolve("llvm/wasm-runner-js/runner.mjs").normalize()
    if (!Files.exists(componentJs)) {
      fail(s"Missing sqlite wasm component artifact: $componentJs")
    }
    if (!Files.exists(runnerModule)) {
      fail(s"Missing sqlite wasm runner module: $runnerModule")
    }
  }

  private def ensureNodeModules(hostDir: Path): Unit = {
    val sqlitePkg = hostDir.resolve("node_modules/@sqlite.org/sqlite-wasm/package.json")
    if (Files.exists(sqlitePkg)) return

    val (exit, output) = exec(List("npm", "ci", "--no-fund", "--no-audit"), hostDir, timeoutSeconds = 240)
    if (exit != 0) {
      fail(s"npm ci failed for sqlite wasm JS host with exit $exit:\n$output")
    }
  }

  private def assertLastNonEmptyLine(rawOutput: String, expected: String, label: String): Unit = {
    val actual = rawOutput.linesIterator.map(_.trim).filter(_.nonEmpty).toList.lastOption.getOrElse("")
    assert(actual == expected, s"unexpected $label output:\n$rawOutput")
  }

  private def copyExampleFixture(src: Path, dst: Path): Unit = {
    val stream = Files.walk(src)
    try {
      stream.iterator().asScala.foreach { source =>
        val rel = src.relativize(source)
        val relText = rel.iterator().asScala.mkString("/")
        val base = source.getFileName.toString
        val relParts = rel.iterator().asScala.map(_.toString).toList
        val skip =
          relParts.contains("build") ||
            relText == "wasm/host/wasm-js/node_modules" ||
            relText.startsWith("wasm/host/wasm-js/node_modules/") ||
            (base.startsWith("crash_report_") && base.endsWith(".txt"))

        if (!skip) {
          val target = dst.resolve(rel.toString)
          if (Files.isDirectory(source)) {
            Files.createDirectories(target)
          } else {
            Files.createDirectories(target.getParent)
            Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING)
          }
        }
      }
    } finally {
      stream.close()
    }
  }

  private def startServer(cwd: Path, port: Int): Process = {
    val pb = new ProcessBuilder(List("node", "serve.mjs").asJava)
    pb.directory(cwd.toFile)
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
    fail(s"Timed out waiting for sqlite browser host server on 127.0.0.1:$port")
  }

  private def runHeadlessChrome(chrome: String, url: String): (Int, String) = {
    val cmd = List(
      "node",
      BrowserHeadlessRunnerScript.toString,
      "--chrome",
      chrome,
      "--url",
      url,
      "--timeoutMs",
      "60000",
    )
    exec(cmd, Paths.get(".").toAbsolutePath.normalize(), timeoutSeconds = 70)
  }

  private def exec(cmd: List[String], cwd: Path, timeoutSeconds: Long): (Int, String) = {
    val pb = new ProcessBuilder(cmd*)
    pb.directory(cwd.toFile)
    pb.redirectErrorStream(true)
    val p = pb.start()

    val baos = new ByteArrayOutputStream()
    val readerThread = new Thread(() => {
      val is = p.getInputStream
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
      fail(s"Command timed out after ${timeoutSeconds}s: ${cmd.mkString(" ")}\n$output")
    }
    (p.exitValue(), output)
  }

  private def hasWasmTools: Boolean =
    hasCmd(List("wasm-tools", "--version"))

  private def hasJco: Boolean =
    hasCmd(List("jco", "--version"))

  private def hasNode: Boolean =
    hasCmd(List("node", "--version"))

  private def hasNpm: Boolean =
    hasCmd(List("npm", "--version"))

  private def hasCargoStable: Boolean =
    hasCmd(List("cargo", "+stable", "--version"))

  private def hasPkgConfigSqlite: Boolean =
    hasCmd(List("pkg-config", "--exists", "sqlite3"))

  private def findChromeBinary(): Option[String] = {
    val home = Paths.get(sys.props("user.home"))

    val fromEnv = sys.env.get("FLIX_CHROME").map(_.trim).filter(_.nonEmpty)
    fromEnv match {
      case Some(bin) if Files.isExecutable(Paths.get(bin)) => return Some(bin)
      case _ =>
    }

    val absCandidates = List(
      "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
      "/Applications/Chromium.app/Contents/MacOS/Chromium",
    )
    absCandidates.find(p => Files.isExecutable(Paths.get(p))) match {
      case some@Some(_) => return some
      case None =>
    }

    findPlaywrightChromeBinary(home) match {
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

  private def findPlaywrightChromeBinary(home: Path): Option[String] = {
    val cacheRoots = List(
      home.resolve("Library/Caches/ms-playwright"),
      home.resolve(".cache/ms-playwright"),
    )

    val suffixes = List(
      Paths.get("chrome-mac-arm64/Google Chrome for Testing.app/Contents/MacOS/Google Chrome for Testing"),
      Paths.get("chrome-mac/Google Chrome for Testing.app/Contents/MacOS/Google Chrome for Testing"),
      Paths.get("chrome-headless-shell-mac-arm64/chrome-headless-shell"),
      Paths.get("chrome-headless-shell-mac-x64/chrome-headless-shell"),
      Paths.get("chrome-linux/chrome"),
      Paths.get("chrome-headless-shell-linux64/chrome-headless-shell"),
    )

    cacheRoots.iterator.flatMap { root =>
      if (!Files.isDirectory(root)) Iterator.empty
      else {
        val stream = Files.list(root)
        try {
          stream.iterator().asScala.toList.iterator.map(_.normalize())
        } finally {
          stream.close()
        }
      }
    }.flatMap { dir =>
      suffixes.iterator.map(dir.resolve)
    }.find(Files.isExecutable(_)).map(_.toString)
  }

  private def hasCmd(cmd: List[String]): Boolean = {
    try {
      val p = new ProcessBuilder(cmd.asJava).redirectErrorStream(true).start()
      p.waitFor(5, TimeUnit.SECONDS) && p.exitValue() == 0
    } catch {
      case _: IOException => false
      case _: InterruptedException => false
    }
  }

  private def deleteRecursive(root: Path): Unit = {
    if (!Files.exists(root)) return
    val stream = Files.walk(root)
    try {
      stream.iterator().asScala.toList
        .sortBy(_.getNameCount)(Ordering.Int.reverse)
        .foreach(p => Files.deleteIfExists(p))
    } finally {
      stream.close()
    }
  }
}
