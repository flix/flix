package ca.uwaterloo.flix

import ca.uwaterloo.flix.api.{Bootstrap, Flix}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.util.{CompilationTarget, Formatter, Options, StdlibProfile}
import org.scalatest.funsuite.AnyFunSuite

import java.io.{IOException, OutputStream, PrintStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*

/**
  * Opt-in live-network smoke test for the real weather app on the LLVM-wasm backend.
  *
  * Enable with:
  *   `FLIX_LIVE_NETWORK_SMOKE=1 ./gradlew test --tests ca.uwaterloo.flix.WeatherAppLlvmWasmSuite`
  */
class WeatherAppLlvmWasmSuite extends AnyFunSuite {

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmWasm,
      incremental = false,
      outputJvm = false,
      githubToken = sys.env.get("GITHUB_TOKEN"),
    )

  private val weatherProject: Path =
    Paths.get("examples/apps/weather").toAbsolutePath.normalize()

  private val wasmRunnerScript: Path =
    Paths.get("tools/wasm-runner-js/run-flix.mjs").toAbsolutePath.normalize()

  test("llvm-wasm-weather-app-live-network") {
    assume(liveNetworkEnabled, "FLIX_LIVE_NETWORK_SMOKE not set (skipping live weather app smoke test)")
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm weather app smoke test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm weather app smoke test)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm weather app smoke test)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm weather app smoke test)")

    val projectRoot = Files.createTempDirectory("flix-weather-app-wasm-project-")
    val outDir = Files.createTempDirectory("flix-weather-app-wasm-out-")
    try {
      copyRecursive(weatherProject, projectRoot)

      val (componentJs, exportsManifest) = compileProject(projectRoot, outDir)
      val (exit, output) = runNode(componentJs, exportsManifest, rootDir = projectRoot, timeoutSeconds = 60)
      if (exit != 0) {
        fail(s"Weather wasm app failed with exit $exit:\n$output")
      }

      assertWeatherOutput(output)
    } finally {
      deleteRecursive(projectRoot)
      deleteRecursive(outDir)
    }
  }

  private def compileProject(projectRoot: Path, outDir: Path): (Path, Path) = {
    implicit val formatter: Formatter = Formatter.getDefault
    implicit val out: PrintStream = new PrintStream(OutputStream.nullOutputStream())

    val flix = new Flix()
    flix.setOptions(TestOptions.copy(outputPath = outDir))
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted

    val bootstrap = Bootstrap.bootstrap(projectRoot, TestOptions.githubToken) match {
      case ca.uwaterloo.flix.util.Result.Ok(b) => b
      case ca.uwaterloo.flix.util.Result.Err(e) => fail(s"Bootstrap failed for weather app: $e")
    }

    bootstrap.reconfigureFlix(flix) match {
      case ca.uwaterloo.flix.util.Result.Ok(()) => ()
      case ca.uwaterloo.flix.util.Result.Err(e) => fail(s"Reconfigure failed for weather app: ${e.message(formatter)}")
    }

    val (optRoot, errors) = flix.check()
    if (errors.nonEmpty) {
      fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
    }

    flix.codeGen(optRoot.get)

    val componentJs = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmDriver.componentJsPath(outDir, "weather")
    val exportsManifest = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmExportWriter.manifestPath(outDir, "weather")

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
      "--httpTimeoutMs",
      "10000",
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
      fail(s"Weather wasm app timed out after ${timeoutSeconds}s. Output so far:\n$output")
    }
    (p.exitValue(), output)
  }

  private def assertWeatherOutput(rawOutput: String): Unit = {
    val output = stripAnsi(rawOutput).trim
    if (output.contains("[Fatal]")) {
      fail(s"Weather wasm app hit fatal path:\n$rawOutput")
    }
    if (!output.matches("(?s).*Weather for .+\\(.+\\) at .+°[NS], .+°[EW]:.*°C.*")) {
      fail(s"Weather wasm app output did not match expected shape:\n$rawOutput")
    }
  }

  private def stripAnsi(s: String): String =
    s.replaceAll("\u001B\\[[0-9;]*m", "")

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

  private def copyRecursive(src: Path, dst: Path): Unit = {
    val stream = Files.walk(src)
    try {
      stream.iterator().asScala.foreach { source =>
        val target = dst.resolve(src.relativize(source).toString)
        if (Files.isDirectory(source)) {
          Files.createDirectories(target)
        } else {
          Files.createDirectories(target.getParent)
          Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING)
        }
      }
    } finally {
      stream.close()
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

  private def liveNetworkEnabled: Boolean =
    sys.env.get("FLIX_LIVE_NETWORK_SMOKE").contains("1")
}
