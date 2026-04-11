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
  * Compile and opt-in live-network coverage for the real langcensus app on the LLVM-native backend.
  */
class LangcensusAppLlvmNativeSuite extends AnyFunSuite {

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmNative,
      incremental = false,
      outputJvm = false,
      githubToken = sys.env.get("GITHUB_TOKEN"),
    )

  private val langcensusProject: Path =
    Paths.get("examples/apps/langcensus").toAbsolutePath.normalize()

  test("llvm-native-langcensus-app-compiles") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native langcensus compile test)")

    val projectRoot = Files.createTempDirectory("flix-langcensus-native-project-")
    val outDir = Files.createTempDirectory("flix-langcensus-native-out-")
    try {
      copyRecursive(langcensusProject, projectRoot)
      compileProject(projectRoot, outDir)
    } finally {
      deleteRecursive(projectRoot)
      deleteRecursive(outDir)
    }
  }

  test("llvm-native-langcensus-app-live-network") {
    assume(liveNetworkEnabled, "FLIX_LIVE_NETWORK_SMOKE not set (skipping live langcensus app smoke test)")
    assume(githubConfigured, "GITHUB_NAME/GITHUB_TOKEN not set (skipping live langcensus app smoke test)")
    assume(hasZig, "zig not found on PATH (skipping LLVM-native langcensus app smoke test)")

    val projectRoot = Files.createTempDirectory("flix-langcensus-native-project-")
    val outDir = Files.createTempDirectory("flix-langcensus-native-out-")
    try {
      copyRecursive(langcensusProject, projectRoot)

      val exe = compileProject(projectRoot, outDir)
      val (exit, output) = runExecutable(exe, cwd = projectRoot, timeoutSeconds = 180)
      if (exit != 0) {
        fail(s"Langcensus app failed with exit $exit:\n$output")
      }

      assertLangcensusOutput(output)
    } finally {
      deleteRecursive(projectRoot)
      deleteRecursive(outDir)
    }
  }

  private def compileProject(projectRoot: Path, outDir: Path): Path = {
    implicit val formatter: Formatter = Formatter.getDefault
    implicit val out: PrintStream = new PrintStream(OutputStream.nullOutputStream())

    val flix = new Flix()
    flix.setOptions(TestOptions.copy(outputPath = outDir))
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted

    val bootstrap = Bootstrap.bootstrap(projectRoot, TestOptions.githubToken) match {
      case ca.uwaterloo.flix.util.Result.Ok(b) => b
      case ca.uwaterloo.flix.util.Result.Err(e) => fail(s"Bootstrap failed for langcensus app: $e")
    }

    bootstrap.reconfigureFlix(flix) match {
      case ca.uwaterloo.flix.util.Result.Ok(()) => ()
      case ca.uwaterloo.flix.util.Result.Err(e) => fail(s"Reconfigure failed for langcensus app: ${e.message(formatter)}")
    }

    val (optRoot, errors) = flix.check()
    if (errors.nonEmpty) {
      fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
    }

    flix.codeGen(optRoot.get)
    executablePath(outDir)
  }

  private def runExecutable(executable: Path, cwd: Path, timeoutSeconds: Long): (Int, String) = {
    val pb = new ProcessBuilder(List(executable.toString).asJava)
    pb.redirectErrorStream(true)
    pb.directory(cwd.toFile)

    val p = pb.start()
    p.getOutputStream.close()

    val baos = new java.io.ByteArrayOutputStream()
    val is = p.getInputStream
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

    readerThread.join(1_000)

    val output = new String(baos.toByteArray, StandardCharsets.UTF_8)
    if (!finished) {
      fail(s"Langcensus app timed out after ${timeoutSeconds}s. Output so far:\n$output")
    }
    (p.exitValue(), output)
  }

  private def assertLangcensusOutput(rawOutput: String): Unit = {
    val output = stripAnsi(rawOutput).trim
    if (output.contains("Parse Error") || output.contains("Environment variable") || output.contains("[Fatal]")) {
      fail(s"Langcensus app hit failure path:\n$rawOutput")
    }
    val expectedSections = List(
      "Analysis Result Per Repo",
      "Analysis Result For User",
      "Analysis Result By Bytes",
    )
    expectedSections.foreach { section =>
      if (!output.contains(section)) {
        fail(s"Langcensus app output missing expected section '$section':\n$rawOutput")
      }
    }
  }

  private def stripAnsi(s: String): String =
    s.replaceAll("\u001B\\[[0-9;]*m", "")

  private def executablePath(outDir: Path): Path =
    ca.uwaterloo.flix.language.phase.llvm.LlvmNativeDriver.executablePath(outDir, "langcensus")

  private def isWindows: Boolean =
    System.getProperty("os.name", "").toLowerCase.contains("win")

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

  private def githubConfigured: Boolean =
    sys.env.contains("GITHUB_NAME") && sys.env.contains("GITHUB_TOKEN")
}
