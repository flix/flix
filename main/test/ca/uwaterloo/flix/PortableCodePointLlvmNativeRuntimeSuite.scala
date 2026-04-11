package ca.uwaterloo.flix

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.util.{CompilationTarget, FileOps, Options, StdlibProfile}
import org.scalatest.funsuite.AnyFunSuite

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*

class PortableCodePointLlvmNativeRuntimeSuite extends AnyFunSuite {

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmNative,
      incremental = false,
      outputJvm = false,
    )

  private val preludeFile = Paths.get("main/test/flix/Prelude.flix")
  private val testsDir = Paths.get("main/test/flix/portable_codepoint")

  test("portable-codepoint-llvm-native-runtime") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native CodePoint slice runtime test)")

    val driverFile = Files.createTempFile("flix-portable-codepoint-llvm-native-driver-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-portable-codepoint-runtime-")
    try {
      val driverSource = mkDriverSource()
      Files.writeString(driverFile, driverSource, StandardCharsets.UTF_8)

      val flix = new Flix()
      flix.setOptions(TestOptions.copy(outputPath = outDir))
      implicit val sctx: SecurityContext = SecurityContext.Unrestricted

      flix.addFile(preludeFile)
      for (p <- FileOps.getFlixFilesIn(testsDir, 1)) flix.addFile(p)
      flix.addFile(driverFile)

      val (optRoot, errors) = flix.check()
      if (errors.nonEmpty) {
        fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
      }

      flix.codeGen(optRoot.get)

      val (exit, output) = runExecutable(executablePath(outDir))
      if (exit != 0) {
        fail(s"LLVM-native CodePoint slice driver failed with exit $exit:\n$output")
      }
    } finally {
      Files.deleteIfExists(driverFile)
      deleteRecursive(outDir)
    }
  }

  private def mkDriverSource(): String = {
    val flix = new Flix()
    flix.setOptions(TestOptions)
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted

    flix.addFile(preludeFile)
    for (p <- FileOps.getFlixFilesIn(testsDir, 1)) flix.addFile(p)

    val (optRoot, errors) = flix.check()
    if (errors.nonEmpty) {
      fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
    }

    val root = optRoot.getOrElse {
      throw new IllegalStateException("Expected a TypedAst root for portable CodePoint slice.")
    }

    implicit val iflix: Flix = flix
    val tests = PortableConformance.collectPortableTests(root)
    PortableConformance.mkDriverSource(tests, banner = "portable-codepoint-llvm-native-runtime")
  }

  private def runExecutable(exe: Path): (Int, String) = {
    val pb = new ProcessBuilder(List(exe.toString).asJava)
    pb.redirectErrorStream(true)
    val p = pb.start()
    val output = new String(p.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val exit = p.waitFor()
    (exit, output)
  }

  private def executablePath(outDir: Path): Path =
    ca.uwaterloo.flix.language.phase.llvm.LlvmNativeDriver.executablePath(outDir)

  private def isWindows: Boolean =
    System.getProperty("os.name").toLowerCase.contains("win")

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
