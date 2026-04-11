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

class PortableBigDecimalLlvmWasmRuntimeSuite extends AnyFunSuite {

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmWasm,
      incremental = false,
      outputJvm = false,
    )

  private val preludeFile = Paths.get("main/test/flix/Prelude.flix")
  private val testsDir = Paths.get("main/test/flix/portable_bigdecimal")

  private val wasmRunnerScript: Path =
    Paths.get("tools/wasm-runner-js/run-flix.mjs").toAbsolutePath.normalize()

  test("portable-bigdecimal-llvm-wasm-runtime") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm BigDecimal slice runtime test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm BigDecimal slice runtime test)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm BigDecimal slice runtime test)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm BigDecimal slice runtime test)")

    val driverFile = Files.createTempFile("flix-portable-bigdecimal-llvm-wasm-driver-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-wasm-portable-bigdecimal-runtime-")
    val sandboxDir = Files.createTempDirectory("flix-llvm-wasm-portable-bigdecimal-sandbox-")
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

      val componentJs = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmDriver.componentJsPath(outDir)
      val exportsManifest = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmExportWriter.manifestPath(outDir)

      if (!Files.exists(componentJs)) {
        fail(s"Missing wasm JS component artifact: $componentJs")
      }
      if (!Files.exists(exportsManifest)) {
        fail(s"Missing wasm exports manifest: $exportsManifest")
      }

      val (exit, output) = runNode(componentJs, exportsManifest, sandboxDir, timeoutSeconds = 45)
      if (exit != 0) {
        fail(s"LLVM-wasm BigDecimal slice driver failed with exit $exit:\n$output")
      }
    } finally {
      Files.deleteIfExists(driverFile)
      deleteRecursive(outDir)
      deleteRecursive(sandboxDir)
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
      throw new IllegalStateException("Expected a TypedAst root for portable BigDecimal slice.")
    }

    implicit val iflix: Flix = flix
    val tests = PortableConformance.collectPortableTests(root)
    PortableConformance.mkDriverSource(tests, banner = "portable-bigdecimal-llvm-wasm-runtime")
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
