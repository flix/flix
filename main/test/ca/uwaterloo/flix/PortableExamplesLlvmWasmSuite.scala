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
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*

/**
  * End-to-end "portable examples" for the LLVM-wasm backend.
  *
  * This suite runs Flix programs through the browser-first wasm component toolchain and executes them
  * via the Node host bridge (`tools/wasm-runner-js/run-flix.mjs`).
  */
class PortableExamplesLlvmWasmSuite extends AnyFunSuite {

  private sealed trait StdoutExpectation

  private object StdoutExpectation {
    case class Exact(value: String) extends StdoutExpectation
    case class Regex(pattern: String) extends StdoutExpectation
    case class OneOf(values: Set[String]) extends StdoutExpectation
  }

  private case class FileExpectation(path: String, contentsNormalized: String)

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmWasm,
      incremental = false,
      outputJvm = false,
    )

  private val wasmRunnerScript: Path =
    Paths.get("tools/wasm-runner-js/run-flix.mjs").toAbsolutePath.normalize()

  private case class Example(name: String,
                             file: Path,
                             stdout: StdoutExpectation,
                             expectedFiles: List[FileExpectation] = Nil,
                             timeoutSeconds: Long = 25)

  // Keep this list small and stable; add new examples intentionally as the wasm substrate grows.
  private val Examples: List[Example] = List(
    Example(
      name = "functional-adt-pattern",
      file = Paths.get("examples/functional-style/algebraic-data-types-and-pattern-matching.flix"),
      stdout = StdoutExpectation.Exact("8")
    ),
    Example(
      name = "functional-hof",
      file = Paths.get("examples/functional-style/higher-order-functions.flix"),
      stdout = StdoutExpectation.Exact("127")
    ),
    Example(
      name = "functional-pipeline",
      file = Paths.get("examples/functional-style/function-composition-pipelines-and-currying.flix"),
      stdout = StdoutExpectation.Exact("true")
    ),
    Example(
      name = "functional-lists",
      file = Paths.get("examples/functional-style/lists-and-list-processing.flix"),
      stdout = StdoutExpectation.Exact("22")
    ),
    Example(
      name = "functional-tce-mutual",
      file = Paths.get("examples/functional-style/mutual-recursion-with-full-tail-call-elimination.flix"),
      stdout = StdoutExpectation.Exact("true")
    ),
    Example(
      name = "modules-declaring",
      file = Paths.get("examples/modules/declaring-a-module.flix"),
      stdout = StdoutExpectation.Exact("579")
    ),
    Example(
      name = "records-poly-update",
      file = Paths.get("examples/records/polymorphic-record-update.flix"),
      stdout = StdoutExpectation.Exact("4")
    ),
    Example(
      name = "package-minimal-main",
      file = Paths.get("examples/package-manager/minimal-project/src/Main.flix"),
      stdout = StdoutExpectation.Exact("Hello World!")
    ),
    Example(
      name = "effects-logger",
      file = Paths.get("examples/effects-and-handlers/using-Logger.flix"),
      stdout = StdoutExpectation.Regex("(?s).*\\[Info\\].*Hello.*\\[Warn\\].*World.*")
    ),
    Example(
      name = "effects-file-write",
      file = Paths.get("examples/effects-and-handlers/using-FileWriteWithResult.flix"),
      stdout = StdoutExpectation.Exact(""),
      expectedFiles = List(FileExpectation("data.txt", "Hello\nWorld\n"))
    ),
    Example(
      name = "effects-collatz",
      file = Paths.get("examples/effects-and-handlers/advanced/collatz.flix"),
      stdout = StdoutExpectation.Exact("573 took 105 steps to go to 1")
    ),
    Example(
      name = "effects-nqueens",
      file = Paths.get("examples/effects-and-handlers/advanced/nqueens.flix"),
      stdout = StdoutExpectation.Exact("92")
    ),
    Example(
      name = "datalog-compiler-puzzle",
      file = Paths.get("examples/datalog/compiler-puzzle.flix"),
      stdout = StdoutExpectation.Exact("Vector#{(C++, x86), (MiniScala, C++), (MiniScala, x86), (Scala, C++), (Scala, MiniScala), (Scala, x86)}")
    ),
    Example(
      name = "datalog-graph-closure",
      file = Paths.get("examples/datalog/graph-closure.flix"),
      stdout = StdoutExpectation.Exact("Set#{(1, 2), (1, 3), (2, 3), (4, 5), (4, 6), (5, 6)}")
    ),
    Example(
      name = "datalog-graph-cycle",
      file = Paths.get("examples/datalog/graph-cycle.flix"),
      stdout = StdoutExpectation.Exact("Some(Vector#{1, 2, 3, 1})")
    ),
    Example(
      name = "datalog-dependency-resolution",
      file = Paths.get("examples/datalog/dependency-resolution.flix"),
      stdout = StdoutExpectation.Regex("(?s).*postcss@8 requires autoprefixer >= 11 \\(MISSING\\).*next@14 requires postcss >= 8.*string-formatter@1 requires left-pad >= 1 \\(MISSING\\).*next-themes@1 requires string-formatter >= 1.*")
    ),
    Example(
      name = "datalog-graph-reachability",
      file = Paths.get("examples/datalog/graph-reachability.flix"),
      stdout = StdoutExpectation.Exact("Set#{4, 5, 6}")
    ),
  )

  for (Example(name, file, stdout, expectedFiles, timeoutSeconds) <- Examples) {
    test(s"llvm-wasm-portable-example-$name") {
      assume(hasZig, "zig not found on PATH (skipping LLVM-wasm portable examples)")
      assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm portable examples)")
      assume(hasJco, "jco not found on PATH (skipping LLVM-wasm portable examples)")
      assume(hasNode, "node not found on PATH (skipping LLVM-wasm portable examples)")

      val outDir = Files.createTempDirectory(s"flix-llvm-wasm-portable-example-$name-")
      val sandboxDir = Files.createTempDirectory(s"flix-llvm-wasm-portable-example-$name-sandbox-")
      try {
        val (componentJs, exportsManifest) = compileLlvmWasm(file, outDir)
        val (exit, output) = runNode(
          componentJs,
          exportsManifest,
          rootDir = sandboxDir,
          timeoutSeconds = timeoutSeconds,
        )
        if (exit != 0) {
          fail(s"Example '$name' failed with exit $exit:\n$output")
        }
        assertStdout(name, stdout, output)
        assertFiles(name, sandboxDir, expectedFiles)
      } finally {
        deleteRecursive(outDir)
        deleteRecursive(sandboxDir)
      }
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

  private def assertStdout(name: String, expectation: StdoutExpectation, output: String): Unit = {
    val trimmed = output.trim
    expectation match {
      case StdoutExpectation.Exact(expected) =>
        if (trimmed != expected) {
          fail(s"Example '$name' output mismatch.\nExpected: '$expected'\nActual:   '$trimmed'\nRaw:\n$output")
        }
      case StdoutExpectation.Regex(pattern) =>
        if (!trimmed.matches(pattern)) {
          fail(s"Example '$name' output mismatch.\nExpected regex: '$pattern'\nActual:         '$trimmed'\nRaw:\n$output")
        }
      case StdoutExpectation.OneOf(values) =>
        if (!values.contains(trimmed)) {
          fail(s"Example '$name' output mismatch.\nExpected one of: ${values.toList.sorted.mkString(", ")}\nActual:          '$trimmed'\nRaw:\n$output")
        }
    }
  }

  private def assertFiles(name: String, rootDir: Path, expectedFiles: List[FileExpectation]): Unit =
    expectedFiles.foreach { expected =>
      val file = rootDir.resolve(expected.path)
      if (!Files.exists(file)) {
        fail(s"Example '$name' expected file not found: $file")
      }
      val actual = normalizeNewlines(Files.readString(file, StandardCharsets.UTF_8))
      if (actual != expected.contentsNormalized) {
        fail(s"Example '$name' file content mismatch for ${expected.path}.\nExpected:\n${expected.contentsNormalized}\nActual:\n$actual")
      }
    }

  private def normalizeNewlines(s: String): String =
    s.replace("\r\n", "\n")

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
