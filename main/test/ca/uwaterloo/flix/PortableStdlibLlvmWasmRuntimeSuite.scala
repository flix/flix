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
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*

/**
  * End-to-end runtime smoke tests for the portable conformance suite on the LLVM-wasm backend.
  *
  * This compiles the portable `@Test` suite to a wasm component (via `wasm-tools component` + `jco transpile`)
  * and executes it using the Node runner (`tools/wasm-runner-js/run-flix.mjs`).
  */
class PortableStdlibLlvmWasmRuntimeSuite extends AnyFunSuite {

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmWasm,
      incremental = false,
      outputJvm = false,
    )

  private val preludeFile = Paths.get("main/test/flix/Prelude.flix")

  private val portableTestsDir = Paths.get("main/test/flix/portable")

  private val wasmRunnerScript: Path =
    Paths.get("tools/wasm-runner-js/run-flix.mjs").toAbsolutePath.normalize()

  /**
    * Known gaps in the current wasm runtime.
    *
    * Keep this list empty if possible: the goal is that portable conformance runs unchanged.
    */
  private val unsupportedNamespaces: Set[List[String]] = Set.empty

  test("portable-stdlib-llvm-wasm-runtime") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm portable stdlib runtime test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm portable stdlib runtime test)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm portable stdlib runtime test)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm portable stdlib runtime test)")

    val driverFile = Files.createTempFile("flix-portable-llvm-wasm-driver-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-wasm-portable-runtime-")
    val sandboxDir = Files.createTempDirectory("flix-llvm-wasm-portable-sandbox-")
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

      val (exit, output) = runNode(componentJs, exportsManifest, sandboxDir)
      if (exit != 0) {
        fail(s"LLVM-wasm portable conformance driver failed with exit $exit:\n$output")
      }
    } finally {
      Files.deleteIfExists(driverFile)
      deleteRecursive(outDir)
      deleteRecursive(sandboxDir)
    }
  }

  test("portable-unicode-print-llvm-wasm") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm unicode print test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm unicode print test)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm unicode print test)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm unicode print test)")

    val driverFile = Files.createTempFile("flix-portable-llvm-wasm-unicode-print-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-wasm-unicode-print-")
    val sandboxDir = Files.createTempDirectory("flix-llvm-wasm-unicode-print-sandbox-")
    try {
      Files.writeString(driverFile, unicodePrintDriverSource, StandardCharsets.UTF_8)

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

      val (exit, output) = runNode(componentJs, exportsManifest, sandboxDir)
      if (exit != 0) {
        fail(s"LLVM-wasm unicode print driver failed with exit $exit:\n$output")
      }
      if (output.trim != "Weather: 12.5°C") {
        fail(s"Expected UTF-8 console output, but got:\n$output")
      }
    } finally {
      Files.deleteIfExists(driverFile)
      deleteRecursive(outDir)
      deleteRecursive(sandboxDir)
    }
  }

  for (tc <- PortableExceptionParityCases.All) {
    test(s"portable-exception-parity-llvm-wasm-${tc.id}") {
      assume(hasZig, s"zig not found on PATH (skipping LLVM-wasm exception parity test: ${tc.id})")
      assume(hasWasmTools, s"wasm-tools not found on PATH (skipping LLVM-wasm exception parity test: ${tc.id})")
      assume(hasJco, s"jco not found on PATH (skipping LLVM-wasm exception parity test: ${tc.id})")
      assume(hasNode, s"node not found on PATH (skipping LLVM-wasm exception parity test: ${tc.id})")

      val driverFile = Files.createTempFile(s"flix-portable-llvm-wasm-${tc.id}-", ".flix")
      val outDir = Files.createTempDirectory(s"flix-llvm-wasm-${tc.id}-")
      val sandboxDir = Files.createTempDirectory(s"flix-llvm-wasm-${tc.id}-sandbox-")
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
        val (exit, output) = runNode(componentJs, exportsManifest, sandboxDir)
        if (exit != 0) {
          fail(s"LLVM-wasm exception parity driver '${tc.id}' failed with exit $exit:\n$output")
        }
        if (output.trim != tc.expectedOutput) {
          fail(s"Expected exception parity result ${tc.expectedOutput} for '${tc.id}', but got:\n$output")
        }
      } finally {
        Files.deleteIfExists(driverFile)
        deleteRecursive(outDir)
        deleteRecursive(sandboxDir)
      }
    }
  }

  for (tc <- PortableControlParityCases.All) {
    test(s"portable-control-parity-llvm-wasm-${tc.id}") {
      assume(hasZig, s"zig not found on PATH (skipping LLVM-wasm control parity test: ${tc.id})")
      assume(hasWasmTools, s"wasm-tools not found on PATH (skipping LLVM-wasm control parity test: ${tc.id})")
      assume(hasJco, s"jco not found on PATH (skipping LLVM-wasm control parity test: ${tc.id})")
      assume(hasNode, s"node not found on PATH (skipping LLVM-wasm control parity test: ${tc.id})")

      val driverFile = Files.createTempFile(s"flix-portable-llvm-wasm-${tc.id}-", ".flix")
      val outDir = Files.createTempDirectory(s"flix-llvm-wasm-${tc.id}-")
      val sandboxDir = Files.createTempDirectory(s"flix-llvm-wasm-${tc.id}-sandbox-")
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
        val (exit, output) = runNode(componentJs, exportsManifest, sandboxDir)
        if (exit != 0) {
          fail(s"LLVM-wasm control parity driver '${tc.id}' failed with exit $exit:\n$output")
        }
        if (output.trim != tc.expectedOutput) {
          fail(s"Expected control parity result ${tc.expectedOutput} for '${tc.id}', but got:\n$output")
        }
      } finally {
        Files.deleteIfExists(driverFile)
        deleteRecursive(outDir)
        deleteRecursive(sandboxDir)
      }
    }
  }

  for (tc <- PortableCancellationParityCases.All) {
    test(s"portable-cancellation-parity-llvm-wasm-${tc.id}") {
      assume(hasZig, s"zig not found on PATH (skipping LLVM-wasm cancellation parity test: ${tc.id})")
      assume(hasWasmTools, s"wasm-tools not found on PATH (skipping LLVM-wasm cancellation parity test: ${tc.id})")
      assume(hasJco, s"jco not found on PATH (skipping LLVM-wasm cancellation parity test: ${tc.id})")
      assume(hasNode, s"node not found on PATH (skipping LLVM-wasm cancellation parity test: ${tc.id})")

      val driverFile = Files.createTempFile(s"flix-portable-llvm-wasm-${tc.id}-", ".flix")
      val outDir = Files.createTempDirectory(s"flix-llvm-wasm-${tc.id}-")
      val sandboxDir = Files.createTempDirectory(s"flix-llvm-wasm-${tc.id}-sandbox-")
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
        val (exit, output) = runNode(componentJs, exportsManifest, sandboxDir)
        if (exit != 0) {
          fail(s"LLVM-wasm cancellation parity driver '${tc.id}' failed with exit $exit:\n$output")
        }
        if (output.trim != tc.expectedOutput) {
          fail(s"Expected cancellation parity result ${tc.expectedOutput} for '${tc.id}', but got:\n$output")
        }
      } finally {
        Files.deleteIfExists(driverFile)
        deleteRecursive(outDir)
        deleteRecursive(sandboxDir)
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
    PortableConformance.mkDriverSource(tests, banner = "portable-stdlib-llvm-wasm-runtime")
  }

  private def runNode(componentJs: Path, exportsManifest: Path, rootDir: Path): (Int, String) = {
    val cmd = List(
      "node",
      wasmRunnerScript.toString,
      "--js",
      componentJs.toAbsolutePath.normalize().toString,
      "--exports",
      exportsManifest.toAbsolutePath.normalize().toString,
      "--rootDir",
      rootDir.toAbsolutePath.normalize().toString,
      // Give the cooperative scheduler a decent budget per step to reduce test runtime.
      "--budget",
      "500"
    )

    val pb = new ProcessBuilder(cmd.asJava)
    pb.redirectErrorStream(true)
    val p = pb.start()
    val output = new String(p.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val exit = p.waitFor()
    (exit, output)
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

  private val unicodePrintDriverSource: String =
    """
      |def main(): Unit \ IO =
      |    println("Weather: 12.5°C")
      |""".stripMargin

}
