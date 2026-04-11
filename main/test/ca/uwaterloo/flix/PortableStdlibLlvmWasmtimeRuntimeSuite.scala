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
  * Executes the portable conformance driver as a wasm component in Wasmtime (embedded via Rust).
  *
  * This complements the Node runner suites by ensuring we can instantiate and run the component
  * in a non-JS host (closer to the WASI/component ecosystem).
  *
  * The test is assume-gated because it depends on external toolchains (`zig`, `wasm-tools`, `jco`, `cargo +stable`).
  */
class PortableStdlibLlvmWasmtimeRuntimeSuite extends AnyFunSuite {

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmWasm,
      incremental = false,
      outputJvm = false,
    )

  private val preludeFile = Paths.get("main/test/flix/Prelude.flix")

  private val portableTestsDir = Paths.get("main/test/flix/portable")

  private val wasmRunnerCargoToml: Path =
    Paths.get("tools/wasm-runner-rs/Cargo.toml").toAbsolutePath.normalize()

  /**
    * Known gaps in the current wasm runtime.
    *
    * Keep this list empty if possible: the goal is that portable conformance runs unchanged.
    */
  private val unsupportedNamespaces: Set[List[String]] = Set.empty

  test("portable-stdlib-llvm-wasm-wasmtime-runtime") {
    assume(hasZig, "zig not found on PATH (skipping Wasmtime wasm runtime test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping Wasmtime wasm runtime test)")
    assume(hasJco, "jco not found on PATH (skipping Wasmtime wasm runtime test)")
    assume(hasCargoStable, "cargo +stable not available (skipping Wasmtime wasm runtime test)")

    val driverFile = Files.createTempFile("flix-portable-llvm-wasm-wasmtime-driver-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-wasm-wasmtime-portable-runtime-")
    val sandboxDir = Files.createTempDirectory("flix-llvm-wasm-wasmtime-portable-sandbox-")
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

      val componentWasm = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmDriver.componentWasmPath(outDir)
      val exportsManifest = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmExportWriter.manifestPath(outDir)

      if (!Files.exists(componentWasm)) {
        fail(s"Missing wasm component artifact: $componentWasm")
      }
      if (!Files.exists(exportsManifest)) {
        fail(s"Missing wasm exports manifest: $exportsManifest")
      }

      val (exit, output) = runWasmtime(componentWasm, exportsManifest, sandboxDir)
      if (exit != 0) {
        fail(s"Wasmtime portable conformance driver failed with exit $exit:\n$output")
      }
    } finally {
      Files.deleteIfExists(driverFile)
      deleteRecursive(outDir)
      deleteRecursive(sandboxDir)
    }
  }

  for (tc <- PortableExceptionParityCases.All) {
    test(s"portable-exception-parity-llvm-wasm-wasmtime-${tc.id}") {
      assume(hasZig, s"zig not found on PATH (skipping Wasmtime exception parity test: ${tc.id})")
      assume(hasWasmTools, s"wasm-tools not found on PATH (skipping Wasmtime exception parity test: ${tc.id})")
      assume(hasJco, s"jco not found on PATH (skipping Wasmtime exception parity test: ${tc.id})")
      assume(hasCargoStable, s"cargo +stable not available (skipping Wasmtime exception parity test: ${tc.id})")

      val driverFile = Files.createTempFile(s"flix-portable-llvm-wasm-wasmtime-${tc.id}-", ".flix")
      val outDir = Files.createTempDirectory(s"flix-llvm-wasm-wasmtime-${tc.id}-")
      val sandboxDir = Files.createTempDirectory(s"flix-llvm-wasm-wasmtime-${tc.id}-sandbox-")
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

        val componentWasm = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmDriver.componentWasmPath(outDir)
        val exportsManifest = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmExportWriter.manifestPath(outDir)
        val (exit, output) = runWasmtime(componentWasm, exportsManifest, sandboxDir)
        if (exit != 0) {
          fail(s"Wasmtime exception parity driver '${tc.id}' failed with exit $exit:\n$output")
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
    test(s"portable-control-parity-llvm-wasm-wasmtime-${tc.id}") {
      assume(hasZig, s"zig not found on PATH (skipping LLVM-wasm Wasmtime control parity test: ${tc.id})")
      assume(hasWasmTools, s"wasm-tools not found on PATH (skipping LLVM-wasm Wasmtime control parity test: ${tc.id})")
      assume(hasCargoStable, s"cargo +stable not found on PATH (skipping LLVM-wasm Wasmtime control parity test: ${tc.id})")

      val driverFile = Files.createTempFile(s"flix-portable-llvm-wasmtime-${tc.id}-", ".flix")
      val outDir = Files.createTempDirectory(s"flix-llvm-wasmtime-${tc.id}-")
      val sandboxDir = Files.createTempDirectory(s"flix-llvm-wasm-wasmtime-${tc.id}-sandbox-")
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

        val componentWasm = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmDriver.componentWasmPath(outDir)
        val exportsManifest = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmExportWriter.manifestPath(outDir)
        val (exit, output) = runWasmtime(componentWasm, exportsManifest, sandboxDir)
        if (exit != 0) {
          fail(s"LLVM-wasm Wasmtime control parity driver '${tc.id}' failed with exit $exit:\n$output")
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
    test(s"portable-cancellation-parity-llvm-wasm-wasmtime-${tc.id}") {
      assume(hasZig, s"zig not found on PATH (skipping LLVM-wasm Wasmtime cancellation parity test: ${tc.id})")
      assume(hasWasmTools, s"wasm-tools not found on PATH (skipping LLVM-wasm Wasmtime cancellation parity test: ${tc.id})")
      assume(hasCargoStable, s"cargo +stable not found on PATH (skipping LLVM-wasm Wasmtime cancellation parity test: ${tc.id})")

      val driverFile = Files.createTempFile(s"flix-portable-llvm-wasmtime-${tc.id}-", ".flix")
      val outDir = Files.createTempDirectory(s"flix-llvm-wasmtime-${tc.id}-")
      val sandboxDir = Files.createTempDirectory(s"flix-llvm-wasm-wasmtime-${tc.id}-sandbox-")
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

        val componentWasm = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmDriver.componentWasmPath(outDir)
        val exportsManifest = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmExportWriter.manifestPath(outDir)
        val (exit, output) = runWasmtime(componentWasm, exportsManifest, sandboxDir)
        if (exit != 0) {
          fail(s"LLVM-wasm Wasmtime cancellation parity driver '${tc.id}' failed with exit $exit:\n$output")
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
    PortableConformance.mkDriverSource(tests, banner = "portable-stdlib-llvm-wasm-wasmtime-runtime")
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

  private def hasWasmTools: Boolean =
    hasCmd(List("wasm-tools", "--version"))

  private def hasJco: Boolean =
    hasCmd(List("jco", "--version"))

  private def hasCargoStable: Boolean =
    hasCmd(List("cargo", "+stable", "--version"))

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
