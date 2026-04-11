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

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*

/**
  * End-to-end runtime smoke tests for the portable conformance suite on the LLVM-native backend.
  *
  * This compiles+links+executes a small driver that calls the portable `@Test` functions explicitly.
  */
class PortableStdlibLlvmNativeRuntimeSuite extends AnyFunSuite {

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmNative,
      incremental = false,
      outputJvm = false,
    )

  private val preludeFile = Paths.get("main/test/flix/Prelude.flix")

  private val portableTestsDir = Paths.get("main/test/flix/portable")

  test("portable-stdlib-llvm-native-runtime") {
    assume(hasZig, "usable zig command not found (skipping LLVM-native portable stdlib runtime test)")

    val driverFile = Files.createTempFile("flix-portable-llvm-native-driver-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-portable-runtime-")
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

      val (exit, output) = runExecutable(executablePath(outDir))
      if (exit != 0) {
        fail(s"LLVM-native portable conformance driver failed with exit $exit:\n$output")
      }
    } finally {
      Files.deleteIfExists(driverFile)
      deleteRecursive(outDir)
    }
  }

  test("portable-uncaught-exception-llvm-native") {
    assume(hasZig, "usable zig command not found (skipping LLVM-native portable uncaught exception test)")

    val driverFile = Files.createTempFile("flix-portable-llvm-native-uncaught-exn-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-portable-uncaught-exn-")
    try {
      Files.writeString(driverFile, uncaughtExnDriverSource, StandardCharsets.UTF_8)

      val flix = new Flix()
      flix.setOptions(TestOptions.copy(outputPath = outDir))
      implicit val sctx: SecurityContext = SecurityContext.Unrestricted

      flix.addFile(driverFile)

      val (optRoot, errors) = flix.check()
      if (errors.nonEmpty) {
        fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
      }

      flix.codeGen(optRoot.get)

      val (exit, output) = runExecutable(executablePath(outDir))
      if (exit != 1) {
        fail(s"Expected exit code 1 for uncaught exception, but got $exit:\n$output")
      }
      if (!output.contains("Uncaught Flix exception")) {
        fail(s"Expected uncaught exception report, but output was:\n$output")
      }
      if (!output.contains("  at ")) {
        fail(s"Expected at least one trace frame, but output was:\n$output")
      }
      if (!output.contains("Test.Portable.UncaughtExn")) {
        fail(s"Expected trace to contain def symbol names, but output was:\n$output")
      }
    } finally {
      Files.deleteIfExists(driverFile)
      deleteRecursive(outDir)
    }
  }

  test("portable-unhandled-suspension-llvm-native") {
    assume(hasZig, "usable zig command not found (skipping LLVM-native portable unhandled suspension test)")

    val driverFile = Files.createTempFile("flix-portable-llvm-native-unhandled-susp-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-portable-unhandled-susp-")
    try {
      Files.writeString(driverFile, unhandledSuspensionDriverSource, StandardCharsets.UTF_8)

      val flix = new Flix()
      flix.setOptions(TestOptions.copy(outputPath = outDir))
      implicit val sctx: SecurityContext = SecurityContext.Unrestricted

      flix.addFile(driverFile)

      val (optRoot, errors) = flix.check()
      if (errors.nonEmpty) {
        fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
      }

      flix.codeGen(optRoot.get)

      val (exit, output) = runExecutable(executablePath(outDir))
      if (exit != 1) {
        fail(s"Expected exit code 1 for unhandled suspension, but got $exit:\n$output")
      }
      if (!output.contains("Unhandled Flix suspension")) {
        fail(s"Expected unhandled suspension report, but output was:\n$output")
      }
      if (!output.contains("Boom.boom")) {
        fail(s"Expected suspension report to contain effect/op names, but output was:\n$output")
      }
    } finally {
      Files.deleteIfExists(driverFile)
      deleteRecursive(outDir)
    }
  }

  test("llvm-native-gc-stress") {
    assume(hasZig, "usable zig command not found (skipping LLVM-native GC stress test)")

    val driverFile = Files.createTempFile("flix-llvm-native-gc-stress-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-gc-stress-")
    try {
      Files.writeString(driverFile, gcStressDriverSource, StandardCharsets.UTF_8)

      val flix = new Flix()
      flix.setOptions(TestOptions.copy(outputPath = outDir))
      implicit val sctx: SecurityContext = SecurityContext.Unrestricted

      flix.addFile(driverFile)

      val (optRoot, errors) = flix.check()
      if (errors.nonEmpty) {
        fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
      }

      flix.codeGen(optRoot.get)

      val (exit, output) = runExecutable(
        executablePath(outDir),
        Map(
          "FLIX_GC_STRESS" -> "1",
          // Keep the heap tiny to ensure collections are requested frequently.
          "FLIX_GC_HEAP_LIMIT_BYTES" -> "65536",
        ),
      )

      if (exit != 0) {
        fail(s"LLVM-native GC stress driver failed with exit $exit:\n$output")
      }
      if (!output.contains("gc-stress: done")) {
        fail(s"Expected GC stress driver to complete, but output was:\n$output")
      }
    } finally {
      Files.deleteIfExists(driverFile)
      deleteRecursive(outDir)
    }
  }

  test("llvm-native-remembered-set-stress") {
    assume(hasZig, "usable zig command not found (skipping LLVM-native remembered-set stress test)")

    val driverFile = Files.createTempFile("flix-llvm-native-remembered-set-stress-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-remembered-set-stress-")
    try {
      Files.writeString(driverFile, rememberedSetStressDriverSource, StandardCharsets.UTF_8)

      val flix = new Flix()
      flix.setOptions(TestOptions.copy(outputPath = outDir))
      implicit val sctx: SecurityContext = SecurityContext.Unrestricted

      flix.addFile(driverFile)

      val (optRoot, errors) = flix.check()
      if (errors.nonEmpty) {
        fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
      }

      flix.codeGen(optRoot.get)

      val (exit, output) = runExecutable(
        executablePath(outDir),
        Map(
          "FLIX_GC_STRESS" -> "1",
          // Keep the heap tiny so the remembered-slot / remembered-array paths are exercised repeatedly.
          "FLIX_GC_HEAP_LIMIT_BYTES" -> "32768",
        ),
      )

      if (exit != 0) {
        fail(s"LLVM-native remembered-set stress driver failed with exit $exit:\n$output")
      }
      if (!output.contains("remembered-set-stress: done")) {
        fail(s"Expected remembered-set stress driver to complete, but output was:\n$output")
      }
    } finally {
      Files.deleteIfExists(driverFile)
      deleteRecursive(outDir)
    }
  }

  test("llvm-native-remembered-set-aggregate-stress") {
    assume(hasZig, "usable zig command not found (skipping LLVM-native aggregate remembered-set stress test)")

    val driverFile = Files.createTempFile("flix-llvm-native-remembered-set-aggregate-stress-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-remembered-set-aggregate-stress-")
    try {
      Files.writeString(driverFile, aggregateRememberedSetStressDriverSource, StandardCharsets.UTF_8)

      val flix = new Flix()
      flix.setOptions(TestOptions.copy(outputPath = outDir))
      implicit val sctx: SecurityContext = SecurityContext.Unrestricted

      flix.addFile(driverFile)

      val (optRoot, errors) = flix.check()
      if (errors.nonEmpty) {
        fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
      }

      flix.codeGen(optRoot.get)

      val (exit, output) = runExecutable(
        executablePath(outDir),
        Map(
          "FLIX_GC_STRESS" -> "1",
          "FLIX_GC_HEAP_LIMIT_BYTES" -> "32768",
        ),
        timeoutMs = 60000L,
      )

      if (exit != 0) {
        fail(s"LLVM-native aggregate remembered-set stress driver failed with exit $exit:\n$output")
      }
      if (!output.contains("remembered-set-aggregate-stress: done")) {
        fail(s"Expected aggregate remembered-set stress driver to complete, but output was:\n$output")
      }
    } finally {
      Files.deleteIfExists(driverFile)
      deleteRecursive(outDir)
    }
  }

  test("llvm-native-remembered-set-container-stress") {
    assume(hasZig, "usable zig command not found (skipping LLVM-native container remembered-set stress test)")

    val driverFile = Files.createTempFile("flix-llvm-native-remembered-set-container-stress-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-remembered-set-container-stress-")
    try {
      Files.writeString(driverFile, containerRememberedSetStressDriverSource, StandardCharsets.UTF_8)

      val flix = new Flix()
      flix.setOptions(TestOptions.copy(outputPath = outDir))
      implicit val sctx: SecurityContext = SecurityContext.Unrestricted

      flix.addFile(driverFile)

      val (optRoot, errors) = flix.check()
      if (errors.nonEmpty) {
        fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
      }

      flix.codeGen(optRoot.get)

      val (exit, output) = runExecutable(
        executablePath(outDir),
        Map(
          "FLIX_GC_STRESS" -> "1",
          "FLIX_GC_HEAP_LIMIT_BYTES" -> "32768",
        ),
        timeoutMs = 60000L,
      )

      if (exit != 0) {
        fail(s"LLVM-native container remembered-set stress driver failed with exit $exit:\n$output")
      }
      if (!output.contains("remembered-set-container-stress: done")) {
        fail(s"Expected container remembered-set stress driver to complete, but output was:\n$output")
      }
    } finally {
      Files.deleteIfExists(driverFile)
      deleteRecursive(outDir)
    }
  }

  test("llvm-native-rooting-stress") {
    assume(hasZig, "usable zig command not found (skipping LLVM-native rooting stress test)")

    val driverFile = Files.createTempFile("flix-llvm-native-rooting-stress-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-rooting-stress-")
    try {
      Files.writeString(driverFile, rootingStressDriverSource, StandardCharsets.UTF_8)

      val flix = new Flix()
      flix.setOptions(TestOptions.copy(outputPath = outDir))
      implicit val sctx: SecurityContext = SecurityContext.Unrestricted

      flix.addFile(driverFile)

      val (optRoot, errors) = flix.check()
      if (errors.nonEmpty) {
        fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
      }

      flix.codeGen(optRoot.get)

      val (exit, output) = runExecutable(
        executablePath(outDir),
        Map(
          "FLIX_GC_STRESS" -> "1",
          "FLIX_GC_HEAP_LIMIT_BYTES" -> "32768",
        ),
      )

      if (exit != 0) {
        fail(s"LLVM-native rooting stress driver failed with exit $exit:\n$output")
      }
      if (!output.contains("rooting-stress: done")) {
        fail(s"Expected rooting stress driver to complete, but output was:\n$output")
      }
    } finally {
      Files.deleteIfExists(driverFile)
      deleteRecursive(outDir)
    }
  }

  test("llvm-native-rooting-aggregate-custom-handler-stress") {
    assume(hasZig, "usable zig command not found (skipping LLVM-native aggregate custom-handler rooting stress test)")

    val driverFile = Files.createTempFile("flix-llvm-native-rooting-aggregate-custom-handler-stress-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-rooting-aggregate-custom-handler-stress-")
    try {
      Files.writeString(driverFile, aggregateCustomHandlerRootingStressDriverSource, StandardCharsets.UTF_8)

      val flix = new Flix()
      flix.setOptions(TestOptions.copy(outputPath = outDir))
      implicit val sctx: SecurityContext = SecurityContext.Unrestricted

      flix.addFile(driverFile)

      val (optRoot, errors) = flix.check()
      if (errors.nonEmpty) {
        fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
      }

      flix.codeGen(optRoot.get)

      val (exit, output) = runExecutable(
        executablePath(outDir),
        Map(
          "FLIX_GC_STRESS" -> "1",
          "FLIX_GC_HEAP_LIMIT_BYTES" -> "32768",
        ),
      )

      if (exit != 0) {
        fail(s"LLVM-native aggregate custom-handler rooting stress driver failed with exit $exit (outDir=$outDir):\n$output")
      }
      if (!output.contains("rooting-aggregate-custom-handler-stress: done")) {
        fail(s"Expected aggregate custom-handler rooting stress driver to complete, but output was:\n$output")
      }
    } finally {
      Files.deleteIfExists(driverFile)
      deleteRecursive(outDir)
    }
  }

  test("llvm-native-rooting-aggregate-default-handler-stress") {
    assume(hasZig, "usable zig command not found (skipping LLVM-native aggregate default-handler rooting stress test)")

    val driverFile = Files.createTempFile("flix-llvm-native-rooting-aggregate-default-handler-stress-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-rooting-aggregate-default-handler-stress-")
    try {
      Files.writeString(driverFile, aggregateDefaultHandlerRootingStressDriverSource, StandardCharsets.UTF_8)

      val flix = new Flix()
      flix.setOptions(TestOptions.copy(outputPath = outDir))
      implicit val sctx: SecurityContext = SecurityContext.Unrestricted

      flix.addFile(driverFile)

      val (optRoot, errors) = flix.check()
      if (errors.nonEmpty) {
        fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
      }

      flix.codeGen(optRoot.get)

      val (exit, output) = runExecutable(
        executablePath(outDir),
        Map(
          "FLIX_GC_STRESS" -> "1",
          "FLIX_GC_HEAP_LIMIT_BYTES" -> "32768",
        ),
      )

      if (exit != 0) {
        fail(s"LLVM-native aggregate default-handler rooting stress driver failed with exit $exit:\n$output")
      }
      if (!output.contains("rooting-aggregate-default-handler-stress: done")) {
        fail(s"Expected aggregate default-handler rooting stress driver to complete, but output was:\n$output")
      }
    } finally {
      Files.deleteIfExists(driverFile)
      deleteRecursive(outDir)
    }
  }

  test("llvm-native-rooting-aggregate-join-stress") {
    assume(hasZig, "usable zig command not found (skipping LLVM-native aggregate join-rooting stress test)")

    val driverFile = Files.createTempFile("flix-llvm-native-rooting-aggregate-join-stress-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-rooting-aggregate-join-stress-")
    try {
      Files.writeString(driverFile, aggregateJoinRootingStressDriverSource, StandardCharsets.UTF_8)

      val flix = new Flix()
      flix.setOptions(TestOptions.copy(outputPath = outDir))
      implicit val sctx: SecurityContext = SecurityContext.Unrestricted

      flix.addFile(driverFile)

      val (optRoot, errors) = flix.check()
      if (errors.nonEmpty) {
        fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
      }

      flix.codeGen(optRoot.get)

      val (exit, output) = runExecutable(
        executablePath(outDir),
        Map(
          "FLIX_GC_STRESS" -> "1",
          "FLIX_GC_HEAP_LIMIT_BYTES" -> "32768",
        ),
      )

      if (exit != 0) {
        fail(s"LLVM-native aggregate join-rooting stress driver failed with exit $exit:\n$output")
      }
      if (!output.contains("rooting-aggregate-join-stress: done")) {
        fail(s"Expected aggregate join-rooting stress driver to complete, but output was:\n$output")
      }
    } finally {
      Files.deleteIfExists(driverFile)
      deleteRecursive(outDir)
    }
  }

  test("llvm-native-pollcheck-handshake-stress") {
    assume(hasZig, "usable zig command not found (skipping LLVM-native pollcheck/handshake stress test)")

    val driverFile = Files.createTempFile("flix-llvm-native-pollcheck-handshake-stress-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-pollcheck-handshake-stress-")
    try {
      Files.writeString(driverFile, pollcheckHandshakeStressDriverSource, StandardCharsets.UTF_8)

      val flix = new Flix()
      flix.setOptions(TestOptions.copy(outputPath = outDir))
      implicit val sctx: SecurityContext = SecurityContext.Unrestricted

      flix.addFile(driverFile)

      val (optRoot, errors) = flix.check()
      if (errors.nonEmpty) {
        fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
      }

      flix.codeGen(optRoot.get)

      val (exit, output) = runExecutable(
        executablePath(outDir),
        Map(
          "FLIX_GC_STRESS" -> "1",
          "FLIX_GC_HEAP_LIMIT_BYTES" -> "32768",
        ),
        timeoutMs = 15000L,
      )

      if (exit != 0) {
        fail(s"LLVM-native pollcheck/handshake stress driver failed with exit $exit:\n$output")
      }
      if (!output.contains("pollcheck-handshake-stress: done")) {
        fail(s"Expected pollcheck/handshake stress driver to complete, but output was:\n$output")
      }
    } finally {
      Files.deleteIfExists(driverFile)
      deleteRecursive(outDir)
    }
  }

  test("portable-unicode-print-llvm-native") {
    assume(hasZig, "usable zig command not found (skipping LLVM-native unicode print test)")

    val driverFile = Files.createTempFile("flix-portable-llvm-native-unicode-print-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-unicode-print-")
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

      val (exit, output) = runExecutable(executablePath(outDir))
      if (exit != 0) {
        fail(s"LLVM-native unicode print driver failed with exit $exit:\n$output")
      }
      if (output.trim != "Weather: 12.5°C") {
        fail(s"Expected UTF-8 console output, but got:\n$output")
      }
    } finally {
      Files.deleteIfExists(driverFile)
      deleteRecursive(outDir)
    }
  }

  for (tc <- PortableExceptionParityCases.All) {
    test(s"portable-exception-parity-llvm-native-${tc.id}") {
      assume(hasZig, s"usable zig command not found (skipping LLVM-native exception parity test: ${tc.id})")

      val driverFile = Files.createTempFile(s"flix-portable-llvm-native-${tc.id}-", ".flix")
      val outDir = Files.createTempDirectory(s"flix-llvm-native-${tc.id}-")
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

        val (exit, output) = runExecutable(executablePath(outDir))
        if (exit != 0) {
          fail(s"LLVM-native exception parity driver '${tc.id}' failed with exit $exit:\n$output")
        }
        if (output.trim != tc.expectedOutput) {
          fail(s"Expected exception parity result ${tc.expectedOutput} for '${tc.id}', but got:\n$output")
        }
      } finally {
        Files.deleteIfExists(driverFile)
        deleteRecursive(outDir)
      }
    }
  }

  for (tc <- PortableControlParityCases.All) {
    test(s"portable-control-parity-llvm-native-${tc.id}") {
      assume(hasZig, s"usable zig command not found (skipping LLVM-native control parity test: ${tc.id})")

      val driverFile = Files.createTempFile(s"flix-portable-llvm-native-${tc.id}-", ".flix")
      val outDir = Files.createTempDirectory(s"flix-llvm-native-${tc.id}-")
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

        val (exit, output) = runExecutable(executablePath(outDir))
        if (exit != 0) {
          fail(s"LLVM-native control parity driver '${tc.id}' failed with exit $exit:\n$output")
        }
        if (output.trim != tc.expectedOutput) {
          fail(s"Expected control parity result ${tc.expectedOutput} for '${tc.id}', but got:\n$output")
        }
      } finally {
        Files.deleteIfExists(driverFile)
        deleteRecursive(outDir)
      }
    }
  }

  for (tc <- PortableCancellationParityCases.All) {
    test(s"portable-cancellation-parity-llvm-native-${tc.id}") {
      assume(hasZig, s"usable zig command not found (skipping LLVM-native cancellation parity test: ${tc.id})")

      val driverFile = Files.createTempFile(s"flix-portable-llvm-native-${tc.id}-", ".flix")
      val outDir = Files.createTempDirectory(s"flix-llvm-native-${tc.id}-")
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

        val (exit, output) = runExecutable(executablePath(outDir))
        if (exit != 0) {
          fail(s"LLVM-native cancellation parity driver '${tc.id}' failed with exit $exit:\n$output")
        }
        if (output.trim != tc.expectedOutput) {
          fail(s"Expected cancellation parity result ${tc.expectedOutput} for '${tc.id}', but got:\n$output")
        }
      } finally {
        Files.deleteIfExists(driverFile)
        deleteRecursive(outDir)
      }
    }
  }

  test("llvm-native-cancel-sleeping-child") {
    assume(hasZig, "usable zig command not found (skipping LLVM-native cancellation timer test)")

    val driverFile = Files.createTempFile("flix-llvm-native-cancel-sleep-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-cancel-sleep-")
    try {
      Files.writeString(driverFile, cancelSleepingChildDriverSource, StandardCharsets.UTF_8)

      val flix = new Flix()
      flix.setOptions(TestOptions.copy(outputPath = outDir))
      implicit val sctx: SecurityContext = SecurityContext.Unrestricted

      flix.addFile(driverFile)

      val (optRoot, errors) = flix.check()
      if (errors.nonEmpty) {
        fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
      }

      flix.codeGen(optRoot.get)

      val (exit, output) = runExecutable(
        executablePath(outDir),
        env = Map.empty,
        timeoutMs = 2000L,
      )

      if (exit != 1) {
        fail(s"Expected child exception to terminate the driver with exit 1, but got $exit:\n$output")
      }
      if (!output.contains("Uncaught Flix exception")) {
        fail(s"Expected uncaught child exception report, but output was:\n$output")
      }
    } finally {
      Files.deleteIfExists(driverFile)
      deleteRecursive(outDir)
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
    val tests = PortableConformance.collectPortableTests(root)
    PortableConformance.mkDriverSource(tests, banner = "portable-stdlib-llvm-native-runtime")
  }

  private val uncaughtExnDriverSource: String =
    """
      |mod Test {}
      |
      |mod Test.Portable.UncaughtExn {
      |
      |    def g(): Unit \ IO = throw Exn.mk(999)
      |
      |    pub def f(): Unit \ IO = g()
      |
      |}
      |
      |def main(): Unit \ IO = Test.Portable.UncaughtExn.f()
      |""".stripMargin

  private val unhandledSuspensionDriverSource: String =
    """
      |mod Test {}
      |
      |eff Boom {
      |    def boom(): Unit
      |}
      |
      |def f(): Unit \ Boom = Boom.boom()
      |
      |def main(): Unit \ IO = unchecked_cast(f() as _ \ IO)
      |""".stripMargin

  private val gcStressDriverSource: String =
    """
      |mod Test {}
      |
      |def allocLoop(n: Int32): Int32 = {
      |    def loop(i: Int32, acc: Int32): Int32 =
      |        if (i == n) acc else {
      |            let s = "${i}-${i}-${i}-${i}";
      |            loop(i + 1, acc + String.length(s))
      |        };
      |    loop(0, 0)
      |}
      |
      |def main(): Unit \ {Chan, NonDet, IO} = region rc {
      |    %%PRINTLN%%("gc-stress: start");
      |
      |    let (tx, rx) = Channel.unbuffered();
      |
      |    spawn {
      |        Timer.runWithIO(() -> Timer.sleepMillis(100i64));
      |        Channel.send(42, tx);
      |        ()
      |    } @ rc;
      |
      |    spawn {
      |        let x = allocLoop(100000);
      |        discard %%NEW_ID%%(());
      |        if (x < 0) () else ()
      |    } @ rc;
      |
      |    let v = Channel.recv(rx);
      |    %%PRINTLN%%("gc-stress: got " + Int32.toString(v));
      |    %%PRINTLN%%("gc-stress: done");
      |    ()
      |}
      |""".stripMargin

  private val rememberedSetStressDriverSource: String =
    """
      |mod Test {}
      |
      |struct Cell[r] {
      |    mut value: String
      |}
      |mod Cell {
      |    pub def put(c: Cell[r], s: String): Unit \ r =
      |        c->value = s
      |
      |    pub def get(c: Cell[r]): String \ r =
      |        c->value
      |}
      |
      |def allocLoop(n: Int32): Int32 = {
      |    def loop(i: Int32, acc: Int32): Int32 =
      |        if (i == n) acc else {
      |            let s = "${i}-${i}-${i}-${i}-${i}";
      |            loop(i + 1, acc + String.length(s))
      |        };
      |    loop(0, 0)
      |}
      |
      |def mkValue(prefix: String, i: Int32): String =
      |    prefix + Int32.toString(i) + "-" + Int32.toString(i) + "-" + Int32.toString(i)
      |
      |def storeCell(c: Cell[r], i: Int32): Unit \ r =
      |    Cell.put(c, mkValue("cell-", i))
      |
      |def storeArray(a: Array[String, r], i: Int32): Unit \ r = {
      |    Array.put(mkValue("arr0-", i), 0, a);
      |    Array.put(mkValue("arr1-", i), 1, a)
      |}
      |
      |def stressLoop(c: Cell[r], a: Array[String, r], i: Int32): Unit \ r =
      |    if (i == 2000) ()
      |    else {
      |        storeCell(c, i);
      |        storeArray(a, i);
      |        let _ = allocLoop(200);
      |        stressLoop(c, a, i + 1)
      |    }
      |
      |def main(): Unit \ IO = region rc {
      |    %%PRINTLN%%("remembered-set-stress: start");
      |
      |    let c = new Cell @ rc { value = "seed" };
      |    let a: Array[String, rc] = Array.repeat(rc, 2, "seed");
      |
      |    stressLoop(c, a, 0);
      |
      |    let _ = allocLoop(5000);
      |
      |    let expectedCell = mkValue("cell-", 1999);
      |    let expectedArr0 = mkValue("arr0-", 1999);
      |    let expectedArr1 = mkValue("arr1-", 1999);
      |    let actualCell = Cell.get(c);
      |    let actualArr0 = Array.get(0, a);
      |    let actualArr1 = Array.get(1, a);
      |
      |    let _ = if (actualCell != expectedCell) bug!("bad cell value: " + actualCell) else ();
      |    let _ = if (actualArr0 != expectedArr0) bug!("bad arr[0] value: " + actualArr0) else ();
      |    let _ = if (actualArr1 != expectedArr1) bug!("bad arr[1] value: " + actualArr1) else ();
      |
      |    %%PRINTLN%%("remembered-set-stress: done");
      |    ()
      |}
      |""".stripMargin

  private val rootingStressDriverSource: String =
    """
      |mod Test {}
      |
      |def allocLoop(n: Int32): Int32 = {
      |    def loop(i: Int32, acc: Int32): Int32 =
      |        if (i == n) acc else {
      |            let s = "${i}-${i}-${i}-${i}-${i}";
      |            loop(i + 1, acc + String.length(s))
      |        };
      |    loop(0, 0)
      |}
      |
      |def mkPayload(seed: Int32, n: Int32): List[(Int32, String)] = {
      |    def loop(i: Int32, acc: List[(Int32, String)]): List[(Int32, String)] =
      |        if (i == n) acc
      |        else {
      |            let x = seed + i;
      |            let s = "payload-" + Int32.toString(x) + "-" + Int32.toString(x) + "-" + Int32.toString(x);
      |            loop(i + 1, (x, s) :: acc)
      |        };
      |    loop(0, Nil)
      |}
      |
      |def checksum(xs: List[(Int32, String)]): Int32 =
      |    match xs {
      |        case Nil => 0
      |        case (i, s) :: rs => i + String.length(s) + checksum(rs)
      |    }
      |
      |def main(): Unit \ {Chan, NonDet, IO} = {
      |    %%PRINTLN%%("rooting-stress: start");
      |
      |    let expectedValue = mkPayload(1000, 64);
      |    let expectedValueSum = checksum(expectedValue);
      |
      |    let (tx, rx): (Sender[List[(Int32, String)]], Receiver[List[(Int32, String)]]) = Channel.unbuffered();
      |    region rc {
      |        spawn {
      |            let payload = mkPayload(1000, 64);
      |            Timer.runWithIO(() -> Timer.sleepMillis(100i64));
      |            Channel.send(payload, tx);
      |            ()
      |        } @ rc;
      |
      |        spawn {
      |            let _ = allocLoop(120000);
      |            discard %%NEW_ID%%(());
      |            ()
      |        } @ rc;
      |
      |        let got = Channel.recv(rx);
      |        let gotSum = checksum(got);
      |        let _ = if (gotSum == expectedValueSum) () else bug!("bad resumed payload checksum");
      |        ()
      |    };
      |
      |    let expectedExn = mkPayload(3000, 64);
      |    let expectedExnSum = checksum(expectedExn);
      |
      |    let exnSum = try {
      |        region rc {
      |            spawn {
      |                let payload = mkPayload(2000, 64);
      |                Timer.runWithIO(() -> Timer.sleepMillis(5000i64));
      |                let _ = if (checksum(payload) < 0) bug!("impossible") else ();
      |                ()
      |            } @ rc;
      |
      |            spawn {
      |                let payload = mkPayload(3000, 64);
      |                let _ = Timer.runWithIO(() -> Timer.sleepMillis(50i64));
      |                throw Exn.mk(payload);
      |                ()
      |            } @ rc;
      |
      |            spawn {
      |                let _ = allocLoop(120000);
      |                discard %%NEW_ID%%(());
      |                ()
      |            } @ rc;
      |
      |            0
      |        }
      |    } catch {
      |        case exn: List[(Int32, String)] => checksum(Exn.payloadAs(exn))
      |        case _: Exn => -1
      |    };
      |
      |    let _ = if (exnSum == expectedExnSum) () else bug!("bad exception payload checksum");
      |
      |    %%PRINTLN%%("rooting-stress: done");
      |    ()
      |}
      |""".stripMargin

  private val aggregateRememberedSetStressDriverSource: String =
    """
      |mod Test {}
      |
      |struct Cells[r] {
      |    mut left: List[(Int32, String)],
      |    mut right: { label = String, payload = List[(Int32, String)] }
      |}
      |mod Cells {
      |    pub def putLeft(c: Cells[r], xs: List[(Int32, String)]): Unit \ r =
      |        c->left = xs
      |
      |    pub def putRight(c: Cells[r], x: { label = String, payload = List[(Int32, String)] }): Unit \ r =
      |        c->right = x
      |
      |    pub def getLeft(c: Cells[r]): List[(Int32, String)] \ r =
      |        c->left
      |
      |    pub def getRight(c: Cells[r]): { label = String, payload = List[(Int32, String)] } \ r =
      |        c->right
      |}
      |
      |def allocLoop(n: Int32): Int32 = {
      |    def loop(i: Int32, acc: Int32): Int32 =
      |        if (i == n) acc else {
      |            let s = "${i}-${i}-${i}-${i}-${i}";
      |            loop(i + 1, acc + String.length(s))
      |        };
      |    loop(0, 0)
      |}
      |
      |def mkText(prefix: String, i: Int32): String =
      |    prefix + Int32.toString(i) + "-" + Int32.toString(i) + "-" + Int32.toString(i)
      |
      |def mkPayload(prefix: String, seed: Int32, n: Int32): List[(Int32, String)] = {
      |    def loop(i: Int32, acc: List[(Int32, String)]): List[(Int32, String)] =
      |        if (i == n) acc
      |        else {
      |            let x = seed + i;
      |            let s = mkText(prefix, x);
      |            loop(i + 1, (x, s) :: acc)
      |        };
      |    loop(0, Nil)
      |}
      |
      |def payloadChecksum(xs: List[(Int32, String)]): Int32 =
      |    match xs {
      |        case Nil => 0
      |        case (i, s) :: rs => i + String.length(s) + payloadChecksum(rs)
      |    }
      |
      |def storeAggregateState(c: Cells[r], a: Array[{ tag = String, payload = List[(Int32, String)] }, r], i: Int32): Unit \ r = {
      |    let sharedLeft = mkPayload("left-", i, 8);
      |    let sharedRight = mkPayload("right-", i, 8);
      |    Cells.putLeft(c, sharedLeft);
      |    Cells.putRight(c, { label = mkText("record-", i), payload = sharedRight });
      |    Array.put({ tag = mkText("arr0-", i), payload = sharedLeft }, 0, a);
      |    Array.put({ tag = mkText("arr1-", i), payload = sharedRight }, 1, a)
      |}
      |
      |def stressLoop(c: Cells[r], a: Array[{ tag = String, payload = List[(Int32, String)] }, r], i: Int32): Unit \ r =
      |    if (i == 1500) ()
      |    else {
      |        storeAggregateState(c, a, i);
      |        let _ = allocLoop(200);
      |        stressLoop(c, a, i + 1)
      |    }
      |
      |def main(): Unit \ IO = region rc {
      |    %%PRINTLN%%("remembered-set-aggregate-stress: start");
      |
      |    let cells = new Cells @ rc {
      |        left = Nil,
      |        right = { label = "seed", payload = Nil }
      |    };
      |    let arr: Array[{ tag = String, payload = List[(Int32, String)] }, rc] =
      |        Array.repeat(rc, 2, { tag = "seed", payload = Nil });
      |
      |    stressLoop(cells, arr, 0);
      |
      |    let _ = allocLoop(5000);
      |
      |    let expectedLeft = mkPayload("left-", 1499, 8);
      |    let expectedRight = mkPayload("right-", 1499, 8);
      |    let expectedRecordLabel = mkText("record-", 1499);
      |    let expectedArr0Tag = mkText("arr0-", 1499);
      |    let expectedArr1Tag = mkText("arr1-", 1499);
      |
      |    let actualLeft = Cells.getLeft(cells);
      |    let actualRight = Cells.getRight(cells);
      |    let actualArr0 = Array.get(0, arr);
      |    let actualArr1 = Array.get(1, arr);
      |
      |    let _ = if (payloadChecksum(actualLeft) == payloadChecksum(expectedLeft)) () else bug!("bad left payload");
      |    let _ = if (actualRight#label == expectedRecordLabel) () else bug!("bad record label: " + actualRight#label);
      |    let _ = if (payloadChecksum(actualRight#payload) == payloadChecksum(expectedRight)) () else bug!("bad record payload");
      |    let _ = if (actualArr0#tag == expectedArr0Tag) () else bug!("bad arr[0] tag: " + actualArr0#tag);
      |    let _ = if (payloadChecksum(actualArr0#payload) == payloadChecksum(expectedLeft)) () else bug!("bad arr[0] payload");
      |    let _ = if (actualArr1#tag == expectedArr1Tag) () else bug!("bad arr[1] tag: " + actualArr1#tag);
      |    let _ = if (payloadChecksum(actualArr1#payload) == payloadChecksum(expectedRight)) () else bug!("bad arr[1] payload");
      |
      |    %%PRINTLN%%("remembered-set-aggregate-stress: done");
      |    ()
      |}
      |""".stripMargin

  private val aggregateRootingStressPrelude: String =
    """
      |mod Test {}
      |
      |eff Ask {
      |    def ask(): { label = String, payload = List[(Int32, String)] }
      |}
      |
      |mod Ask {
      |    @DefaultHandler
      |    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Ask) + IO =
      |        run {
      |            f()
      |        } with handler Ask {
      |            def ask(k) = k(mkAgg("default-", 5000))
      |        }
      |}
      |
      |def allocLoop(n: Int32): Int32 = {
      |    def loop(i: Int32, acc: Int32): Int32 =
      |        if (i == n) acc else {
      |            let s = "${i}-${i}-${i}-${i}-${i}";
      |            loop(i + 1, acc + String.length(s))
      |        };
      |    loop(0, 0)
      |}
      |
      |def mkText(prefix: String, i: Int32): String =
      |    prefix + Int32.toString(i) + "-" + Int32.toString(i) + "-" + Int32.toString(i)
      |
      |def mkPayload(prefix: String, seed: Int32, n: Int32): List[(Int32, String)] = {
      |    def loop(i: Int32, acc: List[(Int32, String)]): List[(Int32, String)] =
      |        if (i == n) acc
      |        else {
      |            let x = seed + i;
      |            let s = mkText(prefix, x);
      |            loop(i + 1, (x, s) :: acc)
      |        };
      |    loop(0, Nil)
      |}
      |
      |def mkAgg(prefix: String, seed: Int32): { label = String, payload = List[(Int32, String)] } =
      |    { label = mkText(prefix, seed), payload = mkPayload(prefix, seed, 8) }
      |
      |def payloadChecksum(xs: List[(Int32, String)]): Int32 =
      |    match xs {
      |        case Nil => 0
      |        case (i, s) :: rs => i + String.length(s) + payloadChecksum(rs)
      |    }
      |
      |def aggChecksum(x: { label = String, payload = List[(Int32, String)] }): Int32 =
      |    String.length(x#label) + payloadChecksum(x#payload)
      |
      |def customHandlerCase(): Int32 \ IO =
      |    run {
      |        let agg = Ask.ask();
      |        let _ = Timer.runWithIO(() -> Timer.sleepMillis(100i64));
      |        let _ = allocLoop(120000);
      |        aggChecksum(agg)
      |    } with handler Ask {
      |        def ask(k) = k(mkAgg("custom-", 4000))
      |    }
      |
      |def defaultHandlerThrowCase(): Int32 \ IO =
      |    try {
      |        Ask.runWithIO(() -> {
      |            let agg = Ask.ask();
      |            let _ = Timer.runWithIO(() -> Timer.sleepMillis(100i64));
      |            let _ = allocLoop(120000);
      |            throw Exn.mk(agg);
      |            0
      |        })
      |    } catch {
      |        case exn: { label = String, payload = List[(Int32, String)] } =>
      |            aggChecksum(Exn.payloadAs(exn))
      |        case _: Exn => -1
      |    }
      |""".stripMargin

  private val aggregateCustomHandlerRootingStressDriverSource: String =
    aggregateRootingStressPrelude +
      """
        |
        |def main(): Unit \ IO = {
        |    %%PRINTLN%%("rooting-aggregate-custom-handler-stress: start");
        |
        |    let expected = aggChecksum(mkAgg("custom-", 4000));
        |    let actual = customHandlerCase();
        |
        |    let _ = if (actual == expected) () else bug!("bad custom-handler aggregate");
        |
        |    %%PRINTLN%%("rooting-aggregate-custom-handler-stress: done");
        |    ()
        |}
        |""".stripMargin

  private val aggregateDefaultHandlerRootingStressDriverSource: String =
    aggregateRootingStressPrelude +
      """
        |
        |def main(): Unit \ IO = {
        |    %%PRINTLN%%("rooting-aggregate-default-handler-stress: start");
        |
        |    let expected = aggChecksum(mkAgg("default-", 5000));
        |    let actual = defaultHandlerThrowCase();
        |
        |    let _ = if (actual == expected) () else bug!("bad default-handler aggregate");
        |
        |    %%PRINTLN%%("rooting-aggregate-default-handler-stress: done");
        |    ()
        |}
        |""".stripMargin

  private val containerRememberedSetStressDriverSource: String =
    """
      |mod Test {}
      |
      |def allocLoop(n: Int32): Int32 = {
      |    def loop(i: Int32, acc: Int32): Int32 =
      |        if (i == n) acc else {
      |            let s = "${i}-${i}-${i}-${i}-${i}";
      |            loop(i + 1, acc + String.length(s))
      |        };
      |    loop(0, 0)
      |}
      |
      |def mkText(prefix: String, i: Int32): String =
      |    prefix + Int32.toString(i) + "-" + Int32.toString(i) + "-" + Int32.toString(i)
      |
      |def mkPayload(prefix: String, seed: Int32, n: Int32): List[(Int32, String)] = {
      |    def loop(i: Int32, acc: List[(Int32, String)]): List[(Int32, String)] =
      |        if (i == n) acc
      |        else {
      |            let x = seed + i;
      |            let s = mkText(prefix, x);
      |            loop(i + 1, (x, s) :: acc)
      |        };
      |    loop(0, Nil)
      |}
      |
      |def mkAgg(prefix: String, seed: Int32): { label = String, payload = List[(Int32, String)] } =
      |    { label = mkText(prefix, seed), payload = mkPayload(prefix, seed, 8) }
      |
      |def payloadChecksum(xs: List[(Int32, String)]): Int32 =
      |    match xs {
      |        case Nil => 0
      |        case (i, s) :: rs => i + String.length(s) + payloadChecksum(rs)
      |    }
      |
      |def aggChecksum(x: { label = String, payload = List[(Int32, String)] }): Int32 =
      |    String.length(x#label) + payloadChecksum(x#payload)
      |
      |def expectSome(msg: String, o: Option[a]): a =
      |    match o {
      |        case Some(x) => x
      |        case None => bug!(msg)
      |    }
      |
      |def stressContainers(
      |    primary: MutList[{ label = String, payload = List[(Int32, String)] }, r],
      |    secondary: MutList[{ label = String, payload = List[(Int32, String)] }, r],
      |    mirror: Array[{ label = String, payload = List[(Int32, String)] }, r],
      |    i: Int32
      |): Unit \ r =
      |    if (i == 150) ()
      |    else {
      |        let a = mkAgg("a-", i);
      |        let b = mkAgg("b-", i);
      |        let c = mkAgg("c-", i);
      |
      |        MutList.truncate(0, primary);
      |        MutList.reserve(12, primary);
      |        MutList.push(a, primary);
      |        MutList.push(b, primary);
      |        MutList.push(c, primary);
      |        let moved = match MutList.pop(primary) {
      |            case Some(x) => x
      |            case None => bug!("missing moved payload")
      |        };
      |        MutList.compress(primary);
      |
      |        MutList.truncate(0, secondary);
      |        MutList.reserve(10, secondary);
      |        MutList.push(moved, secondary);
      |        MutList.push(a, secondary);
      |        MutList.push({ label = mkText("mix-", i), payload = b#payload }, secondary);
      |        MutList.compress(secondary);
      |
      |        Array.put(a, 0, mirror);
      |        Array.put(moved, 1, mirror);
      |        Array.put({ label = mkText("arr-", i), payload = b#payload }, 2, mirror);
      |
      |        let _ = allocLoop(50);
      |        stressContainers(primary, secondary, mirror, i + 1)
      |    }
      |
      |def main(): Unit \ IO = region rc {
      |    %%PRINTLN%%("remembered-set-container-stress: start");
      |
      |    let seed = { label = "seed", payload = Nil };
      |    let primary: MutList[{ label = String, payload = List[(Int32, String)] }, rc] = MutList.empty(rc);
      |    let secondary: MutList[{ label = String, payload = List[(Int32, String)] }, rc] = MutList.empty(rc);
      |    let mirror: Array[{ label = String, payload = List[(Int32, String)] }, rc] = Array.repeat(rc, 3, seed);
      |
      |    stressContainers(primary, secondary, mirror, 0);
      |
      |    let _ = allocLoop(1000);
      |
      |    let expectedA = mkAgg("a-", 149);
      |    let expectedB = mkAgg("b-", 149);
      |    let expectedC = mkAgg("c-", 149);
      |    let expectedMix = { label = mkText("mix-", 149), payload = expectedB#payload };
      |    let expectedArr = { label = mkText("arr-", 149), payload = expectedB#payload };
      |
      |    let _ = if (MutList.length(primary) == 2) () else bug!("bad primary length");
      |    let _ = if (MutList.length(secondary) == 3) () else bug!("bad secondary length");
      |
      |    let primary0 = expectSome("missing primary[0]", MutList.nth(0, primary));
      |    let primary1 = expectSome("missing primary[1]", MutList.nth(1, primary));
      |    let secondary0 = expectSome("missing secondary[0]", MutList.nth(0, secondary));
      |    let secondary1 = expectSome("missing secondary[1]", MutList.nth(1, secondary));
      |    let secondary2 = expectSome("missing secondary[2]", MutList.nth(2, secondary));
      |    let mirror0 = expectSome("missing mirror[0]", Array.nth(0, mirror));
      |    let mirror1 = expectSome("missing mirror[1]", Array.nth(1, mirror));
      |    let mirror2 = expectSome("missing mirror[2]", Array.nth(2, mirror));
      |
      |    let _ = if (aggChecksum(primary0) == aggChecksum(expectedA)) () else bug!("bad primary[0]");
      |    let _ = if (aggChecksum(primary1) == aggChecksum(expectedB)) () else bug!("bad primary[1]");
      |    let _ = if (aggChecksum(secondary0) == aggChecksum(expectedC)) () else bug!("bad secondary[0]");
      |    let _ = if (aggChecksum(secondary1) == aggChecksum(expectedA)) () else bug!("bad secondary[1]");
      |    let _ = if (aggChecksum(secondary2) == aggChecksum(expectedMix)) () else bug!("bad secondary[2]");
      |    let _ = if (aggChecksum(mirror0) == aggChecksum(expectedA)) () else bug!("bad mirror[0]");
      |    let _ = if (aggChecksum(mirror1) == aggChecksum(expectedC)) () else bug!("bad mirror[1]");
      |    let _ = if (aggChecksum(mirror2) == aggChecksum(expectedArr)) () else bug!("bad mirror[2]");
      |
      |    %%PRINTLN%%("remembered-set-container-stress: done");
      |    ()
      |}
      |""".stripMargin

  private val aggregateJoinRootingStressDriverSource: String =
    """
      |mod Test {}
      |
      |def allocLoop(n: Int32): Int32 = {
      |    def loop(i: Int32, acc: Int32): Int32 =
      |        if (i == n) acc else {
      |            let s = "${i}-${i}-${i}-${i}-${i}";
      |            loop(i + 1, acc + String.length(s))
      |        };
      |    loop(0, 0)
      |}
      |
      |def mkText(prefix: String, i: Int32): String =
      |    prefix + Int32.toString(i) + "-" + Int32.toString(i) + "-" + Int32.toString(i)
      |
      |def mkPayload(prefix: String, seed: Int32, n: Int32): List[(Int32, String)] = {
      |    def loop(i: Int32, acc: List[(Int32, String)]): List[(Int32, String)] =
      |        if (i == n) acc
      |        else {
      |            let x = seed + i;
      |            let s = mkText(prefix, x);
      |            loop(i + 1, (x, s) :: acc)
      |        };
      |    loop(0, Nil)
      |}
      |
      |def mkAgg(prefix: String, seed: Int32): { label = String, payload = List[(Int32, String)] } =
      |    { label = mkText(prefix, seed), payload = mkPayload(prefix, seed, 8) }
      |
      |def payloadChecksum(xs: List[(Int32, String)]): Int32 =
      |    match xs {
      |        case Nil => 0
      |        case (i, s) :: rs => i + String.length(s) + payloadChecksum(rs)
      |    }
      |
      |def aggChecksum(x: { label = String, payload = List[(Int32, String)] }): Int32 =
      |    String.length(x#label) + payloadChecksum(x#payload)
      |
      |def main(): Unit \ IO = {
      |    %%PRINTLN%%("rooting-aggregate-join-stress: start");
      |
      |    let expectedJoin = aggChecksum(mkAgg("throwing-", 7000));
      |
      |    let actualJoin = try {
      |        region rc {
      |            spawn {
      |                let agg = mkAgg("sleeping-", 6000);
      |                Timer.runWithIO(() -> Timer.sleepMillis(5000i64));
      |                let _ = if (aggChecksum(agg) < 0) bug!("impossible") else ();
      |                ()
      |            } @ rc;
      |
      |            spawn {
      |                let agg = mkAgg("throwing-", 7000);
      |                let _ = Timer.runWithIO(() -> Timer.sleepMillis(50i64));
      |                throw Exn.mk(agg);
      |                ()
      |            } @ rc;
      |
      |            spawn {
      |                let _ = allocLoop(120000);
      |                discard %%NEW_ID%%(());
      |                ()
      |            } @ rc;
      |
      |            ()
      |        };
      |        -1
      |    } catch {
      |        case exn: { label = String, payload = List[(Int32, String)] } =>
      |            aggChecksum(Exn.payloadAs(exn))
      |        case _: Exn => -2
      |    };
      |
      |    let _ = if (actualJoin == expectedJoin) () else bug!("bad join aggregate");
      |
      |    %%PRINTLN%%("rooting-aggregate-join-stress: done");
      |    ()
      |}
      |""".stripMargin

  private val pollcheckHandshakeStressDriverSource: String =
    """
      |mod Test {}
      |
      |def spinLoop(n: Int32, acc: Int64): Int64 =
      |    if (n == 0) acc else spinLoop(n - 1, acc + Int32.toInt64(n))
      |
      |def allocLoop(n: Int32): Int32 = {
      |    def loop(i: Int32, acc: Int32): Int32 =
      |        if (i == n) acc else {
      |            let s = "poll-" + Int32.toString(i) + "-" + Int32.toString(acc);
      |            loop(i + 1, acc + String.length(s))
      |        };
      |    loop(0, 0)
      |}
      |
      |def allocRounds(rounds: Int32): Int32 = {
      |    def loop(i: Int32, acc: Int32): Int32 =
      |        if (i == rounds) acc
      |        else {
      |            let x = allocLoop(400);
      |            loop(i + 1, acc + x)
      |        };
      |    loop(0, 0)
      |}
      |
      |def recvAll(rx: Receiver[Int32], remaining: Int32, acc: Int32): Int32 \ {Chan, NonDet} =
      |    if (remaining == 0) acc else recvAll(rx, remaining - 1, acc + Channel.recv(rx))
      |
      |def main(): Unit \ {Chan, NonDet, IO} = region rc {
      |    %%PRINTLN%%("pollcheck-handshake-stress: start");
      |
      |    let (doneTx, doneRx) = Channel.buffered(8);
      |
      |    spawn {
      |        let x = spinLoop(6_000_000i32, 1i64);
      |        let _ = if (x == 0i64) bug!("impossible") else ();
      |        Channel.send(1, doneTx);
      |        ()
      |    } @ rc;
      |
      |    spawn {
      |        let x = spinLoop(6_500_000i32, 2i64);
      |        let _ = if (x == 0i64) bug!("impossible") else ();
      |        Channel.send(2, doneTx);
      |        ()
      |    } @ rc;
      |
      |    spawn {
      |        let x = spinLoop(2_000_000i32, 3i64);
      |        let _ = if (x == 0i64) bug!("impossible") else ();
      |        Timer.runWithIO(() -> Timer.sleepMillis(200i64));
      |        let y = spinLoop(2_000_000i32, x);
      |        let _ = if (y == 0i64) bug!("impossible") else ();
      |        Channel.send(3, doneTx);
      |        ()
      |    } @ rc;
      |
      |    spawn {
      |        let total = allocRounds(1_500i32);
      |        let _ = if (total < 0) bug!("impossible") else ();
      |        Channel.send(4, doneTx);
      |        ()
      |    } @ rc;
      |
      |    let sum = recvAll(doneRx, 4, 0);
      |    let _ = if (sum == 10) () else bug!("bad completion sum: " + Int32.toString(sum));
      |
      |    %%PRINTLN%%("pollcheck-handshake-stress: done");
      |    ()
      |}
      |""".stripMargin

  private val unicodePrintDriverSource: String =
    """
      |def main(): Unit \ IO =
      |    println("Weather: 12.5°C")
      |""".stripMargin

  private val cancelSleepingChildDriverSource: String =
    """
      |mod Test {}
      |
      |def main(): Unit \ IO =
      |    region rc {
      |        spawn {
      |            Timer.runWithIO(() -> Timer.sleepMillis(5000i64));
      |            ()
      |        } @ rc;
      |
      |        spawn {
      |            throw Exn.mk(123)
      |        } @ rc;
      |
      |        ()
      |    }
      |""".stripMargin

  private def runExecutable(executable: Path): (Int, String) =
    runExecutable(executable, Map.empty)

  private def runExecutable(executable: Path, env: Map[String, String]): (Int, String) = {
    runExecutable(executable, env, timeoutMs = 30000L)
  }

  private def runExecutable(executable: Path, env: Map[String, String], timeoutMs: Long): (Int, String) = {
    val pb = new ProcessBuilder(List(executable.toString).asJava)
    val pbEnv = pb.environment()
    env.foreach { case (k, v) => pbEnv.put(k, v) }
    pb.redirectErrorStream(true)
    val p = pb.start()
    val finished = p.waitFor(timeoutMs, TimeUnit.MILLISECONDS)
    if (!finished) {
      p.destroyForcibly()
      val output = new String(p.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
      fail(s"Process timed out after ${timeoutMs}ms:\n$output")
    }
    val output = new String(p.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val exit = p.exitValue()
    (exit, output)
  }

  private def executablePath(outDir: Path): Path = {
    ca.uwaterloo.flix.language.phase.llvm.LlvmNativeDriver.executablePath(outDir)
  }

  private def isWindows: Boolean =
    System.getProperty("os.name", "").toLowerCase.contains("win")

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
