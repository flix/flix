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
  * Focused filesystem sandbox/portability semantics for the LLVM-wasm backend (Node host bridge).
  *
  * These tests lock down edge-case behaviors that are easy to regress:
  *   - absolute paths rejected,
  *   - traversal (`..`) rejected,
  *   - backslash paths normalized,
  *   - mkTempDir prefix validation,
  *   - `exists` semantics (missing => Ok(false), invalid path => Err(InvalidPath)).
  */
class PortableFileSystemLlvmWasmSuite extends AnyFunSuite {

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmWasm,
      incremental = false,
      outputJvm = false,
    )

  private val wasmRunnerScript: Path =
    Paths.get("tools/wasm-runner-js/run-flix.mjs").toAbsolutePath.normalize()

  test("portable-filesystem-llvm-wasm") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm portable filesystem test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm portable filesystem test)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm portable filesystem test)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm portable filesystem test)")

    val program =
      """
        |def expect(cond: Bool, msg: String): Unit =
        |    if (cond) () else bug!(msg)
        |
        |def expectOk(res: Result[IoError, a]): a = match res {
        |    case Ok(v) => v
        |    case Err(e) => bug!("expected Ok but got Err: ${ToString.toString(e)}")
        |}
        |
        |def expectOkUnit(res: Result[IoError, Unit]): Unit = match res {
        |    case Ok(_) => ()
        |    case Err(e) => bug!("expected Ok but got Err: ${ToString.toString(e)}")
        |}
        |
        |def expectErrKind(res: Result[IoError, a], expected: IoError.ErrorKind): Unit = match res {
        |    case Err(IoError.IoError(k, _)) if (k == expected) => ()
        |    case Err(e) => bug!("expected Err(${ToString.toString(expected)}) but got: ${ToString.toString(e)}")
        |    case Ok(_) => bug!("expected Err but got Ok.")
        |}
        |
        |def main(): Unit \ IO =
        |    FileWriteWithResult.runWithIO(() ->
        |        FileReadWithResult.runWithIO(() -> {
        |            // mkTempDir prefix validation.
        |            expectErrKind(FileWriteWithResult.mkTempDir("ab"), IoError.ErrorKind.InvalidPath);
        |
        |            // Happy path: write/read inside a temp dir.
        |            let dir = expectOk(FileWriteWithResult.mkTempDir("flix-test"));
        |            let sub = dir / "d";
        |            let f = sub / "file.txt";
        |            expectOkUnit(FileWriteWithResult.mkDirs(sub));
        |            expectOkUnit(FileWriteWithResult.write(str = "hi", f));
        |
        |            let s1 = expectOk(FileReadWithResult.read(f));
        |            let _ = expect(s1 == "hi", "expected read of posix path to return 'hi'");
        |
        |            // Backslash normalization (Windows-style paths).
        |            let f2 = dir + "\\d\\file.txt";
        |            let s2 = expectOk(FileReadWithResult.read(f2));
        |            let _ = expect(s2 == "hi", "expected backslash path normalization");
        |
        |            // exists: missing => Ok(false).
        |            let missing = dir / "missing.txt";
        |            let ex = expectOk(FileReadWithResult.exists(missing));
        |            let _ = expect(not ex, "expected missing file to return Ok(false)");
        |
        |            // Absolute paths are rejected.
        |            expectErrKind(FileReadWithResult.read("/etc/passwd"), IoError.ErrorKind.InvalidPath);
        |            expectErrKind(FileReadWithResult.exists("/etc/passwd"), IoError.ErrorKind.InvalidPath);
        |
        |            // Traversal escapes are rejected.
        |            expectErrKind(FileReadWithResult.read("../escape.txt"), IoError.ErrorKind.InvalidPath);
        |            expectErrKind(FileWriteWithResult.write(str = "x", "../escape.txt"), IoError.ErrorKind.InvalidPath);
        |
        |            ()
        |        })
        |    )
        |""".stripMargin

    val testFile = Files.createTempFile("flix-portable-fs-llvm-wasm-", ".flix")
    Files.writeString(testFile, program, StandardCharsets.UTF_8)

    val outDir = Files.createTempDirectory("flix-llvm-wasm-fs-")
    val sandboxDir = Files.createTempDirectory("flix-llvm-wasm-fs-sandbox-")
    try {
      val (componentJs, exportsManifest) = compileLlvmWasm(testFile, outDir)
      val (exit, output) = runNode(componentJs, exportsManifest, sandboxDir, timeoutSeconds = 45)
      if (exit != 0) {
        fail(s"LLVM-wasm portable filesystem test program failed with exit $exit:\n$output")
      }
    } finally {
      Files.deleteIfExists(testFile)
      deleteRecursive(outDir)
      deleteRecursive(sandboxDir)
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
