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
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*

/**
  * Runtime smoke tests for the portable Process/ProcessWithResult primops on the LLVM-native backend.
  */
class PortableProcessLlvmNativeSuite extends AnyFunSuite {

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmNative,
      incremental = false,
      outputJvm = false,
    )

  test("portable-process-llvm-native") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native portable process runtime test)")

    val (cmd, args) =
      if (isWindows) ("cmd.exe", List("/c", "exit 42"))
      else ("/bin/sh", List("-c", "exit 42"))

    val flixArgs = args.map(escapeFlixString).mkString(" :: ") + " :: Nil"

    val program =
      s"""
         |def expect(cond: Bool, msg: String): Unit =
         |    if (cond) () else bug!(msg)
         |
         |def unexpectedErr(e: a): Unit with ToString[a] =
         |    expect(false, "unexpected Err: " + ToString.toString(e))
         |
         |def main(): Unit \\ IO = {
         |    let args = ${flixArgs};
         |    match ProcessWithResult.runWithIO(() -> ProcessWithResult.exec(${escapeFlixString(cmd)}, args)) {
         |        case Ok(ph) => {
         |            match ProcessWithResult.runWithIO(() -> ProcessWithResult.pid(ph)) {
         |                case Ok(pid) => expect(pid > 0i64, "expected pid > 0")
         |                case Err(e) => unexpectedErr(e)
         |            };
         |            match ProcessWithResult.runWithIO(() -> ProcessWithResult.waitFor(ph)) {
         |                case Ok(code) => expect(code == 42i32, "expected exit 42")
         |                case Err(e) => unexpectedErr(e)
         |            };
         |            match ProcessWithResult.runWithIO(() -> ProcessWithResult.exitValue(ph)) {
         |                case Ok(code) => expect(code == 42i32, "expected exitValue 42")
         |                case Err(e) => unexpectedErr(e)
         |            };
         |            match ProcessHandle.release(ph) {
         |                case Ok(_) => ()
         |                case Err(e) => unexpectedErr(e)
         |            };
         |            ()
         |        }
         |        case Err(e) => unexpectedErr(e)
         |    }
         |}
         |""".stripMargin

    val testFile = Files.createTempFile("flix-portable-process-llvm-native-", ".flix")
    Files.writeString(testFile, program, StandardCharsets.UTF_8)

    val outDir = Files.createTempDirectory("flix-llvm-native-process-")
    try {
      val exe = compileLlvmNative(testFile, outDir)
      val (exit, output) = runExecutable(exe)
      if (exit != 0) {
        fail(s"LLVM-native portable process test program failed with exit $exit:\n$output")
      }
    } finally {
      Files.deleteIfExists(testFile)
      deleteRecursive(outDir)
    }
  }

  test("portable-process-wait-timeout-llvm-native") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native portable process runtime test)")
    assume(!isWindows, "POSIX shell required for LLVM-native portable process timeout test")

    val program =
      s"""
         |def expect(cond: Bool, msg: String): Unit =
         |    if (cond) () else bug!(msg)
         |
         |def expectOk(res: Result[IoError, a]): a = match res {
         |    case Ok(v) => v
         |    case Err(e) => bug!("expected Ok but got Err: " + ToString.toString(e))
         |}
         |
         |def expectOkUnit(res: Result[IoError, Unit]): Unit = match res {
         |    case Ok(_) => ()
         |    case Err(e) => bug!("expected Ok but got Err: " + ToString.toString(e))
         |}
         |
         |def main(): Unit \\ IO = {
         |    let ph = ProcessWithResult.runWithIO(() -> ProcessWithResult.exec("/bin/sh", "-c" :: "sleep 5" :: Nil)) |> expectOk;
         |    let start = Clock.runWithIO(() -> Clock.now());
         |    let done0 = ProcessWithResult.runWithIO(() -> ProcessWithResult.waitForTimeout(ph, 50i64, TimeUnit.Milliseconds)) |> expectOk;
         |    let elapsed = Clock.runWithIO(() -> Clock.now()) - start;
         |    let _ = expect(not done0, "expected waitForTimeout to report false while child is still running");
         |    let _ = expect(elapsed < 2000i64, "expected waitForTimeout to return promptly on a running child");
         |    let alive0 = ProcessWithResult.runWithIO(() -> ProcessWithResult.isAlive(ph)) |> expectOk;
         |    let _ = expect(alive0, "expected child process to remain alive after timed wait");
         |
         |    ProcessWithResult.runWithIO(() -> ProcessWithResult.stop(ph)) |> expectOkUnit;
         |    let done1 = ProcessWithResult.runWithIO(() -> ProcessWithResult.waitForTimeout(ph, 2000i64, TimeUnit.Milliseconds)) |> expectOk;
         |    let _ = expect(done1, "expected waitForTimeout to observe completion after stop");
         |    ProcessHandle.release(ph) |> expectOkUnit;
         |    ()
         |}
         |""".stripMargin

    val testFile = Files.createTempFile("flix-portable-process-timeout-llvm-native-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-process-timeout-")
    try {
      Files.writeString(testFile, program, StandardCharsets.UTF_8)
      val exe = compileLlvmNative(testFile, outDir)
      val (exit, output) = runExecutable(exe)
      if (exit != 0) {
        fail(s"LLVM-native portable process timeout test program failed with exit $exit:\n$output")
      }
    } finally {
      Files.deleteIfExists(testFile)
      deleteRecursive(outDir)
    }
  }

  test("portable-process-wait-cancellation-llvm-native") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native portable process runtime test)")
    assume(!isWindows, "POSIX shell required for LLVM-native portable process cancellation test")

    val program =
      s"""
         |def expect(cond: Bool, msg: String): Unit =
         |    if (cond) () else bug!(msg)
         |
         |def expectOk(res: Result[IoError, a]): a = match res {
         |    case Ok(v) => v
         |    case Err(e) => bug!("expected Ok but got Err: " + ToString.toString(e))
         |}
         |
         |def expectOkUnit(res: Result[IoError, Unit]): Unit = match res {
         |    case Ok(_) => ()
         |    case Err(e) => bug!("expected Ok but got Err: " + ToString.toString(e))
         |}
         |
         |def main(): Unit \\ IO = {
         |    let ph = ProcessWithResult.runWithIO(() -> ProcessWithResult.exec("/bin/sh", "-c" :: "sleep 5" :: Nil)) |> expectOk;
         |
         |    let start = Clock.runWithIO(() -> Clock.now());
         |    let payload = try {
         |        region rc {
         |            spawn {
         |                let _ = ProcessWithResult.runWithIO(() -> ProcessWithResult.waitFor(ph));
         |                ()
         |            } @ rc;
         |
         |            spawn {
         |                Timer.runWithIO(() -> Timer.sleepMillis(50i64));
         |                throw Exn.mk(13);
         |                ()
         |            } @ rc;
         |
         |            ()
         |        };
         |        -1
         |    } catch {
         |        case exn: Int32 => Exn.payloadAs(exn)
         |        case _: Exn => -2
         |    };
         |
         |    let elapsed = Clock.runWithIO(() -> Clock.now()) - start;
         |    let _ = expect(payload == 13, "expected sibling throw to cancel blocked process wait");
         |    let _ = expect(elapsed < 2000i64, "expected region cancellation to interrupt in-flight process wait promptly");
         |
         |    let alive0 = ProcessWithResult.runWithIO(() -> ProcessWithResult.isAlive(ph)) |> expectOk;
         |    let _ = expect(alive0, "expected blocked wait cancellation not to stop the child process");
         |
         |    ProcessWithResult.runWithIO(() -> ProcessWithResult.stop(ph)) |> expectOkUnit;
         |    let done0 = ProcessWithResult.runWithIO(() -> ProcessWithResult.waitForTimeout(ph, 2000i64, TimeUnit.Milliseconds)) |> expectOk;
         |    let _ = expect(done0, "expected waitForTimeout to observe completion after explicit stop");
         |    ProcessHandle.release(ph) |> expectOkUnit;
         |    ()
         |}
         |""".stripMargin

    val testFile = Files.createTempFile("flix-portable-process-cancel-llvm-native-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-process-cancel-")
    try {
      Files.writeString(testFile, program, StandardCharsets.UTF_8)
      val exe = compileLlvmNative(testFile, outDir)
      val (exit, output) = runExecutable(exe)
      if (exit != 0) {
        fail(s"LLVM-native portable process cancellation test program failed with exit $exit:\n$output")
      }
    } finally {
      Files.deleteIfExists(testFile)
      deleteRecursive(outDir)
    }
  }

  test("portable-process-stdout-read-cancellation-llvm-native") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native portable process runtime test)")
    assume(!isWindows, "POSIX shell required for LLVM-native portable process stdout cancellation test")

    val program =
      s"""
         |def expect(cond: Bool, msg: String): Unit =
         |    if (cond) () else bug!(msg)
         |
         |def expectOk(res: Result[IoError, a]): a = match res {
         |    case Ok(v) => v
         |    case Err(e) => bug!("expected Ok but got Err: " + ToString.toString(e))
         |}
         |
         |def expectOkUnit(res: Result[IoError, Unit]): Unit = match res {
         |    case Ok(_) => ()
         |    case Err(e) => bug!("expected Ok but got Err: " + ToString.toString(e))
         |}
         |
         |def main(): Unit \\ IO = {
         |    let ph = ProcessWithResult.runWithIO(() -> ProcessWithResult.exec("/bin/sh", "-c" :: "sleep 5" :: Nil)) |> expectOk;
         |
         |    let start = Clock.runWithIO(() -> Clock.now());
         |    let payload = try {
         |        region rc {
         |            spawn {
         |                let out = Process.StdOut.StdOut(ph);
         |                let buf: Array[Int8, Static] = Array.empty(Static, 64);
         |                let _ = Readable.read(buf, out);
         |                ()
         |            } @ rc;
         |
         |            spawn {
         |                Timer.runWithIO(() -> Timer.sleepMillis(50i64));
         |                throw Exn.mk(17);
         |                ()
         |            } @ rc;
         |
         |            ()
         |        };
         |        -1
         |    } catch {
         |        case exn: Int32 => Exn.payloadAs(exn)
         |        case _: Exn => -2
         |    };
         |
         |    let elapsed = Clock.runWithIO(() -> Clock.now()) - start;
         |    let _ = expect(payload == 17, "expected sibling throw to cancel blocked stdout read");
         |    let _ = expect(elapsed < 2000i64, "expected region cancellation to interrupt blocked stdout read promptly");
         |
         |    let alive0 = ProcessWithResult.runWithIO(() -> ProcessWithResult.isAlive(ph)) |> expectOk;
         |    let _ = expect(alive0, "expected blocked stdout cancellation not to stop the child process");
         |
         |    ProcessWithResult.runWithIO(() -> ProcessWithResult.stop(ph)) |> expectOkUnit;
         |    let done0 = ProcessWithResult.runWithIO(() -> ProcessWithResult.waitForTimeout(ph, 2000i64, TimeUnit.Milliseconds)) |> expectOk;
         |    let _ = expect(done0, "expected waitForTimeout to observe completion after explicit stop");
         |    ProcessHandle.release(ph) |> expectOkUnit;
         |    ()
         |}
         |""".stripMargin

    val testFile = Files.createTempFile("flix-portable-process-stdout-cancel-llvm-native-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-process-stdout-cancel-")
    try {
      Files.writeString(testFile, program, StandardCharsets.UTF_8)
      val exe = compileLlvmNative(testFile, outDir)
      val (exit, output) = runExecutable(exe)
      if (exit != 0) {
        fail(s"LLVM-native portable process stdout cancellation test program failed with exit $exit:\n$output")
      }
    } finally {
      Files.deleteIfExists(testFile)
      deleteRecursive(outDir)
    }
  }

  private def escapeFlixString(s: String): String =
    "\"" + s.flatMap {
      case '\\' => "\\\\"
      case '"' => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c => c.toString
    } + "\""

  private def compileLlvmNative(file: Path, outDir: Path): Path = {
    val flix = new Flix()
    flix.setOptions(TestOptions.copy(outputPath = outDir))
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted
    flix.addFile(file)

    val (optRoot, errors) = flix.check()
    if (errors.nonEmpty) {
      fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
    }

    flix.codeGen(optRoot.get)
    executablePath(outDir)
  }

  private def runExecutable(executable: Path): (Int, String) = {
    val pb = new ProcessBuilder(List(executable.toString).asJava)
    pb.redirectErrorStream(true)
    val p = pb.start()
    val output = new String(p.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val exit = p.waitFor()
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
