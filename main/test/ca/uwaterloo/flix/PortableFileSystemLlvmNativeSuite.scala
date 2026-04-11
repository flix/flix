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

import java.io.{FileOutputStream, IOException, OutputStream, RandomAccessFile}
import java.nio.charset.StandardCharsets
import java.nio.file.{FileSystems, Files, Path}
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors, TimeUnit}
import scala.jdk.CollectionConverters.*

class PortableFileSystemLlvmNativeSuite extends AnyFunSuite {

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmNative,
      incremental = false,
      outputJvm = false,
    )

  test("portable-filesystem-read-async-llvm-native") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native portable filesystem runtime test)")
    assume(isPosix, "POSIX filesystem required for FIFO-based LLVM-native filesystem runtime test")
    assume(hasMkFifo, "mkfifo not found on PATH (skipping LLVM-native portable filesystem runtime test)")

    val testFile = Files.createTempFile("flix-portable-filesystem-read-async-llvm-native-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-filesystem-read-async-")
    val tempDir = Files.createTempDirectory("flix-native-filesystem-async-")
    val listDir = Files.createDirectories(tempDir.resolve("listdir"))
    val textPipe = tempDir.resolve("text.pipe")
    val bytesPipe = tempDir.resolve("bytes.pipe")
    val linesPipe = tempDir.resolve("lines.pipe")

    createFifo(textPipe)
    createFifo(bytesPipe)
    createFifo(linesPipe)

    Files.writeString(listDir.resolve("alpha.txt"), "a", StandardCharsets.UTF_8)
    Files.writeString(listDir.resolve("beta.txt"), "b", StandardCharsets.UTF_8)

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
         |def main(): Unit \\ IO =
         |    FileReadWithResult.runWithIO(() -> {
         |        let s = expectOk(FileReadWithResult.read("${escapePath(textPipe)}"));
         |        let _ = expect(s == "hello", "unexpected text payload");
         |
         |        let bs = expectOk(FileReadWithResult.readBytes("${escapePath(bytesPipe)}"));
         |        let _ = expect(Vector.length(bs) == 3i32, "unexpected byte count");
         |        let _ = expect(Vector.get(0, bs) == 1i8, "bad byte[0]");
         |        let _ = expect(Vector.get(1, bs) == 2i8, "bad byte[1]");
         |        let _ = expect(Vector.get(2, bs) == 3i8, "bad byte[2]");
         |
         |        let lines = expectOk(FileReadWithResult.readLines("${escapePath(linesPipe)}"));
         |        let _ = expect(lines == "a" :: "b" :: Nil, "unexpected lines payload");
         |
         |        let names = expectOk(FileReadWithResult.list("${escapePath(listDir)}"));
         |        let _ = expect(List.exists(n -> n == "alpha.txt", names), "expected alpha.txt in directory listing");
         |        let _ = expect(List.exists(n -> n == "beta.txt", names), "expected beta.txt in directory listing");
         |        ()
         |    })
         |""".stripMargin

    val executor: ExecutorService = Executors.newFixedThreadPool(3)
    try {
      Files.writeString(testFile, program, StandardCharsets.UTF_8)
      val exe = compileLlvmNative(testFile, outDir)

      val textFuture = executor.submit(new Callable[Unit] {
        override def call(): Unit = withPipeWriter(textPipe) { os =>
          Thread.sleep(75)
          os.write("hello".getBytes(StandardCharsets.UTF_8))
        }
      })

      val bytesFuture = executor.submit(new Callable[Unit] {
        override def call(): Unit = withPipeWriter(bytesPipe) { os =>
          Thread.sleep(75)
          os.write(Array[Byte](1, 2, 3))
        }
      })

      val linesFuture = executor.submit(new Callable[Unit] {
        override def call(): Unit = withPipeWriter(linesPipe) { os =>
          Thread.sleep(75)
          os.write("a\nb\n".getBytes(StandardCharsets.UTF_8))
        }
      })

      val (exit, output) = runExecutable(exe, timeoutSeconds = 20)
      if (exit != 0) {
        fail(s"LLVM-native portable filesystem async read test program failed with exit $exit:\n$output")
      }

      textFuture.get(10, TimeUnit.SECONDS)
      bytesFuture.get(10, TimeUnit.SECONDS)
      linesFuture.get(10, TimeUnit.SECONDS)
    } finally {
      Files.deleteIfExists(testFile)
      executor.shutdownNow()
      deleteRecursive(outDir)
      deleteRecursive(tempDir)
    }
  }

  test("portable-filesystem-read-cancellation-llvm-native") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native portable filesystem runtime test)")
    assume(isPosix, "POSIX filesystem required for FIFO-based LLVM-native filesystem runtime test")
    assume(hasMkFifo, "mkfifo not found on PATH (skipping LLVM-native portable filesystem runtime test)")

    val testFile = Files.createTempFile("flix-portable-filesystem-read-cancel-llvm-native-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-filesystem-read-cancel-")
    val tempDir = Files.createTempDirectory("flix-native-filesystem-cancel-")
    val blockPipe = tempDir.resolve("block.pipe")
    createFifo(blockPipe)

    val program =
      s"""
         |def expect(cond: Bool, msg: String): Unit =
         |    if (cond) () else bug!(msg)
         |
         |def main(): Unit \\ IO = {
         |    let start = Clock.runWithIO(() -> Clock.now());
         |    let payload = try {
         |        region rc {
         |            spawn {
         |                let _ = FileReadWithResult.runWithIO(() -> FileReadWithResult.read("${escapePath(blockPipe)}"));
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
         |    let _ = expect(payload == 13, "expected child exception to win during filesystem read cancellation test");
         |    let _ = expect(elapsed < 2000i64, "expected region cancellation to interrupt in-flight filesystem read promptly");
         |    ()
         |}
         |""".stripMargin

    val executor: ExecutorService = Executors.newSingleThreadExecutor()
    try {
      Files.writeString(testFile, program, StandardCharsets.UTF_8)
      val exe = compileLlvmNative(testFile, outDir)

      val opened = new CountDownLatch(1)
      val release = new CountDownLatch(1)
      val writerFuture = executor.submit(new Callable[Unit] {
        override def call(): Unit = withPipeWriter(blockPipe) { _ =>
          opened.countDown()
          release.await(15, TimeUnit.SECONDS)
        }
      })

      val (exit, output) = runExecutable(exe, timeoutSeconds = 20)
      if (exit != 0) {
        fail(s"LLVM-native portable filesystem read cancellation test program failed with exit $exit:\n$output")
      }

      if (!opened.await(2, TimeUnit.SECONDS)) {
        fail("filesystem read cancellation test did not establish the FIFO writer")
      }
      release.countDown()
      writerFuture.get(10, TimeUnit.SECONDS)
    } finally {
      Files.deleteIfExists(testFile)
      executor.shutdownNow()
      deleteRecursive(outDir)
      deleteRecursive(tempDir)
    }
  }

  test("portable-filesystem-write-async-llvm-native") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native portable filesystem runtime test)")
    assume(isPosix, "POSIX filesystem required for FIFO-based LLVM-native filesystem runtime test")
    assume(hasMkFifo, "mkfifo not found on PATH (skipping LLVM-native portable filesystem runtime test)")

    val testFile = Files.createTempFile("flix-portable-filesystem-write-async-llvm-native-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-filesystem-write-async-")
    val tempDir = Files.createTempDirectory("flix-native-filesystem-write-async-")
    val textPipe = tempDir.resolve("text.pipe")
    val bytesPipe = tempDir.resolve("bytes.pipe")
    val appendFile = tempDir.resolve("append.txt")
    val appendBytesFile = tempDir.resolve("append-bytes.bin")

    createFifo(textPipe)
    createFifo(bytesPipe)

    val program =
      s"""
         |def expect(cond: Bool, msg: String): Unit =
         |    if (cond) () else bug!(msg)
         |
         |def expectOkUnit(res: Result[IoError, Unit]): Unit = match res {
         |    case Ok(_) => ()
         |    case Err(e) => bug!("expected Ok but got Err: " + ToString.toString(e))
         |}
         |
         |def expectOk(res: Result[IoError, a]): a = match res {
         |    case Ok(v) => v
         |    case Err(e) => bug!("expected Ok but got Err: " + ToString.toString(e))
         |}
         |
         |def main(): Unit \\ IO =
         |    FileWriteWithResult.runWithIO(() ->
         |        FileReadWithResult.runWithIO(() -> {
         |            expectOkUnit(FileWriteWithResult.write(str = "hello", "${escapePath(textPipe)}"));
         |            expectOkUnit(FileWriteWithResult.writeBytes(Vector.repeat(3i32, 7i8), "${escapePath(bytesPipe)}"));
         |
         |            expectOkUnit(FileWriteWithResult.write(str = "ab", "${escapePath(appendFile)}"));
         |            expectOkUnit(FileWriteWithResult.append(str = "cd", "${escapePath(appendFile)}"));
         |            let s = expectOk(FileReadWithResult.read("${escapePath(appendFile)}"));
         |            let _ = expect(s == "abcd", "unexpected append string payload");
         |
         |            let bs = Vector.repeat(2i32, 5i8);
         |            expectOkUnit(FileWriteWithResult.writeBytes(bs, "${escapePath(appendBytesFile)}"));
         |            expectOkUnit(FileWriteWithResult.appendBytes(bs, "${escapePath(appendBytesFile)}"));
         |            let out = expectOk(FileReadWithResult.readBytes("${escapePath(appendBytesFile)}"));
         |            let _ = expect(Vector.length(out) == 4i32, "unexpected append bytes length");
         |            let _ = expect(Vector.get(0, out) == 5i8, "bad append byte[0]");
         |            let _ = expect(Vector.get(1, out) == 5i8, "bad append byte[1]");
         |            let _ = expect(Vector.get(2, out) == 5i8, "bad append byte[2]");
         |            let _ = expect(Vector.get(3, out) == 5i8, "bad append byte[3]");
         |            ()
         |        })
         |    )
         |""".stripMargin

    val executor: ExecutorService = Executors.newFixedThreadPool(2)
    try {
      Files.writeString(testFile, program, StandardCharsets.UTF_8)
      val exe = compileLlvmNative(testFile, outDir)

      val textFuture = executor.submit(new Callable[String] {
        override def call(): String = withPipeReadWrite(textPipe) { raf =>
          val buf = new Array[Byte](5)
          raf.readFully(buf)
          new String(buf, StandardCharsets.UTF_8)
        }
      })

      val bytesFuture = executor.submit(new Callable[Array[Byte]] {
        override def call(): Array[Byte] = withPipeReadWrite(bytesPipe) { raf =>
          val buf = new Array[Byte](3)
          raf.readFully(buf)
          buf
        }
      })

      val (exit, output) = runExecutable(exe, timeoutSeconds = 20)
      if (exit != 0) {
        fail(s"LLVM-native portable filesystem async write test program failed with exit $exit:\n$output")
      }

      assert(textFuture.get(10, TimeUnit.SECONDS) == "hello")
      assert(bytesFuture.get(10, TimeUnit.SECONDS).sameElements(Array[Byte](7, 7, 7)))
    } finally {
      Files.deleteIfExists(testFile)
      executor.shutdownNow()
      deleteRecursive(outDir)
      deleteRecursive(tempDir)
    }
  }

  test("portable-filesystem-write-cancellation-llvm-native") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native portable filesystem runtime test)")
    assume(isPosix, "POSIX filesystem required for FIFO-based LLVM-native filesystem runtime test")
    assume(hasMkFifo, "mkfifo not found on PATH (skipping LLVM-native portable filesystem runtime test)")

    val testFile = Files.createTempFile("flix-portable-filesystem-write-cancel-llvm-native-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-filesystem-write-cancel-")
    val tempDir = Files.createTempDirectory("flix-native-filesystem-write-cancel-")
    val blockPipe = tempDir.resolve("block.pipe")
    createFifo(blockPipe)

    val program =
      s"""
         |def expect(cond: Bool, msg: String): Unit =
         |    if (cond) () else bug!(msg)
         |
         |def main(): Unit \\ IO = {
         |    let start = Clock.runWithIO(() -> Clock.now());
         |    let payload = try {
         |        region rc {
         |            spawn {
         |                let data = String.repeat(1048576i32, "x");
         |                let _ = FileWriteWithResult.runWithIO(() -> FileWriteWithResult.write(str = data, "${escapePath(blockPipe)}"));
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
         |    let _ = expect(payload == 13, "expected child exception to win during filesystem write cancellation test");
         |    let _ = expect(elapsed < 2000i64, "expected region cancellation to interrupt in-flight filesystem write promptly");
         |    ()
         |}
         |""".stripMargin

    val executor: ExecutorService = Executors.newSingleThreadExecutor()
    try {
      Files.writeString(testFile, program, StandardCharsets.UTF_8)
      val exe = compileLlvmNative(testFile, outDir)

      val opened = new CountDownLatch(1)
      val release = new CountDownLatch(1)
      val readerFuture = executor.submit(new Callable[Unit] {
        override def call(): Unit = withPipeReadWrite(blockPipe) { _ =>
          opened.countDown()
          release.await(15, TimeUnit.SECONDS)
        }
      })

      if (!opened.await(2, TimeUnit.SECONDS)) {
        fail("filesystem write cancellation test did not establish the FIFO holder")
      }

      val (exit, output) = runExecutable(exe, timeoutSeconds = 20)
      if (exit != 0) {
        fail(s"LLVM-native portable filesystem write cancellation test program failed with exit $exit:\n$output")
      }

      release.countDown()
      readerFuture.get(10, TimeUnit.SECONDS)
    } finally {
      Files.deleteIfExists(testFile)
      executor.shutdownNow()
      deleteRecursive(outDir)
      deleteRecursive(tempDir)
    }
  }

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

    val exe = ca.uwaterloo.flix.language.phase.llvm.LlvmNativeDriver.executablePath(outDir)
    if (!Files.exists(exe)) {
      fail(s"Missing native executable artifact: $exe")
    }
    exe
  }

  private def runExecutable(executable: Path, timeoutSeconds: Long): (Int, String) = {
    val pb = new ProcessBuilder(List(executable.toString).asJava)
    pb.redirectErrorStream(true)

    val p = pb.start()
    val output = new String(p.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val finished = p.waitFor(timeoutSeconds, TimeUnit.SECONDS)
    if (!finished) {
      p.destroyForcibly()
      fail(s"native executable timed out after ${timeoutSeconds}s.\nOutput so far:\n$output")
    }
    (p.exitValue(), output)
  }

  private def createFifo(path: Path): Unit = {
    Option(path.getParent).foreach(Files.createDirectories(_))
    val p = new ProcessBuilder("mkfifo", path.toString).redirectErrorStream(true).start()
    val output = new String(p.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val finished = p.waitFor(5, TimeUnit.SECONDS)
    if (!finished) {
      p.destroyForcibly()
      fail(s"mkfifo timed out for $path")
    }
    if (p.exitValue() != 0) {
      fail(s"mkfifo failed for $path with exit ${p.exitValue()}:\n$output")
    }
  }

  private def withPipeWriter(path: Path)(f: OutputStream => Unit): Unit = {
    val os = new FileOutputStream(path.toFile)
    try f(os)
    finally os.close()
  }

  private def withPipeReadWrite[A](path: Path)(f: RandomAccessFile => A): A = {
    val raf = new RandomAccessFile(path.toFile, "rw")
    try f(raf)
    finally raf.close()
  }

  private def escapePath(path: Path): String =
    path.toAbsolutePath.normalize().toString.replace("\\", "\\\\")

  private def hasMkFifo: Boolean = {
    try {
      val p = new ProcessBuilder("sh", "-lc", "command -v mkfifo >/dev/null").redirectErrorStream(true).start()
      p.waitFor(2, TimeUnit.SECONDS) && p.exitValue() == 0
    } catch {
      case _: IOException => false
      case _: InterruptedException => false
    }
  }

  private def isPosix: Boolean =
    FileSystems.getDefault.supportedFileAttributeViews().contains("posix")

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
