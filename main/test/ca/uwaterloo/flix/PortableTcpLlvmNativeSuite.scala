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

import java.io.{BufferedReader, IOException, InputStreamReader}
import java.net.{InetAddress, InetSocketAddress, ServerSocket, Socket}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.concurrent.{ArrayBlockingQueue, Callable, ConcurrentLinkedQueue, CountDownLatch, ExecutorService, Executors, TimeUnit}
import scala.jdk.CollectionConverters.*

/**
  * Runtime smoke tests for the portable TCP primops on the LLVM-native backend.
  *
  * These tests use an in-process loopback server bound to an ephemeral port.
  */
class PortableTcpLlvmNativeSuite extends AnyFunSuite {

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmNative,
      incremental = false,
      outputJvm = false,
    )

  test("portable-tcp-llvm-native") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native portable TCP runtime test)")

    val testFile = Files.createTempFile("flix-portable-tcp-llvm-native-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-tcp-")

    val program =
      s"""
         |def expect(cond: Bool, msg: String): Unit =
         |    if (cond) () else bug!(msg)
         |
         |def unexpectedErr(e: a): Unit with ToString[a] =
         |    expect(false, "unexpected Err: " + ToString.toString(e))
         |
         |def watchdog(): Unit \\ IO = {
         |    %%SLEEP_MILLIS%%(15_000i64);
         |    %%EXIT%%(1i32)
         |}
         |
         |def copyN(i: Int32, n: Int32, off: Int32, src: Array[Int8, r], dst: Array[Int8, r]): Unit \\ r =
         |    if (i < n) {
         |        Array.put(Array.get(i, src), off + i, dst);
         |        copyN(i + 1, n, off, src, dst)
         |    } else {
         |        ()
         |    }
         |
         |def readExactly(rc: Region[r], sock: TcpSocket, dst: Array[Int8, r], off: Int32, remaining: Int32): Unit \\ r + IO =
         |    if (remaining == 0i32) {
         |        ()
         |    } else {
         |        let tmp = Array.empty(rc, remaining);
         |        match TcpSocket.read(tmp, sock) {
         |            case Ok(n) => {
         |                let _ = expect(n > 0i32, "unexpected EOF");
         |                copyN(0i32, n, off, tmp, dst);
         |                readExactly(rc, sock, dst, off + n, remaining - n)
         |            }
         |            case Err(e) => unexpectedErr(e)
         |        }
         |    }
         |
         |def main(): Unit \\ IO = region rc {
         |    let ip = IpAddr.V4(Ipv4Addr.localhost());
         |    match TcpConnect.runWithIO(() -> TcpConnect.connect(ip, PORT_HEREi32)) {
         |        case Ok(sock) => {
         |            let msg = Array#{112i8, 105i8, 110i8, 103i8} @ rc; // "ping"
         |
         |            match TcpSocket.write(msg, sock) {
         |                case Ok(n) => expect(n == 4i32, "expected 4 bytes written")
         |                case Err(e) => unexpectedErr(e)
         |            };
         |
         |            let buf = Array.empty(rc, 4);
         |            let _ = readExactly(rc, sock, buf, 0i32, 4i32);
         |            let _ = expect(Array.get(0, buf) == 112i8, "bad byte[0]");
         |            let _ = expect(Array.get(1, buf) == 111i8, "bad byte[1]");
         |            let _ = expect(Array.get(2, buf) == 110i8, "bad byte[2]");
         |            let _ = expect(Array.get(3, buf) == 103i8, "bad byte[3]");
         |
         |            match TcpSocket.close(sock) {
         |                case Ok(_) => ()
         |                case Err(e) => unexpectedErr(e)
         |            };
         |            ()
         |        }
         |        case Err(e) => unexpectedErr(e)
         |    }
         |}
         |""".stripMargin

    val server = new ServerSocket()
    server.bind(new InetSocketAddress(InetAddress.getByName("127.0.0.1"), 0))
    server.setSoTimeout(10_000)
    val port = server.getLocalPort

    val executor: ExecutorService = Executors.newSingleThreadExecutor()
    try {
      Files.writeString(testFile, program.replace("PORT_HERE", port.toString), StandardCharsets.UTF_8)
      val exe = compileLlvmNative(testFile, outDir)
      val serverFuture = executor.submit(new Callable[Unit] {
        override def call(): Unit = {
          val socket = server.accept()
          try {
            socket.setSoTimeout(10_000)
            handleEcho(socket)
          } finally {
            socket.close()
          }
        }
      })
      val (exit, output) = runExecutable(exe, timeoutSeconds = 15)
      if (exit != 0) {
        fail(s"LLVM-native portable TCP test program failed with exit $exit:\n$output")
      }

      // Ensure the server thread also completed successfully.
      serverFuture.get(10, TimeUnit.SECONDS)
    } finally {
      Files.deleteIfExists(testFile)
      server.close()
      executor.shutdownNow()
      deleteRecursive(outDir)
    }
  }

  test("portable-tcp-server-llvm-native") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native portable TCP runtime test)")

    val program =
      s"""
         |def expect(cond: Bool, msg: String): Unit =
         |    if (cond) () else bug!(msg)
         |
         |def unexpectedErr(e: a): Unit with ToString[a] =
         |    expect(false, "unexpected Err: " + ToString.toString(e))
         |
         |def copyN(i: Int32, n: Int32, off: Int32, src: Array[Int8, r], dst: Array[Int8, r]): Unit \\ r =
         |    if (i < n) {
         |        Array.put(Array.get(i, src), off + i, dst);
         |        copyN(i + 1, n, off, src, dst)
         |    } else {
         |        ()
         |    }
         |
         |def readExactly(rc: Region[r], sock: TcpSocket, dst: Array[Int8, r], off: Int32, remaining: Int32): Unit \\ r + IO =
         |    if (remaining == 0i32) {
         |        ()
         |    } else {
         |        let tmp = Array.empty(rc, remaining);
         |        match TcpSocket.read(tmp, sock) {
         |            case Ok(n) => {
         |                let _ = expect(n > 0i32, "unexpected EOF");
         |                copyN(0i32, n, off, tmp, dst);
         |                readExactly(rc, sock, dst, off + n, remaining - n)
         |            }
         |            case Err(e) => unexpectedErr(e)
         |        }
         |    }
         |
         |def main(): Unit \\ IO = region rc {
         |    let ip = IpAddr.V4(Ipv4Addr.localhost());
         |    match TcpBind.runWithIO(() -> TcpBind.bind(ip, 0i32)) {
         |        case Ok(server) => {
         |            match TcpServer.localPort(server) {
         |                case Ok(port) => {
         |                    println(String.concat("PORT:", Int32.toString(port)));
         |
         |                    match TcpAccept.runWithIO(() -> TcpAccept.accept(server)) {
         |                        case Ok(sock) => {
         |                            let buf = Array.empty(rc, 4);
         |                            let _ = readExactly(rc, sock, buf, 0i32, 4i32);
         |                            let _ = expect(Array.get(0, buf) == 112i8, "bad byte[0]");
         |                            let _ = expect(Array.get(1, buf) == 105i8, "bad byte[1]");
         |                            let _ = expect(Array.get(2, buf) == 110i8, "bad byte[2]");
         |                            let _ = expect(Array.get(3, buf) == 103i8, "bad byte[3]");
         |
         |                            let resp = Array#{112i8, 111i8, 110i8, 103i8} @ rc; // "pong"
         |                            match TcpSocket.write(resp, sock) {
         |                                case Ok(n) => expect(n == 4i32, "expected 4 bytes written")
         |                                case Err(e) => unexpectedErr(e)
         |                            };
         |
         |                            match TcpSocket.close(sock) {
         |                                case Ok(_) => ()
         |                                case Err(e) => unexpectedErr(e)
         |                            };
         |                            ()
         |                        }
         |                        case Err(e) => unexpectedErr(e)
         |                    };
         |
         |                    match TcpServer.close(server) {
         |                        case Ok(_) => ()
         |                        case Err(e) => unexpectedErr(e)
         |                    };
         |
         |                    ()
         |                }
         |                case Err(e) => unexpectedErr(e)
         |            }
         |        }
         |        case Err(e) => unexpectedErr(e)
         |    }
         |}
         |""".stripMargin

    val testFile = Files.createTempFile("flix-portable-tcp-server-llvm-native-", ".flix")
    Files.writeString(testFile, program, StandardCharsets.UTF_8)

    val outDir = Files.createTempDirectory("flix-llvm-native-tcp-server-")
    try {
      val exe = compileLlvmNative(testFile, outDir)
      runServerAndClient(exe)
    } finally {
      Files.deleteIfExists(testFile)
      deleteRecursive(outDir)
    }
  }

  test("portable-tcp-read-cancellation-llvm-native") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native portable TCP runtime test)")

    val testFile = Files.createTempFile("flix-portable-tcp-read-cancel-llvm-native-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-tcp-read-cancel-")

    val program =
      s"""
         |def expect(cond: Bool, msg: String): Unit =
         |    if (cond) () else bug!(msg)
         |
         |def unexpectedErr(e: a): Unit with ToString[a] =
         |    expect(false, "unexpected Err: " + ToString.toString(e))
         |
         |def connectOrCrash(ip: IpAddr, port: Int32): TcpSocket \\ IO =
         |    match TcpConnect.runWithIO(() -> TcpConnect.connect(ip, port)) {
         |        case Ok(sock) => sock
         |        case Err(e) => bug!("unexpected Err: " + ToString.toString(e))
         |    }
         |
         |def main(): Unit \\ IO = {
         |    let start = Clock.runWithIO(() -> Clock.now());
         |    let payload = try {
         |        region rc {
         |            let ip = IpAddr.V4(Ipv4Addr.localhost());
         |            let sock = connectOrCrash(ip, PORT_HEREi32);
         |
         |            spawn {
         |                let buf = Array.empty(Static, 8i32);
         |                let _ = TcpSocket.read(buf, sock);
         |                ()
         |            } @ rc;
         |
         |            spawn {
         |                Timer.runWithIO(() -> Timer.sleepMillis(50i64));
         |                throw Exn.mk(7);
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
         |    let _ = expect(payload == 7, "expected child exception to win during TCP read cancellation test");
         |    let _ = expect(elapsed < 2000i64, "expected region cancellation to interrupt in-flight TCP read promptly");
         |    ()
         |}
         |""".stripMargin

    val server = new ServerSocket()
    server.bind(new InetSocketAddress(InetAddress.getByName("127.0.0.1"), 0))
    server.setSoTimeout(10_000)
    val port = server.getLocalPort

    val executor: ExecutorService = Executors.newSingleThreadExecutor()
    try {
      Files.writeString(testFile, program.replace("PORT_HERE", port.toString), StandardCharsets.UTF_8)
      val exe = compileLlvmNative(testFile, outDir)
      val accepted = new CountDownLatch(1)
      val release = new CountDownLatch(1)
      val serverFuture = executor.submit(new Callable[Unit] {
        override def call(): Unit = {
          val socket = server.accept()
          try {
            accepted.countDown()
            release.await(15, TimeUnit.SECONDS)
          } finally {
            socket.close()
          }
        }
      })
      val (exit, output) = runExecutable(exe, timeoutSeconds = 15)
      if (exit != 0) {
        fail(s"LLVM-native portable TCP read cancellation test program failed with exit $exit:\n$output")
      }
      if (!accepted.await(1, TimeUnit.SECONDS)) {
        fail("cancellation test did not establish the loopback TCP connection")
      }
      release.countDown()
      serverFuture.get(10, TimeUnit.SECONDS)
    } finally {
      Files.deleteIfExists(testFile)
      server.close()
      executor.shutdownNow()
      deleteRecursive(outDir)
    }
  }

  test("portable-tcp-accept-cancellation-llvm-native") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native portable TCP runtime test)")

    val testFile = Files.createTempFile("flix-portable-tcp-accept-cancel-llvm-native-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-tcp-accept-cancel-")

    val program =
      s"""
         |def expect(cond: Bool, msg: String): Unit =
         |    if (cond) () else bug!(msg)
         |
         |def main(): Unit \\ IO = {
         |    let start = Clock.runWithIO(() -> Clock.now());
         |    let payload = try {
         |        region rc {
         |            let ip = IpAddr.V4(Ipv4Addr.localhost());
         |            let server = match TcpBind.runWithIO(() -> TcpBind.bind(ip, 0i32)) {
         |                case Ok(server) => server
         |                case Err(e) => bug!("unexpected bind error: " + ToString.toString(e))
         |            };
         |
         |            spawn {
         |                let _ = TcpAccept.runWithIO(() -> TcpAccept.accept(server));
         |                ()
         |            } @ rc;
         |
         |            spawn {
         |                Timer.runWithIO(() -> Timer.sleepMillis(50i64));
         |                throw Exn.mk(9);
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
         |    let _ = expect(payload == 9, "expected child exception to win during TCP accept cancellation test");
         |    let _ = expect(elapsed < 2000i64, "expected region cancellation to interrupt in-flight TCP accept promptly");
         |    ()
         |}
         |""".stripMargin

    try {
      Files.writeString(testFile, program, StandardCharsets.UTF_8)
      val exe = compileLlvmNative(testFile, outDir)
      val (exit, output) = runExecutable(exe, timeoutSeconds = 15)
      if (exit != 0) {
        fail(s"LLVM-native portable TCP accept cancellation test program failed with exit $exit:\n$output")
      }
    } finally {
      Files.deleteIfExists(testFile)
      deleteRecursive(outDir)
    }
  }

  test("portable-tcp-write-cancellation-llvm-native") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-native portable TCP runtime test)")

    val testFile = Files.createTempFile("flix-portable-tcp-write-cancel-llvm-native-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-native-tcp-write-cancel-")

    val program =
      s"""
         |def expect(cond: Bool, msg: String): Unit =
         |    if (cond) () else bug!(msg)
         |
         |def connectOrCrash(ip: IpAddr, port: Int32): TcpSocket \\ IO =
         |    match TcpConnect.runWithIO(() -> TcpConnect.connect(ip, port)) {
         |        case Ok(sock) => sock
         |        case Err(e) => bug!("unexpected Err: " + ToString.toString(e))
         |    }
         |
         |def main(): Unit \\ IO = {
         |    let start = Clock.runWithIO(() -> Clock.now());
         |    let payload = try {
         |        region rc {
         |            let ip = IpAddr.V4(Ipv4Addr.localhost());
         |            let sock = connectOrCrash(ip, PORT_HEREi32);
         |            let buf = Array.empty(Static, 64_000_000i32);
         |
         |            spawn {
         |                let _ = TcpSocket.write(buf, sock);
         |                ()
         |            } @ rc;
         |
         |            spawn {
         |                Timer.runWithIO(() -> Timer.sleepMillis(50i64));
         |                throw Exn.mk(11);
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
         |    let _ = expect(payload == 11, "expected child exception to win during TCP write cancellation test");
         |    let _ = expect(elapsed < 4000i64, "expected region cancellation to interrupt in-flight TCP write promptly");
         |    ()
         |}
         |""".stripMargin

    val server = new ServerSocket()
    server.setReceiveBufferSize(1024)
    server.bind(new InetSocketAddress(InetAddress.getByName("127.0.0.1"), 0))
    server.setSoTimeout(10_000)
    val port = server.getLocalPort

    val executor: ExecutorService = Executors.newSingleThreadExecutor()
    try {
      Files.writeString(testFile, program.replace("PORT_HERE", port.toString), StandardCharsets.UTF_8)
      val exe = compileLlvmNative(testFile, outDir)
      val accepted = new CountDownLatch(1)
      val release = new CountDownLatch(1)
      val serverFuture = executor.submit(new Callable[Unit] {
        override def call(): Unit = {
          val socket = server.accept()
          try {
            socket.setReceiveBufferSize(1024)
            accepted.countDown()
            release.await(15, TimeUnit.SECONDS)
          } finally {
            socket.close()
          }
        }
      })
      val (exit, output) = runExecutable(exe, timeoutSeconds = 20)
      if (exit != 0) {
        fail(s"LLVM-native portable TCP write cancellation test program failed with exit $exit:\n$output")
      }
      if (!accepted.await(1, TimeUnit.SECONDS)) {
        fail("write cancellation test did not establish the loopback TCP connection")
      }
      release.countDown()
      serverFuture.get(10, TimeUnit.SECONDS)
    } finally {
      Files.deleteIfExists(testFile)
      server.close()
      executor.shutdownNow()
      deleteRecursive(outDir)
    }
  }

  private def runServerAndClient(executable: Path): Unit = {
    val pb = new ProcessBuilder(List(executable.toString).asJava)
    pb.redirectErrorStream(true)
    val p = pb.start()

    val outputLines = new ConcurrentLinkedQueue[String]()
    val portQueue = new ArrayBlockingQueue[Integer](1)

    val readerThread = new Thread(() => {
      val reader = new BufferedReader(new InputStreamReader(p.getInputStream, StandardCharsets.UTF_8))
      var line: String = null
      while ({
        line = reader.readLine()
        line != null
      }) {
        outputLines.add(line)
        if (line.startsWith("PORT:")) {
          val portStr = line.substring("PORT:".length).trim
          try portQueue.offer(Integer.valueOf(portStr))
          catch {
            case _: NumberFormatException => ()
          }
        }
      }
    })

    readerThread.setDaemon(true)
    readerThread.start()

    val port = portQueue.poll(10, TimeUnit.SECONDS)
    if (port == null) {
      p.destroyForcibly()
      readerThread.join(1_000)
      fail(s"did not receive port from server executable.\nOutput so far:\n${outputLines.asScala.mkString("\n")}")
    }

    val client = new Socket()
    try {
      client.connect(new InetSocketAddress(InetAddress.getByName("127.0.0.1"), port.intValue()), 10_000)
      client.setSoTimeout(10_000)

      val out = client.getOutputStream
      out.write("ping".getBytes(StandardCharsets.US_ASCII))
      out.flush()

      val in = client.getInputStream
      val got = in.readNBytes(4)
      if (got.length != 4 || got(0) != 112.toByte || got(1) != 111.toByte || got(2) != 110.toByte || got(3) != 103.toByte) {
        val bytes = got.map(b => f"${b & 0xff}%02x").mkString(" ")
        fail(s"expected pong, got: $bytes\nServer output so far:\n${outputLines.asScala.mkString("\n")}")
      }
    } finally {
      client.close()
    }

    val finished = p.waitFor(10, TimeUnit.SECONDS)
    if (!finished) {
      p.destroyForcibly()
      readerThread.join(1_000)
      fail(s"server executable did not exit in time.\nOutput so far:\n${outputLines.asScala.mkString("\n")}")
    }

    readerThread.join(1_000)
    val exit = p.exitValue()
    val output = outputLines.asScala.mkString("\n")
    if (exit != 0) {
      fail(s"LLVM-native portable TCP server executable failed with exit $exit:\n$output")
    }
  }

  private def handleEcho(socket: Socket): Unit = {
    val in = socket.getInputStream
    val out = socket.getOutputStream

    val got = in.readNBytes(4)
    if (got.length != 4 || got(0) != 112.toByte || got(1) != 105.toByte || got(2) != 110.toByte || got(3) != 103.toByte) {
      val bytes = got.map(b => f"${b & 0xff}%02x").mkString(" ")
      throw new AssertionError(s"expected ping, got: $bytes")
    }

    out.write("pong".getBytes(StandardCharsets.US_ASCII))
    out.flush()
  }

  private def waitForPeerClose(socket: Socket): Unit = {
    val in = socket.getInputStream
    val buf = new Array[Byte](16)
    var done = false
    while (!done) {
      val n = in.read(buf)
      if (n < 0) {
        done = true
      }
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
    executablePath(outDir)
  }

  private def runExecutable(executable: Path, timeoutSeconds: Long): (Int, String) = {
    val pb = new ProcessBuilder(List(executable.toString).asJava)
    pb.redirectErrorStream(true)
    val p = pb.start()
    val outputLines = new ConcurrentLinkedQueue[String]()
    val readerThread = new Thread(() => {
      val reader = new BufferedReader(new InputStreamReader(p.getInputStream, StandardCharsets.UTF_8))
      var line: String = null
      while ({
        line = reader.readLine()
        line != null
      }) {
        outputLines.add(line)
      }
    })

    readerThread.setDaemon(true)
    readerThread.start()

    val finished = p.waitFor(timeoutSeconds, TimeUnit.SECONDS)
    if (!finished) {
      p.destroyForcibly()
      readerThread.join(1_000)
      fail(s"executable did not exit in ${timeoutSeconds}s.\nOutput so far:\n${outputLines.asScala.mkString("\n")}")
    }

    readerThread.join(1_000)
    val exit = p.exitValue()
    val output = outputLines.asScala.mkString("\n")
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
