package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.api.{Bootstrap, BootstrapError, Version}
import ca.uwaterloo.flix.language.phase.llvm.LlvmNativeDriver
import ca.uwaterloo.flix.util.{CompilationTarget, FileOps, Formatter, Result, RunnerKind, StdlibProfile}
import org.scalatest.funsuite.AnyFunSuite

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.{DigestInputStream, MessageDigest}
import java.text.SimpleDateFormat
import java.util.Date
import java.util.zip.ZipFile
import scala.jdk.CollectionConverters.*
import scala.util.Using

class TestBootstrap extends AnyFunSuite {

  private val ProjectPrefix: String = "flix-project-"

  test("init") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
  }

  test("check") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.check(PkgTestUtils.mkFlix)
  }

  test("build") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.build(PkgTestUtils.mkFlix)
  }

  test("build-jar") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    val flix = PkgTestUtils.mkFlix
    b.build(flix)
    b.buildJar(flix)

    val packageName = p.getFileName.toString
    val jarPath = p.resolve("artifact").resolve(packageName + ".jar")
    assert(Files.exists(jarPath))
    assert(jarPath.getFileName.toString.startsWith(ProjectPrefix))
  }

  test("build-jar generates ZIP entries with fixed time") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    val flix = PkgTestUtils.mkFlix
    b.build(flix)
    b.buildJar(flix)

    val packageName = p.getFileName.toString
    val jarPath = p.resolve("artifact").resolve(packageName + ".jar")
    val format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    for (e <- new ZipFile(jarPath.toFile).entries().asScala) {
      val time = new Date(e.getTime)
      val formatted = format.format(time)
      assert(formatted == "2014-06-27 00:00:00")
    }
  }

  test("build-jar always generates package that is byte-for-byte exactly the same modulo concurrency") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val packageName = p.getFileName.toString
    val jarPath = p.resolve("artifact").resolve(packageName + ".jar")

    val flix1 = PkgTestUtils.mkFlix
    // Use 1 thread for deterministic symbols
    flix1.setOptions(flix1.options.copy(threads = 1))

    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.buildJar(flix1)
    val hash1 = calcHash(jarPath)

    // Use new flix instance to reset symbol generation
    val flix2 = PkgTestUtils.mkFlix
    // Use 1 thread for deterministic symbols
    flix2.setOptions(flix2.options.copy(threads = 1))
    b.buildJar(flix2)
    val hash2 = calcHash(jarPath)

    assert(
      hash1 == hash2,
      s"Two file hashes are not same: $hash1 and $hash2")
  }

  test("build-pkg") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)

    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.buildPkg()(Formatter.getDefault)

    val packageName = p.getFileName.toString
    val packagePath = p.resolve("artifact").resolve(packageName + ".fpkg")
    assert(Files.exists(packagePath))
    assert(packagePath.getFileName.toString.startsWith(ProjectPrefix))
  }

  test("build-pkg generates ZIP entries with fixed time") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)

    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.buildPkg()(Formatter.getDefault)

    val packageName = p.getFileName.toString
    val packagePath = p.resolve("artifact").resolve(packageName + ".fpkg")
    val format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    for (e <- new ZipFile(packagePath.toFile).entries().asScala) {
      val time = new Date(e.getTime)
      val formatted = format.format(time)
      assert(formatted == "2014-06-27 00:00:00")
    }
  }

  test("build-pkg always generates package that is byte-for-byte exactly the same") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val packageName = p.getFileName.toString
    val packagePath = p.resolve("artifact").resolve(packageName + ".fpkg")

    val flix = PkgTestUtils.mkFlix

    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.build(flix)

    b.buildPkg()(Formatter.getDefault)

    val hash1 = calcHash(packagePath)

    b.buildPkg()(Formatter.getDefault)

    val hash2 = calcHash(packagePath)

    assert(
      hash1 == hash2,
      s"Two file hashes are not same: $hash1 and $hash2")
  }

  test("build-pkg rejects local path dependencies") {
    val root = Files.createTempDirectory(ProjectPrefix + "local-dep-root-")
    val bridge = root.resolve("bridge-demo").normalize()
    val producer = root.resolve("local-dep-producer").normalize()
    Files.createDirectories(bridge)
    Files.createDirectories(producer)
    Bootstrap.init(bridge)(System.out).unsafeGet
    Bootstrap.init(producer)(System.out).unsafeGet
    FileOps.writeString(producer.resolve("flix.toml").normalize(),
      s"""
         |[package]
         |name = "local-dep-producer"
         |description = "test"
         |version = "0.1.0"
         |flix = "${Version.CurrentVersion}"
         |authors = ["flix"]
         |
         |[dependencies]
         |"bridge-demo" = { path = "../bridge-demo", security = "unrestricted" }
         |""".stripMargin)

    val bootstrap = Bootstrap.bootstrap(producer, None)(Formatter.getDefault, System.out).unsafeGet
    bootstrap.buildPkg()(Formatter.getDefault) match {
      case Result.Ok(_) => fail("expected build-pkg to reject local path dependencies")
      case Result.Err(BootstrapError.FileError(message)) =>
        assert(message.contains("local path dependencies"))
      case Result.Err(other) =>
        fail(s"expected build-pkg local path dependency error, but got: $other")
    }
  }

  test("build-native-with-local-path-interop-dependency") {
    assume(ca.uwaterloo.flix.util.ZigToolchain.hasUsableCommand, "zig not available")

    val root = Files.createTempDirectory(ProjectPrefix + "local-interop-root-")
    val producer = root.resolve("bridge-demo").normalize()
    val consumer = root.resolve("consumer").normalize()

    Files.createDirectories(producer.resolve("src").normalize())
    Files.createDirectories(producer.resolve("bridge").normalize())
    FileOps.writeString(producer.resolve("LICENSE.md").normalize(), "Apache-2.0\n")
    FileOps.writeString(producer.resolve("README.md").normalize(), "# bridge-demo\n")
    FileOps.writeString(producer.resolve("flix.toml").normalize(),
      s"""
         |[package]
         |name = "bridge-demo"
         |description = "Local native bridge dependency"
         |version = "0.1.0"
         |flix = "${Version.CurrentVersion}"
         |authors = ["flix"]
         |
         |[target.native]
         |compile-sources = ["bridge/bridge.c"]
         |compile-include = ["bridge"]
         |
         |[[bindings.native]]
         |header = "bridge/bridge.h"
         |module = "Bridge"
         |""".stripMargin)

    FileOps.writeString(producer.resolve("bridge/bridge.h").normalize(),
      """
        |#ifndef FLIX_BRIDGE_DEMO_H
        |#define FLIX_BRIDGE_DEMO_H
        |
        |#include <stdint.h>
        |
        |int32_t bridge_double(int32_t x);
        |
        |#endif
        |""".stripMargin)

    FileOps.writeString(producer.resolve("bridge/bridge.c").normalize(),
      """
        |#include "bridge.h"
        |
        |int32_t bridge_double(int32_t x) {
        |    return x * 2;
        |}
        |""".stripMargin)

    Files.createDirectories(consumer.resolve("src").normalize())
    FileOps.writeString(consumer.resolve("LICENSE.md").normalize(), "Apache-2.0\n")
    FileOps.writeString(consumer.resolve("README.md").normalize(), "# consumer\n")
    FileOps.writeString(consumer.resolve("flix.toml").normalize(),
      s"""
         |[package]
         |name = "consumer"
         |description = "Consumes local native bridge dependency"
         |version = "0.1.0"
         |flix = "${Version.CurrentVersion}"
         |authors = ["flix"]
         |
         |[build]
         |targets = ["native"]
         |
         |[dependencies]
         |"bridge-demo" = { path = "../bridge-demo", security = "unrestricted" }
         |""".stripMargin)

    FileOps.writeString(consumer.resolve("src/Main.flix").normalize(),
      """
        |def main(): Unit \ IO =
        |    println(Int32.toString(Bridge.bridgeDouble(21)))
        |""".stripMargin)

    val bootstrap = Bootstrap.bootstrap(consumer, None)(Formatter.getDefault, System.out).unsafeGet
    val flix = PkgTestUtils.mkFlix
    flix.setOptions(flix.options.copy(target = CompilationTarget.LlvmNative, stdlibProfile = StdlibProfile.Portable))

    bootstrap.build(flix).unsafeGet

    val exePath = LlvmNativeDriver.executablePath(consumer.resolve("build/native").normalize(), "consumer")
    val (exit, output) = exec(List(exePath.toString), consumer)
    assert(exit == 0, s"expected local interop dependency executable to succeed, got exit $exit:\n$output")
    assert(output.trim == "42", s"unexpected local interop dependency output:\n$output")
  }

  test("build-pkg-native-interop-dependency") {
    assume(ca.uwaterloo.flix.util.ZigToolchain.hasUsableCommand, "zig not available")

    val producer = Files.createTempDirectory(ProjectPrefix + "interop-producer-")
    Files.createDirectories(producer.resolve("src").normalize())
    Files.createDirectories(producer.resolve("bridge").normalize())
    FileOps.writeString(producer.resolve("LICENSE.md").normalize(), "Apache-2.0\n")
    FileOps.writeString(producer.resolve("README.md").normalize(), "# bridge-demo\n")
    FileOps.writeString(producer.resolve("flix.toml").normalize(),
      s"""
         |[package]
         |name = "bridge-demo"
         |description = "Packaged native bridge dependency"
         |version = "0.1.0"
         |flix = "${Version.CurrentVersion}"
         |authors = ["flix"]
         |
         |[target.native]
         |compile-sources = ["bridge/bridge.c"]
         |compile-include = ["bridge"]
         |
         |[[bindings.native]]
         |header = "bridge/bridge.h"
         |module = "Bridge"
         |""".stripMargin)

    FileOps.writeString(producer.resolve("bridge/bridge.h").normalize(),
      """
        |#ifndef FLIX_BRIDGE_DEMO_H
        |#define FLIX_BRIDGE_DEMO_H
        |
        |#include <stdint.h>
        |
        |int32_t bridge_double(int32_t x);
        |
        |#endif
        |""".stripMargin)

    FileOps.writeString(producer.resolve("bridge/bridge.c").normalize(),
      """
        |#include "bridge.h"
        |
        |int32_t bridge_double(int32_t x) {
        |    return x * 2;
        |}
        |""".stripMargin)

    val producerBootstrap = Bootstrap.bootstrap(producer, None)(Formatter.getDefault, System.out).unsafeGet
    producerBootstrap.buildPkg()(Formatter.getDefault).unsafeGet

    val producerPkg = producer.resolve("artifact").resolve(producer.getFileName.toString + ".fpkg").normalize()
    assert(Files.exists(producerPkg))
    Using(new ZipFile(producerPkg.toFile)) { zip =>
      val entries = zip.entries().asScala.map(_.getName).toSet
      assert(entries.contains("bridge/bridge.h"))
      assert(entries.contains("bridge/bridge.c"))
      assert(entries.contains("build/native/generated/bindings/native/00-Bridge/flix/Bridge.flix"))
    }.get

    val consumer = Files.createTempDirectory(ProjectPrefix + "interop-consumer-")
    Files.createDirectories(consumer.resolve("src").normalize())
    Files.createDirectories(consumer.resolve("lib/github/acme/bridge-demo/0.1.0").normalize())
    FileOps.writeString(consumer.resolve("LICENSE.md").normalize(), "Apache-2.0\n")
    FileOps.writeString(consumer.resolve("README.md").normalize(), "# consumer\n")
    FileOps.writeString(consumer.resolve("flix.toml").normalize(),
      s"""
         |[package]
         |name = "consumer"
         |description = "Consumes packaged native bridge dependency"
         |version = "0.1.0"
         |flix = "${Version.CurrentVersion}"
         |authors = ["flix"]
         |
         |[build]
         |targets = ["native"]
         |
         |[dependencies]
         |"github:acme/bridge-demo" = { version = "0.1.0", security = "unrestricted" }
         |""".stripMargin)

    FileOps.writeString(consumer.resolve("src/Main.flix").normalize(),
      """
        |def main(): Unit \ IO =
        |    println(Int32.toString(Bridge.bridgeDouble(21)))
        |""".stripMargin)

    val cachedToml = consumer.resolve("lib/github/acme/bridge-demo/0.1.0/bridge-demo-0.1.0.toml").normalize()
    val cachedPkg = consumer.resolve("lib/github/acme/bridge-demo/0.1.0/bridge-demo-0.1.0.fpkg").normalize()
    Files.copy(producer.resolve("flix.toml").normalize(), cachedToml)
    Files.copy(producerPkg, cachedPkg)

    val bootstrap = Bootstrap.bootstrap(consumer, None)(Formatter.getDefault, System.out).unsafeGet
    val flix = PkgTestUtils.mkFlix
    flix.setOptions(flix.options.copy(target = CompilationTarget.LlvmNative, stdlibProfile = StdlibProfile.Portable))

    bootstrap.build(flix).unsafeGet

    val exePath = LlvmNativeDriver.executablePath(consumer.resolve("build/native").normalize(), "consumer")
    val (exit, output) = exec(List(exePath.toString), consumer)
    assert(exit == 0, s"expected packaged interop dependency executable to succeed, got exit $exit:\n$output")
    assert(output.trim == "42", s"unexpected packaged interop dependency output:\n$output")
  }

  test("run") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.run(PkgTestUtils.mkFlix, Array("arg0", "arg1"))
  }

  test("test") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.test(PkgTestUtils.mkFlix)
  }

  test("test-native") {
    assume(ca.uwaterloo.flix.util.ZigToolchain.hasUsableCommand, "zig not available")

    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet

    val flix = PkgTestUtils.mkFlix
    flix.setOptions(flix.options.copy(target = CompilationTarget.LlvmNative, stdlibProfile = StdlibProfile.Portable))

    b.test(flix, Some(RunnerKind.Native)).unsafeGet
  }

  test("build-native-with-manifest-link-config") {
    val zigCmd = ca.uwaterloo.flix.util.ZigToolchain.usableCommand.getOrElse(cancel("usable zig command not available"))

    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out).unsafeGet

    val nativeLibDir = p.resolve("native-lib").normalize()
    Files.createDirectories(nativeLibDir)
    buildNativeSmokeLibrary(nativeLibDir, zigCmd)

    FileOps.writeString(p.resolve("flix.toml").normalize(),
      s"""
         |[package]
         |name = "test"
         |description = "test"
         |version = "0.1.0"
         |flix = "${Version.CurrentVersion}"
         |authors = ["flix"]
         |
         |[build]
         |targets = ["native"]
         |
         |[run]
         |target = "native"
         |runner = "native"
         |
         |[test]
         |target = "native"
         |runner = "native"
         |
         |[target.native]
         |emit = ["exe"]
         |link-libs = ["nativeffi_smoke"]
         |link-search = [${tomlString(nativeLibDir.toAbsolutePath.normalize().toString)}]
         |""".stripMargin)

    FileOps.writeString(p.resolve("src/Main.flix").normalize(),
      """
        |extern native(symbol = "flix_native_mul2")
        |def mul2(x: Int32): Int32
        |
        |pub def main(): Unit \ IO =
        |    if (mul2(21) == 42)
        |        println("native-ffi: ok")
        |    else
        |        bug!("unexpected native ffi result")
        |""".stripMargin)

    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    val flix = PkgTestUtils.mkFlix
    flix.setOptions(flix.options.copy(target = CompilationTarget.LlvmNative, stdlibProfile = StdlibProfile.Portable))

    b.build(flix).unsafeGet

    val exePath = LlvmNativeDriver.executablePath(Bootstrap.getBuildTargetDirectory(p, CompilationTarget.LlvmNative), b.artifactName)
    val (exit, output) = exec(List(exePath.toString), p)
    assert(exit == 0, s"expected native ffi smoke executable to succeed, got exit $exit:\n$output")
    assert(output.contains("native-ffi: ok"), s"unexpected native ffi smoke output:\n$output")
  }

  test("build-native-with-manifest-link-config-string-bytes") {
    val zigCmd = ca.uwaterloo.flix.util.ZigToolchain.usableCommand.getOrElse(cancel("usable zig command not available"))

    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out).unsafeGet

    val nativeLibDir = p.resolve("native-lib").normalize()
    Files.createDirectories(nativeLibDir)
    buildNativeStringBytesLibrary(nativeLibDir, zigCmd)

    FileOps.writeString(p.resolve("flix.toml").normalize(),
      s"""
         |[package]
         |name = "test"
         |description = "test"
         |version = "0.1.0"
         |flix = "${Version.CurrentVersion}"
         |authors = ["flix"]
         |
         |[build]
         |targets = ["native"]
         |
         |[run]
         |target = "native"
         |runner = "native"
         |
         |[test]
         |target = "native"
         |runner = "native"
         |
         |[target.native]
         |emit = ["exe"]
         |link-libs = ["nativeffi_bridge"]
         |link-search = [${tomlString(nativeLibDir.toAbsolutePath.normalize().toString)}]
         |""".stripMargin)

    FileOps.writeString(p.resolve("src/Main.flix").normalize(),
      """
        |extern native(symbol = "flix_native_echo_bang")
        |def echoBang(s: String): String
        |
        |extern native(symbol = "flix_native_bytes_dup")
        |def bytesDup(bs: Array[Int8, Static]): Array[Int8, Static]
        |
        |pub def main(): Unit \ IO =
        |    let s = echoBang("hello");
        |    let bs = bytesDup(Array#{1i8, 2i8, 3i8} @ Static);
        |    if (s == "hello!" and Array.length(bs) == 3 and Array.get(0, bs) == 1i8 and Array.get(1, bs) == 2i8 and Array.get(2, bs) == 3i8)
        |        println("native-ffi-strings-bytes: ok")
        |    else
        |        bug!("unexpected native ffi string/bytes result")
        |""".stripMargin)

    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    val flix = PkgTestUtils.mkFlix
    flix.setOptions(flix.options.copy(target = CompilationTarget.LlvmNative, stdlibProfile = StdlibProfile.Portable))

    b.build(flix).unsafeGet

    val exePath = LlvmNativeDriver.executablePath(Bootstrap.getBuildTargetDirectory(p, CompilationTarget.LlvmNative), b.artifactName)
    val (exit, output) = exec(List(exePath.toString), p)
    assert(exit == 0, s"expected native ffi string/bytes smoke executable to succeed, got exit $exit:\n$output")
    assert(output.contains("native-ffi-strings-bytes: ok"), s"unexpected native ffi string/bytes smoke output:\n$output")
  }

  test("build-native-with-manifest-pkg-config") {
    assume(ca.uwaterloo.flix.util.ZigToolchain.hasUsableCommand, "zig not available")
    assume(hasCmd(List("pkg-config", "--version")), "pkg-config not available")
    assume(hasCmd(List("pkg-config", "--exists", "sqlite3")), "sqlite3 pkg-config package not available")

    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out).unsafeGet

    val bridgeDir = p.resolve("bridge").normalize()
    Files.createDirectories(bridgeDir)

    FileOps.writeString(bridgeDir.resolve("sqlite_smoke.c").normalize(),
      """
        |#include <sqlite3.h>
        |#include <stdint.h>
        |
        |int32_t flix_sqlite_pkg_config_smoke(void) {
        |    sqlite3* db = 0;
        |    int rc = sqlite3_open(":memory:", &db);
        |    if (rc != SQLITE_OK) {
        |        if (db != 0) sqlite3_close(db);
        |        return (int32_t)rc;
        |    }
        |    sqlite3_close(db);
        |    return 42;
        |}
        |""".stripMargin)

    FileOps.writeString(p.resolve("flix.toml").normalize(),
      s"""
         |[package]
         |name = "test"
         |description = "test"
         |version = "0.1.0"
         |flix = "${Version.CurrentVersion}"
         |authors = ["flix"]
         |
         |[build]
         |targets = ["native"]
         |
         |[run]
         |target = "native"
         |runner = "native"
         |
         |[test]
         |target = "native"
         |runner = "native"
         |
         |[target.native]
         |emit = ["exe"]
         |pkg-config = ["sqlite3"]
         |compile-sources = ["bridge/sqlite_smoke.c"]
         |""".stripMargin)

    FileOps.writeString(p.resolve("src/Main.flix").normalize(),
      """
        |extern native(symbol = "flix_sqlite_pkg_config_smoke")
        |def sqlitePkgConfigSmoke(): Int32
        |
        |pub def main(): Unit \ IO =
        |    if (sqlitePkgConfigSmoke() == 42i32)
        |        println("native-pkg-config: ok")
        |    else
        |        bug!("unexpected native pkg-config result")
        |""".stripMargin)

    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    val flix = PkgTestUtils.mkFlix
    flix.setOptions(flix.options.copy(target = CompilationTarget.LlvmNative, stdlibProfile = StdlibProfile.Portable))

    b.build(flix).unsafeGet

    val exePath = LlvmNativeDriver.executablePath(Bootstrap.getBuildTargetDirectory(p, CompilationTarget.LlvmNative), b.artifactName)
    val (exit, output) = exec(List(exePath.toString), p)
    assert(exit == 0, s"expected native pkg-config executable to succeed, got exit $exit:\n$output")
    assert(output.contains("native-pkg-config: ok"), s"unexpected native pkg-config output:\n$output")
  }

  test("build-native-with-manifest-generated-bindings-and-bridge-sources") {
    assume(ca.uwaterloo.flix.util.ZigToolchain.hasUsableCommand, "zig not available")

    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out).unsafeGet

    val bridgeDir = p.resolve("bridge").normalize()
    Files.createDirectories(bridgeDir)

    FileOps.writeString(bridgeDir.resolve("native_bridge.h").normalize(),
      """
        |#include <stdint.h>
        |#include <stdlib.h>
        |
        |int32_t flix_native_mul2(int32_t x);
        |char* flix_native_echo_bang(const char* s);
        |""".stripMargin)

    FileOps.writeString(bridgeDir.resolve("native_bridge.bind.toml").normalize(),
      """
        |[[binding]]
        |symbol = "flix_native_echo_bang"
        |result = "owned-string"
        |free = "free"
        |""".stripMargin)

    FileOps.writeString(bridgeDir.resolve("native_bridge.c").normalize(),
      """
        |#include "native_bridge.h"
        |
        |#include <string.h>
        |
        |int32_t flix_native_mul2(int32_t x) {
        |    return x * 2;
        |}
        |
        |char* flix_native_echo_bang(const char* s) {
        |    size_t len = strlen(s);
        |    char* out = malloc(len + 2);
        |    memcpy(out, s, len);
        |    out[len] = '!';
        |    out[len + 1] = '\0';
        |    return out;
        |}
        |""".stripMargin)

    FileOps.writeString(p.resolve("flix.toml").normalize(),
      s"""
         |[package]
         |name = "test"
         |description = "test"
         |version = "0.1.0"
         |flix = "${Version.CurrentVersion}"
         |authors = ["flix"]
         |
         |[build]
         |targets = ["native"]
         |
         |[run]
         |target = "native"
         |runner = "native"
         |
         |[test]
         |target = "native"
         |runner = "native"
         |
         |[target.native]
         |emit = ["exe"]
         |compile-sources = ["bridge/native_bridge.c"]
         |compile-include = ["bridge"]
         |
         |[[bindings.native]]
         |header = "bridge/native_bridge.h"
         |module = "Native"
         |spec = "bridge/native_bridge.bind.toml"
         |include = ["bridge"]
         |""".stripMargin)

    FileOps.writeString(p.resolve("src/Main.flix").normalize(),
      """
        |pub def main(): Unit \ IO =
        |    if (Native.flixNativeMul2(21i32) == 42i32 and Native.flixNativeEchoBang("hello") == "hello!")
        |        println("native-generated-bindings: ok")
        |    else
        |        bug!("unexpected generated binding result")
        |""".stripMargin)

    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    val flix = PkgTestUtils.mkFlix
    flix.setOptions(flix.options.copy(target = CompilationTarget.LlvmNative, stdlibProfile = StdlibProfile.Portable))

    b.build(flix).unsafeGet

    val generatedFlix = p.resolve("build/native/generated/bindings/native/00-Native/flix/Native.flix").normalize()
    val generatedShim = p.resolve("build/native/generated/bindings/native/00-Native/native/Native_shim.c").normalize()
    assert(Files.isRegularFile(generatedFlix), s"expected generated native binding source at $generatedFlix")
    assert(Files.isRegularFile(generatedShim), s"expected generated native shim at $generatedShim")

    val exePath = LlvmNativeDriver.executablePath(Bootstrap.getBuildTargetDirectory(p, CompilationTarget.LlvmNative), b.artifactName)
    val (exit, output) = exec(List(exePath.toString), p)
    assert(exit == 0, s"expected generated native binding executable to succeed, got exit $exit:\n$output")
    assert(output.contains("native-generated-bindings: ok"), s"unexpected generated native binding output:\n$output")
  }

  test("test-wasm-node") {
    assume(ca.uwaterloo.flix.util.ZigToolchain.hasUsableCommand, "zig not available")
    assume(hasCmd(List("wasm-tools", "--version")), "wasm-tools not available")
    assume(hasCmd(List("jco", "--version")), "jco not available")
    assume(hasCmd(List("node", "--version")), "node not available")

    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out)
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet

    val flix = PkgTestUtils.mkFlix
    flix.setOptions(flix.options.copy(target = CompilationTarget.LlvmWasm, stdlibProfile = StdlibProfile.Portable))

    b.test(flix, Some(RunnerKind.Node)).unsafeGet
  }

  test("clean-command-should-remove-class-files-and-directories-if-compiled-previously") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out).unsafeGet
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.build(PkgTestUtils.mkFlix)
    val buildDir = p.resolve("./build/").normalize()
    val buildFiles = FileOps.getFilesIn(buildDir, Int.MaxValue)
    if (buildFiles.isEmpty || buildFiles.exists(!FileOps.checkExt(_, "class"))) {
      fail(
        s"""build output is not as expected:
           |${buildFiles.mkString(System.lineSeparator())}
           |""".stripMargin)
    }
    b.clean()
    val newBuildFiles = FileOps.getFilesIn(buildDir, Int.MaxValue)
    if (newBuildFiles.nonEmpty || Files.exists(buildDir)) {
      fail(
        s"""at least one file was not cleaned from build dir:
           |${newBuildFiles.mkString(System.lineSeparator())}
           |""".stripMargin)
    }
  }

  test("clean-should-error-on-unexpected-file") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out).unsafeGet
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    b.build(PkgTestUtils.mkFlix)
    val buildDir = p.resolve("./build/").normalize()
    FileOps.writeString(buildDir.resolve("./other.txt").normalize(), "hello")
    b.clean() match {
      case Result.Ok(_) => fail("expected clean to abort")
      case Result.Err(_) => succeed
    }
  }

  test("clean-should-succeed-on-non-existent-build-dir") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out).unsafeGet
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    val buildDir = p.resolve("./build/").normalize()
    if (Files.exists(buildDir)) {
      fail("did not expected build directory to exist")
    }
    b.clean() match {
      case Result.Ok(_) => succeed
      case Result.Err(_) => fail("expected success")
    }
  }

  test("clean-should-do-nothing-in-directory-mode") {
    val p = Files.createTempDirectory(ProjectPrefix)
    FileOps.writeString(p.resolve("./Main.flix").normalize(),
      """
        |def main(): Unit = ()
        |""".stripMargin)
    val b = Bootstrap.bootstrap(p, None)(Formatter.getDefault, System.out).unsafeGet
    val buildDir = p.resolve("./build/").normalize()
    if (Files.exists(buildDir)) {
      fail("did not expected build directory to exist")
    }
    b.clean() match {
      case Result.Ok(_) => fail("expected failure in directory mode")
      case Result.Err(_) => succeed
    }
  }

  test("eff-lock should write effect lock file") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out).unsafeGet // Unsafe get to crash in case of error

    // Override manifest
    val toml = PkgTestUtils.mkTomlWithDeps(
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-java" = { version = "0.1.1", security = "unrestricted" }
        |"github:flix/test-pkg-trust-java" = { version = "0.1.0", security = "unrestricted" }
        |""".stripMargin
    )
    FileOps.writeString(p.resolve("flix.toml").normalize(), toml)

    // Override main file
    val main =
      """
        |pub def main(): Unit \ IO =
        |    TestPkgTrustTransitive.entry()
        |""".stripMargin
    FileOps.writeString(p.resolve("src/Main.flix").normalize(), main)

    // Assert effects.lock does not exist
    val effectLockFile = p.resolve("effects.lock").normalize()
    if (Files.exists(effectLockFile)) {
      fail("Unexpected 'effects.lock' file. File is not supposed to exist")
    }

    val bootstrap = Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out).unsafeGet
    val flix = PkgTestUtils.mkFlix
    bootstrap.lockEffects(flix).unsafeGet

    // Assert that effects.lock exists now
    if (Files.exists(effectLockFile)) {
      succeed
    } else {
      fail("File 'effects.lock' does not exist")
    }
  }

  test("eff-check on same version as before is ok") {
    // Version 0.1.0 of the dependency has signature `Int32 -> Int32`.
    // There is no upgrade done, but we assert that
    // performing eff-check after eff-lock succeeds.
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out).unsafeGet // Unsafe get to crash in case of error

    // Override manifest
    val toml = PkgTestUtils.mkTomlWithDeps(
      """
        |"github:jaschdoc/flix-test-pkg-eff-upgrade" = "0.1.0"
        |""".stripMargin
    )
    FileOps.writeString(p.resolve("flix.toml").normalize(), toml)

    // Override main file
    val main =
      """
        |pub def main(): Unit \ IO =
        |    println(Upgr.entrypoint(42))
        |""".stripMargin
    FileOps.writeString(p.resolve("src/Main.flix").normalize(), main)

    val bootstrap = Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out).unsafeGet
    bootstrap.lockEffects(PkgTestUtils.mkFlix).unsafeGet

    assert(bootstrap.checkEffects(PkgTestUtils.mkFlix) == Result.Ok(()))
  }

  test("eff-check on effect unsafe upgrade reports error") {
    // Version 0.1.0 of the dependency has signature `Int32 -> Int32`.
    // Version 0.1.1 of the dependency has signature `Int32 -> Int32 \ IO`.
    // We upgrade from `Int32 -> Int32` to `Int32 -> Int32 \ IO`
    // and assert that it does NOT succeed.
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out).unsafeGet // Unsafe get to crash in case of error

    val pkgAuthor = "jaschdoc"
    val pkgName = "flix-test-pkg-eff-upgrade"
    val vOld = "0.1.0"
    val vNew = "0.1.1"

    // Override manifest
    val toml = PkgTestUtils.mkTomlWithDeps(
      s"""
         |"github:$pkgAuthor/$pkgName" = "$vOld"
         |""".stripMargin
    )
    FileOps.writeString(p.resolve("flix.toml").normalize(), toml)

    // Override main file
    val main =
      """
        |pub def main(): Unit \ IO =
        |    println(Upgr.entrypoint(42))
        |""".stripMargin
    FileOps.writeString(p.resolve("src/Main.flix").normalize(), main)

    val bootstrap = Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out).unsafeGet
    bootstrap.lockEffects(PkgTestUtils.mkFlix).unsafeGet

    // Perform upgrade by overriding manifest
    val tomlUpgr = PkgTestUtils.mkTomlWithDeps(
      s"""
         |"github:$pkgAuthor/$pkgName" = "$vNew"
         |""".stripMargin
    )
    FileOps.writeString(p.resolve("flix.toml").normalize(), tomlUpgr)
    // Delete old files
    FileOps.delete(p.resolve(s"lib/github/$pkgAuthor/$pkgName/$vOld/$pkgName-$vOld.toml")).unsafeGet
    FileOps.delete(p.resolve(s"lib/github/$pkgAuthor/$pkgName/$vOld/$pkgName-$vOld.fpkg")).unsafeGet

    val bootstrapUpgr = Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out).unsafeGet

    bootstrapUpgr.checkEffects(PkgTestUtils.mkFlix) match {
      case Result.Err(BootstrapError.EffectUpgradeError(_)) => succeed
      case Result.Err(e) => fail(e.message(Formatter.getDefault))
      case Result.Ok(()) => fail("expected effect upgrade error")
    }
  }

  test("eff-check on effect downgrade is ok") {
    // Version 0.1.0 of the dependency has signature `Int32 -> Int32`.
    // Version 0.1.1 of the dependency has signature `Int32 -> Int32 \ IO`.
    // We downgrade from `Int32 -> Int32 \ IO` to `Int32 -> Int32`
    // and assert that it succeeds.
    val p = Files.createTempDirectory(ProjectPrefix)
    Bootstrap.init(p)(System.out).unsafeGet // Unsafe get to crash in case of error

    val pkgAuthor = "jaschdoc"
    val pkgName = "flix-test-pkg-eff-upgrade"
    val vSafe = "0.1.0"
    val vUnsafe = "0.1.1"

    // Override manifest
    val toml = PkgTestUtils.mkTomlWithDeps(
      s"""
         |"github:$pkgAuthor/$pkgName" = "$vUnsafe"
         |""".stripMargin
    )
    FileOps.writeString(p.resolve("flix.toml").normalize(), toml)

    // Override main file
    val main =
      """
        |pub def main(): Unit \ IO =
        |    println(Upgr.entrypoint(42))
        |""".stripMargin
    FileOps.writeString(p.resolve("src/Main.flix").normalize(), main)

    val bootstrap = Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out).unsafeGet
    bootstrap.lockEffects(PkgTestUtils.mkFlix).unsafeGet

    // Perform upgrade by overriding manifest
    val tomlUpgr = PkgTestUtils.mkTomlWithDeps(
      s"""
         |"github:$pkgAuthor/$pkgName" = "$vSafe"
         |""".stripMargin
    )
    FileOps.writeString(p.resolve("flix.toml").normalize(), tomlUpgr)
    // Delete old files
    FileOps.delete(p.resolve(s"lib/github/$pkgAuthor/$pkgName/$vUnsafe/$pkgName-$vUnsafe.toml")).unsafeGet
    FileOps.delete(p.resolve(s"lib/github/$pkgAuthor/$pkgName/$vUnsafe/$pkgName-$vUnsafe.fpkg")).unsafeGet

    val bootstrapUpgr = Bootstrap.bootstrap(p, PkgTestUtils.gitHubToken)(Formatter.getDefault, System.out).unsafeGet

    assert(bootstrapUpgr.checkEffects(PkgTestUtils.mkFlix) == Result.Ok(()))
  }

  private def calcHash(p: Path): String = {
    val sha = MessageDigest.getInstance("SHA-256")
    Using(new DigestInputStream(Files.newInputStream(p), sha)) { input =>
      input.readNBytes(8192)
      sha.digest.map("%02x".format(_)).mkString
    }.get
  }

  private def buildNativeSmokeLibrary(dir: Path, zigCmd: List[String]): Unit = {
    buildStaticCLibrary(
      dir = dir,
      zigCmd = zigCmd,
      baseName = "nativeffi_smoke",
      source =
      """
        |#include <stdint.h>
        |
        |int32_t flix_native_mul2(int32_t x) {
        |    return x * 2;
        |}
        |""".stripMargin)
  }

  private def buildNativeStringBytesLibrary(dir: Path, zigCmd: List[String]): Unit = {
    buildStaticCLibrary(
      dir = dir,
      zigCmd = zigCmd,
      baseName = "nativeffi_bridge",
      source =
      """
        |#include <stdint.h>
        |#include <stdlib.h>
        |#include <string.h>
        |
        |typedef struct flix_ctx flix_ctx_t;
        |typedef int64_t flix_handle_t;
        |typedef flix_handle_t flix_string_t;
        |typedef flix_handle_t flix_bytes_t;
        |typedef flix_handle_t flix_i8_array_t;
        |
        |extern flix_string_t flix_string_from_utf8(flix_ctx_t* ctx, const uint8_t* bytes, int64_t len);
        |extern uint8_t* flix_string_to_utf8(flix_ctx_t* ctx, flix_string_t str, int64_t* out_len);
        |extern flix_i8_array_t flix_i8_array_from_bytes(flix_ctx_t* ctx, const uint8_t* bytes, int64_t len);
        |extern uint8_t* flix_i8_array_to_bytes(flix_ctx_t* ctx, flix_i8_array_t arr, int64_t* out_len);
        |extern void flix_free(void* ptr);
        |
        |flix_string_t flix_native_echo_bang(flix_ctx_t* ctx, flix_string_t s) {
        |    int64_t len = 0;
        |    uint8_t* in = flix_string_to_utf8(ctx, s, &len);
        |    uint8_t* out = malloc((size_t)len + 1);
        |    memcpy(out, in, (size_t)len);
        |    out[len] = '!';
        |    flix_string_t result = flix_string_from_utf8(ctx, out, len + 1);
        |    flix_free(in);
        |    free(out);
        |    return result;
        |}
        |
        |flix_bytes_t flix_native_bytes_dup(flix_ctx_t* ctx, flix_bytes_t b) {
        |    int64_t len = 0;
        |    uint8_t* in = flix_i8_array_to_bytes(ctx, (flix_i8_array_t)b, &len);
        |    flix_i8_array_t result = flix_i8_array_from_bytes(ctx, in, len);
        |    flix_free(in);
        |    return (flix_bytes_t)result;
        |}
        |""".stripMargin)
  }

  private def buildStaticCLibrary(dir: Path, zigCmd: List[String], baseName: String, source: String): Unit = {
    val cFile = dir.resolve(s"$baseName.c").normalize()
    val objFile = dir.resolve(s"$baseName.o").normalize()
    val libFile = dir.resolve(s"lib$baseName.a").normalize()

    FileOps.writeString(cFile, source)

    val compileCmd = zigCmd ::: List("cc", "-fno-sanitize=undefined", "-c", cFile.toString, "-o", objFile.toString)
    val (compileExit, compileOutput) = exec(compileCmd, dir)
    if (compileExit != 0) {
      fail(s"native ffi smoke C compile failed with exit $compileExit:\n${compileCmd.mkString(" ")}\n\n$compileOutput")
    }

    val archiveCmd = zigCmd ::: List("ar", "rcs", libFile.toString, objFile.toString)
    val (archiveExit, archiveOutput) = exec(archiveCmd, dir)
    if (archiveExit != 0) {
      fail(s"native ffi smoke archive failed with exit $archiveExit:\n${archiveCmd.mkString(" ")}\n\n$archiveOutput")
    }
  }

  private def exec(cmd: List[String], cwd: Path): (Int, String) = {
    val pb = new ProcessBuilder(cmd*)
    pb.directory(cwd.toFile)
    pb.redirectErrorStream(true)
    val p = pb.start()
    val output = new String(p.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val exit = p.waitFor()
    (exit, output)
  }

  private def hasCmd(cmd: List[String]): Boolean = {
    try {
      val p = new ProcessBuilder(cmd.asJava).redirectErrorStream(true).start()
      p.waitFor()
      p.exitValue() == 0
    } catch {
      case _: IOException => false
      case _: InterruptedException => false
    }
  }

  private def tomlString(s: String): String = "\"" + s.flatMap {
    case '\\' => "\\\\"
    case '"' => "\\\""
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    case c => c.toString
  } + "\""

}
