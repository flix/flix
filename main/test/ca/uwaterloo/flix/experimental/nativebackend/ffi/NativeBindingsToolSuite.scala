/*
 * Copyright 2026 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix

import ca.uwaterloo.flix.api.{Bootstrap, Version}
import ca.uwaterloo.flix.language.phase.llvm.LlvmNativeDriver
import ca.uwaterloo.flix.tools.NativeBindingsTool
import ca.uwaterloo.flix.tools.pkg.PkgTestUtils
import ca.uwaterloo.flix.util.{CompilationTarget, FileOps, Formatter, StdlibProfile, ZigToolchain}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}

class NativeBindingsToolSuite extends AnyFunSuite {

  test("bind-native-generates-flix-and-native-project-runs") {
    assume(hasZig, "usable zig command not found (skipping native binding generator test)")

    val zigCmd = ZigToolchain.usableCommand.getOrElse(cancel("usable zig command not available"))
    val workDir = Files.createTempDirectory("flix-native-bindings-tool-")

    try {
      val headerFile = workDir.resolve("nativeffi_smoke.h").normalize()
      val specFile = workDir.resolve("nativeffi_smoke.bind.toml").normalize()
      val libDir = workDir.resolve("native-lib").normalize()
      Files.createDirectories(libDir)
      buildNativeSmokeLibrary(libDir, zigCmd)

      FileOps.writeString(headerFile,
        """
          |#include <stdint.h>
          |#include <stddef.h>
          |
          |typedef struct flix_counter flix_counter_t;
          |typedef int32_t (*flix_int_cb_t)(int32_t);
          |
          |int32_t flix_native_mul2(int32_t x);
          |double flix_native_add_half(double x);
          |int32_t flix_native_strlen_plus(const char* s, int32_t bonus);
          |int64_t flix_native_sum_bytes(const uint8_t* bytes, int64_t len);
          |int32_t flix_native_apply_twice(int32_t x, flix_int_cb_t cb);
          |const char* flix_native_borrowed_greeting(void);
          |char* flix_native_owned_greeting(void);
          |const uint8_t* flix_native_borrowed_bytes(size_t* out_len);
          |uint8_t* flix_native_owned_bytes(size_t* out_len);
          |const char* flix_native_echo(const char* s);
          |flix_counter_t* flix_counter_create(int32_t seed);
          |const flix_counter_t* flix_counter_global(void);
          |const flix_counter_t* flix_counter_view(flix_counter_t* counter);
          |void flix_counter_inc(flix_counter_t* counter, int32_t delta);
          |flix_counter_t* flix_counter_clone(const flix_counter_t* counter);
          |int32_t flix_counter_open(int32_t seed, flix_counter_t** out_counter);
          |const char* flix_counter_label(const flix_counter_t* counter);
          |int32_t flix_counter_value(const flix_counter_t* counter);
          |void flix_counter_dispose(flix_counter_t* counter);
          |""".stripMargin)

      FileOps.writeString(specFile,
        """
          |[[binding]]
          |symbol = "flix_native_apply_twice"
          |callback = "cb"
          |callback-export = "Api.bump"
          |
          |[[binding]]
          |symbol = "flix_native_borrowed_greeting"
          |result = "borrowed-string"
          |
          |[[binding]]
          |symbol = "flix_native_owned_greeting"
          |result = "owned-string"
          |free = "free"
          |
          |[[binding]]
          |symbol = "flix_native_borrowed_bytes"
          |result = "borrowed-bytes"
          |len = "out_len"
          |
          |[[binding]]
          |symbol = "flix_native_owned_bytes"
          |result = "owned-bytes"
          |len = "out_len"
          |free = "free"
          |
          |[[binding]]
          |symbol = "flix_counter_create"
          |result = "owned-handle"
          |type = "Counter"
          |effect = "IO"
          |
          |[[binding]]
          |symbol = "flix_counter_global"
          |result = "borrowed-handle"
          |type = "CounterRef"
          |
          |[[binding]]
          |symbol = "flix_counter_view"
          |result = "borrowed-handle"
          |type = "CounterRef"
          |borrowed-from = "counter"
          |
          |[[binding]]
          |symbol = "flix_counter_inc"
          |effect = "IO"
          |
          |[[binding]]
          |symbol = "flix_counter_clone"
          |result = "owned-handle"
          |type = "Counter"
          |retain = "counter"
          |effect = "IO"
          |
          |[[binding]]
          |symbol = "flix_counter_open"
          |result = "status-owned-handle"
          |type = "Counter"
          |out = "out_counter"
          |ok = "0"
          |effect = "IO"
          |
          |[[binding]]
          |symbol = "flix_counter_label"
          |result = "borrowed-string"
          |borrowed-from = "counter"
          |
          |[[binding]]
          |symbol = "flix_counter_dispose"
          |effect = "IO"
          |destroy = "counter"
          |""".stripMargin)

      val generated = NativeBindingsTool.run(NativeBindingsTool.Config(
        header = headerFile,
        outDir = workDir.resolve("gen"),
        rootModule = "Native",
        spec = Some(specFile),
      )) match {
        case ca.uwaterloo.flix.util.Result.Ok(value) => value
        case ca.uwaterloo.flix.util.Result.Err(msg) => fail(msg)
      }

      val flixSource = Files.readString(generated.flixFile, StandardCharsets.UTF_8)
      assert(flixSource.contains("pub mod Native"))
      assert(flixSource.contains("""extern native(symbol = "flix_native_mul2")"""))
      assert(flixSource.contains("pub def flixNativeMul2(x: Int32): Int32"))
      assert(flixSource.contains("pub def flixNativeAddHalf(x: Float64): Float64"))
      assert(flixSource.contains("""extern native(symbol = "flix_bind_native_shim_flix_native_strlen_plus")"""))
      assert(flixSource.contains("pub def flixNativeStrlenPlus(s: String, bonus: Int32): Int32"))
      assert(flixSource.contains("""extern native(symbol = "flix_bind_native_shim_flix_native_sum_bytes")"""))
      assert(flixSource.contains("pub def flixNativeSumBytes(bytes: Array[Int8, Static]): Int64"))
      assert(flixSource.contains("""extern native(symbol = "flix_bind_native_shim_flix_native_apply_twice")"""))
      assert(flixSource.contains("pub def flixNativeApplyTwice(x: Int32): Int32"))
      assert(flixSource.contains("""extern native(symbol = "flix_bind_native_shim_flix_native_borrowed_greeting")"""))
      assert(flixSource.contains("pub def flixNativeBorrowedGreeting(): String"))
      assert(flixSource.contains("""extern native(symbol = "flix_bind_native_shim_flix_native_owned_greeting")"""))
      assert(flixSource.contains("pub def flixNativeOwnedGreeting(): String"))
      assert(flixSource.contains("""extern native(symbol = "flix_bind_native_shim_flix_native_borrowed_bytes")"""))
      assert(flixSource.contains("pub def flixNativeBorrowedBytes(): Array[Int8, Static]"))
      assert(flixSource.contains("""extern native(symbol = "flix_bind_native_shim_flix_native_owned_bytes")"""))
      assert(flixSource.contains("pub def flixNativeOwnedBytes(): Array[Int8, Static]"))
      assert(flixSource.contains("pub enum Counter(Int64)"))
      assert(flixSource.contains("pub enum CounterRef(Int64)"))
      assert(flixSource.contains("pub def borrowCounter(handle: Counter): CounterRef"))
      assert(flixSource.contains("""extern native(symbol = "flix_bind_native_shim_flix_counter_create")"""))
      assert(flixSource.contains("pub def flixCounterCreate(seed: Int32): Option[Counter] \\ IO"))
      assert(flixSource.contains("""extern native(symbol = "flix_bind_native_shim_flix_counter_global")"""))
      assert(flixSource.contains("pub def flixCounterGlobal(): Option[CounterRef]"))
      assert(flixSource.contains("""extern native(symbol = "flix_bind_native_shim_flix_counter_view")"""))
      assert(flixSource.contains("pub def flixCounterView(counter: Counter): Option[CounterRef]"))
      assert(flixSource.contains("pub def flixCounterInc(counter: Counter, delta: Int32): Unit \\ IO"))
      assert(flixSource.contains("pub def flixCounterClone(counter: CounterRef): Option[Counter] \\ IO"))
      assert(flixSource.contains("pub def retainCounter(handle: Counter): Option[Counter] \\ IO = flixCounterClone(match handle { case Counter.Counter(id) => CounterRef.CounterRef(id) })"))
      assert(flixSource.contains("""extern native(symbol = "flix_bind_native_shim_flix_counter_open")"""))
      assert(flixSource.contains("pub def flixCounterOpen(seed: Int32): Result[Int32, Counter] \\ IO"))
      assert(flixSource.contains("pub def flixCounterLabel(counter: CounterRef): String"))
      assert(flixSource.contains("pub def flixCounterLabelOwned(counter: Counter): String = flixCounterLabel(match counter { case Counter.Counter(id) => CounterRef.CounterRef(id) })"))
      assert(flixSource.contains("pub def flixCounterValue(counter: CounterRef): Int32"))
      assert(flixSource.contains("pub def flixCounterDispose(counter: Counter): Unit \\ IO"))
      assert(flixSource.contains("pub def closeCounter(handle: Counter): Unit \\ IO = flixCounterDispose(handle)"))
      assert(!flixSource.contains("""extern native(symbol = "flix_native_echo")"""))
      assert(!flixSource.contains("pub def flixNativeEcho"))
      assert(generated.skipped.exists(_.symbol == "flix_native_echo"))
      assert(generated.shimFile.nonEmpty)
      assert(generated.shimHeaderFile.nonEmpty)
      val shimFile = generated.shimFile.get
      val shimLibDir = workDir.resolve("shim-lib").normalize()
      Files.createDirectories(shimLibDir)
      buildNativeStaticLibrary(
        sourceFile = shimFile,
        includeDirs = List(generated.shimHeaderFile.get.getParent),
        libraryBaseName = "nativeffi_bindings",
        dir = shimLibDir,
        zigCmd = zigCmd,
      )

      val projectDir = workDir.resolve("project").normalize()
      Files.createDirectories(projectDir)
      Bootstrap.init(projectDir)(System.out).unsafeGet

      FileOps.writeString(projectDir.resolve("flix.toml").normalize(),
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
           |[target.native]
           |emit = ["exe"]
           |link-libs = ["nativeffi_bindings", "nativeffi_smoke"]
           |link-search = [${tomlString(shimLibDir.toString)}, ${tomlString(libDir.toString)}]
           |""".stripMargin)

      Files.copy(generated.flixFile, projectDir.resolve("src/Native.flix"), StandardCopyOption.REPLACE_EXISTING)
      FileOps.writeString(projectDir.resolve("src/Main.flix").normalize(),
        """
          |mod Api {
          |    @Export
          |    pub def bump(x: Int32): Int32 = x + 1
          |}
          |
          |pub def main(): Unit \ IO =
          |    let bytes = Array#{1i8, 2i8, 3i8, 4i8} @ Static;
          |    let borrowed = Native.flixNativeBorrowedBytes();
          |    let owned = Native.flixNativeOwnedBytes();
          |    let created = Native.flixCounterCreate(10i32);
          |    let missing = Native.flixCounterCreate(-1i32);
          |    let global = Native.flixCounterGlobal();
          |    let callbackValue = Native.flixNativeApplyTwice(40i32);
          |    match created {
          |        case Some(counter) =>
          |            let _ = Native.flixCounterInc(counter, 5);
          |            let counterView = Native.flixCounterView(counter);
          |            let retained = Native.retainCounter(counter);
          |            let opened = Native.flixCounterOpen(20i32);
          |            let openMissing = Native.flixCounterOpen(-1i32);
          |            let counterLabel = Native.flixCounterLabelOwned(counter);
          |            match (counterView, retained, missing, opened, openMissing) {
          |                case (Some(counterRef), Some(retainedCounter), None, Ok(openedCounter), Err(openErr)) =>
          |                    let counterValue = Native.flixCounterValue(counterRef);
          |                    let retainedValue = Native.flixCounterValue(Native.borrowCounter(retainedCounter));
          |                    let openedValue = Native.flixCounterValue(Native.borrowCounter(openedCounter));
          |                    match global {
          |                        case Some(globalCounter) =>
          |                            let globalValue = Native.flixCounterValue(globalCounter);
          |                            if (Native.flixNativeMul2(21) == 42 and
          |                                Native.flixNativeAddHalf(1.5) == 2.0 and
          |                                Native.flixNativeStrlenPlus("hello", 7) == 12 and
          |                                Native.flixNativeSumBytes(bytes) == 10i64 and
          |                                Native.flixNativeBorrowedGreeting() == "borrowed hello" and
          |                                Native.flixNativeOwnedGreeting() == "owned hello" and
          |                                Array.length(borrowed) == 3 and
          |                                Array.get(0, borrowed) == 5i8 and
          |                                Array.get(1, borrowed) == 6i8 and
          |                                Array.get(2, borrowed) == 7i8 and
          |                                Array.length(owned) == 4 and
          |                                Array.get(0, owned) == 9i8 and
          |                                Array.get(1, owned) == 8i8 and
          |                                Array.get(2, owned) == 7i8 and
          |                                Array.get(3, owned) == 6i8 and
          |                                callbackValue == 42 and
          |                                counterValue == 15 and
          |                                retainedValue == 15 and
          |                                openedValue == 120 and
          |                                openErr == -7i32 and
          |                                counterLabel == "counter" and
          |                                globalValue == 77) {
          |                                let _ = Native.closeCounter(openedCounter);
          |                                let _ = Native.closeCounter(retainedCounter);
          |                                let _ = Native.closeCounter(counter);
          |                                println("bind-native: ok")
          |                            } else
          |                                bug!(String.concat("unexpected bind native result: counter=", Int32.toString(counterValue)))
          |                        case None =>
          |                            bug!("expected borrowed opaque counter handle")
          |                    }
          |                case (_, _, Some(_), _, _) =>
          |                    bug!("expected null opaque counter handle")
          |                case _ =>
          |                    bug!("expected borrowed/retained/opened opaque counter handles")
          |            }
          |        case None =>
          |            bug!("expected opaque counter handle")
          |    }
          |""".stripMargin)

      val bootstrap = Bootstrap.bootstrap(projectDir, None)(Formatter.getDefault, System.out).unsafeGet
      val flix = PkgTestUtils.mkFlix
      flix.setOptions(flix.options.copy(target = CompilationTarget.LlvmNative, stdlibProfile = StdlibProfile.Portable))
      bootstrap.build(flix).unsafeGet

      val exePath = LlvmNativeDriver.executablePath(Bootstrap.getBuildTargetDirectory(projectDir, CompilationTarget.LlvmNative), bootstrap.artifactName)
      val (exit, output) = exec(List(exePath.toString), projectDir)
      assert(exit == 0, s"expected generated native binding smoke executable to succeed, got exit $exit:\n$output")
      assert(output.contains("bind-native: ok"), s"unexpected generated native binding smoke output:\n$output")
    } finally {
      deleteRecursive(workDir)
    }
  }

  private def buildNativeSmokeLibrary(dir: Path, zigCmd: List[String]): Unit = {
    val cFile = dir.resolve("nativeffi_smoke.c").normalize()

    FileOps.writeString(cFile,
      """
        |#include <stdint.h>
        |#include <stddef.h>
        |#include <stdlib.h>
        |#include <string.h>
        |
        |typedef struct flix_counter {
        |    int32_t value;
        |} flix_counter_t;
        |
        |typedef int32_t (*flix_int_cb_t)(int32_t);
        |
        |static flix_counter_t FLIX_GLOBAL_COUNTER = {77};
        |
        |int32_t flix_native_mul2(int32_t x) {
        |    return x * 2;
        |}
        |
        |double flix_native_add_half(double x) {
        |    return x + 0.5;
        |}
        |
        |int32_t flix_native_strlen_plus(const char* s, int32_t bonus) {
        |    size_t n = 0;
        |    while (s[n] != '\0') {
        |        n++;
        |    }
        |    return (int32_t)n + bonus;
        |}
        |
        |int64_t flix_native_sum_bytes(const uint8_t* bytes, int64_t len) {
        |    int64_t total = 0;
        |    for (int64_t i = 0; i < len; i++) {
        |        total += bytes[i];
        |    }
        |    return total;
        |}
        |
        |int32_t flix_native_apply_twice(int32_t x, flix_int_cb_t cb) {
        |    return cb(cb(x));
        |}
        |
        |const char* flix_native_borrowed_greeting(void) {
        |    return "borrowed hello";
        |}
        |
        |char* flix_native_owned_greeting(void) {
        |    const char* src = "owned hello";
        |    size_t len = strlen(src);
        |    char* out = (char*)malloc(len + 1);
        |    memcpy(out, src, len + 1);
        |    return out;
        |}
        |
        |const uint8_t* flix_native_borrowed_bytes(size_t* out_len) {
        |    static const uint8_t data[] = {5, 6, 7};
        |    *out_len = 3;
        |    return data;
        |}
        |
        |uint8_t* flix_native_owned_bytes(size_t* out_len) {
        |    uint8_t* out = (uint8_t*)malloc(4);
        |    out[0] = 9;
        |    out[1] = 8;
        |    out[2] = 7;
        |    out[3] = 6;
        |    *out_len = 4;
        |    return out;
        |}
        |
        |flix_counter_t* flix_counter_create(int32_t seed) {
        |    if (seed < 0) {
        |        return NULL;
        |    }
        |    flix_counter_t* counter = (flix_counter_t*)malloc(sizeof(flix_counter_t));
        |    counter->value = seed;
        |    return counter;
        |}
        |
        |const flix_counter_t* flix_counter_global(void) {
        |    return &FLIX_GLOBAL_COUNTER;
        |}
        |
        |const flix_counter_t* flix_counter_view(flix_counter_t* counter) {
        |    return counter;
        |}
        |
        |void flix_counter_inc(flix_counter_t* counter, int32_t delta) {
        |    counter->value += delta;
        |}
        |
        |flix_counter_t* flix_counter_clone(const flix_counter_t* counter) {
        |    flix_counter_t* cloned = (flix_counter_t*)malloc(sizeof(flix_counter_t));
        |    cloned->value = counter->value;
        |    return cloned;
        |}
        |
        |int32_t flix_counter_open(int32_t seed, flix_counter_t** out_counter) {
        |    if (seed < 0) {
        |        *out_counter = NULL;
        |        return -7;
        |    }
        |    flix_counter_t* counter = (flix_counter_t*)malloc(sizeof(flix_counter_t));
        |    counter->value = seed + 100;
        |    *out_counter = counter;
        |    return 0;
        |}
        |
        |const char* flix_counter_label(const flix_counter_t* counter) {
        |    (void)counter;
        |    return "counter";
        |}
        |
        |int32_t flix_counter_value(const flix_counter_t* counter) {
        |    return counter->value;
        |}
        |
        |void flix_counter_dispose(flix_counter_t* counter) {
        |    free(counter);
        |}
        |""".stripMargin)

    buildNativeStaticLibrary(cFile, Nil, "nativeffi_smoke", dir, zigCmd)
  }

  private def buildNativeStaticLibrary(sourceFile: Path, includeDirs: List[Path], libraryBaseName: String, dir: Path, zigCmd: List[String]): Unit = {
    val objFile = dir.resolve(s"${libraryBaseName}.o").normalize()
    val libFile = dir.resolve(s"lib${libraryBaseName}.a").normalize()

    val includeFlags = includeDirs.flatMap(p => List("-I", p.toString))
    val compileCmd = zigCmd ::: List("cc", "-fno-sanitize=undefined") ::: includeFlags ::: List("-c", sourceFile.toString, "-o", objFile.toString)
    val (compileExit, compileOutput) = exec(compileCmd, dir)
    if (compileExit != 0) {
      fail(s"native binding smoke C compile failed with exit $compileExit:\n${compileCmd.mkString(" ")}\n\n$compileOutput")
    }

    val archiveCmd = zigCmd ::: List("ar", "rcs", libFile.toString, objFile.toString)
    val (archiveExit, archiveOutput) = exec(archiveCmd, dir)
    if (archiveExit != 0) {
      fail(s"native binding smoke archive failed with exit $archiveExit:\n${archiveCmd.mkString(" ")}\n\n$archiveOutput")
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

  private def deleteRecursive(path: Path): Unit = {
    if (!Files.exists(path)) return
    val stream = Files.walk(path)
    try {
      import scala.jdk.CollectionConverters.*
      stream.iterator().asScala.toList.sortBy(_.getNameCount).reverse.foreach(Files.deleteIfExists)
    } finally {
      stream.close()
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
