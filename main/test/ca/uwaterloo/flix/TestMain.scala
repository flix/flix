/*
 * Copyright 2015-2016 Magnus Madsen
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

import ca.uwaterloo.flix.util.{CompilationTarget, EmitKind, LibLevel, Options, RunnerKind, StdlibProfile}
import org.scalatest.funsuite.AnyFunSuite

class TestMain extends AnyFunSuite {

  test("init") {
    val args = Array("init")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.Init)
  }

  test("build") {
    val args = Array("build")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.Build)
  }

  test("build-jar") {
    val args = Array("build-jar")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.BuildJar)
  }

  test("build-pkg") {
    val args = Array("build-pkg")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.BuildPkg)
  }

  test("package") {
    val args = Array("package")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.BuildPkg)
  }

  test("release") {
    val args = Array("release")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.Release)
  }

  test("outdated") {
    val args = Array("outdated")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.Outdated)
  }

  test("doc") {
    val args = Array("doc")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.Doc)
  }

  test("doctor") {
    val args = Array("doctor")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.Doctor)
  }

  test("bind wasm-effects") {
    val args = Array("bind", "wasm-effects", "--wit", "foo", "--world", "demo", "--out", "bar")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.BindWasmEffects)
    assert(opts.bindWit.contains(java.nio.file.Paths.get("foo")))
    assert(opts.bindWorld.contains("demo"))
    assert(opts.bindOut.contains(java.nio.file.Paths.get("bar")))
  }

  test("bind native") {
    val args = Array("bind", "native", "--header", "foo.h", "--spec", "foo.bind.toml", "--out", "bar", "--native-module", "Native", "--include", "inc", "--define", "FEATURE=1", "--cflag", "-DMORE=1")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.BindNative)
    assert(opts.bindHeader.contains(java.nio.file.Paths.get("foo.h")))
    assert(opts.bindSpec.contains(java.nio.file.Paths.get("foo.bind.toml")))
    assert(opts.bindOut.contains(java.nio.file.Paths.get("bar")))
    assert(opts.bindNativeModule == "Native")
    assert(opts.bindIncludePaths == List(java.nio.file.Paths.get("inc")))
    assert(opts.bindDefines == List("FEATURE=1"))
    assert(opts.bindCFlags == List("-DMORE=1"))
  }

  test("format") {
    val args = Array("format")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.Format)
  }

  test("run") {
    val args = Array("run")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.Run)
  }

  test("test") {
    val args = Array("test")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.Test)
  }

  test("repl") {
    val args = Array("repl")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.Repl)
  }

  test("check") {
    val args = Array("check")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.Check)
  }

  test("check with files") {
    val args = Array("check", "foo.flix", "bar.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.Check)
    assert(opts.files.length == 2)
  }

  test("test with files") {
    val args = Array("test", "foo.flix", "bar.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.Test)
    assert(opts.files.length == 2)
  }

  test("doc with files") {
    val args = Array("doc", "foo.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.Doc)
    assert(opts.files.length == 1)
  }

  test("run -- arg1 arg2") {
    val args = Array("run", "--", "arg1", "arg2")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.Run)
    assert(opts.args == Seq("arg1", "arg2"))
  }

  test("--json") {
    val args = Array("--json")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.json)
  }

  test("--no-install") {
    val args = Array("--no-install")
    val opts = Main.parseCmdOpts(args).get
    assert(!opts.installDeps)
  }

  test("--listen") {
    val args = Array("--listen", "8080", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.listen.nonEmpty)
  }

  test("--threads") {
    val args = Array("--threads", "42", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.threads.contains(42))
  }

  test("--yes") {
    val args = Array("--yes")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.assumeYes)
  }

  test("--Xbenchmark-code-size") {
    val args = Array("--Xbenchmark-code-size", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xbenchmarkCodeSize)
  }

  test("--Xbenchmark-phases") {
    val args = Array("--Xbenchmark-phases", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xbenchmarkPhases)
  }

  test("--Xbenchmark-frontend") {
    val args = Array("--Xbenchmark-frontend", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xbenchmarkFrontend)
  }

  test("--Xbenchmark-throughput") {
    val args = Array("--Xbenchmark-throughput", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xbenchmarkThroughput)
  }

  test("--Xlib nix") {
    val args = Array("--Xlib", "nix", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xlib == LibLevel.Nix)
  }

  test("--Xlib min") {
    val args = Array("--Xlib", "min", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xlib == LibLevel.Min)
  }

  test("--Xlib all") {
    val args = Array("--Xlib", "all", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xlib == LibLevel.All)
  }

  test("--Xno-deprecated") {
    val args = Array("--Xno-deprecated")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xnodeprecated)
  }

  test("--Xsummary") {
    val args = Array("--Xsummary")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xsummary)
  }

  test("--target native") {
    val args = Array("--target", "native", "build")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.targets == List(CompilationTarget.LlvmNative))
  }

  test("--native") {
    val args = Array("--native", "run")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.targets == List(CompilationTarget.LlvmNative))
  }

  test("--wasm") {
    val args = Array("--wasm", "build")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.targets == List(CompilationTarget.LlvmWasm))
  }

  test("--emit jar") {
    val args = Array("build", "--emit", "jar")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.emits == List(EmitKind.Jar))
  }

  test("--emit component --emit js") {
    val args = Array("build", "--emit", "component", "--emit", "js")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.emits == List(EmitKind.Component, EmitKind.Js))
  }

  test("--emit staticlib --emit sharedlib") {
    val args = Array("build", "--emit", "staticlib", "--emit", "sharedlib")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.emits == List(EmitKind.StaticLib, EmitKind.SharedLib))
  }

  test("--runner node") {
    val args = Array("run", "--runner", "node")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.runner.contains(RunnerKind.Node))
  }

  test("--runner wasmtime") {
    val args = Array("run", "--runner", "wasmtime")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.runner.contains(RunnerKind.Wasmtime))
  }

  test("--runner browser") {
    val args = Array("run", "--runner", "browser")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.runner.contains(RunnerKind.Browser))
  }

  test("llvm-native build requires zig") {
    val cmdOpts = Main.CmdOpts(command = Main.Command.Build, targets = List(CompilationTarget.LlvmNative))
    val options = Options.Default.copy(target = CompilationTarget.LlvmNative, stdlibProfile = StdlibProfile.Portable)

    val result = Main.validateCommandPreflight(cmdOpts, options, hasCmd = _ => false, hasUsableZig = false)

    assert(result.nonEmpty)
    assert(result.get.contains("zig"))
  }

  test("llvm-wasm build requires zig wasm-tools and jco") {
    val cmdOpts = Main.CmdOpts(command = Main.Command.Build, targets = List(CompilationTarget.LlvmWasm))
    val options = Options.Default.copy(target = CompilationTarget.LlvmWasm, stdlibProfile = StdlibProfile.Portable)

    val result = Main.validateCommandPreflight(cmdOpts, options, hasCmd = _ => false, hasUsableZig = false)

    assert(result.nonEmpty)
    assert(result.get.contains("zig"))
    assert(result.get.contains("wasm-tools"))
    assert(result.get.contains("jco"))
  }

  test("llvm-wasm run requires node") {
    val cmdOpts = Main.CmdOpts(command = Main.Command.Run, targets = List(CompilationTarget.LlvmWasm))
    val options = Options.Default.copy(target = CompilationTarget.LlvmWasm, stdlibProfile = StdlibProfile.Portable)

    val result = Main.validateCommandPreflight(cmdOpts, options, hasCmd = {
      case cmd if ca.uwaterloo.flix.util.ZigToolchain.probeCommands.contains(cmd) => true
      case List("wasm-tools", "--version") => true
      case List("jco", "--version") => true
      case _ => false
    }, hasUsableZig = true)

    assert(result.nonEmpty)
    assert(result.get.contains("node"))
  }

  test("llvm target rejects jvm stdlib profile") {
    val cmdOpts = Main.CmdOpts(command = Main.Command.Build, targets = List(CompilationTarget.LlvmNative), xstdlibProfile = StdlibProfile.Jvm, xstdlibProfileExplicit = true)
    val options = Options.Default.copy(target = CompilationTarget.LlvmNative, stdlibProfile = StdlibProfile.Jvm)

    val result = Main.validateCommandPreflight(cmdOpts, options, hasCmd = _ => true, hasUsableZig = true)

    assert(result.nonEmpty)
    assert(result.get.contains("portable"))
  }

  test("llvm-native test requires zig") {
    val cmdOpts = Main.CmdOpts(command = Main.Command.Test, targets = List(CompilationTarget.LlvmNative))
    val options = Options.Default.copy(target = CompilationTarget.LlvmNative, stdlibProfile = StdlibProfile.Portable)

    val result = Main.validateCommandPreflight(cmdOpts, options, runner = Some(RunnerKind.Native), hasCmd = _ => false, hasUsableZig = false)

    assert(result.nonEmpty)
    assert(result.get.contains("zig"))
  }

  test("llvm-wasm test requires node by default") {
    val cmdOpts = Main.CmdOpts(command = Main.Command.Test, targets = List(CompilationTarget.LlvmWasm))
    val options = Options.Default.copy(target = CompilationTarget.LlvmWasm, stdlibProfile = StdlibProfile.Portable)

    val result = Main.validateCommandPreflight(cmdOpts, options, runner = Some(RunnerKind.Node), hasCmd = {
      case cmd if ca.uwaterloo.flix.util.ZigToolchain.probeCommands.contains(cmd) => true
      case List("wasm-tools", "--version") => true
      case List("jco", "--version") => true
      case _ => false
    }, hasUsableZig = true)

    assert(result.nonEmpty)
    assert(result.get.contains("node"))
  }

  test("llvm-wasm test requires cargo when using wasmtime") {
    val cmdOpts = Main.CmdOpts(command = Main.Command.Test, targets = List(CompilationTarget.LlvmWasm))
    val options = Options.Default.copy(target = CompilationTarget.LlvmWasm, stdlibProfile = StdlibProfile.Portable)

    val result = Main.validateCommandPreflight(cmdOpts, options, runner = Some(RunnerKind.Wasmtime), hasCmd = {
      case cmd if ca.uwaterloo.flix.util.ZigToolchain.probeCommands.contains(cmd) => true
      case List("wasm-tools", "--version") => true
      case List("jco", "--version") => true
      case _ => false
    }, hasUsableZig = true)

    assert(result.nonEmpty)
    assert(result.get.contains("cargo +stable"))
  }

  test("llvm-native defaults to portable stdlib profile") {
    val cmdOpts = Main.CmdOpts(targets = List(CompilationTarget.LlvmNative))

    assert(Main.effectiveStdlibProfile(cmdOpts, CompilationTarget.LlvmNative) == StdlibProfile.Portable)
  }

  test("llvm-wasm defaults to portable stdlib profile") {
    val cmdOpts = Main.CmdOpts(targets = List(CompilationTarget.LlvmWasm))

    assert(Main.effectiveStdlibProfile(cmdOpts, CompilationTarget.LlvmWasm) == StdlibProfile.Portable)
  }

  test("explicit stdlib profile overrides target default") {
    val cmdOpts = Main.CmdOpts(targets = List(CompilationTarget.LlvmWasm), xstdlibProfile = StdlibProfile.Jvm, xstdlibProfileExplicit = true)

    assert(Main.effectiveStdlibProfile(cmdOpts, CompilationTarget.LlvmWasm) == StdlibProfile.Jvm)
  }

}
