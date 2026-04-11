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
import ca.uwaterloo.flix.language.phase.llvm.{LlvmWasmDriver, LlvmWasmEffectManifestWriter, LlvmWasmExportWriter}
import ca.uwaterloo.flix.tools.WasmEffectBindingsTool
import ca.uwaterloo.flix.util.{CompilationTarget, Options, StdlibProfile}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

class WasmEffectBindingsToolSuite extends AnyFunSuite {

  private val ArtifactName = "wit-effect-bindings"

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmWasm,
      incremental = false,
      outputJvm = false,
    )

  private val WitDir: Path =
    Paths.get("main/test/wit/wasm-effects/demo")

  private val RecursiveWitDir: Path =
    Paths.get("main/test/wit/wasm-effects/recursive")

  private val WorldWitDir: Path =
    Paths.get("main/test/wit/wasm-effects/world")

  private val ResourceWitDir: Path =
    Paths.get("main/test/wit/wasm-effects/resources")

  private val WorldResourceWitDir: Path =
    Paths.get("main/test/wit/wasm-effects/world-resources")

  private val NestedResourceWitDir: Path =
    Paths.get("main/test/wit/wasm-effects/nested-resources")

  private case class CompiledArtifacts(outDir: Path,
                                       componentWasm: Path,
                                       componentJs: Path,
                                       runnerModule: Path,
                                       exportsManifest: Path,
                                       effectsManifest: Path)

  test("bind-wasm-effects-generates-flix-and-js-host-runs") {
    assume(hasCargoStable, "cargo +stable not available (skipping wasm effect bindings generator test)")
    assume(hasZig, "zig not found on PATH (skipping wasm effect bindings generator test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping wasm effect bindings generator test)")
    assume(hasJco, "jco not found on PATH (skipping wasm effect bindings generator test)")
    assume(hasNode, "node not found on PATH (skipping wasm effect bindings generator test)")

    val workDir = Files.createTempDirectory("flix-wasm-effect-bindings-tool-")
    val genDir = workDir.resolve("gen")

    try {
      val generated = requireGenerated(WitDir, genDir)

      val flixSource = Files.readString(generated.flixFile, StandardCharsets.UTF_8)
      assert(flixSource.contains("pub mod Wit"))
      assert(flixSource.contains("pub eff HostDemoClock"))
      assert(flixSource.contains("def echo(s: String): String"))

      val bindingsJson = Files.readString(generated.bindingsFile, StandardCharsets.UTF_8)
      assert(bindingsJson.contains("host:demo/clock@0.1.0"))
      assert(bindingsJson.contains("Wit.HostDemoClock.echo"))
      assert(Files.exists(generated.jsFile))
      assert(Files.exists(generated.dtsFile))
      assert(Files.exists(generated.jsBrowserHostStubFile))
      assert(Files.exists(generated.jsPackageFile))
      assert(Files.exists(generated.rustFile))
      assert(Files.exists(generated.rustLibFile))
      assert(Files.exists(generated.rustHostStubFile))
      assert(Files.exists(generated.rustCargoTomlFile))
      assert(Files.exists(generated.readmeFile))

      val jsStub = Files.readString(generated.jsBrowserHostStubFile, StandardCharsets.UTF_8)
      assert(jsStub.contains("export const browserImplementations"))
      assert(jsStub.contains("hostDemoClock"))
      assert(jsStub.contains("echo: async (arg0)"))
      assert(jsStub.contains("makeBrowserUnknownHandler"))

      val rustStub = Files.readString(generated.rustHostStubFile, StandardCharsets.UTF_8)
      assert(rustStub.contains("pub struct HostDemoClockHost;"))
      assert(rustStub.contains("impl HostDemoClock for HostDemoClockHost"))
      assert(rustStub.contains("""todo!("TODO: implement host:demo/clock@0.1.0#echo for the Wasmtime host")"""))

      val readme = Files.readString(generated.readmeFile, StandardCharsets.UTF_8)
      assert(readme.contains("js/browser-host.stub.mjs"))
      assert(readme.contains("rust/examples/host_stub.rs"))

      val userSource = workDir.resolve("Api.flix")
      Files.writeString(userSource,
        """
          |mod Api {
          |    @Export
          |    pub def callEcho(s: String): String \ Wit.HostDemoClock =
          |        Wit.HostDemoClock.echo(s)
          |}
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val compiled = compileArtifacts(List(generated.flixFile, userSource))
      val nodeFile = Files.createTempFile("flix-wit-effect-bindings-js-", ".mjs")
      try {
        Files.writeString(nodeFile,
          s"""
             |import * as fs from "node:fs/promises";
             |import { runtime } from ${jsStringLiteral(compiled.componentJs.toUri.toString)};
             |import { FlixRunner } from ${jsStringLiteral(compiled.runnerModule.toUri.toString)};
             |import { makeUnknownHandler } from ${jsStringLiteral(generated.jsFile.toUri.toString)};
             |
             |function assert(cond, msg) {
             |  if (!cond) throw new Error(msg);
             |}
             |
             |const disposeSym = Symbol.dispose ?? Symbol.for("dispose");
             |function maybeDispose(x) {
             |  try {
             |    const fn = x?.[disposeSym];
             |    if (typeof fn === "function") fn.call(x);
             |  } catch {}
             |}
             |
             |const exportsManifest = JSON.parse(await fs.readFile(${jsStringLiteral(compiled.exportsManifest.toString)}, "utf8"));
             |const def = exportsManifest.defs.find((d) => d.symbol === "Api.callEcho");
             |assert(def, "missing Api.callEcho export");
             |
             |const unknown = await makeUnknownHandler(
             |  ${jsStringLiteral(compiled.effectsManifest.toString)},
             |  {
             |    hostDemoClock: {
             |      echo: async (s) => `${"$"}{s}!`,
             |    },
             |  }
             |);
             |
             |const runner = new FlixRunner(runtime, { handlers: { unknown } });
             |const ctx = runtime.newCtx();
             |const arg = runtime.boxString(ctx, "hello");
             |try {
             |  const taskId = runtime.startTask(ctx, BigInt(def.defId), [arg]);
             |  const out = await runner.runTaskToCompletion(ctx, taskId);
             |  try {
             |    assert(out.tag === "ok", `unexpected task outcome: $${JSON.stringify(out)}`);
             |    const result = runtime.unboxString(ctx, out.val);
             |    assert(result === "hello!", `bad generated effect binding result: $${result}`);
             |    console.log("OK");
             |  } finally {
             |    maybeDispose(out.val);
             |  }
             |} finally {
             |  maybeDispose(arg);
             |  maybeDispose(ctx);
             |}
             |""".stripMargin,
          StandardCharsets.UTF_8
        )

        val (exit, output) = runCmd(List("node", nodeFile.toString))
        if (exit != 0) {
          fail(s"JS generated effect binding host failed with exit $exit:\n$output")
        }
        if (output.trim != "OK") {
          fail(s"Unexpected JS generated effect binding output:\n$output")
        }
      } finally {
        Files.deleteIfExists(nodeFile)
        deleteRecursive(compiled.outDir)
      }
    } finally {
      deleteRecursive(workDir)
    }
  }

  test("bind-wasm-effects-generates-recursive-js-and-rust-hosts") {
    assume(hasCargoStable, "cargo +stable not available (skipping wasm effect bindings generator test)")
    assume(hasZig, "zig not found on PATH (skipping wasm effect bindings generator test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping wasm effect bindings generator test)")
    assume(hasJco, "jco not found on PATH (skipping wasm effect bindings generator test)")
    assume(hasNode, "node not found on PATH (skipping wasm effect bindings generator test)")

    val workDir = Files.createTempDirectory("flix-wasm-effect-bindings-recursive-")
    val genDir = workDir.resolve("gen")

    try {
      val generated = requireGenerated(RecursiveWitDir, genDir)
      val flixSource = Files.readString(generated.flixFile, StandardCharsets.UTF_8)
      assert(flixSource.contains("pub eff HostDemoUsers"))
      assert(flixSource.contains("def pairUsers"))
      assert(flixSource.contains("def promote"))

      val jsBindings = Files.readString(generated.jsFile, StandardCharsets.UTF_8)
      assert(jsBindings.contains("export async function makeUnknownHandler"))
      assert(jsBindings.contains("hostDemoUsers"))

      val dts = Files.readString(generated.dtsFile, StandardCharsets.UTF_8)
      assert(dts.contains("export interface HostDemoUsers"))
      assert(dts.contains("pairUsers(users: Array<{ name: string; score: number }>)"))
      assert(dts.contains("""promote(user: { name: string; score: number } | null): Awaitable<{ tag: "ok"; val: { name: string; score: number } } | { tag: "err"; val: number }>"""))

      val rustBindings = Files.readString(generated.rustFile, StandardCharsets.UTF_8)
      assert(rustBindings.contains("pub trait HostDemoUsers"))
      assert(rustBindings.contains("pub struct HostDemoUsersUser"))
      assert(rustBindings.contains("pub struct WitEffectHandler"))
      assert(rustBindings.contains("fn pair_users"))

      val rustStub = Files.readString(generated.rustHostStubFile, StandardCharsets.UTF_8)
      assert(rustStub.contains("pub struct HostDemoUsersHost;"))
      assert(rustStub.contains("impl HostDemoUsers for HostDemoUsersHost"))
      assert(rustStub.contains("HostDemoUsersUser"))

      val userSource = workDir.resolve("Api.flix")
      Files.writeString(userSource,
        """
          |mod Api {
          |    @Export
          |    pub def callUsers(seed: Int32): String \ Wit.HostDemoUsers =
          |        let _ = seed;
          |        let promoted = match Wit.HostDemoUsers.promote(Some({name = "alice", score = 10})) {
          |            case Ok(user) => "${user#name}:${Int32.toString(user#score)}"
          |            case Err(code) => "err:${Int32.toString(code)}"
          |        };
          |
          |        let pairs = Wit.HostDemoUsers.pairUsers({name = "alice", score = 1} :: {name = "bob", score = 2} :: Nil);
          |        let pairText = pairs |> List.map(match (name, score) -> "${name}:${Int32.toString(score)}") |> String.intercalate(",");
          |        promoted + "|" + pairText
          |}
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val compiled = compileArtifacts(List(generated.flixFile, userSource))
      val nodeFile = Files.createTempFile("flix-wit-effect-bindings-recursive-js-", ".mjs")
      try {
        Files.writeString(nodeFile,
          s"""
             |import * as fs from "node:fs/promises";
             |import { runtime } from ${jsStringLiteral(compiled.componentJs.toUri.toString)};
             |import { FlixRunner } from ${jsStringLiteral(compiled.runnerModule.toUri.toString)};
             |import { makeUnknownHandler } from ${jsStringLiteral(generated.jsFile.toUri.toString)};
             |
             |function assert(cond, msg) {
             |  if (!cond) throw new Error(msg);
             |}
             |
             |const disposeSym = Symbol.dispose ?? Symbol.for("dispose");
             |function maybeDispose(x) {
             |  try {
             |    const fn = x?.[disposeSym];
             |    if (typeof fn === "function") fn.call(x);
             |  } catch {}
             |}
             |
             |const exportsManifest = JSON.parse(await fs.readFile(${jsStringLiteral(compiled.exportsManifest.toString)}, "utf8"));
             |const def = exportsManifest.defs.find((d) => d.symbol === "Api.callUsers");
             |assert(def, "missing Api.callUsers export");
             |
             |const unknown = await makeUnknownHandler(
             |  ${jsStringLiteral(compiled.effectsManifest.toString)},
             |  {
             |    hostDemoUsers: {
             |      pairUsers: async (users) => users.map((u) => [u.name.toUpperCase(), u.score + 10]),
             |      promote: async (user) =>
             |        user == null
             |          ? { tag: "err", val: 0 }
             |          : { tag: "ok", val: { name: `${"$"}{user.name}!`, score: user.score + 1 } },
             |    },
             |  }
             |);
             |
             |const runner = new FlixRunner(runtime, { handlers: { unknown } });
             |const ctx = runtime.newCtx();
             |const arg = runtime.boxI32(ctx, 0);
             |try {
             |  const taskId = runtime.startTask(ctx, BigInt(def.defId), [arg]);
             |  const out = await runner.runTaskToCompletion(ctx, taskId);
             |  try {
             |    const thrownMsg = out.tag === "thrown" ? runtime.unboxString(ctx, out.val) : null;
             |    const thrownSuffix = thrownMsg ? " (" + thrownMsg + ")" : "";
             |    assert(out.tag === "ok", "unexpected task outcome: " + JSON.stringify(out) + thrownSuffix);
             |    const result = runtime.unboxString(ctx, out.val);
             |    assert(result === "alice!:11|ALICE:11,BOB:12", `bad recursive binding result: $${result}`);
             |    console.log("OK");
             |  } finally {
             |    maybeDispose(out.val);
             |  }
             |} finally {
             |  maybeDispose(arg);
             |  maybeDispose(ctx);
             |}
             |""".stripMargin,
          StandardCharsets.UTF_8
        )

        val (jsExit, jsOutput) = runCmd(List("node", nodeFile.toString))
        if (jsExit != 0) {
          fail(s"JS recursive generated effect binding host failed with exit $jsExit:\n$jsOutput")
        }
        if (jsOutput.trim != "OK") {
          fail(s"Unexpected JS recursive generated effect binding output:\n$jsOutput")
        }
      } finally {
        Files.deleteIfExists(nodeFile)
      }

      val rustHostDir = Files.createTempDirectory("flix-wit-effect-bindings-recursive-rs-")
      try {
        Files.createDirectories(rustHostDir.resolve("src"))
        Files.writeString(rustHostDir.resolve("Cargo.toml"),
          s"""
             |[package]
             |name = "recursive_effect_bindings_host"
             |version = "0.1.0"
             |edition = "2021"
             |
             |[dependencies]
             |anyhow = "1"
             |serde = { version = "1", features = ["derive"] }
             |serde_json = "1"
             |wasmtime = { version = "38", features = ["component-model"] }
             |flix-wit-effect-bindings = { path = ${rustSdkDepLiteral(generated)} }
             |""".stripMargin,
          StandardCharsets.UTF_8
        )
        Files.writeString(rustHostDir.resolve("src").resolve("main.rs"),
          s"""
             |use anyhow::{Context, Result};
             |use flix_wit_effect_bindings::{
             |    bindings,
             |    effects::{AsyncResult, EffectManifest},
             |    runner::FlixRunner,
             |};
             |use serde::Deserialize;
             |use std::fs;
             |use wasmtime::{
             |    Config, Engine, Store,
             |    component::{Component, HasSelf, Linker},
             |};
             |
             |use bindings::flix::sys::sys::{Capability, Host as SysHost, LogLevel};
             |use flix_wit_effect_bindings::{HostDemoUsers, HostDemoUsersUser, WitEffectHandler};
             |
             |#[derive(Default)]
             |struct State;
             |
             |impl SysHost for State {
             |    fn log(&mut self, _level: LogLevel, _msg: String) {}
             |    fn time_now_ms(&mut self) -> i64 { 0 }
             |    fn random_bytes(&mut self, len: u32) -> Vec<u8> { (0..len).map(|i| i as u8).collect() }
             |    fn get_args(&mut self) -> Vec<String> { Vec::new() }
             |    fn has_capability(&mut self, _cap: Capability) -> bool { false }
             |}
             |
             |#[derive(Deserialize)]
             |struct ExportsManifest {
             |    defs: Vec<ExportedDef>,
             |}
             |
             |#[derive(Deserialize)]
             |#[serde(rename_all = "camelCase")]
             |struct ExportedDef {
             |    def_id: u64,
             |    symbol: String,
             |}
             |
             |struct HostUsers;
             |
             |impl HostDemoUsers for HostUsers {
             |    fn pair_users(&mut self, users: Vec<HostDemoUsersUser>) -> Result<AsyncResult<Vec<(String, i32)>>> {
             |        Ok(AsyncResult::Ready(
             |            users.into_iter().map(|u| (u.name.to_uppercase(), u.score + 10)).collect()
             |        ))
             |    }
             |
             |    fn promote(&mut self, user: Option<HostDemoUsersUser>) -> Result<AsyncResult<std::result::Result<HostDemoUsersUser, i32>>> {
             |        Ok(AsyncResult::Ready(match user {
             |            None => Err(0),
             |            Some(u) => Ok(HostDemoUsersUser { name: format!("{}!", u.name), score: u.score + 1 }),
             |        }))
             |    }
             |}
             |
             |fn main() -> Result<()> {
             |    let mut config = Config::new();
             |    config.wasm_component_model(true);
             |    let engine = Engine::new(&config)?;
             |
             |    let component = Component::from_file(&engine, ${cargoStringLiteral(compiled.componentWasm.toString)})?;
             |    let mut linker = Linker::<State>::new(&engine);
             |    bindings::flix::sys::sys::add_to_linker::<_, HasSelf<_>>(&mut linker, |s| s)?;
             |
             |    let mut store = Store::new(&engine, State::default());
             |    let flix = bindings::Flix::instantiate(&mut store, &component, &linker)?;
             |    let rt = flix.flix_runtime_runtime();
             |    let ctx = rt.call_new_ctx(&mut store)?;
             |
             |    let exports: ExportsManifest = serde_json::from_str(&fs::read_to_string(${cargoStringLiteral(compiled.exportsManifest.toString)})?)?;
             |    let main_def = exports.defs.into_iter().find(|d| d.symbol == "Api.callUsers").context("missing Api.callUsers export")?;
             |    let manifest = EffectManifest::from_path(std::path::Path::new(${cargoStringLiteral(compiled.effectsManifest.toString)}))?;
             |
             |    let runner = FlixRunner { budget: 100 };
             |    let mut handler = WitEffectHandler::new(manifest, HostUsers);
             |    let arg = rt.call_box_i32(&mut store, ctx, 0)?;
             |    let task_id = rt.call_start_task(&mut store, ctx, main_def.def_id, &[arg])?;
             |    let _ = arg.resource_drop(&mut store);
             |    let outcome = runner.run_task_to_completion(&mut store, rt, ctx, task_id, &mut handler)?;
             |    match outcome {
             |        bindings::exports::flix::runtime::runtime::TaskOutcome::Ok(v) => {
             |            let result = rt.call_unbox_string(&mut store, ctx, v)?;
             |            anyhow::ensure!(result == "alice!:11|ALICE:11,BOB:12", "bad recursive Rust binding result: {}", result);
             |            let _ = v.resource_drop(&mut store);
             |            println!("OK");
             |            Ok(())
             |        }
             |        other => anyhow::bail!("unexpected task outcome: {:?}", other),
             |    }
             |}
             |""".stripMargin,
          StandardCharsets.UTF_8
        )

        val (rsExit, rsOutput) = runCmd(List("cargo", "+stable", "run", "--quiet"), rustHostDir)
        if (rsExit != 0) {
          fail(s"Rust recursive generated effect binding host failed with exit $rsExit:\n$rsOutput")
        }
        if (rsOutput.trim != "OK") {
          fail(s"Unexpected Rust recursive generated effect binding output:\n$rsOutput")
        }
      } finally {
        deleteRecursive(rustHostDir)
        deleteRecursive(compiled.outDir)
      }
    } finally {
      deleteRecursive(workDir)
    }
  }

  test("bind-wasm-effects-generates-world-level-js-and-rust-hosts") {
    assume(hasCargoStable, "cargo +stable not available (skipping wasm effect bindings generator test)")
    assume(hasZig, "zig not found on PATH (skipping wasm effect bindings generator test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping wasm effect bindings generator test)")
    assume(hasJco, "jco not found on PATH (skipping wasm effect bindings generator test)")
    assume(hasNode, "node not found on PATH (skipping wasm effect bindings generator test)")

    val workDir = Files.createTempDirectory("flix-wasm-effect-bindings-world-")
    val genDir = workDir.resolve("gen")

    try {
      val generated = requireGenerated(WorldWitDir, genDir)
      val flixSource = Files.readString(generated.flixFile, StandardCharsets.UTF_8)
      assert(flixSource.contains("pub eff HostDemoDemoWorld"))
      assert(flixSource.contains("def greet(name: String): String"))
      assert(flixSource.contains("def lucky(seed: Int32): Result[Int32, String]"))

      val dts = Files.readString(generated.dtsFile, StandardCharsets.UTF_8)
      assert(dts.contains("export interface HostDemoDemoWorld"))
      assert(dts.contains("""greet(name: string): Awaitable<string>"""))
      assert(dts.contains("""lucky(seed: number): Awaitable<{ tag: "ok"; val: string } | { tag: "err"; val: number }>"""))

      val rustBindings = Files.readString(generated.rustFile, StandardCharsets.UTF_8)
      assert(rustBindings.contains("pub trait HostDemoDemoWorld"))
      assert(rustBindings.contains("fn greet"))
      assert(rustBindings.contains("fn lucky"))

      val userSource = workDir.resolve("Api.flix")
      Files.writeString(userSource,
        """
          |mod Api {
          |    @Export
          |    pub def callWorld(name: String, seed: Int32): String \ Wit.HostDemoDemoWorld =
          |        match Wit.HostDemoDemoWorld.lucky(seed) {
          |            case Ok(suffix) => Wit.HostDemoDemoWorld.greet(name) + " " + suffix
          |            case Err(code) => "err:" + Int32.toString(code)
          |        }
          |}
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val compiled = compileArtifacts(List(generated.flixFile, userSource))
      val nodeFile = Files.createTempFile("flix-wit-effect-bindings-world-js-", ".mjs")
      try {
        Files.writeString(nodeFile,
          s"""
             |import * as fs from "node:fs/promises";
             |import { runtime } from ${jsStringLiteral(compiled.componentJs.toUri.toString)};
             |import { FlixRunner } from ${jsStringLiteral(compiled.runnerModule.toUri.toString)};
             |import { makeUnknownHandler } from ${jsStringLiteral(generated.jsFile.toUri.toString)};
             |
             |function assert(cond, msg) {
             |  if (!cond) throw new Error(msg);
             |}
             |
             |const disposeSym = Symbol.dispose ?? Symbol.for("dispose");
             |function maybeDispose(x) {
             |  try {
             |    const fn = x?.[disposeSym];
             |    if (typeof fn === "function") fn.call(x);
             |  } catch {}
             |}
             |
             |const exportsManifest = JSON.parse(await fs.readFile(${jsStringLiteral(compiled.exportsManifest.toString)}, "utf8"));
             |const def = exportsManifest.defs.find((d) => d.symbol === "Api.callWorld");
             |assert(def, "missing Api.callWorld export");
             |
             |const unknown = await makeUnknownHandler(
             |  ${jsStringLiteral(compiled.effectsManifest.toString)},
             |  {
             |    hostDemoDemoWorld: {
             |      greet: async (name) => `hello ${"$"}{name}`,
             |      lucky: async (seed) => seed === 7 ? { tag: "ok", val: "winner" } : { tag: "err", val: seed },
             |    },
             |  }
             |);
             |
             |const runner = new FlixRunner(runtime, { handlers: { unknown } });
             |const ctx = runtime.newCtx();
             |const arg1 = runtime.boxString(ctx, "alice");
             |const arg2 = runtime.boxI32(ctx, 7);
             |try {
             |  const taskId = runtime.startTask(ctx, BigInt(def.defId), [arg1, arg2]);
             |  const out = await runner.runTaskToCompletion(ctx, taskId);
             |  try {
             |    assert(out.tag === "ok", `unexpected task outcome: $${JSON.stringify(out)}`);
             |    const result = runtime.unboxString(ctx, out.val);
             |    assert(result === "hello alice winner", `bad world binding result: $${result}`);
             |    console.log("OK");
             |  } finally {
             |    maybeDispose(out.val);
             |  }
             |} finally {
             |  maybeDispose(arg1);
             |  maybeDispose(arg2);
             |  maybeDispose(ctx);
             |}
             |""".stripMargin,
          StandardCharsets.UTF_8
        )

        val (jsExit, jsOutput) = runCmd(List("node", nodeFile.toString))
        if (jsExit != 0) {
          fail(s"JS world generated effect binding host failed with exit $jsExit:\n$jsOutput")
        }
        if (jsOutput.trim != "OK") {
          fail(s"Unexpected JS world generated effect binding output:\n$jsOutput")
        }
      } finally {
        Files.deleteIfExists(nodeFile)
      }

      val rustHostDir = Files.createTempDirectory("flix-wit-effect-bindings-world-rs-")
      try {
        Files.createDirectories(rustHostDir.resolve("src"))
        Files.writeString(rustHostDir.resolve("Cargo.toml"),
          s"""
             |[package]
             |name = "world_effect_bindings_host"
             |version = "0.1.0"
             |edition = "2021"
             |
             |[dependencies]
             |anyhow = "1"
             |serde = { version = "1", features = ["derive"] }
             |serde_json = "1"
             |wasmtime = { version = "38", features = ["component-model"] }
             |flix-wit-effect-bindings = { path = ${rustSdkDepLiteral(generated)} }
             |""".stripMargin,
          StandardCharsets.UTF_8
        )
        Files.writeString(rustHostDir.resolve("src").resolve("main.rs"),
          s"""
             |use anyhow::{Context, Result};
             |use flix_wit_effect_bindings::{
             |    bindings,
             |    effects::{AsyncResult, EffectManifest},
             |    runner::FlixRunner,
             |};
             |use serde::Deserialize;
             |use std::fs;
             |use wasmtime::{
             |    Config, Engine, Store,
             |    component::{Component, HasSelf, Linker},
             |};
             |
             |use bindings::flix::sys::sys::{Capability, Host as SysHost, LogLevel};
             |use flix_wit_effect_bindings::{HostDemoDemoWorld, WitEffectHandler};
             |
             |#[derive(Default)]
             |struct State;
             |
             |impl SysHost for State {
             |    fn log(&mut self, _level: LogLevel, _msg: String) {}
             |    fn time_now_ms(&mut self) -> i64 { 0 }
             |    fn random_bytes(&mut self, len: u32) -> Vec<u8> { (0..len).map(|i| i as u8).collect() }
             |    fn get_args(&mut self) -> Vec<String> { Vec::new() }
             |    fn has_capability(&mut self, _cap: Capability) -> bool { false }
             |}
             |
             |#[derive(Deserialize)]
             |struct ExportsManifest {
             |    defs: Vec<ExportedDef>,
             |}
             |
             |#[derive(Deserialize)]
             |#[serde(rename_all = "camelCase")]
             |struct ExportedDef {
             |    def_id: u64,
             |    symbol: String,
             |}
             |
             |struct HostWorld;
             |
             |impl HostDemoDemoWorld for HostWorld {
             |    fn greet(&mut self, name: String) -> Result<AsyncResult<String>> {
             |        Ok(AsyncResult::Ready(format!("hello {}", name)))
             |    }
             |
             |    fn lucky(&mut self, seed: i32) -> Result<AsyncResult<std::result::Result<String, i32>>> {
             |        Ok(AsyncResult::Ready(if seed == 7 { Ok("winner".to_string()) } else { Err(seed) }))
             |    }
             |}
             |
             |fn main() -> Result<()> {
             |    let mut config = Config::new();
             |    config.wasm_component_model(true);
             |    let engine = Engine::new(&config)?;
             |
             |    let component = Component::from_file(&engine, ${cargoStringLiteral(compiled.componentWasm.toString)})?;
             |    let mut linker = Linker::<State>::new(&engine);
             |    bindings::flix::sys::sys::add_to_linker::<_, HasSelf<_>>(&mut linker, |s| s)?;
             |
             |    let mut store = Store::new(&engine, State::default());
             |    let flix = bindings::Flix::instantiate(&mut store, &component, &linker)?;
             |    let rt = flix.flix_runtime_runtime();
             |    let ctx = rt.call_new_ctx(&mut store)?;
             |
             |    let exports: ExportsManifest = serde_json::from_str(&fs::read_to_string(${cargoStringLiteral(compiled.exportsManifest.toString)})?)?;
             |    let main_def = exports.defs.into_iter().find(|d| d.symbol == "Api.callWorld").context("missing Api.callWorld export")?;
             |    let manifest = EffectManifest::from_path(std::path::Path::new(${cargoStringLiteral(compiled.effectsManifest.toString)}))?;
             |
             |    let runner = FlixRunner { budget: 100 };
             |    let mut handler = WitEffectHandler::new(manifest, HostWorld);
             |    let arg1 = rt.call_box_string(&mut store, ctx, "alice")?;
             |    let arg2 = rt.call_box_i32(&mut store, ctx, 7)?;
             |    let task_id = rt.call_start_task(&mut store, ctx, main_def.def_id, &[arg1, arg2])?;
             |    let _ = arg1.resource_drop(&mut store);
             |    let _ = arg2.resource_drop(&mut store);
             |    let outcome = runner.run_task_to_completion(&mut store, rt, ctx, task_id, &mut handler)?;
             |    match outcome {
             |        bindings::exports::flix::runtime::runtime::TaskOutcome::Ok(v) => {
             |            let result = rt.call_unbox_string(&mut store, ctx, v)?;
             |            anyhow::ensure!(result == "hello alice winner", "bad world Rust binding result: {}", result);
             |            let _ = v.resource_drop(&mut store);
             |            println!("OK");
             |            Ok(())
             |        }
             |        other => anyhow::bail!("unexpected task outcome: {:?}", other),
             |    }
             |}
             |""".stripMargin,
          StandardCharsets.UTF_8
        )

        val (rsExit, rsOutput) = runCmd(List("cargo", "+stable", "run", "--quiet"), rustHostDir)
        if (rsExit != 0) {
          fail(s"Rust world generated effect binding host failed with exit $rsExit:\n$rsOutput")
        }
        if (rsOutput.trim != "OK") {
          fail(s"Unexpected Rust world generated effect binding output:\n$rsOutput")
        }
      } finally {
        deleteRecursive(rustHostDir)
        deleteRecursive(compiled.outDir)
      }
    } finally {
      deleteRecursive(workDir)
    }
  }

  test("bind-wasm-effects-generates-resource-js-and-rust-hosts") {
    assume(hasCargoStable, "cargo +stable not available (skipping wasm effect bindings generator test)")
    assume(hasZig, "zig not found on PATH (skipping wasm effect bindings generator test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping wasm effect bindings generator test)")
    assume(hasJco, "jco not found on PATH (skipping wasm effect bindings generator test)")
    assume(hasNode, "node not found on PATH (skipping wasm effect bindings generator test)")

    val workDir = Files.createTempDirectory("flix-wasm-effect-bindings-resource-")
    val genDir = workDir.resolve("gen")

    try {
      val generated = requireGenerated(ResourceWitDir, genDir)
      val flixSource = Files.readString(generated.flixFile, StandardCharsets.UTF_8)
      assert(flixSource.contains("pub eff HostDemoCounters"))
      assert(flixSource.contains("pub enum HostDemoCountersCounter(Int64)"))
      assert(flixSource.contains("def counterNew(seed: Int32): HostDemoCountersCounter"))
      assert(flixSource.contains("def counterGet(counter: HostDemoCountersCounter): Int32"))
      assert(flixSource.contains("def counterAdd(counter: HostDemoCountersCounter, delta: Int32): Int32"))
      assert(flixSource.contains("def counterDuplicate(src: HostDemoCountersCounter): HostDemoCountersCounter"))
      assert(flixSource.contains("def counterDrop(counter: HostDemoCountersCounter): Unit"))
      assert(flixSource.contains("HostDemoCountersCounter.HostDemoCountersCounter("))

      val dts = Files.readString(generated.dtsFile, StandardCharsets.UTF_8)
      assert(dts.contains("export interface HostDemoCounters"))
      assert(dts.contains("""counterNew(seed: number): Awaitable<{ __resource: "counter"; __ownership: "own"; id: bigint }>"""))
      assert(dts.contains("""counterGet(counter: { __resource: "counter"; __ownership: "borrow"; id: bigint }): Awaitable<number>"""))
      assert(dts.contains("""counterDrop(counter: { __resource: "counter"; __ownership: "own"; id: bigint }): Awaitable<void>"""))

      val rustBindings = Files.readString(generated.rustFile, StandardCharsets.UTF_8)
      assert(rustBindings.contains("pub struct HostDemoCountersCounter(pub i64);"))
      assert(rustBindings.contains("pub trait HostDemoCounters"))
      assert(rustBindings.contains("fn counter_new"))
      assert(rustBindings.contains("fn counter_drop"))

      val jsStub = Files.readString(generated.jsBrowserHostStubFile, StandardCharsets.UTF_8)
      assert(jsStub.contains("hostDemoCounters"))
      assert(jsStub.contains("counterDrop: async (arg0)"))

      val rustStub = Files.readString(generated.rustHostStubFile, StandardCharsets.UTF_8)
      assert(rustStub.contains("pub struct HostDemoCountersHost;"))
      assert(rustStub.contains("impl HostDemoCounters for HostDemoCountersHost"))
      assert(rustStub.contains("HostDemoCountersCounter"))
      assert(rustStub.contains("pub fn make_handler("))

      val userSource = workDir.resolve("Api.flix")
      Files.writeString(userSource,
        """
          |mod Api {
          |    @Export
          |    pub def callCounter(seed: Int32): String \ Wit.HostDemoCounters =
          |        let c1 = Wit.counterNew(seed);
          |        let v1 = Wit.counterGet(c1);
          |        let v2 = Wit.counterAdd(c1, 5);
          |        let c2 = Wit.counterDuplicate(c1);
          |        let v3 = Wit.counterGet(c2);
          |        Wit.counterDrop(c1);
          |        Wit.counterDrop(c2);
          |        "${Int32.toString(v1)}:${Int32.toString(v2)}:${Int32.toString(v3)}"
          |}
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val compiled = compileArtifacts(List(generated.flixFile, userSource))
      val nodeFile = Files.createTempFile("flix-wit-effect-bindings-resource-js-", ".mjs")
      try {
        Files.writeString(nodeFile,
          s"""
             |import * as fs from "node:fs/promises";
             |import { runtime } from ${jsStringLiteral(compiled.componentJs.toUri.toString)};
             |import { FlixRunner } from ${jsStringLiteral(compiled.runnerModule.toUri.toString)};
             |import { makeUnknownHandler } from ${jsStringLiteral(generated.jsFile.toUri.toString)};
             |
             |function assert(cond, msg) {
             |  if (!cond) throw new Error(msg);
             |}
             |
             |const disposeSym = Symbol.dispose ?? Symbol.for("dispose");
             |function maybeDispose(x) {
             |  try {
             |    const fn = x?.[disposeSym];
             |    if (typeof fn === "function") fn.call(x);
             |  } catch {}
             |}
             |
             |const table = new Map();
             |let nextId = 1n;
             |
             |const exportsManifest = JSON.parse(await fs.readFile(${jsStringLiteral(compiled.exportsManifest.toString)}, "utf8"));
             |const def = exportsManifest.defs.find((d) => d.symbol === "Api.callCounter");
             |assert(def, "missing Api.callCounter export");
             |
             |const unknown = await makeUnknownHandler(
             |  ${jsStringLiteral(compiled.effectsManifest.toString)},
             |  {
             |    hostDemoCounters: {
             |      counterNew: async (seed) => {
             |        const handle = { __resource: "counter", __ownership: "own", id: nextId++ };
             |        table.set(handle.id, seed);
             |        return handle;
             |      },
             |      counterGet: async (counter) => {
             |        assert(table.has(counter.id), `missing counter ${"$"}{counter.id.toString()}`);
             |        return table.get(counter.id);
             |      },
             |      counterAdd: async (counter, delta) => {
             |        const next = table.get(counter.id) + delta;
             |        table.set(counter.id, next);
             |        return next;
             |      },
             |      counterDuplicate: async (src) => {
             |        const handle = { __resource: "counter", __ownership: "own", id: nextId++ };
             |        table.set(handle.id, table.get(src.id));
             |        return handle;
             |      },
             |      counterDrop: async (counter) => {
             |        table.delete(counter.id);
             |      },
             |    },
             |  }
             |);
             |
             |const runner = new FlixRunner(runtime, { handlers: { unknown } });
             |const ctx = runtime.newCtx();
             |const arg = runtime.boxI32(ctx, 10);
             |try {
             |  const taskId = runtime.startTask(ctx, BigInt(def.defId), [arg]);
             |  const out = await runner.runTaskToCompletion(ctx, taskId);
             |  try {
             |    assert(out.tag === "ok", `unexpected task outcome: $${JSON.stringify(out)}`);
             |    const result = runtime.unboxString(ctx, out.val);
             |    assert(result === "10:15:15", `bad resource binding result: $${result}`);
             |    assert(table.size === 0, `resource leak: $${table.size}`);
             |    console.log("OK");
             |  } finally {
             |    maybeDispose(out.val);
             |  }
             |} finally {
             |  maybeDispose(arg);
             |  maybeDispose(ctx);
             |}
             |""".stripMargin,
          StandardCharsets.UTF_8
        )

        val (jsExit, jsOutput) = runCmd(List("node", nodeFile.toString))
        if (jsExit != 0) {
          fail(s"JS resource generated effect binding host failed with exit $jsExit:\n$jsOutput")
        }
        if (jsOutput.trim != "OK") {
          fail(s"Unexpected JS resource generated effect binding output:\n$jsOutput")
        }
      } finally {
        Files.deleteIfExists(nodeFile)
      }

      val rustHostDir = Files.createTempDirectory("flix-wit-effect-bindings-resource-rs-")
      try {
        Files.createDirectories(rustHostDir.resolve("src"))
        Files.writeString(rustHostDir.resolve("Cargo.toml"),
          s"""
             |[package]
             |name = "resource_effect_bindings_host"
             |version = "0.1.0"
             |edition = "2021"
             |
             |[dependencies]
             |anyhow = "1"
             |serde = { version = "1", features = ["derive"] }
             |serde_json = "1"
             |wasmtime = { version = "38", features = ["component-model"] }
             |flix-wit-effect-bindings = { path = ${rustSdkDepLiteral(generated)} }
             |""".stripMargin,
          StandardCharsets.UTF_8
        )
        Files.writeString(rustHostDir.resolve("src").resolve("main.rs"),
          s"""
             |use anyhow::{Context, Result};
             |use flix_wit_effect_bindings::{
             |    bindings,
             |    effects::{AsyncResult, EffectManifest},
             |    runner::FlixRunner,
             |};
             |use serde::Deserialize;
             |use std::{collections::HashMap, fs};
             |use wasmtime::{
             |    Config, Engine, Store,
             |    component::{Component, HasSelf, Linker},
             |};
             |
             |use bindings::flix::sys::sys::{Capability, Host as SysHost, LogLevel};
             |use flix_wit_effect_bindings::{HostDemoCounters, HostDemoCountersCounter, WitEffectHandler};
             |
             |#[derive(Default)]
             |struct State;
             |
             |impl SysHost for State {
             |    fn log(&mut self, _level: LogLevel, _msg: String) {}
             |    fn time_now_ms(&mut self) -> i64 { 0 }
             |    fn random_bytes(&mut self, len: u32) -> Vec<u8> { (0..len).map(|i| i as u8).collect() }
             |    fn get_args(&mut self) -> Vec<String> { Vec::new() }
             |    fn has_capability(&mut self, _cap: Capability) -> bool { false }
             |}
             |
             |#[derive(Deserialize)]
             |struct ExportsManifest {
             |    defs: Vec<ExportedDef>,
             |}
             |
             |#[derive(Deserialize)]
             |#[serde(rename_all = "camelCase")]
             |struct ExportedDef {
             |    def_id: u64,
             |    symbol: String,
             |}
             |
             |struct HostCounters {
             |    next_id: i64,
             |    table: HashMap<i64, i32>,
             |}
             |
             |impl HostDemoCounters for HostCounters {
             |    fn counter_new(&mut self, seed: i32) -> Result<AsyncResult<HostDemoCountersCounter>> {
             |        let id = self.next_id;
             |        self.next_id += 1;
             |        self.table.insert(id, seed);
             |        Ok(AsyncResult::Ready(HostDemoCountersCounter(id)))
             |    }
             |
             |    fn counter_get(&mut self, counter: HostDemoCountersCounter) -> Result<AsyncResult<i32>> {
             |        let value = *self.table.get(&counter.0).context("missing counter")?;
             |        Ok(AsyncResult::Ready(value))
             |    }
             |
             |    fn counter_add(&mut self, counter: HostDemoCountersCounter, delta: i32) -> Result<AsyncResult<i32>> {
             |        let slot = self.table.get_mut(&counter.0).context("missing counter")?;
             |        *slot += delta;
             |        Ok(AsyncResult::Ready(*slot))
             |    }
             |
             |    fn counter_duplicate(&mut self, src: HostDemoCountersCounter) -> Result<AsyncResult<HostDemoCountersCounter>> {
             |        let value = *self.table.get(&src.0).context("missing counter")?;
             |        let id = self.next_id;
             |        self.next_id += 1;
             |        self.table.insert(id, value);
             |        Ok(AsyncResult::Ready(HostDemoCountersCounter(id)))
             |    }
             |
             |    fn counter_drop(&mut self, counter: HostDemoCountersCounter) -> Result<AsyncResult<()>> {
             |        self.table.remove(&counter.0);
             |        Ok(AsyncResult::Ready(()))
             |    }
             |}
             |
             |fn main() -> Result<()> {
             |    let mut config = Config::new();
             |    config.wasm_component_model(true);
             |    let engine = Engine::new(&config)?;
             |
             |    let component = Component::from_file(&engine, ${cargoStringLiteral(compiled.componentWasm.toString)})?;
             |    let mut linker = Linker::<State>::new(&engine);
             |    bindings::flix::sys::sys::add_to_linker::<_, HasSelf<_>>(&mut linker, |s| s)?;
             |
             |    let mut store = Store::new(&engine, State::default());
             |    let flix = bindings::Flix::instantiate(&mut store, &component, &linker)?;
             |    let rt = flix.flix_runtime_runtime();
             |    let ctx = rt.call_new_ctx(&mut store)?;
             |
             |    let exports: ExportsManifest = serde_json::from_str(&fs::read_to_string(${cargoStringLiteral(compiled.exportsManifest.toString)})?)?;
             |    let main_def = exports.defs.into_iter().find(|d| d.symbol == "Api.callCounter").context("missing Api.callCounter export")?;
             |    let manifest = EffectManifest::from_path(std::path::Path::new(${cargoStringLiteral(compiled.effectsManifest.toString)}))?;
             |
             |    let runner = FlixRunner { budget: 100 };
             |    let mut handler = WitEffectHandler::new(manifest, HostCounters { next_id: 1, table: HashMap::new() });
             |    let arg = rt.call_box_i32(&mut store, ctx, 10)?;
             |    let task_id = rt.call_start_task(&mut store, ctx, main_def.def_id, &[arg])?;
             |    let _ = arg.resource_drop(&mut store);
             |    let outcome = runner.run_task_to_completion(&mut store, rt, ctx, task_id, &mut handler)?;
             |    match outcome {
             |        bindings::exports::flix::runtime::runtime::TaskOutcome::Ok(v) => {
             |            let result = rt.call_unbox_string(&mut store, ctx, v)?;
             |            anyhow::ensure!(result == "10:15:15", "bad resource Rust binding result: {}", result);
             |            anyhow::ensure!(handler.host_demo_counters.table.is_empty(), "resource leak: {}", handler.host_demo_counters.table.len());
             |            let _ = v.resource_drop(&mut store);
             |            println!("OK");
             |            Ok(())
             |        }
             |        other => anyhow::bail!("unexpected task outcome: {:?}", other),
             |    }
             |}
             |""".stripMargin,
          StandardCharsets.UTF_8
        )

        val (rsExit, rsOutput) = runCmd(List("cargo", "+stable", "run", "--quiet"), rustHostDir)
        if (rsExit != 0) {
          fail(s"Rust resource generated effect binding host failed with exit $rsExit:\n$rsOutput")
        }
        if (rsOutput.trim != "OK") {
          fail(s"Unexpected Rust resource generated effect binding output:\n$rsOutput")
        }
      } finally {
        deleteRecursive(rustHostDir)
        deleteRecursive(compiled.outDir)
      }
    } finally {
      deleteRecursive(workDir)
    }
  }

  test("bind-wasm-effects-generates-world-resource-js-and-rust-hosts") {
    assume(hasCargoStable, "cargo +stable not available (skipping wasm effect bindings generator test)")
    assume(hasZig, "zig not found on PATH (skipping wasm effect bindings generator test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping wasm effect bindings generator test)")
    assume(hasJco, "jco not found on PATH (skipping wasm effect bindings generator test)")
    assume(hasNode, "node not found on PATH (skipping wasm effect bindings generator test)")

    val workDir = Files.createTempDirectory("flix-wit-effect-bindings-world-resource-")
    val genDir = workDir.resolve("gen")

    try {
      val generated = requireGenerated(WorldResourceWitDir, genDir)
      val flixSource = Files.readString(generated.flixFile, StandardCharsets.UTF_8)
      assert(flixSource.contains("pub eff HostDemoDemoWorld"))
      assert(flixSource.contains("pub enum HostDemoDemoWorldCounter(Int64)"))
      assert(flixSource.contains("def counterNew(seed: Int32): HostDemoDemoWorldCounter"))
      assert(flixSource.contains("def counterGet(counter: HostDemoDemoWorldCounter): Int32"))
      assert(flixSource.contains("def counterAdd(counter: HostDemoDemoWorldCounter, delta: Int32): Int32"))
      assert(flixSource.contains("def counterDuplicate(src: HostDemoDemoWorldCounter): HostDemoDemoWorldCounter"))
      assert(flixSource.contains("def someWorldFunc(): HostDemoDemoWorldCounter"))
      assert(flixSource.contains("def counterDrop(counter: HostDemoDemoWorldCounter): Unit"))

      val dts = Files.readString(generated.dtsFile, StandardCharsets.UTF_8)
      assert(dts.contains("export interface HostDemoDemoWorld"))
      assert(dts.contains("""counterNew(seed: number): Awaitable<{ __resource: "counter"; __ownership: "own"; id: bigint }>"""))
      assert(dts.contains("""someWorldFunc(): Awaitable<{ __resource: "counter"; __ownership: "own"; id: bigint }>"""))

      val rustBindings = Files.readString(generated.rustFile, StandardCharsets.UTF_8)
      assert(rustBindings.contains("pub struct HostDemoDemoWorldCounter(pub i64);"))
      assert(rustBindings.contains("pub trait HostDemoDemoWorld"))
      assert(rustBindings.contains("fn counter_new"))
      assert(rustBindings.contains("fn some_world_func"))
      assert(rustBindings.contains("fn counter_drop"))

      val userSource = workDir.resolve("Api.flix")
      Files.writeString(userSource,
        """
          |mod Api {
          |    @Export
          |    pub def callWorldCounter(seed: Int32): String \ Wit.HostDemoDemoWorld =
          |        let c1 = Wit.counterNew(seed);
          |        let c2 = Wit.someWorldFunc();
          |        let v1 = Wit.counterGet(c1);
          |        let v2 = Wit.counterAdd(c1, 5);
          |        let c3 = Wit.counterDuplicate(c2);
          |        let v3 = Wit.counterGet(c3);
          |        Wit.counterDrop(c1);
          |        Wit.counterDrop(c2);
          |        Wit.counterDrop(c3);
          |        "${Int32.toString(v1)}:${Int32.toString(v2)}:${Int32.toString(v3)}"
          |}
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val compiled = compileArtifacts(List(generated.flixFile, userSource))
      val nodeFile = Files.createTempFile("flix-wit-effect-bindings-world-resource-js-", ".mjs")
      try {
        Files.writeString(nodeFile,
          s"""
             |import * as fs from "node:fs/promises";
             |import { runtime } from ${jsStringLiteral(compiled.componentJs.toUri.toString)};
             |import { FlixRunner } from ${jsStringLiteral(compiled.runnerModule.toUri.toString)};
             |import { makeUnknownHandler } from ${jsStringLiteral(generated.jsFile.toUri.toString)};
             |
             |function assert(cond, msg) {
             |  if (!cond) throw new Error(msg);
             |}
             |
             |const disposeSym = Symbol.dispose ?? Symbol.for("dispose");
             |function maybeDispose(x) {
             |  try {
             |    const fn = x?.[disposeSym];
             |    if (typeof fn === "function") fn.call(x);
             |  } catch {}
             |}
             |
             |const table = new Map();
             |let nextId = 1n;
             |
             |const exportsManifest = JSON.parse(await fs.readFile(${jsStringLiteral(compiled.exportsManifest.toString)}, "utf8"));
             |const def = exportsManifest.defs.find((d) => d.symbol === "Api.callWorldCounter");
             |assert(def, "missing Api.callWorldCounter export");
             |
             |const unknown = await makeUnknownHandler(
             |  ${jsStringLiteral(compiled.effectsManifest.toString)},
             |  {
             |    hostDemoDemoWorld: {
             |      counterNew: async (seed) => {
             |        const handle = { __resource: "counter", __ownership: "own", id: nextId++ };
             |        table.set(handle.id, seed);
             |        return handle;
             |      },
             |      counterGet: async (counter) => {
             |        assert(table.has(counter.id), `missing counter ${"$"}{counter.id.toString()}`);
             |        return table.get(counter.id);
             |      },
             |      counterAdd: async (counter, delta) => {
             |        const next = table.get(counter.id) + delta;
             |        table.set(counter.id, next);
             |        return next;
             |      },
             |      counterDuplicate: async (src) => {
             |        const handle = { __resource: "counter", __ownership: "own", id: nextId++ };
             |        table.set(handle.id, table.get(src.id));
             |        return handle;
             |      },
             |      someWorldFunc: async () => {
             |        const handle = { __resource: "counter", __ownership: "own", id: nextId++ };
             |        table.set(handle.id, 20);
             |        return handle;
             |      },
             |      counterDrop: async (counter) => {
             |        table.delete(counter.id);
             |      },
             |    },
             |  }
             |);
             |
             |const runner = new FlixRunner(runtime, { handlers: { unknown } });
             |const ctx = runtime.newCtx();
             |const arg = runtime.boxI32(ctx, 10);
             |try {
             |  const taskId = runtime.startTask(ctx, BigInt(def.defId), [arg]);
             |  const out = await runner.runTaskToCompletion(ctx, taskId);
             |  try {
             |    assert(out.tag === "ok", `unexpected task outcome: $${JSON.stringify(out)}`);
             |    const result = runtime.unboxString(ctx, out.val);
             |    assert(result === "10:15:20", `bad world resource binding result: $${result}`);
             |    assert(table.size === 0, `resource leak: $${table.size}`);
             |    console.log("OK");
             |  } finally {
             |    maybeDispose(out.val);
             |  }
             |} finally {
             |  maybeDispose(arg);
             |  maybeDispose(ctx);
             |}
             |""".stripMargin,
          StandardCharsets.UTF_8
        )

        val (jsExit, jsOutput) = runCmd(List("node", nodeFile.toString))
        if (jsExit != 0) {
          fail(s"JS world-resource generated effect binding host failed with exit $jsExit:\n$jsOutput")
        }
        if (jsOutput.trim != "OK") {
          fail(s"Unexpected JS world-resource generated effect binding output:\n$jsOutput")
        }
      } finally {
        Files.deleteIfExists(nodeFile)
      }

      val rustHostDir = Files.createTempDirectory("flix-wit-effect-bindings-world-resource-rs-")
      try {
        Files.createDirectories(rustHostDir.resolve("src"))
        Files.writeString(rustHostDir.resolve("Cargo.toml"),
          s"""
             |[package]
             |name = "world_resource_effect_bindings_host"
             |version = "0.1.0"
             |edition = "2021"
             |
             |[dependencies]
             |anyhow = "1"
             |serde = { version = "1", features = ["derive"] }
             |serde_json = "1"
             |wasmtime = { version = "38", features = ["component-model"] }
             |flix-wit-effect-bindings = { path = ${rustSdkDepLiteral(generated)} }
             |""".stripMargin,
          StandardCharsets.UTF_8
        )
        Files.writeString(rustHostDir.resolve("src").resolve("main.rs"),
          s"""
             |use anyhow::{Context, Result};
             |use flix_wit_effect_bindings::{
             |    bindings,
             |    effects::{AsyncResult, EffectManifest},
             |    runner::FlixRunner,
             |};
             |use serde::Deserialize;
             |use std::{collections::HashMap, fs};
             |use wasmtime::{
             |    Config, Engine, Store,
             |    component::{Component, HasSelf, Linker},
             |};
             |
             |use bindings::flix::sys::sys::{Capability, Host as SysHost, LogLevel};
             |use flix_wit_effect_bindings::{HostDemoDemoWorld, HostDemoDemoWorldCounter, WitEffectHandler};
             |
             |#[derive(Default)]
             |struct State;
             |
             |impl SysHost for State {
             |    fn log(&mut self, _level: LogLevel, _msg: String) {}
             |    fn time_now_ms(&mut self) -> i64 { 0 }
             |    fn random_bytes(&mut self, len: u32) -> Vec<u8> { (0..len).map(|i| i as u8).collect() }
             |    fn get_args(&mut self) -> Vec<String> { Vec::new() }
             |    fn has_capability(&mut self, _cap: Capability) -> bool { false }
             |}
             |
             |#[derive(Deserialize)]
             |struct ExportsManifest {
             |    defs: Vec<ExportedDef>,
             |}
             |
             |#[derive(Deserialize)]
             |#[serde(rename_all = "camelCase")]
             |struct ExportedDef {
             |    def_id: u64,
             |    symbol: String,
             |}
             |
             |struct HostWorld {
             |    next_id: i64,
             |    table: HashMap<i64, i32>,
             |}
             |
             |impl HostDemoDemoWorld for HostWorld {
             |    fn counter_new(&mut self, seed: i32) -> Result<AsyncResult<HostDemoDemoWorldCounter>> {
             |        let id = self.next_id;
             |        self.next_id += 1;
             |        self.table.insert(id, seed);
             |        Ok(AsyncResult::Ready(HostDemoDemoWorldCounter(id)))
             |    }
             |
             |    fn counter_get(&mut self, counter: HostDemoDemoWorldCounter) -> Result<AsyncResult<i32>> {
             |        let value = *self.table.get(&counter.0).context("missing counter")?;
             |        Ok(AsyncResult::Ready(value))
             |    }
             |
             |    fn counter_add(&mut self, counter: HostDemoDemoWorldCounter, delta: i32) -> Result<AsyncResult<i32>> {
             |        let slot = self.table.get_mut(&counter.0).context("missing counter")?;
             |        *slot += delta;
             |        Ok(AsyncResult::Ready(*slot))
             |    }
             |
             |    fn counter_duplicate(&mut self, src: HostDemoDemoWorldCounter) -> Result<AsyncResult<HostDemoDemoWorldCounter>> {
             |        let value = *self.table.get(&src.0).context("missing counter")?;
             |        let id = self.next_id;
             |        self.next_id += 1;
             |        self.table.insert(id, value);
             |        Ok(AsyncResult::Ready(HostDemoDemoWorldCounter(id)))
             |    }
             |
             |    fn some_world_func(&mut self) -> Result<AsyncResult<HostDemoDemoWorldCounter>> {
             |        let id = self.next_id;
             |        self.next_id += 1;
             |        self.table.insert(id, 20);
             |        Ok(AsyncResult::Ready(HostDemoDemoWorldCounter(id)))
             |    }
             |
             |    fn counter_drop(&mut self, counter: HostDemoDemoWorldCounter) -> Result<AsyncResult<()>> {
             |        self.table.remove(&counter.0);
             |        Ok(AsyncResult::Ready(()))
             |    }
             |}
             |
             |fn main() -> Result<()> {
             |    let mut config = Config::new();
             |    config.wasm_component_model(true);
             |    let engine = Engine::new(&config)?;
             |
             |    let component = Component::from_file(&engine, ${cargoStringLiteral(compiled.componentWasm.toString)})?;
             |    let mut linker = Linker::<State>::new(&engine);
             |    bindings::flix::sys::sys::add_to_linker::<_, HasSelf<_>>(&mut linker, |s| s)?;
             |
             |    let mut store = Store::new(&engine, State::default());
             |    let flix = bindings::Flix::instantiate(&mut store, &component, &linker)?;
             |    let rt = flix.flix_runtime_runtime();
             |    let ctx = rt.call_new_ctx(&mut store)?;
             |
             |    let exports: ExportsManifest = serde_json::from_str(&fs::read_to_string(${cargoStringLiteral(compiled.exportsManifest.toString)})?)?;
             |    let main_def = exports.defs.into_iter().find(|d| d.symbol == "Api.callWorldCounter").context("missing Api.callWorldCounter export")?;
             |    let manifest = EffectManifest::from_path(std::path::Path::new(${cargoStringLiteral(compiled.effectsManifest.toString)}))?;
             |
             |    let runner = FlixRunner { budget: 100 };
             |    let mut handler = WitEffectHandler::new(manifest, HostWorld { next_id: 1, table: HashMap::new() });
             |    let arg = rt.call_box_i32(&mut store, ctx, 10)?;
             |    let task_id = rt.call_start_task(&mut store, ctx, main_def.def_id, &[arg])?;
             |    let _ = arg.resource_drop(&mut store);
             |    let outcome = runner.run_task_to_completion(&mut store, rt, ctx, task_id, &mut handler)?;
             |    match outcome {
             |        bindings::exports::flix::runtime::runtime::TaskOutcome::Ok(v) => {
             |            let result = rt.call_unbox_string(&mut store, ctx, v)?;
             |            anyhow::ensure!(result == "10:15:20", "bad world-resource Rust binding result: {}", result);
             |            anyhow::ensure!(handler.host_demo_demo_world.table.is_empty(), "resource leak: {}", handler.host_demo_demo_world.table.len());
             |            let _ = v.resource_drop(&mut store);
             |            println!("OK");
             |            Ok(())
             |        }
             |        other => anyhow::bail!("unexpected task outcome: {:?}", other),
             |    }
             |}
             |""".stripMargin,
          StandardCharsets.UTF_8
        )

        val (rsExit, rsOutput) = runCmd(List("cargo", "+stable", "run", "--quiet"), rustHostDir)
        if (rsExit != 0) {
          fail(s"Rust world-resource generated effect binding host failed with exit $rsExit:\n$rsOutput")
        }
        if (rsOutput.trim != "OK") {
          fail(s"Unexpected Rust world-resource generated effect binding output:\n$rsOutput")
        }
      } finally {
        deleteRecursive(rustHostDir)
        deleteRecursive(compiled.outDir)
      }
    } finally {
      deleteRecursive(workDir)
    }
  }

  test("bind-wasm-effects-generates-nested-resource-js-and-rust-hosts") {
    assume(hasCargoStable, "cargo +stable not available (skipping wasm effect bindings generator test)")
    assume(hasZig, "zig not found on PATH (skipping wasm effect bindings generator test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping wasm effect bindings generator test)")
    assume(hasJco, "jco not found on PATH (skipping wasm effect bindings generator test)")
    assume(hasNode, "node not found on PATH (skipping wasm effect bindings generator test)")

    val workDir = Files.createTempDirectory("flix-wit-effect-bindings-nested-resource-")
    val genDir = workDir.resolve("gen")

    try {
      val generated = requireGenerated(NestedResourceWitDir, genDir)
      val flixSource = Files.readString(generated.flixFile, StandardCharsets.UTF_8)
      assert(flixSource.contains("pub eff HostDemoCounters"))
      assert(flixSource.contains("pub enum HostDemoCountersCounter(Int64)"))
      assert(flixSource.contains("def tupleHop"))
      assert(flixSource.contains("def optionHop"))
      assert(flixSource.contains("def resultHop"))
      assert(flixSource.contains("def listHop"))
      assert(flixSource.contains("def recordHop"))

      val dts = Files.readString(generated.dtsFile, StandardCharsets.UTF_8)
      assert(dts.contains("export interface HostDemoCounters"))
      assert(dts.contains("""tupleHop(x: [{ __resource: "counter"; __ownership: "borrow"; id: bigint }, number]): Awaitable<[{ __resource: "counter"; __ownership: "own"; id: bigint }, number]>"""))
      assert(dts.contains("""optionHop(x: { __resource: "counter"; __ownership: "borrow"; id: bigint } | null): Awaitable<{ __resource: "counter"; __ownership: "own"; id: bigint } | null>"""))
      assert(dts.contains("""listHop(xs: Array<{ __resource: "counter"; __ownership: "borrow"; id: bigint }>): Awaitable<Array<{ __resource: "counter"; __ownership: "own"; id: bigint }>>"""))

      val rustBindings = Files.readString(generated.rustFile, StandardCharsets.UTF_8)
      assert(rustBindings.contains("pub struct HostDemoCountersCounter(pub i64);"))
      assert(rustBindings.contains("pub trait HostDemoCounters"))
      assert(rustBindings.contains("fn tuple_hop"))
      assert(rustBindings.contains("fn option_hop"))
      assert(rustBindings.contains("fn result_hop"))
      assert(rustBindings.contains("fn list_hop"))
      assert(rustBindings.contains("fn record_hop"))

      val stepStruct = """pub struct (\w+) \{\s+pub counter: HostDemoCountersCounter,\s+pub delta: i32,""".r
        .findFirstMatchIn(rustBindings).map(_.group(1)).getOrElse(fail("missing generated nested-resource step struct"))
      val totalStruct = """pub struct (\w+) \{\s+pub counter: HostDemoCountersCounter,\s+pub total: i32,""".r
        .findFirstMatchIn(rustBindings).map(_.group(1)).getOrElse(fail("missing generated nested-resource total struct"))

      val userSource = workDir.resolve("Api.flix")
      Files.writeString(userSource,
        """
          |mod Api {
          |    @Export
          |    pub def callNested(seed: Int32): String \ Wit.HostDemoCounters =
          |        let c0 = Wit.counterNew(seed);
          |        let (c1, d1) = Wit.tupleHop((c0, 1));
          |        let c2 = match Wit.optionHop(Some(c1)) {
          |            case Some(counter) => counter
          |            case None => c1
          |        };
          |        let c3 = match Wit.listHop(c2 :: Nil) {
          |            case counter :: Nil => counter
          |            case _ => c2
          |        };
          |        let c4 = match Wit.resultHop(true, c3) {
          |            case Ok(counter) => counter
          |            case Err(_) => c3
          |        };
          |        let moved = Wit.recordHop({counter = c4, delta = d1});
          |        let c5 = moved#counter;
          |        let total = moved#total;
          |        let v = Wit.counterGet(c5);
          |        Wit.counterDrop(c0);
          |        Wit.counterDrop(c1);
          |        Wit.counterDrop(c2);
          |        Wit.counterDrop(c3);
          |        Wit.counterDrop(c4);
          |        Wit.counterDrop(c5);
          |        "${Int32.toString(total)}:${Int32.toString(v)}"
          |}
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val compiled = compileArtifacts(List(generated.flixFile, userSource))
      val nodeFile = Files.createTempFile("flix-wit-effect-bindings-nested-resource-js-", ".mjs")
      try {
        Files.writeString(nodeFile,
          s"""
             |import * as fs from "node:fs/promises";
             |import { runtime } from ${jsStringLiteral(compiled.componentJs.toUri.toString)};
             |import { FlixRunner } from ${jsStringLiteral(compiled.runnerModule.toUri.toString)};
             |import { makeUnknownHandler } from ${jsStringLiteral(generated.jsFile.toUri.toString)};
             |
             |function assert(cond, msg) {
             |  if (!cond) throw new Error(msg);
             |}
             |
             |const disposeSym = Symbol.dispose ?? Symbol.for("dispose");
             |function maybeDispose(x) {
             |  try {
             |    const fn = x?.[disposeSym];
             |    if (typeof fn === "function") fn.call(x);
             |  } catch {}
             |}
             |
             |const table = new Map();
             |let nextId = 1n;
             |
             |function alloc(value) {
             |  const handle = { __resource: "counter", __ownership: "own", id: nextId++ };
             |  table.set(handle.id, value);
             |  return handle;
             |}
             |
             |const exportsManifest = JSON.parse(await fs.readFile(${jsStringLiteral(compiled.exportsManifest.toString)}, "utf8"));
             |const def = exportsManifest.defs.find((d) => d.symbol === "Api.callNested");
             |assert(def, "missing Api.callNested export");
             |
             |const unknown = await makeUnknownHandler(
             |  ${jsStringLiteral(compiled.effectsManifest.toString)},
             |  {
             |    hostDemoCounters: {
             |      counterNew: async (seed) => alloc(seed),
             |      counterGet: async (counter) => table.get(counter.id),
             |      tupleHop: async ([counter, delta]) => [alloc(table.get(counter.id) + delta), delta + 1],
             |      optionHop: async (counter) => counter == null ? null : alloc(table.get(counter.id) + 10),
             |      listHop: async (xs) => xs.map((counter) => alloc(table.get(counter.id) * 2)),
             |      resultHop: async (flag, counter) => flag ? { tag: "ok", val: alloc(table.get(counter.id) + 100) } : { tag: "err", val: "bad" },
             |      recordHop: async ({ counter, delta }) => {
             |        const total = table.get(counter.id) + delta;
             |        return { counter: alloc(total), total };
             |      },
             |      counterDrop: async (counter) => {
             |        table.delete(counter.id);
             |      },
             |    },
             |  }
             |);
             |
             |const runner = new FlixRunner(runtime, { handlers: { unknown } });
             |const ctx = runtime.newCtx();
             |const arg = runtime.boxI32(ctx, 10);
             |try {
             |  const taskId = runtime.startTask(ctx, BigInt(def.defId), [arg]);
             |  const out = await runner.runTaskToCompletion(ctx, taskId);
             |  try {
             |    assert(out.tag === "ok", `unexpected task outcome: $${JSON.stringify(out)}`);
             |    const result = runtime.unboxString(ctx, out.val);
             |    assert(result === "144:144", `bad nested resource binding result: $${result}`);
             |    assert(table.size === 0, `resource leak: $${table.size}`);
             |    console.log("OK");
             |  } finally {
             |    maybeDispose(out.val);
             |  }
             |} finally {
             |  maybeDispose(arg);
             |  maybeDispose(ctx);
             |}
             |""".stripMargin,
          StandardCharsets.UTF_8
        )

        val (jsExit, jsOutput) = runCmd(List("node", nodeFile.toString))
        if (jsExit != 0) {
          fail(s"JS nested-resource generated effect binding host failed with exit $jsExit:\n$jsOutput")
        }
        if (jsOutput.trim != "OK") {
          fail(s"Unexpected JS nested-resource generated effect binding output:\n$jsOutput")
        }
      } finally {
        Files.deleteIfExists(nodeFile)
      }

      val rustHostDir = Files.createTempDirectory("flix-wit-effect-bindings-nested-resource-rs-")
      try {
        Files.createDirectories(rustHostDir.resolve("src"))
        Files.writeString(rustHostDir.resolve("Cargo.toml"),
          s"""
             |[package]
             |name = "nested_resource_effect_bindings_host"
             |version = "0.1.0"
             |edition = "2021"
             |
             |[dependencies]
             |anyhow = "1"
             |serde = { version = "1", features = ["derive"] }
             |serde_json = "1"
             |wasmtime = { version = "38", features = ["component-model"] }
             |flix-wit-effect-bindings = { path = ${rustSdkDepLiteral(generated)} }
             |""".stripMargin,
          StandardCharsets.UTF_8
        )
        Files.writeString(rustHostDir.resolve("src").resolve("main.rs"),
          s"""
             |use anyhow::{Context, Result};
             |use flix_wit_effect_bindings::{
             |    bindings,
             |    effects::{AsyncResult, EffectManifest},
             |    runner::FlixRunner,
             |};
             |use serde::Deserialize;
             |use std::{collections::HashMap, fs};
             |use wasmtime::{
             |    Config, Engine, Store,
             |    component::{Component, HasSelf, Linker},
             |};
             |
             |use bindings::flix::sys::sys::{Capability, Host as SysHost, LogLevel};
             |use flix_wit_effect_bindings::{HostDemoCounters, HostDemoCountersCounter, ${stepStruct}, ${totalStruct}, WitEffectHandler};
             |
             |#[derive(Default)]
             |struct State;
             |
             |impl SysHost for State {
             |    fn log(&mut self, _level: LogLevel, _msg: String) {}
             |    fn time_now_ms(&mut self) -> i64 { 0 }
             |    fn random_bytes(&mut self, len: u32) -> Vec<u8> { (0..len).map(|i| i as u8).collect() }
             |    fn get_args(&mut self) -> Vec<String> { Vec::new() }
             |    fn has_capability(&mut self, _cap: Capability) -> bool { false }
             |}
             |
             |#[derive(Deserialize)]
             |struct ExportsManifest {
             |    defs: Vec<ExportedDef>,
             |}
             |
             |#[derive(Deserialize)]
             |#[serde(rename_all = "camelCase")]
             |struct ExportedDef {
             |    def_id: u64,
             |    symbol: String,
             |}
             |
             |struct HostCounters {
             |    next_id: i64,
             |    table: HashMap<i64, i32>,
             |}
             |
             |impl HostCounters {
             |    fn alloc(&mut self, value: i32) -> HostDemoCountersCounter {
             |        let id = self.next_id;
             |        self.next_id += 1;
             |        self.table.insert(id, value);
             |        HostDemoCountersCounter(id)
             |    }
             |}
             |
             |impl HostDemoCounters for HostCounters {
             |    fn counter_new(&mut self, seed: i32) -> Result<AsyncResult<HostDemoCountersCounter>> {
             |        Ok(AsyncResult::Ready(self.alloc(seed)))
             |    }
             |
             |    fn counter_get(&mut self, counter: HostDemoCountersCounter) -> Result<AsyncResult<i32>> {
             |        let value = *self.table.get(&counter.0).context("missing counter")?;
             |        Ok(AsyncResult::Ready(value))
             |    }
             |
             |    fn tuple_hop(&mut self, x: (HostDemoCountersCounter, i32)) -> Result<AsyncResult<(HostDemoCountersCounter, i32)>> {
             |        let value = *self.table.get(&x.0.0).context("missing counter")?;
             |        Ok(AsyncResult::Ready((self.alloc(value + x.1), x.1 + 1)))
             |    }
             |
             |    fn option_hop(&mut self, x: Option<HostDemoCountersCounter>) -> Result<AsyncResult<Option<HostDemoCountersCounter>>> {
             |        let result = x.map(|counter| {
             |            let value = *self.table.get(&counter.0).expect("missing counter");
             |            self.alloc(value + 10)
             |        });
             |        Ok(AsyncResult::Ready(result))
             |    }
             |
             |    fn result_hop(&mut self, flag: bool, x: HostDemoCountersCounter) -> Result<AsyncResult<std::result::Result<HostDemoCountersCounter, String>>> {
             |        if flag {
             |            let value = *self.table.get(&x.0).context("missing counter")?;
             |            Ok(AsyncResult::Ready(Ok(self.alloc(value + 100))))
             |        } else {
             |            Ok(AsyncResult::Ready(Err("bad".to_string())))
             |        }
             |    }
             |
             |    fn list_hop(&mut self, xs: Vec<HostDemoCountersCounter>) -> Result<AsyncResult<Vec<HostDemoCountersCounter>>> {
             |        let mut out = Vec::new();
             |        for counter in xs {
             |            let value = *self.table.get(&counter.0).context("missing counter")?;
             |            out.push(self.alloc(value * 2));
             |        }
             |        Ok(AsyncResult::Ready(out))
             |    }
             |
             |    fn record_hop(&mut self, x: ${stepStruct}) -> Result<AsyncResult<${totalStruct}>> {
             |        let total = *self.table.get(&x.counter.0).context("missing counter")? + x.delta;
             |        Ok(AsyncResult::Ready(${totalStruct} { counter: self.alloc(total), total }))
             |    }
             |
             |    fn counter_drop(&mut self, counter: HostDemoCountersCounter) -> Result<AsyncResult<()>> {
             |        self.table.remove(&counter.0);
             |        Ok(AsyncResult::Ready(()))
             |    }
             |}
             |
             |fn main() -> Result<()> {
             |    let mut config = Config::new();
             |    config.wasm_component_model(true);
             |    let engine = Engine::new(&config)?;
             |
             |    let component = Component::from_file(&engine, ${cargoStringLiteral(compiled.componentWasm.toString)})?;
             |    let mut linker = Linker::<State>::new(&engine);
             |    bindings::flix::sys::sys::add_to_linker::<_, HasSelf<_>>(&mut linker, |s| s)?;
             |
             |    let mut store = Store::new(&engine, State::default());
             |    let flix = bindings::Flix::instantiate(&mut store, &component, &linker)?;
             |    let rt = flix.flix_runtime_runtime();
             |    let ctx = rt.call_new_ctx(&mut store)?;
             |
             |    let exports: ExportsManifest = serde_json::from_str(&fs::read_to_string(${cargoStringLiteral(compiled.exportsManifest.toString)})?)?;
             |    let main_def = exports.defs.into_iter().find(|d| d.symbol == "Api.callNested").context("missing Api.callNested export")?;
             |    let manifest = EffectManifest::from_path(std::path::Path::new(${cargoStringLiteral(compiled.effectsManifest.toString)}))?;
             |
             |    let runner = FlixRunner { budget: 100 };
             |    let mut handler = WitEffectHandler::new(manifest, HostCounters { next_id: 1, table: HashMap::new() });
             |    let arg = rt.call_box_i32(&mut store, ctx, 10)?;
             |    let task_id = rt.call_start_task(&mut store, ctx, main_def.def_id, &[arg])?;
             |    let _ = arg.resource_drop(&mut store);
             |    let outcome = runner.run_task_to_completion(&mut store, rt, ctx, task_id, &mut handler)?;
             |    match outcome {
             |        bindings::exports::flix::runtime::runtime::TaskOutcome::Ok(v) => {
             |            let result = rt.call_unbox_string(&mut store, ctx, v)?;
             |            anyhow::ensure!(result == "144:144", "bad nested-resource Rust binding result: {}", result);
             |            anyhow::ensure!(handler.host_demo_counters.table.is_empty(), "resource leak: {}", handler.host_demo_counters.table.len());
             |            let _ = v.resource_drop(&mut store);
             |            println!("OK");
             |            Ok(())
             |        }
             |        other => anyhow::bail!("unexpected task outcome: {:?}", other),
             |    }
             |}
             |""".stripMargin,
          StandardCharsets.UTF_8
        )

        val (rsExit, rsOutput) = runCmd(List("cargo", "+stable", "run", "--quiet"), rustHostDir)
        if (rsExit != 0) {
          fail(s"Rust nested-resource generated effect binding host failed with exit $rsExit:\n$rsOutput")
        }
        if (rsOutput.trim != "OK") {
          fail(s"Unexpected Rust nested-resource generated effect binding output:\n$rsOutput")
        }
      } finally {
        deleteRecursive(rustHostDir)
        deleteRecursive(compiled.outDir)
      }
    } finally {
      deleteRecursive(workDir)
    }
  }

  private def compileArtifacts(files: List[Path]): CompiledArtifacts = {
    val outDir = Files.createTempDirectory("flix-wit-effect-bindings-build-")
    val flix = new Flix()
    flix.setOptions(TestOptions.copy(outputPath = outDir, artifactName = ArtifactName))
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted
    files.foreach(flix.addFile)

    flix.check() match {
      case (Some(root), Nil) =>
        flix.codeGen(root)
      case (optRoot, errors) =>
        fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
    }

    val runnerModule =
      if (Files.exists(Paths.get("tools/wasm-runner-js/runner.mjs"))) Paths.get("tools/wasm-runner-js/runner.mjs").toAbsolutePath.normalize()
      else outDir.resolve("llvm").resolve("wasm-runner-js").resolve("runner.mjs")

    CompiledArtifacts(
      outDir = outDir,
      componentWasm = LlvmWasmDriver.componentWasmPath(outDir, ArtifactName),
      componentJs = LlvmWasmDriver.componentJsPath(outDir, ArtifactName),
      runnerModule = runnerModule,
      exportsManifest = LlvmWasmExportWriter.manifestPath(outDir, ArtifactName),
      effectsManifest = LlvmWasmEffectManifestWriter.manifestPath(outDir, ArtifactName)
    )
  }

  private def requireGenerated(witDir: Path, outDir: Path): WasmEffectBindingsTool.Generated =
    WasmEffectBindingsTool.run(WasmEffectBindingsTool.Config(
      witDir = witDir,
      world = "demo",
      outDir = outDir
    )) match {
      case ca.uwaterloo.flix.util.Result.Ok(generated) => generated
      case ca.uwaterloo.flix.util.Result.Err(msg) => fail(msg)
    }

  private def jsStringLiteral(s: String): String =
    "\"" + s.flatMap {
      case '\\' => "\\\\"
      case '"' => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c => c.toString
    } + "\""

  private def cargoStringLiteral(s: String): String = jsStringLiteral(s)

  private def rustSdkDepLiteral(generated: WasmEffectBindingsTool.Generated): String =
    cargoStringLiteral(generated.rustCargoTomlFile.getParent.toAbsolutePath.normalize().toString)

  private def hasCmd(cmd: List[String]): Boolean = {
    val (exit, _) = runCmd(cmd)
    exit == 0
  }

  private def hasCargoStable: Boolean = hasCmd(List("cargo", "+stable", "--version"))

  private def hasWasmTools: Boolean = hasCmd(List("wasm-tools", "--version"))
  private def hasJco: Boolean = hasCmd(List("jco", "--version"))
  private def hasNode: Boolean = hasCmd(List("node", "--version"))

  private def runCmd(cmd: List[String], cwd: Path = Paths.get(".").toAbsolutePath.normalize()): (Int, String) = {
    val pb = new ProcessBuilder(cmd*)
    pb.directory(cwd.toFile)
    pb.redirectErrorStream(true)
    val proc = pb.start()
    val output = new String(proc.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    (proc.waitFor(), output)
  }

  private def deleteRecursive(path: Path): Unit = {
    if (!Files.exists(path)) return
    Files.walk(path).iterator().asScala.toList.reverse.foreach(Files.deleteIfExists)
  }
}
