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
import ca.uwaterloo.flix.util.{CompilationTarget, Options, StdlibProfile}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

class WasmAsyncEffectsLlvmWasmSuite extends AnyFunSuite {

  private val ArtifactName = "async-effect-echo"

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmWasm,
      incremental = false,
      outputJvm = false,
    )

  private val FixtureFile: Path =
    Paths.get("main/test/flix/wasm/apps/async_effect_echo/Main.flix")

  private case class CompiledArtifacts(outDir: Path,
                                       componentWasm: Path,
                                       componentJs: Path,
                                       runnerModule: Path,
                                       exportsManifest: Path,
                                       effectsManifest: Path)

  test("llvm-wasm-async-effects-js-host") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm async effect test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm async effect test)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm async effect test)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm async effect test)")

    val compiled = compileArtifacts()
    val nodeFile = Files.createTempFile("flix-llvm-wasm-async-effect-js-", ".mjs")
    try {
      val effectsText = Files.readString(compiled.effectsManifest, StandardCharsets.UTF_8)
      assert(effectsText.contains("Api.HostEcho"))
      assert(effectsText.contains("Api.HostEcho.echo"))
      assert(effectsText.contains(""""kind":"string""""))

      Files.writeString(nodeFile,
        s"""
           |import * as fs from "node:fs/promises";
           |import { runtime } from ${jsStringLiteral(compiled.componentJs.toUri.toString)};
           |import { FlixRunner } from ${jsStringLiteral(compiled.runnerModule.toUri.toString)};
           |import { loadEffectManifest, makeUnknownEffectHandler } from ${jsStringLiteral(compiled.componentJs.getParent.resolve("effect-handlers.mjs").toUri.toString)};
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
           |const effectManifest = await loadEffectManifest(${jsStringLiteral(compiled.effectsManifest.toString)});
           |const runner = new FlixRunner(runtime, {
           |  handlers: {
           |    unknown: makeUnknownEffectHandler(effectManifest, {
           |      "Api.HostEcho": {
           |        echo: async (s) => {
           |          await new Promise((resolve) => setTimeout(resolve, 5));
           |          return `${"$"}{s}!`;
           |        },
           |      },
           |    }),
           |  },
           |});
           |
           |const ctx = runtime.newCtx();
           |const arg = runtime.boxString(ctx, "hello");
           |try {
           |  const taskId = runtime.startTask(ctx, BigInt(def.defId), [arg]);
           |  const out = await runner.runTaskToCompletion(ctx, taskId);
           |  try {
           |    assert(out.tag === "ok", `unexpected task outcome: $${JSON.stringify(out)}`);
           |    const result = runtime.unboxString(ctx, out.val);
           |    assert(result === "hello!", `bad async effect result: $${result}`);
           |    console.log("OK");
           |  } finally {
           |    maybeDispose(out.val);
           |  }
           |} finally {
           |  maybeDispose(arg);
           |  maybeDispose(ctx);
           |}
           |""".stripMargin,
        StandardCharsets.UTF_8)

      val (exit, output) = runCmd(List("node", nodeFile.toString))
      if (exit != 0) {
        fail(s"JS async wasm host failed with exit $exit:\n$output")
      }
      if (output.trim != "OK") {
        fail(s"Unexpected JS async wasm host output:\n$output")
      }
    } finally {
      Files.deleteIfExists(nodeFile)
      deleteRecursive(compiled.outDir)
    }
  }

  test("llvm-wasm-async-effects-wasmtime-host") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm async effect test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm async effect test)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm async effect test)")
    assume(hasCargoStable, "cargo +stable not available (skipping LLVM-wasm async effect test)")

    val compiled = compileArtifacts()
    val hostDir = Files.createTempDirectory("flix-llvm-wasm-async-effect-wasmtime-")
    try {
      Files.createDirectories(hostDir.resolve("src"))
      Files.writeString(hostDir.resolve("Cargo.toml"),
        s"""
           |[package]
           |name = "async_effect_host"
           |version = "0.1.0"
           |edition = "2021"
           |
           |[dependencies]
           |anyhow = "1"
           |serde = { version = "1", features = ["derive"] }
           |serde_json = "1"
           |wasmtime = { version = "38", features = ["component-model"] }
           |flix-wasm-runner = { path = ${cargoStringLiteral(Paths.get("tools/wasm-runner-rs").toAbsolutePath.normalize().toString)} }
           |""".stripMargin,
        StandardCharsets.UTF_8)

      Files.writeString(hostDir.resolve("src").resolve("main.rs"),
        s"""
           |use anyhow::{bail, Context, Result};
           |use flix_wasm_runner::{
           |    bindings,
           |    effects::{box_leaf_value, decode_leaf_args, resume_throw_string, EffectManifest, LeafValue},
           |    runner::{FlixRunner, OpHandler, PendingIo},
           |};
           |use serde::Deserialize;
           |use std::fs;
           |use std::path::PathBuf;
           |use std::time::{Duration, Instant};
           |use wasmtime::{
           |    Config, Engine, Store,
           |    component::{Component, HasSelf, Linker},
           |};
           |
           |use bindings::exports::flix::runtime::runtime::{Guest, OpRequest, Suspension};
           |use bindings::flix::sys::sys::{Capability, Host as SysHost, LogLevel};
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
           |struct EchoHandler {
           |    manifest: EffectManifest,
           |    pending: Option<(Instant, String)>,
           |}
           |
           |impl EchoHandler {
           |    fn new(manifest: EffectManifest) -> Self {
           |        Self { manifest, pending: None }
           |    }
           |}
           |
           |impl OpHandler<State> for EchoHandler {
           |    fn handle_op(
           |        &mut self,
           |        store: &mut Store<State>,
           |        rt: &Guest,
           |        ctx: bindings::exports::flix::runtime::runtime::Ctx,
           |        suspension: Suspension,
           |        req: OpRequest,
           |    ) -> Result<()> {
           |        match req {
           |            OpRequest::Unknown(r) => {
           |                let Some(op) = self.manifest.lookup(r.eff_id, r.op_id as u32) else {
           |                    resume_throw_string(store, rt, ctx, suspension, format!("unsupported effect op: {}:{}", r.eff_id, r.op_id))?;
           |                    return Ok(());
           |                };
           |
           |                if op.op_symbol != "Api.HostEcho.echo" {
           |                    resume_throw_string(store, rt, ctx, suspension, format!("unexpected async effect op: {}", op.op_symbol))?;
           |                    return Ok(());
           |                }
           |
           |                let args = decode_leaf_args(store, rt, ctx, suspension, op)?;
           |                let input = match args.as_slice() {
           |                    [LeafValue::String(s)] => s.clone(),
           |                    other => bail!("bad async effect args: {:?}", other),
           |                };
           |
           |                match &self.pending {
           |                    Some((ready_at, out)) if Instant::now() >= *ready_at => {
           |                        let v = box_leaf_value(store, rt, ctx, &op.result, &LeafValue::String(out.clone()))?;
           |                        rt.call_resume_ok(&mut *store, ctx, suspension, v)?;
           |                        let _ = v.resource_drop(store);
           |                        self.pending = None;
           |                        Ok(())
           |                    }
           |                    Some(_) => Err(PendingIo.into()),
           |                    None => {
           |                        self.pending = Some((Instant::now() + Duration::from_millis(5), format!("{}!", input)));
           |                        Err(PendingIo.into())
           |                    }
           |                }
           |            }
           |            other => bail!("unexpected host op during async effect test: {:?}", other),
           |        }
           |    }
           |}
           |
           |fn main() -> Result<()> {
           |    let component_path = PathBuf::from(std::env::args().nth(1).context("missing component path")?);
           |    let exports_path = PathBuf::from(std::env::args().nth(2).context("missing exports manifest path")?);
           |    let effects_path = PathBuf::from(std::env::args().nth(3).context("missing effects manifest path")?);
           |
           |    let exports: ExportsManifest = serde_json::from_str(&fs::read_to_string(&exports_path)?)?;
           |    let def = exports.defs.into_iter().find(|d| d.symbol == "Api.callEcho").context("missing Api.callEcho export")?;
           |    let manifest = EffectManifest::from_path(&effects_path)?;
           |
           |    let mut config = Config::new();
           |    config.wasm_component_model(true);
           |    let engine = Engine::new(&config)?;
           |    let component = Component::from_file(&engine, &component_path)
           |        .with_context(|| format!("failed to load component: {}", component_path.display()))?;
           |
           |    let mut linker = Linker::<State>::new(&engine);
           |    bindings::flix::sys::sys::add_to_linker::<_, HasSelf<_>>(&mut linker, |s| s)?;
           |
           |    let mut store = Store::new(&engine, State::default());
           |    let flix = bindings::Flix::instantiate(&mut store, &component, &linker)?;
           |    let rt = flix.flix_runtime_runtime();
           |    let ctx = rt.call_new_ctx(&mut store)?;
           |
           |    let arg = rt.call_box_string(&mut store, ctx, "hello")?;
           |    let task = rt.call_start_task(&mut store, ctx, def.def_id, &[arg])?;
           |    let _ = arg.resource_drop(&mut store);
           |
           |    let runner = FlixRunner { budget: 100 };
           |    let mut handler = EchoHandler::new(manifest);
           |    let out = runner.run_task_to_completion(&mut store, rt, ctx, task, &mut handler)?;
           |
           |    match out {
           |        bindings::exports::flix::runtime::runtime::TaskOutcome::Ok(v) => {
           |            let result = rt.call_unbox_string(&mut store, ctx, v)?;
           |            let _ = v.resource_drop(&mut store);
           |            if result != "hello!" {
           |                bail!("bad async effect result: {}", result);
           |            }
           |            println!("OK");
           |            Ok(())
           |        }
           |        bindings::exports::flix::runtime::runtime::TaskOutcome::Thrown(v) => {
           |            let _ = v.resource_drop(&mut store);
           |            bail!("guest threw during async effect host test");
           |        }
           |    }
           |}
           |""".stripMargin,
        StandardCharsets.UTF_8)

      val (exit, output) = runCmd(List(
        "cargo", "+stable", "run", "--quiet",
        "--manifest-path", hostDir.resolve("Cargo.toml").toString,
        "--",
        compiled.componentWasm.toString,
        compiled.exportsManifest.toString,
        compiled.effectsManifest.toString
      ))
      if (exit != 0) {
        fail(s"Wasmtime async wasm host failed with exit $exit:\n$output")
      }
      if (output.trim != "OK") {
        fail(s"Unexpected Wasmtime async wasm host output:\n$output")
      }
    } finally {
      deleteRecursive(hostDir)
      deleteRecursive(compiled.outDir)
    }
  }

  private def compileArtifacts(): CompiledArtifacts = {
    val outDir = Files.createTempDirectory("flix-llvm-wasm-async-effect-out-")
    val flix = new Flix()
    flix.setOptions(TestOptions.copy(outputPath = outDir, artifactName = ArtifactName))
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted
    flix.addFile(FixtureFile)

    val (optRoot, errors) = flix.check()
    if (errors.nonEmpty) {
      fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
    }

    flix.codeGen(optRoot.get)

    val defaultRunner = Paths.get("tools/wasm-runner-js/runner.mjs").toAbsolutePath.normalize()

    CompiledArtifacts(
      outDir = outDir,
      componentWasm = LlvmWasmDriver.componentWasmPath(outDir, ArtifactName),
      componentJs = LlvmWasmDriver.componentJsPath(outDir, ArtifactName),
      runnerModule = if (Files.exists(defaultRunner)) defaultRunner else outDir.resolve("llvm").resolve("wasm-runner-js").resolve("runner.mjs"),
      exportsManifest = LlvmWasmExportWriter.manifestPath(outDir, ArtifactName),
      effectsManifest = LlvmWasmEffectManifestWriter.manifestPath(outDir, ArtifactName)
    )
  }

  private def runCmd(cmd: List[String]): (Int, String) = {
    val pb = new ProcessBuilder(cmd.asJava)
    pb.redirectErrorStream(true)
    val p = pb.start()
    p.getOutputStream.close()
    val output = new String(p.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val exit = p.waitFor()
    (exit, output)
  }

  private def deleteRecursive(root: Path): Unit = {
    if (!Files.exists(root)) return
    Files.walk(root)
      .sorted(java.util.Comparator.reverseOrder())
      .iterator()
      .asScala
      .foreach(Files.deleteIfExists)
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

  private def hasCommand(cmd: List[String]): Boolean = {
    val pb = new ProcessBuilder(cmd.asJava)
    pb.redirectErrorStream(true)
    try {
      val p = pb.start()
      p.waitFor() == 0
    } catch {
      case _: Throwable => false
    }
  }
  private def hasWasmTools: Boolean = hasCommand(List("wasm-tools", "--version"))
  private def hasJco: Boolean = hasCommand(List("jco", "--version"))
  private def hasNode: Boolean = hasCommand(List("node", "--version"))
  private def hasCargoStable: Boolean = hasCommand(List("cargo", "+stable", "--version"))
}
