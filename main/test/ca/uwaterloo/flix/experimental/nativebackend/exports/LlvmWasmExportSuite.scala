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

class LlvmWasmExportSuite extends AnyFunSuite {

  private val ArtifactName = "export-smoke"

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmWasm,
      incremental = false,
      outputJvm = false,
    )

  private val ExampleSourceFile: Path =
    Paths.get("examples/native-backend/export-smoke/src/Api.flix")

  private case class CompiledArtifacts(outDir: Path,
                                       sdkManifest: Path,
                                       bindingsJs: Path,
                                       bindingsTypes: Path,
                                       typedExportComponent: Path,
                                       typedWitDir: Path)

  test("llvm-wasm-main-manifest") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm export test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm export test)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm export test)")

    val artifactName = "main-manifest"
    val sourceFile = Files.createTempFile("flix-llvm-wasm-main-manifest-", ".flix")
    val outDir = Files.createTempDirectory("flix-llvm-wasm-main-manifest-out-")
    try {
      Files.writeString(sourceFile,
        """
          |def main(): Unit \ IO =
          |    println("ok")
          |""".stripMargin,
        StandardCharsets.UTF_8
      )

      val flix = new Flix()
      flix.setOptions(TestOptions.copy(outputPath = outDir, artifactName = artifactName))
      implicit val sctx: SecurityContext = SecurityContext.Unrestricted
      flix.addFile(sourceFile)

      val (optRoot, errors) = flix.check()
      if (errors.nonEmpty) {
        fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
      }

      flix.codeGen(optRoot.get)

      val manifest = ca.uwaterloo.flix.language.phase.llvm.LlvmWasmExportWriter.manifestPath(outDir, artifactName)
      if (!Files.exists(manifest)) {
        fail(s"Missing wasm exports manifest: $manifest")
      }

      val manifestText = Files.readString(manifest, StandardCharsets.UTF_8)
      assert(manifestText.contains(""""count": 1"""))
      assert(manifestText.contains(""""isMain": true"""))
      assert(manifestText.contains(""""isExport": false"""))
      assert(manifestText.contains(""""arity": 0"""))
      assert(manifestText.contains(""""params": []"""))
      assert(manifestText.contains(""""result": "Unit""""))
    } finally {
      Files.deleteIfExists(sourceFile)
      deleteRecursive(outDir)
    }
  }

  test("llvm-wasm-typed-export-bindings") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm export test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm export test)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm export test)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm export test)")
    val nodeFile = Files.createTempFile("flix-llvm-wasm-export-bindings-", ".mjs")
    val compiled = compileArtifacts()
    try {
      if (!Files.exists(compiled.sdkManifest)) fail(s"Missing wasm export SDK manifest: ${compiled.sdkManifest}")
      if (!Files.exists(compiled.bindingsJs)) fail(s"Missing wasm JS export bindings: ${compiled.bindingsJs}")
      if (!Files.exists(compiled.bindingsTypes)) fail(s"Missing wasm TS export bindings: ${compiled.bindingsTypes}")
      if (!Files.exists(compiled.typedExportComponent)) fail(s"Missing wasm typed export component: ${compiled.typedExportComponent}")
      if (!Files.isDirectory(compiled.typedWitDir)) fail(s"Missing wasm typed export WIT directory: ${compiled.typedWitDir}")
      val manifestText = Files.readString(compiled.sdkManifest, StandardCharsets.UTF_8)
      assert(manifestText.contains("flix-export-sdk-v0"))
      assert(manifestText.contains("\"target\": \"wasm\""))
      assert(manifestText.contains("\"symbol\": \"Api.add\""))
      assert(manifestText.contains("\"symbol\": \"Api.fortyTwo\""))
      val publicWit = Files.readString(compiled.typedWitDir.resolve("bindings.wit"), StandardCharsets.UTF_8)
      assert(publicWit.contains("type list-int32 = list<s32>;"))
      assert(publicWit.contains("type array-int32 = list<s32>;"))
      assert(publicWit.contains("type array-string = list<string>;"))
      assert(publicWit.contains("type list-tuple2-int32-string = list<tuple2-int32-string>;"))
      assert(publicWit.contains("type list-tuple2-string-int32 = list<tuple2-string-int32>;"))
      assert(publicWit.contains("type array-option-int32 = list<option-int32>;"))
      assert(publicWit.contains("type list-record-name-string-score-int32 = list<record-name-string-score-int32>;"))
      assert(publicWit.contains("type list-record-label-string-score-int32 = list<record-label-string-score-int32>;"))
      assert(publicWit.contains("type array-record-name-string-score-int32 = list<record-name-string-score-int32>;"))
      assert(publicWit.contains("record record-name-string-score-int32 {"))
      assert(publicWit.contains("name: string,"))
      assert(publicWit.contains("score: s32,"))
      assert(publicWit.contains("record record-label-string-score-int32 {"))
      assert(publicWit.contains("label: string,"))
      assert(publicWit.contains("record request-api-suspendecho {"))
      assert(publicWit.contains("request-api-suspendecho: func(s: borrow<suspension>) -> request-api-suspendecho;"))
      assert(publicWit.contains("resume-api-suspendecho: func(s: suspension, resume: string) -> exec-string;"))

      val (witExit, witOutput) = runCmd(List("wasm-tools", "component", "wit", compiled.typedExportComponent.toString))
      if (witExit != 0) {
        fail(s"Failed to inspect typed export component WIT:\n$witOutput")
      }
      if (!witOutput.contains("export flix:exports/api@0.1.0;")) {
        fail(s"Typed export component did not expose the expected typed API:\n$witOutput")
      }
      if (witOutput.contains("export flix:runtime/runtime@0.1.0;")) {
        fail(s"Typed export component leaked the internal runtime interface:\n$witOutput")
      }

      val nodeProgram =
        s"""
           |import { newCtx, Exports } from ${jsStringLiteral(compiled.bindingsJs.toUri.toString)};
           |
           |const disposeSym = Symbol.dispose ?? Symbol.for("dispose");
           |function maybeDispose(x) {
           |  try {
           |    const fn = x?.[disposeSym];
           |    if (typeof fn === "function") fn.call(x);
           |  } catch {}
           |}
           |
           |function assert(cond, msg) {
           |  if (!cond) throw new Error(msg);
           |}
           |
           |function eqBytes(a, b) {
           |  if (!(a instanceof Uint8Array) || !(b instanceof Uint8Array)) return false;
           |  if (a.length !== b.length) return false;
           |  for (let i = 0; i < a.length; i++) {
           |    if (a[i] !== b[i]) return false;
           |  }
           |  return true;
           |}
           |
           |const ctx = newCtx();
           |try {
           |  const add = Exports.Api.add(ctx, 1, 2);
           |  assert(add.tag === "ok" && add.val === 3, `bad add: $${JSON.stringify(add)}`);
           |
           |  const fortyTwo = Exports.Api.fortyTwo(ctx);
           |  assert(fortyTwo.tag === "ok" && fortyTwo.val === 42, `bad fortyTwo: $${JSON.stringify(fortyTwo)}`);
           |
           |  const echo = Exports.Api.echo(ctx, "hello");
           |  assert(echo.tag === "ok" && echo.val === "hello", `bad echo: $${JSON.stringify(echo)}`);
           |
           |  const data = new Uint8Array([0, 1, 2, 255]);
           |  const bytes = Exports.Api.bytesId(ctx, data);
           |  assert(bytes.tag === "ok" && eqBytes(bytes.val, data), "bad bytes");
           |
           |  const some = Exports.Api.maybeSucc(ctx, 41);
           |  assert(some.tag === "ok" && some.val === 42, `bad maybeSucc(Some): $${JSON.stringify(some)}`);
           |  const none = Exports.Api.maybeSucc(ctx, null);
           |  assert(none.tag === "ok" && none.val === null, `bad maybeSucc(None): $${JSON.stringify(none)}`);
           |
           |  const pair = Exports.Api.flipPair(ctx, [7, "hello"]);
           |  assert(pair.tag === "ok" && pair.val[0] === "hello" && pair.val[1] === 7, `bad flipPair: $${JSON.stringify(pair)}`);
           |
           |  const even = Exports.Api.halfEven(ctx, 8);
           |  assert(even.tag === "ok" && even.val.tag === "ok" && even.val.val === 4, `bad halfEven(8): $${JSON.stringify(even)}`);
           |  const odd = Exports.Api.halfEven(ctx, 7);
           |  assert(odd.tag === "ok" && odd.val.tag === "err" && odd.val.val === "odd", `bad halfEven(7): $${JSON.stringify(odd)}`);
           |
           |  const badge = Exports.Api.badge(ctx, { name: "hello", score: 41 });
           |  assert(badge.tag === "ok" && badge.val.label === "hello" && badge.val.score === 42, `bad badge: $${JSON.stringify(badge)}`);
           |
           |  const list = Exports.Api.prependAnswer(ctx, [1, 2, 3]);
           |  assert(list.tag === "ok" && JSON.stringify(list.val) === JSON.stringify([42, 1, 2, 3]), `bad prependAnswer: $${JSON.stringify(list)}`);
           |
           |  const ints = Exports.Api.echoInts(ctx, [1, 2, 3]);
           |  assert(ints.tag === "ok" && JSON.stringify(ints.val) === JSON.stringify([1, 2, 3]), `bad echoInts: $${JSON.stringify(ints)}`);
           |
           |  const names = Exports.Api.echoNames(ctx, ["hello", "world"]);
           |  assert(names.tag === "ok" && JSON.stringify(names.val) === JSON.stringify(["hello", "world"]), `bad echoNames: $${JSON.stringify(names)}`);
           |
           |  const promoted = Exports.Api.promoteUsers(ctx, [
           |    { name: "hello", score: 41 },
           |    { name: "world", score: 9 },
           |  ]);
           |  assert(
           |    promoted.tag === "ok" &&
           |      JSON.stringify(promoted.val) === JSON.stringify([
           |        { label: "hello", score: 42 },
           |        { label: "world", score: 10 },
           |      ]),
           |    `bad promoteUsers: $${JSON.stringify(promoted)}`
           |  );
           |
           |  const userArray = Exports.Api.echoUserArray(ctx, [
           |    { name: "hello", score: 41 },
           |    { name: "world", score: 9 },
           |  ]);
           |  assert(
           |    userArray.tag === "ok" &&
           |      JSON.stringify(userArray.val) === JSON.stringify([
           |        { name: "hello", score: 41 },
           |        { name: "world", score: 9 },
           |      ]),
           |    `bad echoUserArray: $${JSON.stringify(userArray)}`
           |  );
           |
           |  const flippedPairs = Exports.Api.flipPairs(ctx, [
           |    [7, "hello"],
           |    [9, "world"],
           |  ]);
           |  assert(
           |    flippedPairs.tag === "ok" &&
           |      JSON.stringify(flippedPairs.val) === JSON.stringify([
           |        ["hello", 7],
           |        ["world", 9],
           |      ]),
           |    `bad flipPairs: $${JSON.stringify(flippedPairs)}`
           |  );
           |
           |  const maybeInts = Exports.Api.echoMaybeInts(ctx, [41, null, 9]);
           |  assert(
           |    maybeInts.tag === "ok" &&
           |      JSON.stringify(maybeInts.val) === JSON.stringify([41, null, 9]),
           |    `bad echoMaybeInts: $${JSON.stringify(maybeInts)}`
           |  );
           |
           |  const susp = Exports.Api.suspendEcho(ctx, "hello");
           |  assert(susp.tag === "suspended", `bad suspend tag: $${String(susp.tag)}`);
           |  assert(Exports.Api.requestSuspendEcho(ctx, susp.val) === "hello", "bad suspension request");
           |
           |  const resumed = Exports.Api.resumeSuspendEcho(ctx, susp.val, "ok");
           |  assert(resumed.tag === "ok" && resumed.val === "ok", `bad resumed result: $${JSON.stringify(resumed)}`);
           |
           |  console.log("OK");
           |} finally {
           |  maybeDispose(ctx);
           |}
           |""".stripMargin

      Files.writeString(nodeFile, nodeProgram, StandardCharsets.UTF_8)

      val (exit, output) = runNode(nodeFile)
      if (exit != 0) {
        fail(s"Typed wasm export bindings smoke failed with exit $exit:\n$output")
      }
      if (output.trim != "OK") {
        fail(s"Unexpected typed wasm export bindings output:\n$output")
      }
    } finally {
      Files.deleteIfExists(nodeFile)
      deleteRecursive(compiled.outDir)
    }
  }

  test("llvm-wasm-typed-export-component-wasmtime-host") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm export test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm export test)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm export test)")
    assume(hasCargoStable, "cargo +stable not available (skipping LLVM-wasm Wasmtime export test)")

    val compiled = compileArtifacts()
    val hostDir = Files.createTempDirectory("flix-llvm-wasm-export-wasmtime-host-")
    try {
      val cargoToml =
        """
          |[package]
          |name = "typed_export_host"
          |version = "0.1.0"
          |edition = "2021"
          |
          |[dependencies]
          |anyhow = "1"
          |wasmtime = { version = "38", features = ["component-model"] }
          |""".stripMargin

      Files.createDirectories(hostDir.resolve("src"))
      Files.writeString(hostDir.resolve("Cargo.toml"), cargoToml, StandardCharsets.UTF_8)

      val rustMain =
        s"""
           |use anyhow::{bail, Context, Result};
           |use wasmtime::{
           |    Config, Engine, Store,
           |    component::{Component, HasSelf, Linker},
           |};
           |
           |mod bindings {
           |    wasmtime::component::bindgen!({
           |        path: ${jsStringLiteral(compiled.typedWitDir.toString)},
           |        world: "flix",
           |    });
           |}
           |
           |use bindings::exports::flix::exports::api;
           |use bindings::flix::sys::sys::{Capability, Host as SysHost, LogLevel};
           |
           |#[derive(Default)]
           |struct State;
           |
           |impl SysHost for State {
           |    fn log(&mut self, _level: LogLevel, _msg: String) {}
           |
           |    fn time_now_ms(&mut self) -> i64 { 0 }
           |
           |    fn random_bytes(&mut self, len: u32) -> Vec<u8> {
           |        (0..len).map(|i| (i as u8).wrapping_mul(31)).collect()
           |    }
           |
           |    fn get_args(&mut self) -> Vec<String> { Vec::new() }
           |
           |    fn has_capability(&mut self, _cap: Capability) -> bool { false }
           |}
           |
           |fn main() -> Result<()> {
           |    let component_path = std::env::args().nth(1).context("missing component path")?;
           |
           |    let mut config = Config::new();
           |    config.wasm_component_model(true);
           |    let engine = Engine::new(&config)?;
           |
           |    let component = Component::from_file(&engine, &component_path)
           |        .with_context(|| format!("failed to load component: {}", component_path))?;
           |
           |    let mut linker = Linker::<State>::new(&engine);
           |    bindings::flix::sys::sys::add_to_linker::<_, HasSelf<_>>(&mut linker, |s| s)?;
           |
           |    let mut store = Store::new(&engine, State::default());
           |    let flix = bindings::Flix::instantiate(&mut store, &component, &linker)?;
           |    let api = flix.flix_exports_api();
           |    let ctx_api = api.ctx();
           |    let ctx = ctx_api.call_constructor(&mut store)?;
           |
           |    match ctx_api.call_api_add(&mut store, ctx, 1, 2)? {
           |        api::ExecInt32::Ok(v) if v == 3 => {}
           |        other => bail!("bad add result: {:?}", other),
           |    }
           |
           |    match ctx_api.call_api_echo(&mut store, ctx, "hello")? {
           |        api::ExecString::Ok(v) if v == "hello" => {}
           |        other => bail!("bad echo result: {:?}", other),
           |    }
           |
           |    let data = vec![0u8, 1, 2, 255];
           |    match ctx_api.call_api_bytesid(&mut store, ctx, &data)? {
           |        api::ExecBytes::Ok(v) if v == data => {}
           |        other => bail!("bad bytes result: {:?}", other),
           |    }
           |
           |    let some_in = api::OptionInt32 { is_some: true, val: 41 };
           |    match ctx_api.call_api_maybesucc(&mut store, ctx, some_in)? {
           |        api::ExecOptionInt32::Ok(v) if v.is_some && v.val == 42 => {}
           |        other => bail!("bad maybeSucc(Some): {:?}", other),
           |    }
           |
           |    let none_in = api::OptionInt32 { is_some: false, val: 0 };
           |    match ctx_api.call_api_maybesucc(&mut store, ctx, none_in)? {
           |        api::ExecOptionInt32::Ok(v) if !v.is_some => {}
           |        other => bail!("bad maybeSucc(None): {:?}", other),
           |    }
           |
           |    let pair_in = api::Tuple2Int32String { f0: 7, f1: "hello".to_string() };
           |    match ctx_api.call_api_flippair(&mut store, ctx, &pair_in)? {
           |        api::ExecTuple2StringInt32::Ok(v) if v.f0 == "hello" && v.f1 == 7 => {}
           |        other => bail!("bad flipPair result: {:?}", other),
           |    }
           |
           |    match ctx_api.call_api_halfeven(&mut store, ctx, 8)? {
           |        api::ExecResultInt32String::Ok(v) if v.is_ok && v.ok == 4 => {}
           |        other => bail!("bad halfEven(8): {:?}", other),
           |    }
           |
           |    match ctx_api.call_api_halfeven(&mut store, ctx, 7)? {
           |        api::ExecResultInt32String::Ok(v) if !v.is_ok && v.err == "odd" => {}
           |        other => bail!("bad halfEven(7): {:?}", other),
           |    }
           |
           |    let badge_in = api::RecordNameStringScoreInt32 {
           |        name: "hello".to_string(),
           |        score: 41,
           |    };
           |    match ctx_api.call_api_badge(&mut store, ctx, &badge_in)? {
           |        api::ExecRecordLabelStringScoreInt32::Ok(v) if v.label == "hello" && v.score == 42 => {}
           |        other => bail!("bad badge result: {:?}", other),
           |    }
           |
           |    let ints = vec![1i32, 2, 3];
           |    match ctx_api.call_api_prependanswer(&mut store, ctx, &ints)? {
           |        api::ExecListInt32::Ok(v) if v == vec![42, 1, 2, 3] => {}
           |        other => bail!("bad prependAnswer result: {:?}", other),
           |    }
           |
           |    match ctx_api.call_api_echoints(&mut store, ctx, &ints)? {
           |        api::ExecArrayInt32::Ok(v) if v == vec![1, 2, 3] => {}
           |        other => bail!("bad echoInts result: {:?}", other),
           |    }
           |
           |    let names = vec!["hello".to_string(), "world".to_string()];
           |    match ctx_api.call_api_echonames(&mut store, ctx, &names)? {
           |        api::ExecArrayString::Ok(v) if v == names => {}
           |        other => bail!("bad echoNames result: {:?}", other),
           |    }
           |
           |    let users = vec![
           |        api::RecordNameStringScoreInt32 {
           |            name: "hello".to_string(),
           |            score: 41,
           |        },
           |        api::RecordNameStringScoreInt32 {
           |            name: "world".to_string(),
           |            score: 9,
           |        },
           |    ];
           |    match ctx_api.call_api_promoteusers(&mut store, ctx, &users)? {
           |        api::ExecListRecordLabelStringScoreInt32::Ok(v)
           |            if v.len() == 2
           |                && v[0].label == "hello"
           |                && v[0].score == 42
           |                && v[1].label == "world"
           |                && v[1].score == 10 => {}
           |        other => bail!("bad promoteUsers result: {:?}", other),
           |    }
           |
           |    match ctx_api.call_api_echouserarray(&mut store, ctx, &users)? {
           |        api::ExecArrayRecordNameStringScoreInt32::Ok(v)
           |            if v.len() == 2
           |                && v[0].name == "hello"
           |                && v[0].score == 41
           |                && v[1].name == "world"
           |                && v[1].score == 9 => {}
           |        other => bail!("bad echoUserArray result: {:?}", other),
           |    }
           |
           |    let pairs = vec![
           |        api::Tuple2Int32String { f0: 7, f1: "hello".to_string() },
           |        api::Tuple2Int32String { f0: 9, f1: "world".to_string() },
           |    ];
           |    match ctx_api.call_api_flippairs(&mut store, ctx, &pairs)? {
           |        api::ExecListTuple2StringInt32::Ok(v)
           |            if v.len() == 2
           |                && v[0].f0 == "hello"
           |                && v[0].f1 == 7
           |                && v[1].f0 == "world"
           |                && v[1].f1 == 9 => {}
           |        other => bail!("bad flipPairs result: {:?}", other),
           |    }
           |
           |    let maybe_ints = vec![
           |        api::OptionInt32 { is_some: true, val: 41 },
           |        api::OptionInt32 { is_some: false, val: 0 },
           |        api::OptionInt32 { is_some: true, val: 9 },
           |    ];
           |    match ctx_api.call_api_echomaybeints(&mut store, ctx, &maybe_ints)? {
           |        api::ExecArrayOptionInt32::Ok(v)
           |            if v.len() == 3
           |                && v[0].is_some
           |                && v[0].val == 41
           |                && !v[1].is_some
           |                && v[2].is_some
           |                && v[2].val == 9 => {}
           |        other => bail!("bad echoMaybeInts result: {:?}", other),
           |    }
           |
           |    let susp = match ctx_api.call_api_suspendecho(&mut store, ctx, "hello")? {
           |        api::ExecString::Suspended(s) => s,
           |        other => bail!("bad suspend result: {:?}", other),
           |    };
           |
           |    let req = ctx_api.call_request_api_suspendecho(&mut store, ctx, susp)?;
           |    if req.arg0 != "hello" {
           |        bail!("bad suspension arg: {}", req.arg0);
           |    }
           |
           |    match ctx_api.call_resume_api_suspendecho(&mut store, ctx, susp, "ok")? {
           |        api::ExecString::Ok(v) if v == "ok" => {}
           |        other => bail!("bad resume result: {:?}", other),
           |    }
           |    ctx.resource_drop(&mut store)?;
           |
           |    println!("OK");
           |    Ok(())
           |}
           |""".stripMargin

      Files.writeString(hostDir.resolve("src").resolve("main.rs"), rustMain, StandardCharsets.UTF_8)

      val (exit, output) = runCmd(List(
        "cargo",
        "+stable",
        "run",
        "--quiet",
        "--manifest-path",
        hostDir.resolve("Cargo.toml").toString,
        "--",
        compiled.typedExportComponent.toString
      ))

      if (exit != 0) {
        fail(s"Typed Wasmtime host smoke failed with exit $exit:\n$output")
      }
      if (output.trim != "OK") {
        fail(s"Unexpected typed Wasmtime host output:\n$output")
      }
    } finally {
      deleteRecursive(hostDir)
      deleteRecursive(compiled.outDir)
    }
  }

  private def runNode(nodeFile: Path): (Int, String) = {
    runCmd(List("node", nodeFile.toAbsolutePath.normalize().toString))
  }

  private def runCmd(cmd: List[String]): (Int, String) = {
    val pb = new ProcessBuilder(cmd.asJava)
    pb.redirectErrorStream(true)
    val p = pb.start()
    val output = new String(p.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val exit = p.waitFor()
    (exit, output)
  }

  private def hasWasmTools: Boolean = hasCmd(List("wasm-tools", "--version"))

  private def hasJco: Boolean = hasCmd(List("jco", "--version"))

  private def hasNode: Boolean = hasCmd(List("node", "--version"))

  private def hasCargoStable: Boolean = hasCmd(List("cargo", "+stable", "--version"))

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

  private def compileArtifacts(): CompiledArtifacts = {
    val outDir = Files.createTempDirectory("flix-llvm-wasm-export-out-")
    val flix = new Flix()
    flix.setOptions(TestOptions.copy(outputPath = outDir, artifactName = ArtifactName))
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted
    flix.addFile(ExampleSourceFile)

    val (optRoot, errors) = flix.check()
    if (errors.nonEmpty) {
      fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
    }

    flix.codeGen(optRoot.get)

    CompiledArtifacts(
      outDir = outDir,
      sdkManifest = ca.uwaterloo.flix.language.phase.llvm.LlvmExportSdkWriter.wasmManifestPath(outDir),
      bindingsJs = ca.uwaterloo.flix.language.phase.llvm.LlvmExportSdkWriter.wasmBindingsJsPath(outDir, ArtifactName),
      bindingsTypes = ca.uwaterloo.flix.language.phase.llvm.LlvmExportSdkWriter.wasmBindingsTypesPath(outDir, ArtifactName),
      typedExportComponent = ca.uwaterloo.flix.language.phase.llvm.LlvmExportSdkWriter.wasmComponentPath(outDir, ArtifactName),
      typedWitDir = ca.uwaterloo.flix.language.phase.llvm.LlvmExportSdkWriter.wasmWitDir(outDir)
    )
  }

}
