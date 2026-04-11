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

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

class WasmImportsLlvmWasmSuite extends AnyFunSuite {

  private val SyncArtifactName = "extern-wasm-sync"
  private val AggregateArtifactName = "extern-wasm-aggregate"

  private val TestOptions: Options =
    Options.TestWithLibAll.copy(
      stdlibProfile = StdlibProfile.Portable,
      target = CompilationTarget.LlvmWasm,
      incremental = false,
      outputJvm = false,
    )

  private val SyncFixtureFile: Path =
    Paths.get("main/test/flix/wasm/apps/extern_wasm_sync/Main.flix")

  private val AggregateFixtureFile: Path =
    Paths.get("main/test/flix/wasm/apps/extern_wasm_aggregate/Main.flix")

  private case class CompiledArtifacts(outDir: Path,
                                       bindingsJs: Path,
                                       bindingsTypes: Path,
                                       component: Path,
                                       witDir: Path)

  test("llvm-wasm-extern-imports-js-host") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm extern import test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm extern import test)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm extern import test)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm extern import test)")

    val compiled = compileArtifacts(SyncFixtureFile, SyncArtifactName)
    val nodeFile = Files.createTempFile("flix-llvm-wasm-extern-import-js-", ".mjs")
    try {
      val bindingsText = Files.readString(compiled.bindingsJs, StandardCharsets.UTF_8)
      assert(bindingsText.contains("configureImports"))
      assert(bindingsText.contains("\"host:math/basic@0.1.0\""))

      val nodeProgram =
        s"""
           |import { configureImports, newCtx, Exports } from ${jsStringLiteral(compiled.bindingsJs.toUri.toString)};
           |
           |function assert(cond, msg) {
           |  if (!cond) throw new Error(msg);
           |}
           |
           |configureImports({
           |  "host:math/basic@0.1.0": {
           |    cos: (x) => Math.cos(x),
           |    neg: (x) => -x,
           |    iseven: (x) => (x % 2) === 0,
           |  },
           |});
           |
           |const disposeSym = Symbol.dispose ?? Symbol.for("dispose");
           |const ctx = newCtx();
           |try {
           |  const cosRes = Exports.Api.callCos(ctx, 1.0);
           |  assert(cosRes.tag === "ok" && Math.abs(cosRes.val - Math.cos(1.0)) < 1e-12, `bad cos: $${JSON.stringify(cosRes)}`);
           |
           |  const negRes = Exports.Api.callNeg(ctx, -42);
           |  assert(negRes.tag === "ok" && negRes.val === 42, `bad neg: $${JSON.stringify(negRes)}`);
           |
           |  const evenRes = Exports.Api.callIsEven(ctx, 8);
           |  assert(evenRes.tag === "ok" && evenRes.val === true, `bad even: $${JSON.stringify(evenRes)}`);
           |
           |  const oddRes = Exports.Api.callIsEven(ctx, 7);
           |  assert(oddRes.tag === "ok" && oddRes.val === false, `bad odd: $${JSON.stringify(oddRes)}`);
           |
           |  console.log("OK");
           |} finally {
           |  try {
           |    const fn = ctx?.[disposeSym];
           |    if (typeof fn === "function") fn.call(ctx);
           |  } catch {}
           |}
           |""".stripMargin

      Files.writeString(nodeFile, nodeProgram, StandardCharsets.UTF_8)

      val (exit, output) = runCmd(List("node", nodeFile.toString))
      if (exit != 0) {
        fail(s"JS extern wasm host failed with exit $exit:\n$output")
      }
      if (output.trim != "OK") {
        fail(s"Unexpected JS extern wasm host output:\n$output")
      }
    } finally {
      Files.deleteIfExists(nodeFile)
      deleteRecursive(compiled.outDir)
    }
  }

  test("llvm-wasm-extern-imports-wasmtime-host") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm extern import test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm extern import test)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm extern import test)")
    assume(hasCargoStable, "cargo +stable not available (skipping LLVM-wasm extern import test)")

    val compiled = compileArtifacts(SyncFixtureFile, SyncArtifactName)
    val hostDir = Files.createTempDirectory("flix-llvm-wasm-extern-import-wasmtime-")
    try {
      Files.createDirectories(hostDir.resolve("src"))
      Files.writeString(hostDir.resolve("Cargo.toml"),
        """
          |[package]
          |name = "extern_import_host"
          |version = "0.1.0"
          |edition = "2021"
          |
          |[dependencies]
          |anyhow = "1"
          |wasmtime = { version = "38", features = ["component-model"] }
          |""".stripMargin,
        StandardCharsets.UTF_8)

      Files.writeString(hostDir.resolve("src").resolve("main.rs"),
        s"""
           |use anyhow::{bail, Context, Result};
           |use wasmtime::{
           |    Config, Engine, Store,
           |    component::{Component, HasSelf, Linker},
           |};
           |
           |mod bindings {
           |    wasmtime::component::bindgen!({
           |        path: ${jsStringLiteral(compiled.witDir.toString)},
           |        world: "flix",
           |    });
           |}
           |
           |use bindings::exports::flix::exports::api;
           |use bindings::flix::sys::sys::{Capability, Host as SysHost, LogLevel};
           |use bindings::host::math::basic::Host as MathHost;
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
           |impl MathHost for State {
           |    fn cos(&mut self, x: f64) -> f64 { x.cos() }
           |    fn neg(&mut self, x: i32) -> i32 { -x }
           |    fn iseven(&mut self, x: i32) -> bool { x % 2 == 0 }
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
           |    bindings::host::math::basic::add_to_linker::<_, HasSelf<_>>(&mut linker, |s| s)?;
           |
           |    let mut store = Store::new(&engine, State::default());
           |    let flix = bindings::Flix::instantiate(&mut store, &component, &linker)?;
           |    let api = flix.flix_exports_api();
           |    let ctx_api = api.ctx();
           |    let ctx = ctx_api.call_constructor(&mut store)?;
           |
           |    match ctx_api.call_api_callcos(&mut store, ctx, 1.0)? {
           |        api::ExecFloat64::Ok(v) if (v - 1.0f64.cos()).abs() < 1e-12 => {}
           |        other => bail!("bad cos result: {:?}", other),
           |    }
           |
           |    match ctx_api.call_api_callneg(&mut store, ctx, -42)? {
           |        api::ExecInt32::Ok(v) if v == 42 => {}
           |        other => bail!("bad neg result: {:?}", other),
           |    }
           |
           |    match ctx_api.call_api_calliseven(&mut store, ctx, 8)? {
           |        api::ExecBool::Ok(v) if v => {}
           |        other => bail!("bad isEven(8): {:?}", other),
           |    }
           |
           |    match ctx_api.call_api_calliseven(&mut store, ctx, 7)? {
           |        api::ExecBool::Ok(v) if !v => {}
           |        other => bail!("bad isEven(7): {:?}", other),
           |    }
           |
           |    println!("OK");
           |    Ok(())
           |}
           |""".stripMargin,
        StandardCharsets.UTF_8)

      val (exit, output) = runCmd(List(
        "cargo", "+stable", "run", "--quiet",
        "--manifest-path", hostDir.resolve("Cargo.toml").toString,
        "--", compiled.component.toString
      ))
      if (exit != 0) {
        fail(s"Wasmtime extern wasm host failed with exit $exit:\n$output")
      }
      if (output.trim != "OK") {
        fail(s"Unexpected Wasmtime extern wasm host output:\n$output")
      }
    } finally {
      deleteRecursive(hostDir)
      deleteRecursive(compiled.outDir)
    }
  }

  test("llvm-wasm-extern-imports-aggregates-js-host") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm extern import aggregate test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm extern import aggregate test)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm extern import aggregate test)")
    assume(hasNode, "node not found on PATH (skipping LLVM-wasm extern import aggregate test)")

    val compiled = compileArtifacts(AggregateFixtureFile, AggregateArtifactName)
    val nodeFile = Files.createTempFile("flix-llvm-wasm-extern-import-agg-js-", ".mjs")
    try {
      val nodeProgram =
        s"""
           |import { configureImports, newCtx, Exports } from ${jsStringLiteral(compiled.bindingsJs.toUri.toString)};
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
           |configureImports({
           |  "host:demo/basic@0.1.0": {
           |    echoText: (s) => s,
           |    bytesId: (xs) => new Uint8Array(xs),
           |    badge: (user) => ({ label: `${"$"}{user.name}!`, score: user.score + 1 }),
           |    prepend: (xs) => [0, ...xs],
           |    echoInts: (xs) => [...xs],
           |    echoNames: (xs) => [...xs],
           |    flip: (x) => [x[1], x[0]],
           |    flipPairs: (xs) => xs.map(([n, s]) => [s, n]),
           |    maybeInc: (x) => x === null ? null : x + 1,
           |    echoMaybeInts: (xs) => [...xs],
           |    halfEven: (x) => (x % 2) === 0 ? { tag: "ok", val: x / 2 } : { tag: "err", val: "odd" },
           |    promoteUsers: (xs) => xs.map((u) => ({ label: u.name, score: u.score + 1 })),
           |    echoUserArray: (xs) => xs.map((u) => ({ name: u.name, score: u.score })),
           |  },
           |});
           |
           |const disposeSym = Symbol.dispose ?? Symbol.for("dispose");
           |const ctx = newCtx();
           |try {
           |  const text = Exports.Api.callEchoText(ctx, "hello");
           |  assert(text.tag === "ok" && text.val === "hello", `bad echoText: $${JSON.stringify(text)}`);
           |
           |  const bytesData = new Uint8Array([0, 1, 2, 255]);
           |  const bytes = Exports.Api.callBytesId(ctx, bytesData);
           |  assert(bytes.tag === "ok" && eqBytes(bytes.val, bytesData), `bad bytesId: $${JSON.stringify(bytes)}`);
           |
           |  const badge = Exports.Api.callBadge(ctx, { name: "Ada", score: 7 });
           |  assert(badge.tag === "ok" && badge.val.label === "Ada!" && badge.val.score === 8, `bad badge: $${JSON.stringify(badge)}`);
           |
           |  const prep = Exports.Api.callPrepend(ctx, [1, 2, 3]);
           |  assert(prep.tag === "ok" && JSON.stringify(prep.val) === JSON.stringify([0, 1, 2, 3]), `bad prepend: $${JSON.stringify(prep)}`);
           |
           |  const ints = Exports.Api.callEchoInts(ctx, [1, 2, 3]);
           |  assert(ints.tag === "ok" && JSON.stringify(ints.val) === JSON.stringify([1, 2, 3]), `bad echoInts: $${JSON.stringify(ints)}`);
           |
           |  const names = Exports.Api.callEchoNames(ctx, ["Ada", "Flix"]);
           |  assert(names.tag === "ok" && JSON.stringify(names.val) === JSON.stringify(["Ada", "Flix"]), `bad echoNames: $${JSON.stringify(names)}`);
           |
           |  const flip = Exports.Api.callFlip(ctx, ["flix", 9]);
           |  assert(flip.tag === "ok" && JSON.stringify(flip.val) === JSON.stringify([9, "flix"]), `bad flip: $${JSON.stringify(flip)}`);
           |
           |  const flipPairs = Exports.Api.callFlipPairs(ctx, [[7, "Ada"], [9, "Flix"]]);
           |  assert(flipPairs.tag === "ok" && JSON.stringify(flipPairs.val) === JSON.stringify([["Ada", 7], ["Flix", 9]]), `bad flipPairs: $${JSON.stringify(flipPairs)}`);
           |
           |  const some = Exports.Api.callMaybeInc(ctx, 41);
           |  assert(some.tag === "ok" && some.val === 42, `bad some: $${JSON.stringify(some)}`);
           |
           |  const none = Exports.Api.callMaybeInc(ctx, null);
           |  assert(none.tag === "ok" && none.val === null, `bad none: $${JSON.stringify(none)}`);
           |
           |  const maybeInts = Exports.Api.callEchoMaybeInts(ctx, [41, null, 9]);
           |  assert(maybeInts.tag === "ok" && JSON.stringify(maybeInts.val) === JSON.stringify([41, null, 9]), `bad echoMaybeInts: $${JSON.stringify(maybeInts)}`);
           |
           |  const even = Exports.Api.callHalfEven(ctx, 8);
           |  assert(even.tag === "ok" && even.val.tag === "ok" && even.val.val === 4, `bad halfEven(8): $${JSON.stringify(even)}`);
           |
           |  const odd = Exports.Api.callHalfEven(ctx, 7);
           |  assert(odd.tag === "ok" && odd.val.tag === "err" && odd.val.val === "odd", `bad halfEven(7): $${JSON.stringify(odd)}`);
           |
           |  const promoted = Exports.Api.callPromoteUsers(ctx, [{ name: "Ada", score: 7 }, { name: "Flix", score: 9 }]);
           |  assert(
           |    promoted.tag === "ok" &&
           |      JSON.stringify(promoted.val) === JSON.stringify([{ label: "Ada", score: 8 }, { label: "Flix", score: 10 }]),
           |    `bad promoteUsers: $${JSON.stringify(promoted)}`
           |  );
           |
           |  const userArray = Exports.Api.callEchoUserArray(ctx, [{ name: "Ada", score: 7 }, { name: "Flix", score: 9 }]);
           |  assert(
           |    userArray.tag === "ok" &&
           |      JSON.stringify(userArray.val) === JSON.stringify([{ name: "Ada", score: 7 }, { name: "Flix", score: 9 }]),
           |    `bad echoUserArray: $${JSON.stringify(userArray)}`
           |  );
           |
           |  console.log("OK");
           |} finally {
           |  try {
           |    const fn = ctx?.[disposeSym];
           |    if (typeof fn === "function") fn.call(ctx);
           |  } catch {}
           |}
           |""".stripMargin

      Files.writeString(nodeFile, nodeProgram, StandardCharsets.UTF_8)
      val (exit, output) = runCmd(List("node", nodeFile.toString))
      if (exit != 0) fail(s"JS aggregate extern wasm host failed with exit $exit:\n$output")
      if (output.trim != "OK") fail(s"Unexpected JS aggregate extern wasm host output:\n$output")
    } finally {
      Files.deleteIfExists(nodeFile)
      deleteRecursive(compiled.outDir)
    }
  }

  test("llvm-wasm-extern-imports-aggregates-wasmtime-host") {
    assume(hasZig, "zig not found on PATH (skipping LLVM-wasm extern import aggregate test)")
    assume(hasWasmTools, "wasm-tools not found on PATH (skipping LLVM-wasm extern import aggregate test)")
    assume(hasJco, "jco not found on PATH (skipping LLVM-wasm extern import aggregate test)")
    assume(hasCargoStable, "cargo +stable not available (skipping LLVM-wasm extern import aggregate test)")

    val compiled = compileArtifacts(AggregateFixtureFile, AggregateArtifactName)
    val hostDir = Files.createTempDirectory("flix-llvm-wasm-extern-import-agg-wasmtime-")
    try {
      Files.createDirectories(hostDir.resolve("src"))
      Files.writeString(hostDir.resolve("Cargo.toml"),
        """
          |[package]
          |name = "extern_import_aggregate_host"
          |version = "0.1.0"
          |edition = "2021"
          |
          |[dependencies]
          |anyhow = "1"
          |wasmtime = { version = "38", features = ["component-model"] }
          |""".stripMargin,
        StandardCharsets.UTF_8)

      Files.writeString(hostDir.resolve("src").resolve("main.rs"),
        s"""
           |use anyhow::{bail, Context, Result};
           |use wasmtime::{
           |    Config, Engine, Store,
           |    component::{Component, HasSelf, Linker},
           |};
           |
           |mod bindings {
           |    wasmtime::component::bindgen!({
           |        path: ${jsStringLiteral(compiled.witDir.toString)},
           |        world: "flix",
           |    });
           |}
           |
           |use bindings::exports::flix::exports::api;
           |use bindings::flix::sys::sys::{Capability, Host as SysHost, LogLevel};
           |use bindings::host::demo::basic::{
           |    Host as DemoHost,
           |    ResultInt32String,
           |    OptionInt32,
           |    RecordLabelStringScoreInt32,
           |    RecordNameStringScoreInt32,
           |    Tuple2StringInt32,
           |    Tuple2Int32String,
           |};
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
           |impl DemoHost for State {
           |    fn echo_text(&mut self, s: String) -> String {
           |        s
           |    }
           |
           |    fn bytes_id(&mut self, xs: Vec<u8>) -> Vec<u8> {
           |        xs
           |    }
           |
           |    fn badge(&mut self, user: RecordNameStringScoreInt32) -> RecordLabelStringScoreInt32 {
           |        RecordLabelStringScoreInt32 { label: format!("{}!", user.name), score: user.score + 1 }
           |    }
           |
           |    fn prepend(&mut self, xs: Vec<i32>) -> Vec<i32> {
           |        let mut out = Vec::with_capacity(xs.len() + 1);
           |        out.push(0);
           |        out.extend(xs);
           |        out
           |    }
           |
           |    fn echo_ints(&mut self, xs: Vec<i32>) -> Vec<i32> {
           |        xs
           |    }
           |
           |    fn echo_names(&mut self, xs: Vec<String>) -> Vec<String> {
           |        xs
           |    }
           |
           |    fn flip(&mut self, x: Tuple2StringInt32) -> Tuple2Int32String {
           |        Tuple2Int32String { f0: x.f1, f1: x.f0 }
           |    }
           |
           |    fn flip_pairs(&mut self, xs: Vec<Tuple2Int32String>) -> Vec<Tuple2StringInt32> {
           |        xs.into_iter().map(|x| Tuple2StringInt32 { f0: x.f1, f1: x.f0 }).collect()
           |    }
           |
           |    fn maybe_inc(&mut self, x: OptionInt32) -> OptionInt32 {
           |        if x.is_some {
           |            OptionInt32 { is_some: true, val: x.val + 1 }
           |        } else {
           |            OptionInt32 { is_some: false, val: 0 }
           |        }
           |    }
           |
           |    fn echo_maybe_ints(&mut self, xs: Vec<OptionInt32>) -> Vec<OptionInt32> {
           |        xs
           |    }
           |
           |    fn half_even(&mut self, x: i32) -> ResultInt32String {
           |        if x % 2 == 0 {
           |            ResultInt32String { is_ok: true, ok: x / 2, err: String::new() }
           |        } else {
           |            ResultInt32String { is_ok: false, ok: 0, err: "odd".to_string() }
           |        }
           |    }
           |
           |    fn promote_users(&mut self, xs: Vec<RecordNameStringScoreInt32>) -> Vec<RecordLabelStringScoreInt32> {
           |        xs.into_iter().map(|u| RecordLabelStringScoreInt32 { label: u.name, score: u.score + 1 }).collect()
           |    }
           |
           |    fn echo_user_array(&mut self, xs: Vec<RecordNameStringScoreInt32>) -> Vec<RecordNameStringScoreInt32> {
           |        xs
           |    }
           |}
           |
           |fn main() -> Result<()> {
           |    let component_path = std::env::args().nth(1).context("missing component path")?;
           |
           |    let mut config = Config::new();
           |    config.wasm_component_model(true);
           |    let engine = Engine::new(&config)?;
           |    let component = Component::from_file(&engine, &component_path)
           |        .with_context(|| format!("failed to load component: {}", component_path))?;
           |
           |    let mut linker = Linker::<State>::new(&engine);
           |    bindings::flix::sys::sys::add_to_linker::<_, HasSelf<_>>(&mut linker, |s| s)?;
           |    bindings::host::demo::basic::add_to_linker::<_, HasSelf<_>>(&mut linker, |s| s)?;
           |
           |    let mut store = Store::new(&engine, State::default());
           |    let flix = bindings::Flix::instantiate(&mut store, &component, &linker)?;
           |    let api = flix.flix_exports_api();
           |    let ctx_api = api.ctx();
           |    let ctx = ctx_api.call_constructor(&mut store)?;
           |
           |    match ctx_api.call_api_callechotext(&mut store, ctx, "hello")? {
           |        api::ExecString::Ok(v) if v == "hello" => {}
           |        other => bail!("bad echoText result: {:?}", other),
           |    }
           |
           |    let bytes_in = vec![0u8, 1, 2, 255];
           |    match ctx_api.call_api_callbytesid(&mut store, ctx, &bytes_in)? {
           |        api::ExecBytes::Ok(v) if v == bytes_in => {}
           |        other => bail!("bad bytesId result: {:?}", other),
           |    }
           |
           |    match ctx_api.call_api_callbadge(&mut store, ctx, &api::RecordNameStringScoreInt32 { name: "Ada".to_string(), score: 7 })? {
           |        api::ExecRecordLabelStringScoreInt32::Ok(v) if v.label == "Ada!" && v.score == 8 => {}
           |        other => bail!("bad badge result: {:?}", other),
           |    }
           |
           |    match ctx_api.call_api_callprepend(&mut store, ctx, &vec![1, 2, 3])? {
           |        api::ExecListInt32::Ok(v) if v == vec![0, 1, 2, 3] => {}
           |        other => bail!("bad prepend result: {:?}", other),
           |    }
           |
           |    match ctx_api.call_api_callechoints(&mut store, ctx, &vec![1, 2, 3])? {
           |        api::ExecArrayInt32::Ok(v) if v == vec![1, 2, 3] => {}
           |        other => bail!("bad echoInts result: {:?}", other),
           |    }
           |
           |    match ctx_api.call_api_callechonames(&mut store, ctx, &vec!["Ada".to_string(), "Flix".to_string()])? {
           |        api::ExecArrayString::Ok(v) if v == vec!["Ada".to_string(), "Flix".to_string()] => {}
           |        other => bail!("bad echoNames result: {:?}", other),
           |    }
           |
           |    match ctx_api.call_api_callflip(&mut store, ctx, &api::Tuple2StringInt32 { f0: "flix".to_string(), f1: 9 })? {
           |        api::ExecTuple2Int32String::Ok(v) if v.f0 == 9 && v.f1 == "flix" => {}
           |        other => bail!("bad flip result: {:?}", other),
           |    }
           |
           |    match ctx_api.call_api_callflippairs(&mut store, ctx, &vec![
           |        api::Tuple2Int32String { f0: 7, f1: "Ada".to_string() },
           |        api::Tuple2Int32String { f0: 9, f1: "Flix".to_string() },
           |    ])? {
           |        api::ExecListTuple2StringInt32::Ok(v)
           |            if v.len() == 2
           |                && v[0].f0 == "Ada"
           |                && v[0].f1 == 7
           |                && v[1].f0 == "Flix"
           |                && v[1].f1 == 9 => {}
           |        other => bail!("bad flipPairs result: {:?}", other),
           |    }
           |
           |    match ctx_api.call_api_callmaybeinc(&mut store, ctx, api::OptionInt32 { is_some: true, val: 41 })? {
           |        api::ExecOptionInt32::Ok(v) if v.is_some && v.val == 42 => {}
           |        other => bail!("bad some result: {:?}", other),
           |    }
           |
           |    match ctx_api.call_api_callmaybeinc(&mut store, ctx, api::OptionInt32 { is_some: false, val: 0 })? {
           |        api::ExecOptionInt32::Ok(v) if !v.is_some => {}
           |        other => bail!("bad none result: {:?}", other),
           |    }
           |
           |    match ctx_api.call_api_callechomaybeints(&mut store, ctx, &vec![
           |        api::OptionInt32 { is_some: true, val: 41 },
           |        api::OptionInt32 { is_some: false, val: 0 },
           |        api::OptionInt32 { is_some: true, val: 9 },
           |    ])? {
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
           |    match ctx_api.call_api_callhalfeven(&mut store, ctx, 8)? {
           |        api::ExecResultInt32String::Ok(v) if v.is_ok && v.ok == 4 => {}
           |        other => bail!("bad halfEven(8) result: {:?}", other),
           |    }
           |
           |    match ctx_api.call_api_callhalfeven(&mut store, ctx, 7)? {
           |        api::ExecResultInt32String::Ok(v) if !v.is_ok && v.err == "odd" => {}
           |        other => bail!("bad halfEven(7) result: {:?}", other),
           |    }
           |
           |    match ctx_api.call_api_callpromoteusers(&mut store, ctx, &vec![
           |        api::RecordNameStringScoreInt32 { name: "Ada".to_string(), score: 7 },
           |        api::RecordNameStringScoreInt32 { name: "Flix".to_string(), score: 9 },
           |    ])? {
           |        api::ExecListRecordLabelStringScoreInt32::Ok(v)
           |            if v.len() == 2
           |                && v[0].label == "Ada"
           |                && v[0].score == 8
           |                && v[1].label == "Flix"
           |                && v[1].score == 10 => {}
           |        other => bail!("bad promoteUsers result: {:?}", other),
           |    }
           |
           |    match ctx_api.call_api_callechouserarray(&mut store, ctx, &vec![
           |        api::RecordNameStringScoreInt32 { name: "Ada".to_string(), score: 7 },
           |        api::RecordNameStringScoreInt32 { name: "Flix".to_string(), score: 9 },
           |    ])? {
           |        api::ExecArrayRecordNameStringScoreInt32::Ok(v)
           |            if v.len() == 2
           |                && v[0].name == "Ada"
           |                && v[0].score == 7
           |                && v[1].name == "Flix"
           |                && v[1].score == 9 => {}
           |        other => bail!("bad echoUserArray result: {:?}", other),
           |    }
           |
           |    println!("OK");
           |    Ok(())
           |}
           |""".stripMargin,
        StandardCharsets.UTF_8)

      val (exit, output) = runCmd(List(
        "cargo", "+stable", "run", "--quiet",
        "--manifest-path", hostDir.resolve("Cargo.toml").toString,
        "--", compiled.component.toString
      ))
      if (exit != 0) fail(s"Wasmtime aggregate extern wasm host failed with exit $exit:\n$output")
      if (output.trim != "OK") fail(s"Unexpected Wasmtime aggregate extern wasm host output:\n$output")
    } finally {
      deleteRecursive(hostDir)
      deleteRecursive(compiled.outDir)
    }
  }

  private def compileArtifacts(fixtureFile: Path, artifactName: String): CompiledArtifacts = {
    val outDir = Files.createTempDirectory("flix-llvm-wasm-import-out-")
    val flix = new Flix()
    flix.setOptions(TestOptions.copy(outputPath = outDir, artifactName = artifactName))
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted
    flix.addFile(fixtureFile)

    val (optRoot, errors) = flix.check()
    if (errors.nonEmpty) {
      fail(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
    }

    flix.codeGen(optRoot.get)

    CompiledArtifacts(
      outDir = outDir,
      bindingsJs = ca.uwaterloo.flix.language.phase.llvm.LlvmExportSdkWriter.wasmBindingsJsPath(outDir, artifactName),
      bindingsTypes = ca.uwaterloo.flix.language.phase.llvm.LlvmExportSdkWriter.wasmBindingsTypesPath(outDir, artifactName),
      component = ca.uwaterloo.flix.language.phase.llvm.LlvmExportSdkWriter.wasmComponentPath(outDir, artifactName),
      witDir = ca.uwaterloo.flix.language.phase.llvm.LlvmExportSdkWriter.wasmWitDir(outDir),
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

  private def hasCommand(cmd: List[String]): Boolean = {
    val pb = new ProcessBuilder(cmd.asJava)
    pb.redirectErrorStream(true)
    try {
      val p = pb.start()
      val finished = p.waitFor()
      finished == 0
    } catch {
      case _: Throwable => false
    }
  }
  private def hasWasmTools: Boolean = hasCommand(List("wasm-tools", "--version"))
  private def hasJco: Boolean = hasCommand(List("jco", "--version"))
  private def hasNode: Boolean = hasCommand(List("node", "--version"))
  private def hasCargoStable: Boolean = hasCommand(List("cargo", "+stable", "--version"))
}
