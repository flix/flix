import * as fs from "node:fs/promises";
import sqlite3InitModule from "@sqlite.org/sqlite-wasm";
import { runtime } from "../../build/wasm/llvm/wasm/js/sqlite-wasm.component.js";
import { FlixRunner } from "../../build/wasm/llvm/wasm-runner-js/runner.mjs";
import { makeUnknownHandler } from "../../build/wasm/generated/bindings/wasm/00-Wit/js/index.mjs";
import { SqliteBridgeState } from "./sqlite-bridge-runtime.mjs";

function assert(cond, msg) {
  if (!cond) throw new Error(msg);
}

const disposeSym = Symbol.dispose ?? Symbol.for("dispose");

function maybeDispose(x) {
  try {
    const fn = x?.[disposeSym];
    if (typeof fn === "function") fn.call(x);
  } catch {}
}

const root = new URL("../../", import.meta.url);
const exportsManifest = JSON.parse(
  await fs.readFile(new URL("build/wasm/llvm/sqlite-wasm.exports.json", root), "utf8"),
);
const def = exportsManifest.defs.find((d) => d.symbol === "Api.runDemo");
assert(def, "missing Api.runDemo export");

const sqlite3 = await sqlite3InitModule();
const state = new SqliteBridgeState(sqlite3);
const unknown = await makeUnknownHandler(
  new URL("build/wasm/llvm/sqlite-wasm.effects.json", root),
  state.makeImplementations(),
);

const runner = new FlixRunner(runtime, { handlers: { unknown } });
const ctx = runtime.newCtx();

try {
  const taskId = runtime.startTask(ctx, BigInt(def.defId), []);
  const out = await runner.runTaskToCompletion(ctx, taskId);
  try {
    assert(out.tag === "ok", `unexpected task outcome: ${JSON.stringify(out)}`);
    const result = runtime.unboxString(ctx, out.val);
    assert(result === "1:hello:4", `bad result: ${result}`);
    const counts = state.resourceCounts();
    assert(counts.dbs === 0 && counts.stmts === 0, `resource leak: ${JSON.stringify(counts)}`);
    console.log("OK");
  } finally {
    maybeDispose(out.val);
  }
} finally {
  maybeDispose(ctx);
}
