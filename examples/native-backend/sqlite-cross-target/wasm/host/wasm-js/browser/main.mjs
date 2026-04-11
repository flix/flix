import { runtime } from "../../../build/wasm/llvm/wasm/js/sqlite-wasm.component.js";
import { FlixRunner } from "../../../build/wasm/llvm/wasm-runner-js/runner.mjs";
import { makeUnknownHandler } from "../../../build/wasm/generated/bindings/wasm/00-Wit/js/index.mjs";
import { createBrowserSqliteBridgeClient } from "./sqlite-worker-client.mjs";

const disposeSym = Symbol.dispose ?? Symbol.for("dispose");
const output = document.querySelector("#output");
const status = document.querySelector("#status");

if (!Array.isArray(globalThis.__flix_console_lines)) {
  globalThis.__flix_console_lines = [];
}

function log(msg) {
  output.textContent += `${msg}\n`;
  globalThis.__flix_console_lines.push(String(msg));
}

function setStatus(msg) {
  if (status) status.textContent = msg;
}

function setOutcome(tag) {
  document.documentElement?.setAttribute("data-flix-status", tag);
}

function maybeDispose(x) {
  try {
    const fn = x?.[disposeSym];
    if (typeof fn === "function") fn.call(x);
  } catch {}
}

async function main() {
  setOutcome("running");
  setStatus("running…");
  output.textContent = "";
  const exportsManifest = await fetch(new URL("../../../build/wasm/llvm/sqlite-wasm.exports.json", import.meta.url)).then((r) => r.json());
  const def = exportsManifest.defs.find((d) => d.symbol === "Api.runDemo");
  if (!def) throw new Error("missing Api.runDemo export");

  const client = await createBrowserSqliteBridgeClient(new URL("./sqlite-worker.mjs", import.meta.url));
  const unknown = await makeUnknownHandler(
    new URL("../../../build/wasm/llvm/sqlite-wasm.effects.json", import.meta.url),
    { exampleSqliteBridge: client.exampleSqliteBridge },
  );

  const runner = new FlixRunner(runtime, { handlers: { unknown } });
  const ctx = runtime.newCtx();

  try {
    const taskId = runtime.startTask(ctx, BigInt(def.defId), []);
    const out = await runner.runTaskToCompletion(ctx, taskId);
    try {
      if (out.tag !== "ok") {
        throw new Error(`unexpected task outcome: ${JSON.stringify(out)}`);
      }
      const result = runtime.unboxString(ctx, out.val);
      log(result);
      setStatus(result);
      setOutcome("ok");
    } finally {
      maybeDispose(out.val);
    }
  } finally {
    maybeDispose(ctx);
    client.close();
  }
}

main().catch((err) => {
  const message = err instanceof Error ? err.message : String(err);
  output.textContent = "";
  log(`ERROR: ${message}`);
  setStatus(message);
  setOutcome("error");
  console.error(err);
});
