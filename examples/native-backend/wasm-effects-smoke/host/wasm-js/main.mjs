import * as fs from "node:fs/promises";
import { runtime } from "../../build/wasm/llvm/wasm/js/wasm-effects-smoke.component.js";
import { FlixRunner } from "../../build/wasm/llvm/wasm-runner-js/runner.mjs";
import { makeUnknownHandler } from "../../build/wasm/generated/bindings/wasm/00-Wit/js/index.mjs";

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

const table = new Map();
let nextId = 1n;

const exportsManifest = JSON.parse(
  await fs.readFile(new URL("../../build/wasm/llvm/wasm-effects-smoke.exports.json", import.meta.url), "utf8"),
);
const def = exportsManifest.defs.find((d) => d.symbol === "Api.callCounter");
assert(def, "missing Api.callCounter export");

const unknown = await makeUnknownHandler(
  new URL("../../build/wasm/llvm/wasm-effects-smoke.effects.json", import.meta.url),
  {
    hostDemoCounters: {
      counterNew: async (seed) => {
        const handle = { __resource: "counter", __ownership: "own", id: nextId++ };
        table.set(handle.id, seed);
        return handle;
      },
      counterGet: async (counter) => {
        assert(table.has(counter.id), `missing counter ${counter.id.toString()}`);
        return table.get(counter.id);
      },
      counterAdd: async (counter, delta) => {
        const next = table.get(counter.id) + delta;
        table.set(counter.id, next);
        return next;
      },
      counterDuplicate: async (src) => {
        const handle = { __resource: "counter", __ownership: "own", id: nextId++ };
        table.set(handle.id, table.get(src.id));
        return handle;
      },
      counterDrop: async (counter) => {
        table.delete(counter.id);
      },
    },
  },
);

const runner = new FlixRunner(runtime, { handlers: { unknown } });
const ctx = runtime.newCtx();
const arg = runtime.boxI32(ctx, 10);

try {
  const taskId = runtime.startTask(ctx, BigInt(def.defId), [arg]);
  const out = await runner.runTaskToCompletion(ctx, taskId);
  try {
    assert(out.tag === "ok", `unexpected task outcome: ${JSON.stringify(out)}`);
    const result = runtime.unboxString(ctx, out.val);
    assert(result === "10:15:15", `bad result: ${result}`);
    assert(table.size === 0, `resource leak: ${table.size}`);
    console.log("OK");
  } finally {
    maybeDispose(out.val);
  }
} finally {
  maybeDispose(arg);
  maybeDispose(ctx);
}
