import * as fs from "node:fs/promises";
import { runtime } from "../../build/wasm/llvm/wasm/js/wasm-effects-recursive-smoke.component.js";
import { FlixRunner } from "../../build/wasm/llvm/wasm-runner-js/runner.mjs";
import { makeUnknownHandler } from "../../generated/js/index.mjs";

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

const exportsManifest = JSON.parse(
  await fs.readFile(new URL("../../build/wasm/llvm/wasm-effects-recursive-smoke.exports.json", import.meta.url), "utf8"),
);
const def = exportsManifest.defs.find((d) => d.symbol === "Api.callUsers");
assert(def, "missing Api.callUsers export");

const unknown = await makeUnknownHandler(
  new URL("../../build/wasm/llvm/wasm-effects-recursive-smoke.effects.json", import.meta.url),
  {
    hostDemoUsers: {
      pairUsers: async (users) => users.map(([name, score]) => [name.toUpperCase(), score + 10]),
      promote: async (user) =>
        user == null
          ? { tag: "err", val: 0 }
          : { tag: "ok", val: [`${user[0]}!`, user[1] + 1] },
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
    assert(result === "alice!:11|ALICE:11,BOB:12", `bad result: ${result}`);
    console.log("OK");
  } finally {
    maybeDispose(out.val);
  }
} finally {
  maybeDispose(arg);
  maybeDispose(ctx);
}
