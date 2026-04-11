#!/usr/bin/env node
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { pathToFileURL } from "node:url";

import { FlixRunner } from "./runner.mjs";
import { makeNodeFsHandlers } from "./node-handlers.mjs";
import { makeNodeTcpHandlers } from "./node-tcp-handlers.mjs";
import { makeNodeProcessHandlers } from "./node-process-handlers.mjs";

const disposeSym = Symbol.dispose ?? Symbol.for("dispose");
function maybeDispose(x) {
  try {
    const fn = x?.[disposeSym];
    if (typeof fn === "function") fn.call(x);
  } catch {
    // Ignore dispose errors; this is best-effort cleanup for a CLI tool.
  }
}

function usage(code) {
  const msg = `usage:
  node tools/wasm-runner-js/run-flix.mjs --js <component.js> [options]

options:
  --exports <flix_wasm_exports.json>   Default: sibling ../../flix_wasm_exports.json (relative to --js)
  --defId <u64>                        Run this def-id instead of selecting 'isMain'
  --symbol <sym>                       Run the def with this symbol (requires --exports)
  --arg <value>                        Add a positional argument (typed via manifest params)
  --argv <string>                      Add a program argv entry (visible via Env.getArgs)
  --stdinLine <string>                 Queue a line for Console.readln (may be repeated)
  --rootDir <dir>                      Root dir for Node filesystem handlers (default: cwd)
  --budget <u32>                       Scheduler budget per step (default: 100)
  --maxRedirects <u32>                 HTTP redirect limit (default: 20)
  --httpTimeoutMs <u32>                HTTP timeout (ms). Omit => no timeout
  --printResult                         Print an unboxed result when possible

notes:
  - This runs Flix wasm *components* transpiled by 'jco transpile' (ES module output).
  - Supported scalar argument types: Unit/Bool/Int8/Int16/Int32/Int64/Float32/Float64/String.
  - Supported '--printResult' types: Unit/Bool/Int32/String.
  - Bytes are not yet supported by this CLI runner.
`;
  console.error(msg);
  process.exit(code);
}

function parseArgs(argv) {
  const out = {};
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    switch (a) {
      case "--js":
      case "--exports":
      case "--defId":
      case "--symbol":
      case "--rootDir":
      case "--budget":
      case "--maxRedirects":
      case "--httpTimeoutMs": {
        const v = argv[i + 1];
        if (v == null) usage(2);
        out[a.slice(2)] = v;
        i++;
        break;
      }
      case "--arg": {
        const v = argv[i + 1];
        if (v == null) usage(2);
        if (!Array.isArray(out.arg)) out.arg = [];
        out.arg.push(v);
        i++;
        break;
      }
      case "--argv": {
        const v = argv[i + 1];
        if (v == null) usage(2);
        if (!Array.isArray(out.argv)) out.argv = [];
        out.argv.push(v);
        i++;
        break;
      }
      case "--stdinLine": {
        const v = argv[i + 1];
        if (v == null) usage(2);
        if (!Array.isArray(out.stdinLine)) out.stdinLine = [];
        out.stdinLine.push(v);
        i++;
        break;
      }
      case "--printResult":
        out.printResult = true;
        break;
      case "-h":
      case "--help":
        usage(0);
        break;
      default:
        console.error(`unknown arg: ${a}`);
        usage(2);
    }
  }
  if (!out.js) usage(2);
  return out;
}

function parseU32(x, name) {
  const n = Number.parseInt(x, 10);
  if (!Number.isInteger(n) || n < 0 || n > 0xffffffff) {
    throw new Error(`invalid ${name}: ${x}`);
  }
  return n;
}

function parseU64BigInt(x, name) {
  try {
    const v = BigInt(x);
    if (v < 0n) throw new Error("negative");
    return v;
  } catch {
    throw new Error(`invalid ${name}: ${x}`);
  }
}

function parseI64BigInt(x, name) {
  try {
    const v = BigInt(x);
    const min = -(1n << 63n);
    const max = (1n << 63n) - 1n;
    if (v < min || v > max) throw new Error("out of range");
    return v;
  } catch {
    throw new Error(`invalid ${name}: ${x}`);
  }
}

async function loadExportsManifest(exportsPath) {
  const txt = await fs.readFile(exportsPath, { encoding: "utf8" });
  const json = JSON.parse(txt);
  if (json?.schema !== "flix-llvm-wasm-exports-v0") {
    throw new Error(`unsupported exports manifest schema: ${json?.schema ?? "<missing>"}`);
  }
  const defs = Array.isArray(json.defs) ? json.defs : [];
  return defs;
}

function selectDefId(defs, args) {
  if (args.defId) {
    return parseU64BigInt(args.defId, "--defId");
  }

  if (args.symbol) {
    const hit = defs.find((d) => d?.symbol === args.symbol);
    if (!hit) throw new Error(`symbol not found in exports manifest: ${args.symbol}`);
    return BigInt(hit.defId);
  }

  const main = defs.find((d) => d?.isMain === true);
  if (!main) throw new Error("no main def found (pass --defId or --symbol)");
  return BigInt(main.defId);
}

function getDefMeta(defs, defId) {
  const hit = defs.find((d) => BigInt(d?.defId ?? -1) === defId);
  if (!hit) return null;
  const params = Array.isArray(hit.params) ? hit.params.map(String) : [];
  const result = typeof hit.result === "string" ? hit.result : "";
  return { params, result, symbol: hit.symbol ?? "" };
}

function boxArg(runtime, ctx, tpe, raw) {
  switch (tpe) {
    case "Unit": {
      // Unit is represented as `0` in the v0 runtime layout.
      // Prefer a dedicated `box-unit` once it exists in the substrate.
      return runtime.boxI32(ctx, 0);
    }
    case "String":
      return runtime.boxString(ctx, raw);
    case "Int8": {
      const n = Number.parseInt(raw, 10);
      if (!Number.isInteger(n) || n < -128 || n > 127) {
        throw new Error(`invalid Int8 arg: ${raw}`);
      }
      return runtime.boxI8(ctx, n);
    }
    case "Int16": {
      const n = Number.parseInt(raw, 10);
      if (!Number.isInteger(n) || n < -32768 || n > 32767) {
        throw new Error(`invalid Int16 arg: ${raw}`);
      }
      return runtime.boxI16(ctx, n);
    }
    case "Int32": {
      const n = Number.parseInt(raw, 10);
      if (!Number.isInteger(n) || n < -2147483648 || n > 2147483647) {
        throw new Error(`invalid Int32 arg: ${raw}`);
      }
      return runtime.boxI32(ctx, n);
    }
    case "Int64":
      return runtime.boxI64(ctx, parseI64BigInt(raw, "Int64"));
    case "Float32": {
      const n = Number(raw);
      if (Number.isNaN(n) && raw.toLowerCase() !== "nan") {
        throw new Error(`invalid Float32 arg: ${raw}`);
      }
      return runtime.boxF32(ctx, n);
    }
    case "Float64": {
      const n = Number(raw);
      if (Number.isNaN(n) && raw.toLowerCase() !== "nan") {
        throw new Error(`invalid Float64 arg: ${raw}`);
      }
      return runtime.boxF64(ctx, n);
    }
    case "Bool": {
      if (raw === "true") return runtime.boxBool(ctx, true);
      if (raw === "false") return runtime.boxBool(ctx, false);
      throw new Error(`invalid Bool arg: ${raw} (expected 'true' or 'false')`);
    }
    default:
      throw new Error(`unsupported param type: ${tpe}`);
  }
}

function maybeUnbox(runtime, ctx, tpe, v) {
  switch (tpe) {
    case "Unit":
      return null;
    case "String":
      return runtime.unboxString(ctx, v);
    case "Int32":
      return runtime.unboxI32(ctx, v);
    case "Bool":
      return runtime.unboxBool(ctx, v);
    default:
      throw new Error(`printResult unsupported for result type: ${tpe}`);
  }
}

const args = parseArgs(process.argv.slice(2));

const jsPath = path.resolve(args.js);
const jsUrl = pathToFileURL(jsPath).href;

// Provide program argv entries to the default `flix:sys/sys` host implementation.
globalThis.__flix_args = Array.isArray(args.argv) ? args.argv : [];

// Provide stdin lines to the default runner (`console-readln` suspension handler).
globalThis.__flix_stdin_lines = Array.isArray(args.stdinLine) ? args.stdinLine : [];

const exportsPath =
  args.exports ??
  path.resolve(path.dirname(jsPath), "..", "..", "flix_wasm_exports.json");

const defs = await loadExportsManifest(exportsPath);
const defId = selectDefId(defs, args);
const meta = getDefMeta(defs, defId);
if (!meta) throw new Error(`defId not found in exports manifest: ${defId}`);

const { runtime } = await import(jsUrl);
if (!runtime) throw new Error(`JS module did not export 'runtime': ${jsPath}`);

const ctx = runtime.newCtx();
try {
  const rootDir = path.resolve(args.rootDir ?? process.cwd());

  const runner = new FlixRunner(runtime, {
    budget: args.budget ? parseU32(args.budget, "--budget") : undefined,
    maxRedirects: args.maxRedirects ? parseU32(args.maxRedirects, "--maxRedirects") : undefined,
    httpTimeoutMs: args.httpTimeoutMs ? parseU32(args.httpTimeoutMs, "--httpTimeoutMs") : null,
    handlers: {
      ...makeNodeFsHandlers({ rootDir }),
      ...makeNodeTcpHandlers({ connectTimeoutMs: 2000 }),
      ...makeNodeProcessHandlers(),
    },
  });

  const rawArgs = Array.isArray(args.arg) ? args.arg : [];
  let boxed;
  if (rawArgs.length === meta.params.length) {
    boxed = rawArgs.map((raw, i) => boxArg(runtime, ctx, meta.params[i], raw));
  } else if (rawArgs.length === 0 && meta.params.every((t) => t === "Unit")) {
    boxed = meta.params.map(() => boxArg(runtime, ctx, "Unit", ""));
  } else {
    throw new Error(
      `wrong arity for ${meta.symbol ?? "<def>"} (defId=${defId}): expected ${
        meta.params.length
      } args (${meta.params.join(", ")}), got ${rawArgs.length}`
    );
  }
  try {
    const taskId = runtime.startTask(ctx, defId, boxed);
    const out = await runner.runTaskToCompletion(ctx, taskId);
    try {
      if (out.tag === "ok") {
        if (args.printResult) {
          const unboxed = maybeUnbox(runtime, ctx, meta.result, out.val);
          if (unboxed != null) console.log(unboxed);
        }
        process.exitCode = 0;
      } else if (out.tag === "thrown") {
        // A thrown value is a designated `Exn` value; its structure is not yet part of the
        // handle-based substrate, so we cannot reliably print it here.
        process.exitCode = 1;
      } else {
        throw new Error(`unexpected task outcome tag: ${out.tag}`);
      }
    } finally {
      maybeDispose(out.val);
    }
  } finally {
    boxed.forEach(maybeDispose);
  }
} finally {
  maybeDispose(ctx);
}
