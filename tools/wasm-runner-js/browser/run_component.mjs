import { FlixRunner } from "../runner.mjs";
import { makeOpfsFsHandlers, makeOpfsSandbox } from "../opfs-handlers.mjs";
import { makeUnsupportedProcessHandlers } from "./process-handlers.mjs";

const disposeSym = Symbol.dispose ?? Symbol.for("dispose");
const consoleLevels = ["log", "info", "warn", "error"];
const maybeDispose = (x) => {
  try {
    const fn = x?.[disposeSym];
    if (typeof fn === "function") fn.call(x);
  } catch {
    // Best-effort cleanup; ignore dispose failures.
  }
};

if (!Array.isArray(globalThis.__flix_console_lines)) {
  globalThis.__flix_console_lines = [];
}

for (const level of consoleLevels) {
  const current = globalThis.console?.[level];
  if (typeof current !== "function") continue;
  if (current.__flixWrapped === true) continue;

  const wrapped = (...args) => {
    const rendered = args.map((x) => String(x)).join(" ").trim();
    if (rendered.length > 0) {
      globalThis.__flix_console_lines.push(level === "log" || level === "info" ? rendered : `[console:${level}] ${rendered}`);
    }
    return current.apply(globalThis.console, args);
  };
  wrapped.__flixWrapped = true;
  globalThis.console[level] = wrapped;
}

function setStatus(msg) {
  const el = globalThis.document?.getElementById?.("status");
  if (el) el.textContent = msg;
}

function setOutcome(tag) {
  const root = globalThis.document?.documentElement;
  if (root) root.setAttribute("data-flix-status", tag);
}

async function ensureParentDir(rootDirHandle, parts) {
  let dir = rootDirHandle;
  for (const part of parts) {
    dir = await dir.getDirectoryHandle(part, { create: true });
  }
  return dir;
}

async function writeSeedFile(rootDirHandle, relPath, bytes) {
  const parts = String(relPath)
    .replaceAll("\\", "/")
    .split("/")
    .filter((p) => p.length > 0 && p !== ".");

  if (parts.length === 0) {
    throw new Error(`invalid seed path: ${relPath}`);
  }

  const parent = await ensureParentDir(rootDirHandle, parts.slice(0, -1));
  const fh = await parent.getFileHandle(parts[parts.length - 1], { create: true });
  const w = await fh.createWritable();
  try {
    await w.write(bytes);
  } finally {
    await w.close();
  }
}

async function preloadSeedFiles(rootDirHandle, seedUrl) {
  const resp = await fetch(seedUrl.toString(), { redirect: "error" });
  if (!resp.ok) {
    throw new Error(`failed to fetch browser seed manifest: ${resp.status}`);
  }

  const json = await resp.json();
  if (json?.schema !== "flix-browser-seed-v0") {
    throw new Error(`unsupported browser seed manifest schema: ${json?.schema ?? "<missing>"}`);
  }

  const files = Array.isArray(json.files) ? json.files : [];
  for (const entry of files) {
    const relPath = typeof entry?.path === "string" ? entry.path : "";
    const url = typeof entry?.url === "string" ? entry.url : "";
    if (relPath.length === 0 || url.length === 0) {
      throw new Error("invalid browser seed manifest entry");
    }

    const fileResp = await fetch(new URL(url, seedUrl), { redirect: "error" });
    if (!fileResp.ok) {
      throw new Error(`failed to fetch seeded file '${relPath}': ${fileResp.status}`);
    }
    const bytes = new Uint8Array(await fileResp.arrayBuffer());
    await writeSeedFile(rootDirHandle, relPath, bytes);
  }
}

function parseU32Param(params, name, fallback) {
  const raw = params.get(name);
  if (raw == null) return fallback;
  const n = Number.parseInt(raw, 10);
  if (!Number.isInteger(n) || n < 0 || n > 0xffffffff) {
    throw new Error(`invalid ${name}: ${raw}`);
  }
  return n;
}

function parseNullableU32Param(params, name) {
  const raw = params.get(name);
  if (raw == null) return null;
  return parseU32Param(params, name, 0);
}

async function loadExportsManifest(exportsUrl) {
  const resp = await fetch(exportsUrl.toString(), { redirect: "error" });
  if (!resp.ok) throw new Error(`failed to fetch exports manifest: ${resp.status}`);
  const json = await resp.json();
  if (json?.schema !== "flix-llvm-wasm-exports-v0") {
    throw new Error(`unsupported exports manifest schema: ${json?.schema ?? "<missing>"}`);
  }
  const defs = Array.isArray(json.defs) ? json.defs : [];
  return defs;
}

function selectDefId(defs, params) {
  const defIdRaw = params.get("defId");
  if (defIdRaw != null) {
    try {
      const v = BigInt(defIdRaw);
      if (v < 0n) throw new Error("negative");
      return v;
    } catch {
      throw new Error(`invalid defId: ${defIdRaw}`);
    }
  }

  const symbol = params.get("symbol");
  if (symbol != null) {
    const hit = defs.find((d) => d?.symbol === symbol);
    if (!hit) throw new Error(`symbol not found in exports manifest: ${symbol}`);
    return BigInt(hit.defId);
  }

  const main = defs.find((d) => d?.isMain === true);
  if (!main) throw new Error("no main def found (pass ?defId= or ?symbol=)");
  return BigInt(main.defId);
}

function getDefMeta(defs, defId) {
  const hit = defs.find((d) => BigInt(d?.defId ?? -1) === defId);
  if (!hit) return null;
  const params = Array.isArray(hit.params) ? hit.params.map(String) : [];
  const result = typeof hit.result === "string" ? hit.result : "";
  return { params, result, symbol: hit.symbol ?? "" };
}

function boxUnit(runtime, ctx) {
  // Unit is represented as `0` in the v0 runtime layout.
  // Prefer a dedicated `box-unit` once it exists in the substrate.
  return runtime.boxI32(ctx, 0);
}

async function main() {
  setOutcome("running");
  setStatus("running…");

  const params = new URLSearchParams(globalThis.location?.search ?? "");
  globalThis.__flix_args = params.getAll("argv");

  const componentParam = params.get("component");
  if (!componentParam) {
    setOutcome("error");
    setStatus("missing required query param: component");
    return;
  }

  const componentUrl = new URL(componentParam, globalThis.location.href);

  const exportsParam = params.get("exports");
  const exportsUrl = exportsParam
    ? new URL(exportsParam, globalThis.location.href)
    : new URL("../../flix_wasm_exports.json", componentUrl);

  const { runtime } = await import(componentUrl.toString());
  if (!runtime) {
    throw new Error(`component module did not export 'runtime': ${componentUrl}`);
  }

  const defs = await loadExportsManifest(exportsUrl);
  const defId = selectDefId(defs, params);
  const meta = getDefMeta(defs, defId);

  const budget = parseU32Param(params, "budget", 500);
  const maxRedirects = parseU32Param(params, "maxRedirects", 20);
  const httpTimeoutMs = parseNullableU32Param(params, "httpTimeoutMs");
  const seedParam = params.get("seed");

  const ctx = runtime.newCtx();
  const sandbox = await makeOpfsSandbox("flix-wasm-browser-");
  try {
    if (seedParam) {
      setStatus("seeding browser filesystem…");
      await preloadSeedFiles(sandbox.rootDirHandle, new URL(seedParam, globalThis.location.href));
    }

    const runner = new FlixRunner(runtime, {
      budget,
      maxRedirects,
      httpTimeoutMs,
      handlers: {
        ...makeOpfsFsHandlers({ rootDirHandle: sandbox.rootDirHandle }),
        ...makeUnsupportedProcessHandlers(),
      },
    });

    let boxed = [];
    try {
      // If a def only takes Unit params, allow hosts to omit args (JVM/Node parity).
      if (meta && meta.params.every((t) => t === "Unit")) {
        boxed = meta.params.map(() => boxUnit(runtime, ctx));
      } else if (meta && meta.params.length === 0) {
        boxed = [];
      } else {
        const sym = meta?.symbol?.length ? meta.symbol : "<def>";
        throw new Error(`unsupported args for ${sym} (defId=${defId})`);
      }

      const taskId = runtime.startTask(ctx, defId, boxed);
      const out = await runner.runTaskToCompletion(ctx, taskId);
      try {
        if (out.tag === "ok") {
          setOutcome("ok");
          setStatus("OK");
        } else if (out.tag === "thrown") {
          setOutcome("thrown");
          setStatus("THROWN");
        } else {
          setOutcome("error");
          setStatus(`unexpected outcome tag: ${out.tag}`);
        }
      } finally {
        maybeDispose(out.val);
      }
    } finally {
      boxed.forEach(maybeDispose);
    }
  } finally {
    maybeDispose(ctx);
    await sandbox.cleanup();
  }
}

try {
  await main();
} catch (e) {
  setOutcome("error");
  const msg = e instanceof Error ? e.message : String(e);
  setStatus(`ERROR: ${msg}`);
  console.error(e);
}
