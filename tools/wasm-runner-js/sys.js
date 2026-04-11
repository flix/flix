// Host implementation for the WIT interface `flix:sys/sys@0.1.0`.
//
// Intended for `jco transpile` output via `--map "flix:sys/sys=./sys.js"`.
//
// Design goals:
// - Work in both Node and browsers (no Node-only APIs).
// - Stay minimal; higher-level op handlers live in `tools/wasm-runner-js/*-handlers.mjs`.

export function log(level, msg) {
  if (!Array.isArray(globalThis.__flix_logs)) {
    globalThis.__flix_logs = [];
  }

  // `level` is a WIT enum lifted as a string, e.g. "info".
  //
  // Keep `info` unadorned to preserve `println`-style output semantics across targets.
  // (The Flix portable stdlib currently uses `info` for user-facing console output.)
  if (level === "info") {
    const line = String(msg);
    globalThis.__flix_logs.push(line);
    console.log(line);
  } else {
    const line = `[flix:${level}] ${msg}`;
    globalThis.__flix_logs.push(line);
    console.log(line);
  }
}

export function timeNowMs() {
  // WIT expects `s64`, lifted as JS BigInt by jco.
  return BigInt(Date.now());
}

export function randomBytes(len) {
  const bytes = new Uint8Array(len);

  // Prefer WebCrypto if available.
  const cryptoObj = globalThis.crypto;
  if (cryptoObj?.getRandomValues) {
    cryptoObj.getRandomValues(bytes);
    return bytes;
  }

  // Deterministic fallback (not cryptographic).
  for (let i = 0; i < bytes.length; i++) {
    bytes[i] = (i * 31) & 0xff;
  }
  return bytes;
}

export function getArgs() {
  // WIT expects `list<string>`, lifted as `string[]` by jco.
  //
  // Default to `[]` unless the host explicitly configures args.
  const args = globalThis.__flix_args;
  if (Array.isArray(args)) {
    return args.map((x) => String(x));
  }
  return [];
}

export function hasCapability(cap) {
  // `cap` is a WIT enum lifted as a string (e.g. "http").
  //
  // Keep capability checks conservative:
  // - Only claim a capability if the JS host environment can plausibly implement it.
  // - The portable stdlib should still treat capabilities as optional.
  const isNode = typeof process !== "undefined" && !!process.versions?.node;

  switch (cap) {
    case "http":
      return typeof globalThis.fetch === "function";
    case "filesystem":
      return isNode || typeof navigator?.storage?.getDirectory === "function";
    case "sockets":
    case "process":
      return isNode;
    case "threads":
      // Best-effort. Browser threads require COOP/COEP (crossOriginIsolated).
      return (
        isNode ||
        (globalThis.crossOriginIsolated === true &&
          typeof globalThis.SharedArrayBuffer !== "undefined")
      );
    default:
      return false;
  }
}
