#!/usr/bin/env node
import * as fs from "node:fs/promises";
import * as net from "node:net";
import * as os from "node:os";
import * as path from "node:path";
import { spawn } from "node:child_process";

function usage(code) {
  console.error(`usage:
  node tools/wasm-runner-js/browser/run_headless_chrome.mjs --url <url> [options]

options:
  --chrome <path>        Chrome/Chromium binary (or set FLIX_CHROME)
  --headless <bool>      Default: true
  --timeoutMs <u32>      Default: 60000

notes:
  - Uses Chrome DevTools Protocol (CDP) to wait until the page reports completion via:
      document.documentElement.getAttribute("data-flix-status") === "ok"
  - Exits 0 on ok, 1 on thrown/error/timeout.
`);
  process.exit(code);
}

function parseArgs(argv) {
  const out = {};
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    switch (a) {
      case "--url":
      case "--chrome":
      case "--headless":
      case "--timeoutMs": {
        const v = argv[i + 1];
        if (v == null) usage(2);
        out[a.slice(2)] = v;
        i++;
        break;
      }
      case "-h":
      case "--help":
        usage(0);
        break;
      default:
        console.error(`unknown arg: ${a}`);
        usage(2);
    }
  }
  if (!out.url) usage(2);
  return out;
}

function parseU32(x, name, fallback) {
  if (x == null) return fallback;
  const n = Number.parseInt(String(x), 10);
  if (!Number.isInteger(n) || n < 0 || n > 0xffffffff) {
    throw new Error(`invalid ${name}: ${x}`);
  }
  return n;
}

function parseBool(x, name, fallback) {
  if (x == null) return fallback;
  const v = String(x).toLowerCase();
  if (v === "true" || v === "1" || v === "yes") return true;
  if (v === "false" || v === "0" || v === "no") return false;
  throw new Error(`invalid ${name}: ${x}`);
}

async function getFreePort() {
  return new Promise((resolve, reject) => {
    const s = net.createServer();
    s.on("error", reject);
    s.listen(0, "127.0.0.1", () => {
      const addr = s.address();
      const port = typeof addr === "object" && addr ? addr.port : 0;
      s.close(() => resolve(port));
    });
  });
}

async function sleep(ms) {
  await new Promise((resolve) => setTimeout(resolve, ms));
}

async function waitForExit(proc, timeoutMs) {
  if (proc.exitCode !== null || proc.signalCode !== null) return true;
  return await new Promise((resolve) => {
    const onExit = () => {
      clearTimeout(timer);
      resolve(true);
    };
    const timer = setTimeout(() => {
      proc.off("exit", onExit);
      resolve(false);
    }, timeoutMs);
    proc.once("exit", onExit);
  });
}

async function terminateProcess(proc) {
  if (proc.exitCode !== null || proc.signalCode !== null) return;
  proc.kill("SIGTERM");
  if (await waitForExit(proc, 2_000)) return;
  proc.kill("SIGKILL");
  await waitForExit(proc, 2_000);
}

async function removeTreeWithRetries(dir, retries = 20, delayMs = 100) {
  for (let attempt = 0; attempt <= retries; attempt++) {
    try {
      await fs.rm(dir, { recursive: true, force: true });
      return;
    } catch (err) {
      const code = typeof err?.code === "string" ? err.code : "";
      if (!["ENOTEMPTY", "EBUSY", "EPERM"].includes(code) || attempt === retries) {
        throw err;
      }
      await sleep(delayMs);
    }
  }
}

async function fetchJson(url, timeoutMs) {
  const ac = new AbortController();
  const t = setTimeout(() => ac.abort(new Error("timeout")), timeoutMs);
  try {
    const resp = await fetch(url, { signal: ac.signal });
    if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
    return await resp.json();
  } finally {
    clearTimeout(t);
  }
}

function makeCdpClient(ws) {
  let nextId = 1;
  const pending = new Map();
  const events = new Map();

  function on(method, cb) {
    if (!events.has(method)) events.set(method, []);
    events.get(method).push(cb);
  }

  function send(method, params, sessionId = null) {
    const id = nextId++;
    const msg = { id, method, params };
    if (sessionId) msg.sessionId = sessionId;
    ws.send(JSON.stringify(msg));
    return new Promise((resolve, reject) => {
      pending.set(id, { resolve, reject });
    });
  }

  ws.addEventListener("message", (ev) => {
    let msg;
    try {
      msg = JSON.parse(String(ev.data));
    } catch {
      return;
    }

    if (msg.id != null) {
      const p = pending.get(msg.id);
      if (!p) return;
      pending.delete(msg.id);
      if (msg.error) {
        p.reject(new Error(msg.error.message ?? "CDP error"));
      } else {
        p.resolve(msg.result);
      }
      return;
    }

    if (msg.method) {
      const cbs = events.get(msg.method) ?? [];
      for (const cb of cbs) {
        try {
          cb(msg.params, msg.sessionId ?? null);
        } catch {
          // Ignore event handler errors.
        }
      }
    }
  });

  return { send, on };
}

async function main() {
  const args = parseArgs(process.argv.slice(2));
  const url = String(args.url);

  const chrome = args.chrome ?? process.env.FLIX_CHROME;
  if (!chrome) {
    throw new Error("missing --chrome (or set FLIX_CHROME)");
  }

  const timeoutMs = parseU32(args.timeoutMs, "--timeoutMs", 60_000);
  const headless = parseBool(args.headless, "--headless", true);

  const debugPort = await getFreePort();
  const userDataDir = await fs.mkdtemp(path.join(os.tmpdir(), "flix-headless-chrome-"));

  const chromeArgs = [
    "--disable-gpu",
    "--no-first-run",
    "--no-default-browser-check",
    "--disable-background-networking",
    "--disable-sync",
    "--disable-translate",
    "--metrics-recording-only",
    "--mute-audio",
    `--user-data-dir=${userDataDir}`,
    `--remote-debugging-port=${debugPort}`,
    "about:blank",
  ];
  if (headless) {
    chromeArgs.unshift("--headless=new");
  }

  const p = spawn(chrome, chromeArgs, { stdio: ["ignore", "pipe", "pipe"] });
  let stderr = "";
  p.stderr.on("data", (b) => (stderr += b.toString("utf8")));

  const deadline = Date.now() + timeoutMs;
  try {
    // Wait for the CDP HTTP endpoint to come up.
    while (Date.now() < deadline) {
      try {
        await fetchJson(`http://127.0.0.1:${debugPort}/json/version`, 500);
        break;
      } catch {
        await sleep(50);
      }
    }

    const version = await fetchJson(`http://127.0.0.1:${debugPort}/json/version`, 2_000);
    const wsUrl = version.webSocketDebuggerUrl;
    if (!wsUrl) throw new Error("CDP: missing browser webSocketDebuggerUrl");

    const ws = new WebSocket(wsUrl);
    await new Promise((resolve, reject) => {
      ws.addEventListener("open", resolve, { once: true });
      ws.addEventListener("error", () => reject(new Error("CDP websocket error")), { once: true });
    });

    const cdp = makeCdpClient(ws);

    const created = await cdp.send("Target.createTarget", { url });
    const targetId = created?.targetId;
    if (!targetId) throw new Error("CDP: Target.createTarget returned no targetId");

    const attached = await cdp.send("Target.attachToTarget", { targetId, flatten: true });
    const sessionId = attached?.sessionId;
    if (!sessionId) throw new Error("CDP: Target.attachToTarget returned no sessionId");

    await cdp.send("Page.enable", {}, sessionId);
    await cdp.send("Runtime.enable", {}, sessionId);

    const consoleErrBuf = [];
    const exnBuf = [];

    cdp.on("Runtime.consoleAPICalled", (params, sid) => {
      if (sid !== sessionId) return;
      const t = params?.type;
      if (t !== "error" && t !== "warning") return;
      const args = Array.isArray(params?.args) ? params.args : [];
      const rendered = args
        .map((a) => {
          if (typeof a?.value === "string") return a.value;
          if (a?.description) return a.description;
          return String(a?.value ?? "");
        })
        .join(" ")
        .trim();
      if (rendered.length === 0) return;
      consoleErrBuf.push(`[console:${t}] ${rendered}`);
    });

    cdp.on("Runtime.exceptionThrown", (params, sid) => {
      if (sid !== sessionId) return;
      const details = params?.exceptionDetails ?? {};
      const text = details.text ?? "exception";
      const exc = details.exception;
      const desc = typeof exc?.description === "string" ? exc.description : "";
      exnBuf.push(`[exception] ${text}${desc ? `: ${desc}` : ""}`.trim());
    });

    async function readHostLogs() {
      try {
        const res = await cdp.send("Runtime.evaluate", {
          expression: "Array.isArray(globalThis.__flix_console_lines) ? globalThis.__flix_console_lines : (Array.isArray(globalThis.__flix_logs) ? globalThis.__flix_logs : [])",
          returnByValue: true,
        }, sessionId);
        return Array.isArray(res?.result?.value) ? res.result.value.map((x) => String(x)) : [];
      } catch {
        return [];
      }
    }

    // Poll until the page reports completion.
    while (Date.now() < deadline) {
      let status = null;
      try {
        const res = await cdp.send("Runtime.evaluate", {
          expression: "document.documentElement.getAttribute('data-flix-status')",
          returnByValue: true,
        }, sessionId);
        status = res?.result?.value ?? null;
      } catch {
        // Page may be navigating; retry.
      }

      if (status === "ok") {
        const hostLogs = await readHostLogs();
        ws.close();
        for (const line of hostLogs) console.log(line);
        if (hostLogs.length === 0) {
          for (const line of consoleErrBuf) console.error(line);
        }
        process.exitCode = 0;
        return;
      }

      if (status === "thrown" || status === "error") {
        let msg = "";
        try {
          const res = await cdp.send("Runtime.evaluate", {
            expression: "document.getElementById('status')?.textContent ?? ''",
            returnByValue: true,
          }, sessionId);
          msg = String(res?.result?.value ?? "");
        } catch {
          // Ignore.
        }

        const hostLogs = await readHostLogs();
        ws.close();
        console.error(`[headless] status=${status} ${msg}`);
        if (hostLogs.length > 0) {
          for (const line of hostLogs) console.error(line);
        } else {
          for (const line of consoleErrBuf) console.error(line);
        }
        for (const line of exnBuf) console.error(line);
        process.exitCode = 1;
        return;
      }

      await sleep(50);
    }

    throw new Error("timeout waiting for data-flix-status");
  } finally {
    await terminateProcess(p);
    await removeTreeWithRetries(userDataDir);
    if (stderr.trim().length > 0) {
      // Only print stderr on failure to keep test output small.
      if (process.exitCode && process.exitCode !== 0) {
        console.error(stderr.trim());
      }
    }
  }
}

main().catch((e) => {
  console.error(e instanceof Error ? e.message : String(e));
  process.exit(1);
});
