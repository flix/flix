import * as fs from "node:fs/promises";
import * as http from "node:http";
import * as path from "node:path";
import { fileURLToPath } from "node:url";

const ROOT_DIR = path.resolve(process.env.ROOT_DIR ?? fileURLToPath(new URL("../../..", import.meta.url)));

const MIME = {
  ".html": "text/html; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".mjs": "text/javascript; charset=utf-8",
  ".json": "application/json; charset=utf-8",
  ".map": "application/json; charset=utf-8",
  ".wasm": "application/wasm",
  ".css": "text/css; charset=utf-8",
  ".txt": "text/plain; charset=utf-8",
};

function send(res, status, body, headers = {}) {
  res.writeHead(status, { "content-length": Buffer.byteLength(body), ...headers });
  res.end(body);
}

function safeResolveUrlPath(urlPath) {
  const decoded = decodeURIComponent(urlPath);
  const rel = decoded.replace(/^[\\/]+/, "");
  const abs = path.resolve(ROOT_DIR, rel);
  if (!abs.startsWith(ROOT_DIR + path.sep) && abs !== ROOT_DIR) return null;
  return abs;
}

const port = Number(process.env.PORT ?? 8000);

const server = http.createServer(async (req, res) => {
  const url = new URL(req.url ?? "/", `http://${req.headers.host ?? "localhost"}`);
  const abs = safeResolveUrlPath(url.pathname);
  if (!abs) {
    send(res, 403, "forbidden\n", { "content-type": "text/plain; charset=utf-8" });
    return;
  }

  let st;
  try {
    st = await fs.stat(abs);
  } catch {
    send(res, 404, "not found\n", { "content-type": "text/plain; charset=utf-8" });
    return;
  }

  if (st.isDirectory()) {
    send(res, 403, "directory listing disabled\n", { "content-type": "text/plain; charset=utf-8" });
    return;
  }

  const ext = path.extname(abs).toLowerCase();
  const ctype = MIME[ext] ?? "application/octet-stream";

  try {
    const body = await fs.readFile(abs);
    res.writeHead(200, {
      "content-type": ctype,
      "content-length": body.byteLength,
      // Useful for future wasm threads experiments; harmless for current smoke.
      "cross-origin-opener-policy": "same-origin",
      "cross-origin-embedder-policy": "require-corp",
    });
    res.end(body);
  } catch (e) {
    send(res, 500, `${e instanceof Error ? e.message : String(e)}\n`, { "content-type": "text/plain; charset=utf-8" });
  }
});

server.listen(port, "127.0.0.1", () => {
  const addr = server.address();
  const boundPort = typeof addr === "object" && addr ? addr.port : port;
  console.log(`[wasm-runner] serving ${ROOT_DIR} at http://127.0.0.1:${boundPort}/`);
  console.log(`[wasm-runner] browser runner: http://127.0.0.1:${boundPort}/tools/wasm-runner-js/browser/run_component.html`);
});
