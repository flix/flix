import http from "node:http";
import * as fs from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

const rootDir = fileURLToPath(new URL("../../", import.meta.url));
const port = Number(process.env.PORT ?? "4173");

const mimeTypes = {
  ".css": "text/css; charset=utf-8",
  ".html": "text/html; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".json": "application/json; charset=utf-8",
  ".mjs": "text/javascript; charset=utf-8",
  ".wasm": "application/wasm",
};

function safeJoin(root, requestPath) {
  const clean = path.normalize(requestPath).replace(/^(\.\.[/\\])+/, "");
  const full = path.join(root, clean);
  if (!full.startsWith(root)) throw new Error("path escape");
  return full;
}

const server = http.createServer(async (req, res) => {
  try {
    let requestPath = decodeURIComponent(new URL(req.url, `http://${req.headers.host}`).pathname);
    if (requestPath === "/") {
      res.writeHead(302, {
        Location: "/host/wasm-js/browser/",
        "Cross-Origin-Embedder-Policy": "require-corp",
        "Cross-Origin-Opener-Policy": "same-origin",
      });
      res.end();
      return;
    }
    if (requestPath.endsWith("/")) requestPath += "index.html";
    const filePath = safeJoin(rootDir, requestPath);
    const body = await fs.readFile(filePath);
    res.writeHead(200, {
      "Content-Type": mimeTypes[path.extname(filePath)] ?? "application/octet-stream",
      "Cross-Origin-Embedder-Policy": "require-corp",
      "Cross-Origin-Opener-Policy": "same-origin",
    });
    res.end(body);
  } catch {
    res.writeHead(404, {
      "Content-Type": "text/plain; charset=utf-8",
      "Cross-Origin-Embedder-Policy": "require-corp",
      "Cross-Origin-Opener-Policy": "same-origin",
    });
    res.end("not found\n");
  }
});

server.listen(port, () => {
  console.log(`Serving ${rootDir} at http://127.0.0.1:${port}/`);
});
