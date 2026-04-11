# Flix Wasm JS Runner (Draft)

This directory contains a **small JS “runner”** for Flix wasm components compiled against the
handle-based WIT substrate (`flix:runtime@0.1.0`).

It is intended to be:

- **Browser-first** (uses `fetch` + `setTimeout`),
- compatible with **Node** (when `fetch` is available), and
- extensible via **pluggable op handlers** for capabilities like filesystem, TCP, and process.

## What It Does

Given a `runtime` object produced by **`jco transpile`** (i.e. a module exporting
`newCtx/startTask/schedStep/pollTask/suspensionRequest` and typed `resume-*` functions), the runner:

- repeatedly calls `schedStep(ctx, budget)` to advance the cooperative scheduler,
- dispatches each `op-request` to a handler,
- and resumes each suspension exactly once.

Default handlers:

- `timer-sleep`: implemented via `setTimeout`.
- `http-request`: implemented via `fetch` with explicit redirect handling (portable semantics).

If no handler is available for an op, the runner resumes it with:

- `IoError { kindCode: 12, msg: "unsupported" }` (portable `Unsupported`).

## CLI (Node)

For a quick way to run a transpiled Flix wasm component in Node (driving the cooperative scheduler):

```bash
node tools/wasm-runner-js/run-flix.mjs --js build/llvm/wasm/js/flix-llvm-wasm.component.js
```

Notes:

- Uses `build/llvm/flix_wasm_exports.json` to select `main` by default.
- Pass `--defId <u64>` or `--symbol <sym>` to run a specific exported def.
- For `main(Unit)` entrypoints, the CLI auto-supplies a Unit argument.

For Node-based embeddings, a filesystem handler set is available in:

- `tools/wasm-runner-js/node-handlers.mjs`

For browser embeddings with OPFS (Origin Private File System), a filesystem handler set is available in:

- `tools/wasm-runner-js/opfs-handlers.mjs`

And a TCP handler set is available in:

- `tools/wasm-runner-js/node-tcp-handlers.mjs`

And a process handler set is available in:

- `tools/wasm-runner-js/node-process-handlers.mjs`

## Browser Harness

To execute a transpiled component in a real browser:

1. Serve the repo root (adds COOP/COEP headers for future threads work):

   ```bash
   PORT=8000 node tools/wasm-runner-js/browser/serve.mjs
   ```

2. Open the runner page with query params:

   - `component`: URL path to `flix-llvm-wasm.component.js`
   - `exports`: URL path to `flix_wasm_exports.json`

Example (using `build/llvm`):

```text
http://127.0.0.1:8000/tools/wasm-runner-js/browser/run_component.html?component=/build/llvm/wasm/js/flix-llvm-wasm.component.js&exports=/build/llvm/flix_wasm_exports.json
```

For headless Chrome runs (used by the Scala browser runtime suite), see:

- `tools/wasm-runner-js/browser/run_headless_chrome.mjs`
