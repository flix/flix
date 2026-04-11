# Wasm SQLite Example

This project is the wasm half of the cross-target SQLite example.

It does not use direct `extern wasm` imports. SQLite wants resource handles and,
in the browser, often a worker-backed execution model. The honest wasm surface is
therefore a WIT world with resources, generated into Flix effects.

Canonical WIT source:

- [../contracts/wit/world.wit](../contracts/wit/world.wit)

## Generate Bindings

Bindings are declared in `flix.toml` with `[[bindings.wasm]]`.

`flix check` and `flix build --target wasm` generate the wasm effects bundle automatically into:

- `build/wasm/generated/bindings/wasm/...`

## Build

```bash
flix check
flix build --target wasm
```

## Hosts

The example now includes two concrete hosts under `host/`:

- `host/wasm-js/`
  - `node-main.mjs` runs the demo under Node against the official SQLite wasm OO1 API
  - `browser/` runs the same WIT world in the browser through a dedicated module worker
- `host/wasm-rust/`
  - runs the same WIT world in Wasmtime
  - uses raw SQLite handles through `libsqlite3-sys`, not a higher-level Rust wrapper, so the host resource model stays aligned with the WIT contract

The browser host intentionally does not use the Worker1 Promiser API. This demo
needs explicit prepared-statement resource handles, so the correct boundary is
our own worker on top of SQLite's OO1 API.

## Run

Build the guest first:

```bash
flix build --target wasm
```

Run the Node smoke host:

```bash
cd host/wasm-js
npm install
node node-main.mjs
```

Run the Wasmtime host:

```bash
cd host/wasm-rust
cargo +stable run
```

Run the browser host:

```bash
cd host/wasm-js
npm install
node serve.mjs
```

Then open the served URL in a browser with module-worker support.

For the full automated cross-target smoke path, run:

```bash
./gradlew test --tests ca.uwaterloo.flix.SqliteCrossTargetSuite
```
