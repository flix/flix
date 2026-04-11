# Wasm Effects Smoke

Minimal end-to-end example for async wasm effect bindings.

This example shows the current import-side workflow:

1. generate async effect bindings from a WIT world
2. vendor the generated Flix source into the project
3. build the Flix project to `llvm-wasm`
4. run the wasm component from:
   - a JS host
   - a Rust/Wasmtime host

The WIT source is in [wit/world.wit](./wit/world.wit).
The Flix app is in [src/Api.flix](./src/Api.flix).

## Generate Bindings

From this directory:

```bash
bash scripts/generate-bindings.sh
```

The script resolves `flix` in this order:

- `FLIX=/path/to/flix`
- `flix` on `PATH`
- the repo-local launcher at `build/install/flix/bin/flix`

That generates a structured SDK bundle under `generated/` and copies `generated/flix/Wit.flix` to `src/Wit.flix`.

Current limitation:

- project-mode `flix build` consumes project sources under `src/`, so the generated Flix module is currently vendored into `src/Wit.flix` before build

## Build

After generating bindings:

```bash
flix build --target wasm
```

## JS Host

After generating bindings and building:

```bash
node host/wasm-js/main.mjs
```

## Rust / Wasmtime Host

After generating bindings and building:

```bash
cargo +stable run --manifest-path host/wasm-rust/Cargo.toml
```

## What This Exercises

- async wasm effect bindings generated from WIT
- imported WIT resources
- generated JS host SDK
- generated Rust/Wasmtime host crate
- wasm build output plus unknown-effect dispatch

If everything is wired correctly, both hosts print:

```text
OK
```
