# Wasm Effects Smoke

Minimal end-to-end example for async wasm effect bindings.

This example shows the current import-side workflow:

1. declare async effect bindings from a WIT world in `flix.toml`
2. build the Flix project to `llvm-wasm`
3. let project mode generate the bindings bundle under `build/wasm/generated/...`
4. run the wasm component from:
   - a JS host
   - a Rust/Wasmtime host

The WIT source is in [wit/world.wit](./wit/world.wit).
The Flix app is in [src/Api.flix](./src/Api.flix).

## Binding Declaration

Bindings are declared in `flix.toml` with `[[bindings.wasm]]`.

`flix check` and `flix build --target wasm` generate the bindings bundle automatically into:

- `build/wasm/generated/bindings/wasm/...`

## Build

```bash
flix build --target wasm
```

## JS Host

After building:

```bash
node host/wasm-js/main.mjs
```

## Rust / Wasmtime Host

After building:

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
