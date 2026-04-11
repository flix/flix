# SQLite Cross-Target

This example suite is intended to pressure the current Flix interop story with one
real library: SQLite.

The goal is not to build an interesting application. The goal is to exercise the
same database engine across:

- `llvm-native` through native C FFI
- `llvm-wasm` in the browser through WIT resource imports
- `llvm-wasm` in Wasmtime through the same WIT resource world

## Why This Is Structured As A Suite

The integration boundary is not actually the same on every target:

- native can call a curated C shim directly
- browser and Wasmtime cannot honestly reuse that raw C ABI
- wasm should use WIT resources and generated effect bindings instead of faking
  resource handles as naked integers

So this example uses one logical SQLite bridge contract with two target-specific
surface descriptions:

- [native-bridge/](./native-bridge/)
- [contracts/wit/world.wit](./contracts/wit/world.wit)

Those contracts intentionally describe the same operations and ownership model:

- open a database
- execute schema SQL
- prepare statements
- bind text and blob parameters
- step and reset statements
- read text and blob columns
- close/finalize resources

## Layout

- `contracts/`
  Canonical wasm-side bridge definitions.
- `native-bridge/`
  Reusable native bridge package for the curated SQLite C surface.
- `native/`
  Native Flix project consuming the sibling `native-bridge` package through a local path dependency.
- `wasm/`
  Wasm Flix project generated from the WIT world.

## Current State

This example suite now uses the intended interop workflow:

- bindings are declared in each project's `flix.toml`
- native binding semantics live in a sidecar spec instead of C comments
- local reusable native bridges can live in sibling Flix packages and be consumed through local path dependencies during workspace development
- publishable reuse is the packaged bridge path, not a local path dependency
- `flix check` / `flix build` generate bindings into `build/<target>/generated/bindings/`
- native bridge compilation is declared in `flix.toml`
- native system library discovery can be declared through `pkg-config` instead of hard-coded `-l...` flags
- those `pkg-config` C flags also feed native header translation, so curated bridge headers can include system headers without duplicating include-path metadata in `[[bindings.native]]`
- generated Flix modules are no longer vendored into `src/`
- the native example now builds and runs end to end against system `sqlite3`
- the SQLite column/error accessors are marked `effect = "IO"` in the sidecar spec intentionally because they observe mutable statement/database state even though the C signatures are read-only

The suite now has real host implementations:

- native: curated C bridge packaged in [native-bridge](./native-bridge/) and consumed from [native](./native/) via a local path dependency
- browser/Node: official SQLite wasm OO1 API wrapped behind the generated WIT effect surface
- Wasmtime: Rust host using raw SQLite handles through `libsqlite3-sys`

The important design choice on the wasm side is deliberate: the browser host uses
its own module worker on top of SQLite's OO1 API instead of Worker1 Promiser,
because this example needs explicit prepared-statement resources and lifetime
management.

Current verification status:

- native example builds and runs, returning `1:hello:4`
- Node wasm host builds and runs, returning `OK`
- Wasmtime host builds and runs with `cargo +stable run`, returning `OK`
- browser host passes a real headless Chromium run, returning `1:hello:4`

There is also a dedicated end-to-end smoke suite for this example:

```bash
./gradlew test --tests ca.uwaterloo.flix.SqliteCrossTargetSuite
```
